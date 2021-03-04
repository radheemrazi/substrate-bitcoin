// the imports 
use super::Aura;
use codec::{Decode, Encode};
use frame_support::{
	decl_event, decl_module, decl_storage,
	dispatch::{DispatchResult, Vec},
	ensure,
};
use sp_core::{H256, H512};
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};
use sp_core::sr25519::{Public, Signature};
use sp_runtime::traits::{BlakeTwo256, Hash, SaturatedConversion};
use sp_std::collections::btree_map::BTreeMap;
use sp_runtime::transaction_validity::{TransactionLongevity, ValidTransaction};

// casting custom types for ease
pub type Value = u128;
// the defining trait for the module 
pub trait Trait: system::Trait {
	type Event: From<Event> + Into<<Self as system::Trait>::Event>;
}

// Transaction input: The input UTXOs for a transaction
#[cfg_attr(feature = "std",derive(Serialize,Deserialize))] // if features set to std in toml then import serialize and deserialize
#[derive(PartialEq,Eq,PartialOrd,Ord,Default,Clone,Encode,Decode,Hash,Debug)] //this struct inherits all these traits(properties)
pub struct TransactionInput{
	pub outpoint: H256, // the transaction UTXO we are spending in this transaction
	pub sigscript: H512, // the prood or digital sig of owner
}

// Transaction output: The output UTXO for a transaction
#[cfg_attr(feature = "std",derive(Serialize,Deserialize))]
#[derive(PartialEq,Eq,PartialOrd,Ord,Default,Clone,Encode,Decode,Hash,Debug)]
pub struct TransactionOutput{
	pub value: Value, // the amount to be spent
	pub pubkey: H256, // the owner's pub key 
}

// The transaction struct encapsulating all input and output UTXOs
#[cfg_attr(feature = "std",derive(Serialize,Deserialize))]  
#[derive(PartialEq,Eq,PartialOrd,Ord,Default,Clone,Encode,Decode,Hash,Debug)]
pub struct Transaction{
	pub input: Vec<TransactionInput>,
	pub output: Vec<TransactionOutput>,
}

decl_storage! {
	trait Store for Module<T: Trait> as Utxo {
		// contains all the UTXOs in the system, build intializes the system with some UTXO from the config
		UtxoStore build(|config: &GenesisConfig|{ // from GenesisConfig(a json)
			config.genesis_utxos // get genesis_utxos
			.iter() // iterate over the entries
			.cloned() // clone them
			.map(|u|(BlakeTwo256::hash_of(&u),u)) // create a map where the hash of the UTXO maps to the UTXO 
			.collect::<Vec<_>>() //create a collection in a vector 
		}): map hasher(identity) H256 => Option<TransactionOutput>; // populate the store with what we just built

		pub RewardTotal get(fn reward_total): Value; // the total reward gathered in a block  

	}
	add_extra_genesis{
		config(genesis_utxos): Vec<TransactionOutput>
	}
}

// External functions: callable by the end user, hence their response will always be DispatchResult 
decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;
		// Allows a user to spend some UTXO they own
		pub fn spend(_origin, transaction: Transaction) -> DispatchResult{
			// check if the transaction is valid 
			let reward = Self::validate_transaction(&transaction)?;

			// adds the new UTXOs and removes the spent ones 
			let reward: Value = 0;
			Self::update_storage(&transaction,reward)?;
			
			// emit event indicating successful execution

			Ok(())
		}

		//  A custom substrate function for implementing some custom logic at end of each block
		fn on_finalize(){
			// get a list of AURA authorities for the epoch 
			let auth: Vec<_> = Aura::authorities().iter().map(|x|{
				let r: &Public = x.as_ref(); // here Public is public key type for sp_io pallet
				r.0.into() 
			}).collect(); // casts into a vec of type whatever it recieves
			// disperse the reward among these authorities
			Self::disperse_reward(&auth);
		} 
	}
}

decl_event! {
	pub enum Event {
		TransactionSuccess(Transaction),
	}
}

impl<T:Trait> Module<T> {

	fn update_storage(transaction: &Transaction, reward:Value) -> DispatchResult{
		// add reward form the transaction in to total reward 
		let new_total = <RewardTotal>::get().checked_add(reward).ok_or("Reward total phat gaya!!!")?;
		<RewardTotal>::put(new_total);
		
		// remove UTXOs used as input in the current transaction 
		for input in &transaction.input{
			<UtxoStore>::remove(input.outpoint);  
		}
		// add the new UTXOs generated from the transaction
		let mut index:u64 = 0; // this is a counter over utxo to provide uniqueness
		for output in &transaction.output{
			let hash = BlakeTwo256::hash_of(&(&transaction.encode(),index));
			index = index.checked_add(1).ok_or("output index phat gaya")?; // index++
			<UtxoStore>::insert(hash,output); // insert transaction against the hash of the transaction
		}
		Ok(())
	}

	fn disperse_reward(authorities: &[H256]){
		// divide reward among all authorities
		let reward = <RewardTotal>::take();
		// divide the total reward by the number of AURA authorities
		let share_value: Value = reward.checked_div(authorities.len() as Value).ok_or("No authorities(share value phat gaya!!!!)").unwrap();
		// if the share value turns out to be  save further coimputation and return 
		if share_value == 0 {
			return
		}
		// in case there was a remainder after intial divide of total reward just save remainder for next epoch (so put in RewardTotal)
		let remainder = reward.checked_sub(share_value * authorities.len() as Value).ok_or("remainder phat gaya").unwrap();
		<RewardTotal>::put(remainder);

		// create UTXO per validator 
		for authority in authorities{
			// the utxo to be stored
			let utxo = TransactionOutput{
				value:share_value,
				pubkey: *authority,
			};
			// compute hash for UTXO and put in UTXO store
			let hash = BlakeTwo256::hash_of(&(&utxo,<system::Module<T>>::block_number().saturated_into::<u64>()));  // this is a tuple of validator's address and block number for uniqueness
			// update UTXO store
			if !<UtxoStore>::contains_key(hash){
				<UtxoStore>::insert(hash,utxo);
			}
			else{
				sp_runtime::print("collision occured in reward allocation"); // incase our systen generates two identical UTXOs or a hash collision with previous
			}
		}
	}

	pub fn get_simple_transaction(transaction:&Transaction)-> Vec<u8>{
		// turns a complete transaction in to a simple transaction by removing all signatures in order to match transaction signature with perticular signer 
		let mut trx = transaction.clone();
		for input in trx.input.iter_mut(){
			input.sigscript = H512::zero();
		}
		trx.encode()
	}
	pub fn validate_transaction(transaction: &Transaction) -> Result<Value,&'static str>{
		// validates a transaction{
		// 1. input and output Vec arent empty
		// 2. each input and output is unique
		// 3. new Outputs dont collide with previous UTXO/ Replay attack not possible 
		// 4. Each UTXO is signed by the owner 
		// 5. UTXO is tamper proof
		// } 

		// ensuring not empty
		ensure!(!transaction.input.is_empty(), "Empty transaction inputs");
		ensure!(!transaction.output.is_empty(), "Empty transaction outputs");
		
		// ensuring not repeated, to avoid double spending 
		{
			let input_set: BTreeMap<_,()> = transaction.input.iter().map(|input| {(input,())}).collect();
			ensure!(input_set.len() == transaction.input.len(), "UTXO repeated input");
		}
		{
			let output_set: BTreeMap<_,()> = transaction.output.iter().map(|input| {(input,())}).collect();
			ensure!(output_set.len() == transaction.output.len(), "UTXO repeated output");
		}
		// Ensuring signed by the respective owners, inorder to do so we first  convert the transaction into a simple transaction then we hash it with the spender's key and match it with the owner's signature.
		// The matching part is done by sp_io::crypto::sr25519_verify  
		let simple_transaction = Self::get_simple_transaction(transaction); //simple transaction created
		let mut total_input:Value = 0;
		let mut total_output:Value = 0;
		
		for input in transaction.input.iter(){
			// check if the transaction referred in transaction input actually exists in the UTXO store
			// get the UTXO from UtxoStore given its hash 
			if let Some(input_utxo) = <UtxoStore>::get(&input.outpoint) {
				// if UTXO existthen does it belong to the person who is trying to spend it 
				ensure!(sp_io::crypto::sr25519_verify(
					&Signature::from_raw(*input.sigscript.as_fixed_bytes()),
					&simple_transaction,
					&Public::from_h256(input_utxo.pubkey)
				), "invalid signature");
				// if all checks out then add the amount to total input value 
				total_input = total_input.checked_add(input_utxo.value).ok_or("total_input overflow")?; 
			}
			else{ 
				// if the UTXO doesnt exist then the person is either try double spending or making fake UTXO

			}

		}
		// now we validating if an output UTXO already exists to avoid double spending 
		let output_index: u64 = 0; // a output UTXO counter
		for output in transaction.output.iter(){
			ensure!(output.value > 0, "UTXO value zero"); // ensure there are no dummy transactions
			let hash = BlakeTwo256::hash_of(&(&transaction.encode(),output_index)); // get hash of the UTXO 
			ensure!(!<UtxoStore>::contains_key(hash), "hash reused, existing UTXO used again"); // compare the hash of Utxo to existing hash in UtxoStore
			total_output = total_output.checked_add(output.value).ok_or("total_output overflowed")?; //all good add output value to total output value 
		}
		ensure!(total_input >= total_output, "total output exceeds total input hence overspending");
		let reward = total_input.checked_sub(total_output).ok_or("reward underflowed")?;
		Ok(reward)
		
	}
}


/// Tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use frame_support::{assert_ok, assert_err, impl_outer_origin, parameter_types, weights::Weight};
	use sp_runtime::{testing::Header, traits::IdentityLookup, Perbill};
	use sp_core::testing::{KeyStore, SR25519};
	use sp_core::traits::KeystoreExt;

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;
	parameter_types! {
			pub const BlockHashCount: u64 = 250;
			pub const MaximumBlockWeight: Weight = 1024;
			pub const MaximumBlockLength: u32 = 2 * 1024;
			pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Call = ();
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
		type ModuleToIndex = ();
		type AccountData = ();
		type OnNewAccount = ();
		type OnKilledAccount = ();
	}
	impl Trait for Test {
		type Event = ();
	}

	type Utxo = Module<Test>;

}
