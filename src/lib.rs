/* Future TimeSteward API goals:
 *
 * – groups
 * I made up API functions for this before, but I'm not sure they were perfect.
 * We can use RowId's for group ids, but what kinds of things can be stored IN a group? Previously I said only RowId's could, which doesn't seem ideal. I think it would work to allow anything hashable. That would basically make a group behave like a HashSet. But then, why not make it behave like a HashMap instead? But accessor itself already behaves like a HashMap over RowId's – the essential thing groups do is to allow iterating particular subsets of that HashMap.
 *
 * – conveniences for serializing snapshots (not sure what)
 *
 * – be able to construct a new TimeSteward from any snapshot
 * Issues: should snapshots expose the predictors? Also, the predictors can't be serialized. So you'd probably actually have to construct a TimeSteward from snapshot + predictors. This requires a way to iterate all fields (and group data if we add groups) in a snapshot, which is similar to the previous 2 items (iterating the whole set, which is a special case of iterating a subset; and serializing requires you to iterate everything as well)
 *
 * – parallelism support for predictors and events
 * When an event or predictor gets invalidated while it is still running, it would be nice for it to save time by exiting early.
 * Moreover, it would probably be more efficient to discard invalidated fields than to preserve them for predictors/events that are in process. The natural way for the accessors to handle this is to have get() return None, which would mean that you can never safely unwrap() the result. We could provide a "unwrap or return" macro.
 *
 * – Optimization features
 * one possibility: user can provide a function FieldId->[(PredictorId, RowId)] that lists predictors you KNOW will be invalidated by a change to that field, then have that predictor run its get() calls with an input called "promise_inferred" or something so that we don't spend time and memory recording the dependency
 * another: a predictor might have a costly computation to find the exact time of a future event, which it won't need to do if it gets invalidated long before that time comes. For that, we can provide a defer_until(time) method
 *
 *
 * */

#![feature(unboxed_closures, fn_traits)]
#![feature (plugin, custom_derive)]
#![plugin (serde_macros)]
// #![plugin (quickcheck_macros)]

extern crate rand;
extern crate polynomial;
extern crate nalgebra;
extern crate roots;
#[macro_use]
extern crate glium;
extern crate serde;
extern crate serde_json;
extern crate bincode;
#[cfg (test)]
extern crate quickcheck;

use std::collections::HashMap;
use std::hash::{Hash, Hasher, SipHasher};
use std::any::Any;
use std::sync::Arc;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::borrow::Borrow;
use std::marker::PhantomData;
use rand::{Rng, ChaChaRng, SeedableRng};
use serde::{Serialize, Serializer, Deserialize};
use serde::ser::Error;
use serde::de;

mod insert_only;

// TODO check whether hashes in rust vary by CPU endianness?
// Answer: they do, so maybe stop using Hash for this sometime
// ( https://doc.rust-lang.org/std/hash/trait.Hasher.html
// "represents the ability to hash an arbitrary stream of bytes").
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct DeterministicRandomId {
  data: [u64; 2],
}
pub struct SiphashIdGenerator {
  data: [SipHasher; 2],
}
impl Hasher for SiphashIdGenerator {
  fn finish(&self) -> u64 {
    panic!("Hack: this is actually for generating DeterministicRandomId, not 64-bit")
  }
  fn write(&mut self, bytes: &[u8]) {
    self.data[0].write(bytes);
    self.data[1].write(bytes);
  }
  // TODO: should we implement the others for efficiency?
}
impl SiphashIdGenerator {
  fn generate(&self) -> DeterministicRandomId {
    DeterministicRandomId { data: [self.data[0].finish(), self.data[1].finish()] }
  }
  fn new() -> SiphashIdGenerator {
    SiphashIdGenerator {
      data: [SipHasher::new_with_keys(0xb82a9426fd1a574f, 0x9d9d5b703dcb1bcc),
             SipHasher::new_with_keys(0x03e0d6037ff980a4, 0x65b790a0825b83bd)],
    }
  }
}
impl DeterministicRandomId {
  pub fn new<T: Hash>(data: &T) -> DeterministicRandomId {
    let mut s = SiphashIdGenerator::new();
    data.hash(&mut s);
    s.generate()
  }
}
impl fmt::Display for DeterministicRandomId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "id:{:016x}{:016x}", self.data[0], self.data[1])
  }
}


// Database analogy: time stewards kind of contain a database table.
// Rows' ids are special; there isn't an "id" column.
// Row ids are random 128 bit ids.
// Rows aren't explicitly inserted and deleted.
// Only the existence or nonexistence of a (row, column) pair
// -- a field -- is meaningful.
// Generally, space usage is proportional to the number of
// existent fields.  For example, a row with only one existent field
// only takes up the space of one field.

pub type RowId = DeterministicRandomId;
type TimeId = DeterministicRandomId;

#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ColumnId(u64);

#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct PredictorId(u64);


pub trait Column {
  type FieldType: Any + Send + Sync + Serialize + Deserialize;// = Self;

  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  Thanks to the [birthday problem](https://en.wikipedia.org/wiki/Birthday_problem), this would have a >1% chance of a collision with a mere 700 million Column implementors. I don't think we need to worry about this. 128 bit IDs are necessary for rows, because computers can generate billions of them easily, but this isn't the same situation.
  
  It might seem desirable to default to a hash of the TypeId of Self, for the convenience of some implementations. However, Rust does not guarantee that the TypeId remains the same across different compilations or different compiler versions. And it certainly doesn't remain the same when you add or remove types from your code, which would be desirable for compatibility between different versions of your program. Also, the Rust interface for getting the TypeId is currently unstable. Using an explicit constant is simply better.
  
  TODO: change this into an associated constant once associated constants become stable.
  */
  fn column_id() -> ColumnId;

  /**
  Implementors MAY return true if first and second are indistinguishable.
  
  This is labeled "unsafe" because imperfect implementations can easily cause nondeterminism in the TimeSteward. Using the default is fine, and implementing it is only an optimization. Don't implement this unless you know what you're doing.

  This is similar to requiring FieldType to be PartialEq, but with an important difference. PartialEq only requires the comparison to be an equivalence relation, but does NOT require that (first == second) guarantee that there is no observable difference between the values. Therefore, trusting arbitrary implementations of PartialEq would be unsafe (in the sense that it would allow the TimeSteward to behave non-deterministically).

  TODO: perhaps we can create default implementations of this for POD types.
  TODO: can we create automated tests for implementations of this function?
  */
  fn guaranteed_equal__unsafe(_: &Self::FieldType, _: &Self::FieldType) -> bool {
    false
  }
}

macro_rules! use_default_equality__unsafe {
  () => {
    fn guaranteed_equal__unsafe(first: &Self::FieldType, second: &Self::FieldType) -> bool {
      first == second
    }
  };
}


#[derive (Copy, Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct FieldId {
  row_id: RowId,
  column_id: ColumnId,
}
impl FieldId {
  pub fn new(row_id: RowId, column_id: ColumnId) -> FieldId {
    FieldId {
      row_id: row_id,
      column_id: column_id,
    }
  }
}

// I'm not sure exactly what synchronization properties we will need for these callbacks,
// so I'm requiring both Send and Sync for now to future-proof them.
// Serialize is required for synchronization checking.
// Serialize + Deserialize is needed for fiat events in order to transmit them.
// Clone makes things easier for crossverified time stewards, and
//   shouldn't be too hard for a Serialize + Deserialize type.
// I don't have plans to use Deserialize for other events/predictors,
// but it's possible that I might, so I included it for more future-proofing.
// I'm not sure if 'static is strictly necessary, but it makes things easier, and
// wanting a non-'static callback (which still must live at least as long as the TimeSteward)
// seems like a very strange situation.
pub trait EventFn <B: Basics>: Send + Sync + Clone + Serialize + Deserialize + 'static {
  fn call<M: Mutator<B>>(&self, mutator: &mut M);
}
pub trait PredictorFn <B: Basics>: Send + Sync + Clone + Serialize + Deserialize + 'static {
  fn call<PA: PredictorAccessor<B>>(&self, accessor: &mut PA, id: RowId);
}

macro_rules! time_steward_predictor {
  ($B: ty, struct $name: ident [$($generic_parameters:tt)*]=[$($specific_parameters:ty),*] {$($field_name: ident: $field_type: ty = $field_value: expr),*} , | &$self_name: ident, $accessor_name: ident, $row_name: ident | $contents: expr) => {{
    #[derive (Clone, Serialize, Deserialize)]
    struct $name <$($generic_parameters)*> {$($field_name: $field_type),*}
    impl<$($generic_parameters)*> $crate::PredictorFn <$B> for $name<$($specific_parameters),*> {
      fn call <P: $crate::PredictorAccessor <$B>> (&$self_name, $accessor_name: &mut P, $row_name: RowId) {
        $contents
      }
    }
    $name::<$($specific_parameters),*> {$($field_name: $field_value),*}
  }};
  ($B: ty, struct $name: ident {$($field_name: ident: $field_type: ty = $field_value: expr),*}, | &$self_name: ident, $accessor_name: ident, $row_name: ident | $contents: expr) => {
    time_steward_predictor! ($B, struct $name []=[] {$($field_name :$field_type = $field_value)*}, | & $self_name, $accessor_name, $row_name | $contents)
  };
}

macro_rules! time_steward_event {
  ($B: ty, struct $name: ident [$($generic_parameters:tt)*]=[$($specific_parameters:ty),*] {$($field_name: ident: $field_type: ty = $field_value: expr),*}, | &$self_name: ident, $mutator_name: ident | $contents: expr) => {{
    #[derive (Clone, Serialize, Deserialize)]
    struct $name <$($generic_parameters)*> {$($field_name: $field_type),*}
    impl<$($generic_parameters)*> $crate::EventFn <$B> for $name<$($specific_parameters),*> {
      fn call <M: $crate::Mutator <$B>> (&$self_name, $mutator_name: &mut M) {
        $contents
      }
    }
    $name::<$($specific_parameters),*> {$($field_name: $field_value),*}
  }};
  ($B: ty, struct $name: ident {$($field_name: ident: $field_type: ty = $field_value: expr),*}, | &$self_name: ident, $mutator_name: ident | $contents: expr) => {
    time_steward_event! ($B, struct $name []=[] {$($field_name :$field_type = $field_value)*}, | & $self_name, $mutator_name | $contents)
  };

}

macro_rules! time_steward_predictor_from_generic_fn {
  ($B: ty, struct $name: ident, $function_name: ident) => {
    time_steward_predictor! ($B, struct $name {}, | &self, accessor, id | {
      $function_name (accessor, id)
    })
  }
}

/**
This is intended to be implemented on an empty struct. Requiring Clone etc. is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait Basics: Clone + Send + Sync + Serialize + Deserialize + 'static {
  type Time: Clone + Ord + Send + Sync + Serialize + Deserialize;
  type Constants: Clone + Send + Sync + Serialize + Deserialize;
}
pub type ExtendedTime<B: Basics> = GenericExtendedTime<B::Time>;

type IterationType = u32;
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct GenericExtendedTime<Base: Ord> {
  base: Base,
  iteration: IterationType,
  id: TimeId,
}

pub type StewardRc<T> = Arc<T>;
pub type FieldRc = StewardRc<Any>;

// Note: in the future, we expect we might use a custom hash table type that knows it can rely on DeterministicRandomId to already be random, so we don't need to hash it again. This also applies to FieldId, although there may be some complications in that case.
// (for now, we need to be able to hash full siphash ids to create other siphash ids.)
// ( Also -- do we ever try to hash GenericExtendedTime with id of 0 from beginning_of_moment.... )
// impl Hash for DeterministicRandomId {
//  fn hash<H: Hasher>(&self, state: &mut H) {
//    self.data[0].hash(state);
//  }
// }
// impl<Base: BaseTime> Hash for GenericExtendedTime<Base> {
//  fn hash<H: Hasher>(&self, state: &mut H) {
//    self.id.hash(state);
//  }
// }

fn unwrap_field<'a, C: Column>(field: &'a FieldRc) -> &'a C::FieldType {
  field.downcast_ref::<C::FieldType>().expect("a field had the wrong type for its column").borrow()
}

pub trait Accessor<B: Basics> {
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<B>)>;
  fn data_and_extended_last_change<C: Column>(&self,
                                              id: RowId)
                                              -> Option<(&C::FieldType, &ExtendedTime<B>)> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
        .map(|pair| (unwrap_field::<C>(pair.0), pair.1))
  }
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
        .map(|pair| (unwrap_field::<C>(pair.0), &pair.1.base))
  }
  fn get<C: Column>(&self, id: RowId) -> Option<&C::FieldType> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
        .map(|p| unwrap_field::<C>(p.0))
  }
  fn last_change<C: Column>(&self, id: RowId) -> Option<&B::Time> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id())).map(|p| &p.1.base)
  }
  fn constants(&self) -> &B::Constants;

  /**
  In general, predictions may NOT depend on the time the predictor is called.
  However, in some cases, you may want to have a predictor that does something like
  "predict an event to happen at the beginning of any minute", which isn't technically
  time-dependent – but if you did it in a time-independent way, you'd have to predict
  infinitely many events. Unsafe_now() is provided to enable you to only compute one of them.
  
  When you call unsafe_now() in a predictor, you promise that you will
  make the SAME predictions for ANY given return value of unsafe_now(), UNLESS:
  1. The return value is BEFORE the last change time of any of the fields you access, or
  2. You predict an event, and the return value is AFTER that event.
  
  This function is provided by Accessor rather than PredictorAccessor so that functions can be generic in whether they are used in a predictor or not.
  */
  fn unsafe_now(&self) -> &B::Time;
}

pub trait MomentaryAccessor<B: Basics>: Accessor<B> {
  fn now(&self) -> &B::Time {
    self.unsafe_now()
  }
}

pub trait Mutator<B: Basics>: MomentaryAccessor<B> + Rng {
  fn extended_now(&self) -> &ExtendedTime<B>;
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>);
  fn gen_id(&mut self) -> RowId;
  fn underlying_rng_unsafe(&mut self) -> &mut EventRng;
}
pub trait PredictorAccessor<B: Basics>: Accessor<B> {
  fn predict_at_time<E: EventFn<B>>(&mut self, time: B::Time, event: E);

  ///A specific use of unsafe_now() that is guaranteed to be safe
  fn predict_immediately<E: EventFn<B>>(&mut self, event: E) {
    let time = self.unsafe_now().clone();
    self.predict_at_time(time, event)
  }
}
pub type SnapshotEntry<'a, B: Basics> = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
// where for <'a> & 'a Self: IntoIterator <Item = SnapshotEntry <'a, B>>
pub trait Snapshot<B: Basics>: MomentaryAccessor<B> {
  fn num_fields(&self) -> usize;
// with slightly better polymorphism we could do this more straightforwardly
// type Iter<'a>: Iterator<(FieldId, (&'a FieldRc, &'a ExtendedTime<B>))>;
// fn iter (&self)->Iter;
}

pub struct FiatSnapshot<B: Basics> {
  now: B::Time,
  constants: StewardRc<B::Constants>,
  fields: HashMap<FieldId, (FieldRc, ExtendedTime<B>)>,
}
impl<B: Basics> Accessor<B> for FiatSnapshot<B> {
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.fields.get(&id).map(|pair| (&pair.0, &pair.1))
  }
  fn constants(&self) -> &B::Constants {
    self.constants.borrow()
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.now
  }
}
impl<B: Basics> MomentaryAccessor<B> for FiatSnapshot<B> {}
impl<B: Basics> Snapshot<B> for FiatSnapshot<B> {
  fn num_fields(&self) -> usize {
    self.fields.len()
  }
}
use std::collections::hash_map;
pub struct FiatSnapshotIter<'a, B: Basics>(hash_map::Iter<'a, FieldId, (FieldRc, ExtendedTime<B>)>);
impl<'a, B: Basics> Iterator for FiatSnapshotIter<'a, B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  fn next(&mut self) -> Option<Self::Item> {
    (self.0).next().map(|(id, stuff)| (id.clone(), (&stuff.0, &stuff.1)))
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.0.size_hint()
  }
}
impl<'a, B: Basics> IntoIterator for &'a FiatSnapshot<B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  type IntoIter = FiatSnapshotIter <'a, B>;
  fn into_iter(self) -> Self::IntoIter {
    FiatSnapshotIter(self.fields.iter())
  }
}


pub struct SerializationTable<S: Serializer>(HashMap<ColumnId,
                                                     fn(&FieldRc, &mut S) -> Result<(), S::Error>>);
pub struct SerializationField<'a, 'b, Hack: Serializer + 'b>(ColumnId,
                                                             &'a FieldRc,
                                                             &'b SerializationTable<Hack>);
impl<'a, 'b, Hack: Serializer> Serialize for SerializationField<'a, 'b, Hack> {
  fn serialize<'c, S: Serializer + 'b>(&'c self, serializer: &mut S) -> Result<(), S::Error> {
    // assert!(TypeId::of::<S>() == TypeId::of::<Hack>(), "hack: this can only actually serialize for the serializer it has tables for");
    let table = unsafe {
      std::mem::transmute::<&'b SerializationTable<Hack>, &'b SerializationTable<S>>(self.2)
    };
    match (table.0).get(&self.0) {
      None => {
        Err(S::Error::custom(format!("Attempt to serialize field from uninitialized column \
                                      {:?}; did you forget to initialize all the columns from \
                                      your own code and/or the libraries you're using?",
                                     self.0)))
      }
      Some(function) => function(self.1, serializer),
    }
  }
}

pub fn deserialize_column<B: Basics, C: Column, M: de::MapVisitor>
  (visitor: &mut M)
   -> Result<(FieldRc, ExtendedTime<B>), M::Error> {
  let (data, time) = try!(visitor.visit_value::<(C::FieldType, ExtendedTime<B>)>());
  Ok((StewardRc::new(data), time))
}

pub fn serialize_column<C: Column, S: Serializer>(field: &FieldRc,
                                                  serializer: &mut S)
                                                  -> Result<(), S::Error> {
  try!(field.downcast_ref::<C::FieldType>()
            .expect("a field had the wrong type for its column")
            .serialize(serializer));
  Ok(())
}

#[macro_export]
macro_rules! __time_steward_insert_deserialization_function {
  ($column: ty, $B: ty, $M: ty, $table: ident) => {
    $table.insert (<$column as $crate::Column>::column_id(), $crate::deserialize_column::<$B, $column, $M>);
  }
}

#[macro_export]
macro_rules! __time_steward_insert_serialization_function {
  ($column: ty, $S: ty, $table: ident) => {
    $table.0.insert (<$column as $crate::Column>::column_id(), $crate::serialize_column::<$column, $S>);
  }
}

#[macro_export]
macro_rules! for_these_columns {
  ($macro_name: ident {Column, $($macro_arguments:tt)*}, $column: ty $(, $more_columns:tt)*) => {{
    $macro_name! {$column, $($macro_arguments)*};
    for_these_columns! {$macro_name {Column, $($macro_arguments)*}, $($more_columns:tt,)*}
  }};
  ($macro_name: ident {Column, $($macro_arguments:tt)*},) => {{}};
  ($macro_name: ident {Column, $($macro_arguments:tt)*}) => {{}};
}

#[macro_export]
macro_rules! make_snapshot_serde_functions {
($serialize_snapshot_name: ident, $deserialize_snapshot_name: ident) => {

use serde as __time_steward_make_snapshot_serde_functions_impl_serde;

pub fn $serialize_snapshot_name <'a, B: $crate::Basics, Shot: $crate::Snapshot <B>, S: __time_steward_make_snapshot_serde_functions_impl_serde::Serializer> (snapshot: & 'a Shot, serializer: &mut S)->Result <(), S::Error> where B::Time: __time_steward_make_snapshot_serde_functions_impl_serde::Serialize, & 'a Shot: IntoIterator <Item = $crate::SnapshotEntry <'a, B>>  {
  use std::collections::HashMap;
  use serde::Serialize;
  let mut table = $crate::SerializationTable (HashMap::new());
  for_all_columns! (__time_steward_insert_serialization_function {Column, S, table});
  
  try! (snapshot.now().serialize (serializer));
  try! (snapshot.constants().serialize (serializer));
  let mut state = try!(serializer.serialize_map(Some (snapshot.num_fields())));
  for (id, (data, changed)) in snapshot {
    try! (serializer.serialize_map_key (&mut state, id));
    try! (serializer.serialize_map_value (&mut state, (& $crate::SerializationField (id.column_id, & data, & table), changed)));
  };
  serializer.serialize_map_end (state)
}

mod __time_steward_make_snapshot_serde_functions_impl {
use std::collections::HashMap;
use serde;
use std::marker::PhantomData;

pub type DeserializationTable <B: $crate::Basics, M: serde::de::MapVisitor> = HashMap<$crate::ColumnId, fn (&mut M)->Result <($crate::FieldRc, $crate::ExtendedTime <B>), M::Error>>;
pub struct SerdeMapVisitor <B: $crate::Basics> {
  marker: PhantomData<$crate::FiatSnapshot <B >>
}
pub struct DeserializedMap <B: $crate::Basics> {
  pub data: HashMap<$crate::FieldId, ($crate::FieldRc, $crate::ExtendedTime<B>)>,
}
impl<B: $crate::Basics> serde::Deserialize for DeserializedMap <B> {
  fn deserialize <D> (_: &mut D)->Result <Self, D::Error> where D: serde::Deserializer {
    panic!("I believe that Visitor::Value requiring Deserialize is bogus, so this panic will never occur.")
  }
}


impl<B: $crate::Basics> SerdeMapVisitor<B> {
  pub fn new() -> Self {
    SerdeMapVisitor {
      marker: PhantomData
    }
  }
}
impl<B: $crate::Basics>  serde::de::Visitor for SerdeMapVisitor<B>
{
  type Value = DeserializedMap <B>;
  fn visit_map<M>(&mut self, mut visitor: M) -> Result<Self::Value, M::Error>
    where M: serde::de::MapVisitor
  {
    let mut table: DeserializationTable <B, M> = DeserializationTable::<B, M>::new();
    super::__time_steward_make_snapshot_serde_functions_impl_populate_table:: <B, M> (&mut table);
    
    let mut fields = HashMap::with_capacity(visitor.size_hint().0); 
    
    use serde::Error;
    while let Some (key) = try!(visitor.visit_key::<$crate::FieldId>()) {
      fields.insert (key, match table.get(& key.column_id) {
        None => return Err (M::Error::custom (format! ("Attempt to deserialize field from uninitialized column {:?}; Maybe you're trying to load a snapshot from a different version of your program? Or did you forget to initialize all the columns from your own code and/or the libraries you're using?", key. column_id))),
        Some (function) => try! (function (&mut visitor)),
      });
    }

    try!(visitor.end());
    Ok(DeserializedMap {data: fields})
  }
}

}


fn __time_steward_make_snapshot_serde_functions_impl_populate_table <B: $crate::Basics, M: __time_steward_make_snapshot_serde_functions_impl_serde::de::MapVisitor> (table: &mut __time_steward_make_snapshot_serde_functions_impl::DeserializationTable <B, M>) {
    for_all_columns! (__time_steward_insert_deserialization_function {Column, B, M, table});

}

fn $deserialize_snapshot_name <B: $crate::Basics, D: __time_steward_make_snapshot_serde_functions_impl_serde::Deserializer> (deserializer: &mut D)->Result <$crate::FiatSnapshot <B>, D::Error> {
  use serde::{Deserialize};
  Ok ($crate::FiatSnapshot {
    now: try! (B::Time::deserialize (deserializer)),
    constants: StewardRc::new (try! (B::Constants::deserialize (deserializer))),
    fields: try! (deserializer.deserialize_map(__time_steward_make_snapshot_serde_functions_impl ::SerdeMapVisitor::<B>::new())). data,
  })
}

}}




#[derive (Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

// This exists to support a variety of time stewards
// along with allowing BaseTime to be dense (e.g. a
// rational number rather than an integer).
// It is an acceptable peculiarity that even for integer times,
// After(2) < Before(3).
// #[derive (Copy, Clone, PartialEq, Eq, Hash)]
#[derive (Clone, PartialEq, Eq)]
pub enum ValidSince<BaseTime> {
  TheBeginning,
  Before(BaseTime),
  After(BaseTime),
}

impl<T: Ord> Ord for ValidSince<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&ValidSince::TheBeginning, &ValidSince::TheBeginning) => Ordering::Equal,
      (&ValidSince::TheBeginning, _) => Ordering::Less,
      (_, &ValidSince::TheBeginning) => Ordering::Greater,
      (&ValidSince::Before(ref something),
       &ValidSince::Before(ref anything)) => something.cmp(anything),
      (&ValidSince::After(ref something),
       &ValidSince::After(ref anything)) => something.cmp(anything),
      (&ValidSince::Before(ref something),
       &ValidSince::After(ref anything)) => {
        if something <= anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      (&ValidSince::After(ref something),
       &ValidSince::Before(ref anything)) => {
        if something < anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T> PartialEq<T> for ValidSince<T> {
  fn eq(&self, _: &T) -> bool {
    false
  }
}

impl<T: Ord> PartialOrd<T> for ValidSince<T> {
  fn partial_cmp(&self, other: &T) -> Option<Ordering> {
    Some(match self {
      &ValidSince::TheBeginning => Ordering::Less,
      &ValidSince::Before(ref something) => {
        if something <= other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      &ValidSince::After(ref something) => {
        if something < other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    })
  }
}
impl<T: Ord> PartialOrd for ValidSince<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
// impl <T: Ord> PartialOrd <ValidSince <T>> for T {
//  fn partial_cmp (&self, other: & ValidSince <T>)->Option <Ordering> {
//    Some (other.partial_cmp (self).unwrap().reverse());
//  }
// }

pub trait TimeStewardSettings <B: Basics> {
  fn new() -> Self;
  fn insert_predictor<P: PredictorFn<B>>(&mut self,
                                         predictor_id: PredictorId,
                                         column_id: ColumnId,
                                         function: P);
}

pub trait TimeSteward <B: Basics> {
  type Snapshot: Snapshot <B>;
  type Settings: TimeStewardSettings <B>;

  /**
  You are allowed to call snapshot_before(), insert_fiat_event(),
  and erase_fiat_event() for times >= valid_since().
  
  TimeSteward implementors are permitted, but not required, to discard old data in order to save memory. This may make the TimeSteward unusable at some points in its history.
  
  All implementors must obey certain restrictions on how other TimeSteward methods may change the result of valid_since(). Implementors may have their own methods that can alter this in customized ways, which should be documented with those individual methods.
  */
  fn valid_since(&self) -> ValidSince<B::Time>;

  /**
  Creates a new, empty TimeSteward.
  
  new_empty().valid_since() must equal TheBeginning.
  */
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self;

  /**
  Creates a new TimeSteward from a snapshot.
  
  from_snapshot().valid_since() must equal Before(snapshot.now()),
  and must never go lower than that.
  */
  fn from_snapshot<'a, S: Snapshot<B>>(snapshot: &'a S, settings: Self::Settings) -> Self
    where &'a S: IntoIterator<Item = SnapshotEntry<'a, B>>;


  /**
  Inserts a fiat event at some point in the history.
  
  If time < valid_since(), this does nothing and returns Err(InvalidTime). If there is already a fiat event with the same time and distinguisher, this does nothing and returns Err(InvalidInput). Otherwise, it inserts the event and returns Ok.
  
  steward.insert_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.insert_fiat_event() may not change steward.valid_since().
  */
  fn insert_fiat_event<E: EventFn<B>>(&mut self,
                                      time: B::Time,
                                      id: DeterministicRandomId,
                                      event: E)
                                      -> Result<(), FiatEventOperationError>;

  /**
  Erases a fiat event that has been inserted previously.
  
  If time < valid_since(), this does nothing and returns Err(InvalidTime). If there is no fiat event with the specified time and distinguisher, this does nothing and returns Err(InvalidInput). Otherwise, it erases the event and returns Ok.
  
  steward.erase_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.erase_fiat_event() may not change steward.valid_since().
  */
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError>;

  /** Returns a "snapshot" into the TimeSteward.
  
  The snapshot is guaranteed to be valid and unchanging for the full lifetime of the TimeSteward. It is specific to both the time argument, and the current collection of fiat events. Callers may freely call mutable methods of the same TimeSteward after taking a snapshot, without changing the contents of the snapshot.
  
  Each TimeSteward implementor determines exactly how to provide these guarantees. Implementors should provide individual guarantees about the processor-time bounds of snapshot operations.
  
  steward.snapshot_before(time) must return Some if time > steward.valid_since().
  steward.snapshot_before(time) may not increase steward.valid_since() beyond Before(time).
  */
  fn snapshot_before(&mut self, time: &B::Time) -> Option<Self::Snapshot>;
}


// TODO: everything between here and the modules at the bottom are implementation details that should be moved into a different file


// #[derive (Clone)]
// enum Prediction<B: Basics, E> {
//   Nothing,
//   Immediately(E),
//   At(B::Time, E),
// }

// type Event<M> = StewardRc<for<'d> Fn(&'d mut M)>;
// type PredictorFn<B: Basics, M: Mutator<B>, PA: PredictorAccessor<B>> =
//  StewardRc<for<'b, 'c> Fn(&'b mut PA, RowId) -> Prediction<B, Event<M>>>;


type EventRng = ChaChaRng;
fn generator_for_event(id: TimeId) -> EventRng {
  EventRng::from_seed(&[(id.data[0] >> 32) as u32,
                        (id.data[0] & 0xffffffff) as u32,
                        (id.data[1] >> 32) as u32,
                        (id.data[1] & 0xffffffff) as u32])
}

macro_rules! make_dynamic_callbacks {

($M: ident, $PA: ident, $DynamicEventFn: ident, $DynamicPredictorFn: ident, $DynamicPredictor: ident, $StandardSettings: ident) => {

mod __time_steward_make_dynamic_callbacks_impl {

use $crate::{Basics, EventFn, PredictorFn, RowId, ColumnId, PredictorId, StewardRc, TimeStewardSettings};
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct DynamicEventFn <B: Basics, E: EventFn <B>> (E, PhantomData <B>);
pub struct DynamicPredictorFn <B: Basics, P: PredictorFn <B>> (P, PhantomData <B>);

impl<B: Basics, E: EventFn <B>> DynamicEventFn <B, E> {
  pub fn new (event: E)->Self {DynamicEventFn (event, PhantomData)}
}
impl<B: Basics, P: PredictorFn <B>> DynamicPredictorFn <B, P> {
  pub fn new (predictor: P)->Self {DynamicPredictorFn (predictor, PhantomData)}
}

impl<'a, 'b, B: Basics, E: EventFn <B>> Fn <(& 'a mut super::$M <'b, B>,)> for DynamicEventFn <B, E> {
  extern "rust-call" fn call (&self, arguments: (& 'a mut super::$M <'b, B>,)) {
    self.0.call (arguments.0)
  }
}
impl<'a, 'b, B: Basics, P: PredictorFn <B>> Fn <(& 'a mut super::$PA <'b, B>, RowId)> for DynamicPredictorFn <B, P> {
  extern "rust-call" fn call (&self, arguments: (& 'a mut super::$PA <'b, B>, RowId)) {
    self.0.call (arguments.0, arguments.1)
  }
}

impl<'a, 'b, B: Basics, E: EventFn <B>> FnMut <(& 'a mut super::$M <'b, B>,)> for DynamicEventFn <B, E> {
  extern "rust-call" fn call_mut (&mut self, arguments: (& 'a mut super::$M <'b, B>,)) {
    self.call (arguments)
  }
}
impl<'a, 'b, B: Basics, E: EventFn <B>> FnOnce <(& 'a mut super::$M <'b, B>,)> for DynamicEventFn <B, E> {
  type Output = ();
  extern "rust-call" fn call_once (self, arguments: (& 'a mut super::$M <'b, B>,)) {
    self.call (arguments)
  }
}


impl<'a, 'b, B: Basics, P: PredictorFn <B>> FnMut <(& 'a mut super::$PA <'b, B>, RowId)> for DynamicPredictorFn <B, P> {
  extern "rust-call" fn call_mut (&mut self, arguments: (& 'a mut super::$PA <'b, B>, RowId)) {
    self.call (arguments)
  }
}
impl<'a, 'b, B: Basics, P: PredictorFn <B>> FnOnce <(& 'a mut super::$PA <'b, B>, RowId)> for DynamicPredictorFn <B, P> {
  type Output = ();
  extern "rust-call" fn call_once (self, arguments: (& 'a mut super::$PA <'b, B>, RowId)) {
    self.call (arguments)
  }
}

// #[derive (Clone)]
pub struct DynamicPredictor <B: Basics> {
  pub predictor_id: PredictorId,
  pub column_id: ColumnId,
  pub function: StewardRc <for <'a, 'b> Fn(& 'a mut super::$PA <'b, B>, RowId)>,
  _marker: PhantomData <B>,
}
// explicitly implement Clone to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
impl<B: Basics> Clone for DynamicPredictor<B> {
  fn clone(&self) -> Self {
    DynamicPredictor {
      predictor_id: self.predictor_id,
      column_id: self.column_id,
      function: self.function.clone(),
      _marker: PhantomData,
    }
  }
}

pub struct StandardSettings <B: Basics> {
  pub predictors_by_column: HashMap<ColumnId, Vec<DynamicPredictor <B>>>,
  pub predictors_by_id: HashMap<PredictorId, DynamicPredictor <B>>,
}
impl<B: Basics> TimeStewardSettings <B> for StandardSettings <B> {
  fn new()->Self {
    StandardSettings {
      predictors_by_id: HashMap::new(),
      predictors_by_column: HashMap::new(),
    }
  }
  fn insert_predictor <P: PredictorFn <B>> (&mut self, predictor_id: PredictorId, column_id: ColumnId, function: P) {
    let predictor = DynamicPredictor {
      predictor_id: predictor_id,
      column_id: column_id,
      function: StewardRc::new (DynamicPredictorFn::new (function)),
      _marker: PhantomData,
    };
    self.predictors_by_id.insert(predictor.predictor_id, predictor.clone());
    self.predictors_by_column.entry(predictor.column_id).or_insert(Vec::new()).push(predictor);
  }
}

}

use self::__time_steward_make_dynamic_callbacks_impl::DynamicEventFn as $DynamicEventFn;
use self::__time_steward_make_dynamic_callbacks_impl::DynamicPredictor as $DynamicPredictor;
use self::__time_steward_make_dynamic_callbacks_impl::DynamicPredictorFn as $DynamicPredictorFn;
pub use self::__time_steward_make_dynamic_callbacks_impl::StandardSettings as $StandardSettings;

}}

struct GenericPredictorAccessor<B: Basics, E> {
  soonest_prediction: Option<(B::Time, E)>,
  dependencies: RefCell<(Vec<FieldId>, SiphashIdGenerator)>,
}
impl<B: Basics, E> GenericPredictorAccessor<B, E> {
  fn new() -> Self {
    GenericPredictorAccessor {
      soonest_prediction: None,
      dependencies: RefCell::new((Vec::new(), SiphashIdGenerator::new())),
    }
  }
}

macro_rules! predictor_accessor_common_accessor_methods {
  ($B: ty, $get: ident) => {
    fn generic_data_and_extended_last_change(&self,
                                             id: FieldId)
                                             -> Option<(&FieldRc, &ExtendedTime<$B>)> {
      let dependencies: &mut (Vec<FieldId>, SiphashIdGenerator) = &mut*self.generic.dependencies.borrow_mut();
      dependencies.0.push(id);
      self.$get(id).map(|p| {
        p.1.id.hash(&mut dependencies.1);
        p
      })
    }
  }
}
macro_rules! predictor_accessor_common_methods {
  ($B: ty, $DynamicEventFn: ident) => {
    fn predict_at_time <E: $crate::EventFn <$B>> (&mut self, time: <$B as $crate::Basics>::Time, event: E) {
      if time < *self.unsafe_now() {
        return;
      }
      if let Some((ref old_time, _)) = self.generic.soonest_prediction {
        if old_time <= &time {
          return;
        }
      }
      self.generic.soonest_prediction = Some((time, StewardRc::new ($DynamicEventFn ::new (event))));
    }
  }
}


struct GenericMutator<B: Basics> {
  now: ExtendedTime<B>,
  generator: ChaChaRng,
}
impl<B: Basics> GenericMutator<B> {
  fn new(now: ExtendedTime<B>) -> Self {
    let generator = generator_for_event(now.id);
    GenericMutator {
      now: now,
      generator: generator,
    }
  }
}
macro_rules! mutator_common_accessor_methods {
  ($B: ty) => {
    fn unsafe_now(& self)->& <$B as $crate::Basics>::Time {& self.generic.now.base}
  }
}
macro_rules! mutator_common_methods {
  ($B: ty) => {
    fn extended_now(& self)->& ExtendedTime <$B> {& self.generic.now}
    fn gen_id(&mut self) -> RowId {RowId {data: [self.gen::<u64>(), self.gen::<u64>()]}}
    fn underlying_rng_unsafe (&mut self) ->&mut EventRng{&mut self.generic.generator}
  }
}
macro_rules! mutator_rng_methods {
  () => {
    fn next_u32(&mut self) -> u32 {self.generic.generator.next_u32()}
    
    fn next_f32(&mut self) -> f32 {panic!("Using floating point numbers in TimeSteward events is forbidden by default because it is nondeterministic across platforms. If you know you don't need to be consistent between different computers, or you otherwise know EXACTLY what you're doing, you may explicitly call underlying_rng_unsafe() to get a generator that can produce floats.")}
    fn next_f64(&mut self) -> f64 {panic!("Using floating point numbers in TimeSteward events is forbidden by default because it is nondeterministic across platforms. If you know you don't need to be consistent between different computers, or you otherwise know EXACTLY what you're doing, you may explicitly call underlying_rng_unsafe() to get a generator that can produce floats.")}
  }
}





fn extended_time_of_fiat_event<BaseTime: Ord>(time: BaseTime,
                                              id: TimeId)
                                              -> GenericExtendedTime<BaseTime> {
  GenericExtendedTime {
    base: time,
    iteration: 0,
    id: id,
  }
}

fn time_id_for_predicted_event(predictor_id: PredictorId,
                               row_id: RowId,
                               iteration: IterationType,
                               dependencies_hash: DeterministicRandomId)
                               -> TimeId {
  TimeId::new(&(predictor_id, row_id, iteration, dependencies_hash))
}
fn next_extended_time_of_predicted_event<BaseTime: Ord>
  (predictor_id: PredictorId,
   row_id: RowId,
   dependencies_hash: DeterministicRandomId,
   event_base_time: BaseTime,
   from: &GenericExtendedTime<BaseTime>)
   -> Option<GenericExtendedTime<BaseTime>> {
  let (iteration, id) = match event_base_time.cmp(&from.base) {
    Ordering::Less => return None, // short-circuit
    Ordering::Greater => {
      (0,
       time_id_for_predicted_event(predictor_id, row_id, 0, dependencies_hash))
    }
    Ordering::Equal => {
      let id = time_id_for_predicted_event(predictor_id, row_id, from.iteration, dependencies_hash);
      if id > from.id {
        (from.iteration, id)
      } else {
        if from.iteration == IterationType::max_value() {
          panic!("Too many iterations at the same base time; probably an infinite loop")
        }
        (from.iteration + 1,
         time_id_for_predicted_event(predictor_id, row_id, from.iteration + 1, dependencies_hash))
      }
    }
  };
  Some(GenericExtendedTime {
    base: event_base_time,
    iteration: iteration,
    id: id,
  })
}

pub mod data_structures;

pub mod inefficient_flat_time_steward;
pub mod memoized_flat_time_steward;
// pub mod amortized_time_steward;

pub mod crossverified_time_stewards;

pub mod rounding_error_tolerant_math;
pub mod time_functions;
#[macro_use]
pub mod collision_detection;

pub mod examples {
  pub mod handshakes;
  pub mod bouncy_circles;
}
