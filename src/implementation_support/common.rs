use std::collections::{BTreeMap, BTreeSet};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::io::{Read, Write};
use std::any::Any;
use std::borrow::Borrow;
use rand::{ChaChaRng, SeedableRng};
use {DeterministicRandomId, PredictorId, EventId, TimeId, RowId, ColumnId, FieldId, SiphashIdGenerator,
     IterationType, Basics, ExtendedTime, Column, Predictor, Event,
     PredictorAccessor, Mutator, FieldRc, StewardRc,};
use std::marker::PhantomData;

// https://github.com/rust-lang/rfcs/issues/1485
pub trait Filter<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self;
}
impl<T> Filter<T> for Option<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self {
    self.and_then(|x| { if predicate(&x) { Some(x) } else { None } })
  }
}


pub fn split_off_greater<K: Ord + Borrow<Q> + Clone, V, Q: Ord + ?Sized>(input: &mut BTreeMap<K, V>,
                                            split: &Q)
                                            -> BTreeMap<K, V> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  let mut transfer = None;
  if let Some(whoops) = result.iter().next() {
    if whoops.0.borrow() == split {
      transfer = Some(whoops.0.clone());
    }
  }
  if let Some(key) = transfer {
    input.insert(key, result.remove (split).unwrap());
  }
  result
}

pub fn split_off_greater_set<K: Ord + Borrow<Q>, Q: Ord + ?Sized>(input: &mut BTreeSet<K>, split: &Q) -> BTreeSet<K> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  if let Some(whoops) = result.take(split) {
    input.insert(whoops);
  }
  #[cfg(debug_assertions)]
  {
  if result.take(split).is_some() {
    panic!("Some code broke the Ord/Borrow rules for BTreeSet calls")
  }
  }
  result
}

type EventRng = ChaChaRng;
fn generator_for_event(id: TimeId) -> EventRng {
  EventRng::from_seed(&[(id.data()[0] >> 32) as u32,
                        (id.data()[0] & 0xffffffff) as u32,
                        (id.data()[1] >> 32) as u32,
                        (id.data()[1] & 0xffffffff) as u32])
}


pub struct DynamicEventFn<E: Event>(E);
pub struct DynamicPredictorFn<P: Predictor>(PhantomData<P>);

impl<E: Event> DynamicEventFn<E> {
  pub fn new(event: E) -> Self {
    DynamicEventFn(event)
  }
}
impl<P: Predictor> DynamicPredictorFn<P> {
  pub fn new() -> Self {
    DynamicPredictorFn(PhantomData)
  }
}

impl<'a, E: Event, M: Mutator<Basics = E::Basics>> Fn<(&'a mut M,)> for DynamicEventFn<E> {
  extern "rust-call" fn call(&self, arguments: (&'a mut M,)) {
    self.0.call(arguments.0)
  }
}
impl<'a, P: Predictor, PA: PredictorAccessor <Basics = P::Basics>> Fn <(& 'a mut PA, RowId)> for DynamicPredictorFn <P> {
  extern "rust-call" fn call (&self, arguments: (& 'a mut PA, RowId)) {
    P::call (arguments.0, arguments.1)
  }
}

impl<'a, E: Event, M: Mutator<Basics = E::Basics>> FnMut<(&'a mut M,)> for DynamicEventFn<E> {
  extern "rust-call" fn call_mut(&mut self, arguments: (&'a mut M,)) {
    self.call(arguments)
  }
}
impl<'a, E: Event, M: Mutator<Basics = E::Basics>> FnOnce<(&'a mut M,)> for DynamicEventFn<E> {
  type Output = ();
  extern "rust-call" fn call_once(self, arguments: (&'a mut M,)) {
    self.call(arguments)
  }
}


impl<'a, P: Predictor, PA: PredictorAccessor <Basics = P::Basics>> FnMut <(& 'a mut PA,RowId)> for DynamicPredictorFn <P> {
  extern "rust-call" fn call_mut (&mut self, arguments: (& 'a mut PA, RowId)) {
    self.call (arguments)
  }
}
impl<'a, P: Predictor, PA: PredictorAccessor <Basics = P::Basics>> FnOnce <(& 'a mut PA,RowId)> for DynamicPredictorFn <P> {
  type Output = ();
  extern "rust-call" fn call_once (self, arguments: (& 'a mut PA,RowId)) {
    self.call (arguments)
  }
}

// Eventually, these implementation detail macros should not be hackily doc (hidden),
// but instead be part of a different crate dedicated to time_steward implementation details.
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_dynamic_callback_structs {

(pub, $M: ident, $PA: ident, $DynamicEvent: ident, $DynamicPredictor: ident, $StandardSettings: ident) => {
time_steward_common_dynamic_callback_structs! ($M, $PA, $DynamicEvent, $DynamicPredictor, $StandardSettings[pub] );
};

($M: ident, $PA: ident, $DynamicEvent: ident, $DynamicPredictor: ident, $StandardSettings: ident) => {
time_steward_common_dynamic_callback_structs! ($M, $PA, $DynamicEvent, $DynamicPredictor, $StandardSettings[] );
};

($M: ident, $PA: ident, $DynamicEvent: ident, $DynamicPredictor: ident, $StandardSettings: ident[$($privacy:tt)*] ) => {

mod __time_steward_make_dynamic_callbacks_impl {

use $crate::{Basics, Column, Event, Predictor, RowId, ColumnId, EventId, PredictorId, StewardRc};
use std::collections::HashMap;
use std::marker::PhantomData;
use $crate::implementation_support::common::*;
use $crate::implementation_support::list_of_types::predictor_list;
use $crate::implementation_support::data_structures::BuildTrivialU64Hasher;

pub trait DynamicEventTrait <B: Basics>: for <'a, 'b> Fn(& 'a mut super:: $M<'b, B>) {
  fn event_id(&self)->EventId;
}
impl<B: Basics, E: Event <Basics = B >> DynamicEventTrait <B> for DynamicEventFn <E> {
  fn event_id(&self)->EventId {E::event_id()}
}

pub trait DynamicPredictorTrait <B: Basics>: for <'a, 'b> Fn(& 'a mut super:: $PA <'b, B>, RowId) {
  fn predictor_id(&self)->PredictorId;
  fn column_id(&self)->ColumnId;
}
impl<B: Basics, P: Predictor<Basics = B >> DynamicPredictorTrait <B> for DynamicPredictorFn <P> {
// Implementing these is redundant with restoring the ids in DynamicPredictor itself. TODO: resolve
  fn predictor_id(&self)->PredictorId {P::predictor_id()}
  fn column_id(&self)->ColumnId {P::WatchedColumn::column_id()}
}

pub type DynamicEvent <B> = StewardRc <DynamicEventTrait <B, Output =()>>;

// #[derive (Clone)]
pub struct DynamicPredictor <B: Basics> {
  pub predictor_id: PredictorId,
  pub column_id: ColumnId,
  pub function: StewardRc <DynamicPredictorTrait <B, Output =()>>,
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

#[derive (Clone)]
pub struct StandardSettings <B: Basics> {
  pub predictors_by_column: HashMap<ColumnId, Vec<DynamicPredictor <B>>, BuildTrivialU64Hasher>,
  pub predictors_by_id: HashMap<PredictorId, DynamicPredictor <B>, BuildTrivialU64Hasher>,
}
impl<B: Basics> predictor_list::User <B> for StandardSettings <B> {
  fn apply <P: Predictor <Basics = B>> (&mut self) {
    let predictor = DynamicPredictor {
      predictor_id: P::predictor_id(),
      column_id: P::WatchedColumn::column_id(),
      function: StewardRc::new (DynamicPredictorFn::<P>::new ()),
      _marker: PhantomData,
    };
    self.predictors_by_id.insert(predictor.predictor_id, predictor.clone());
    self.predictors_by_column.entry(predictor.column_id).or_insert(Vec::new()).push(predictor);
  }
}
impl<B: Basics> StandardSettings <B> {
  pub fn new()->Self {
    let mut result = StandardSettings {
      predictors_by_id: HashMap::default(),
      predictors_by_column: HashMap::default(),
    };
    <B::IncludedTypes as predictor_list::List<B>>::apply (&mut result);
    result
  }
}

}

#[allow (unused_imports)]
$($privacy)* use self::__time_steward_make_dynamic_callbacks_impl::DynamicEvent as $DynamicEvent;
#[allow (unused_imports)]
$($privacy)* use self::__time_steward_make_dynamic_callbacks_impl::DynamicPredictor as $DynamicPredictor;
#[allow (unused_imports)]
$($privacy)* use self::__time_steward_make_dynamic_callbacks_impl::StandardSettings as $StandardSettings;

};

}

pub struct GenericPredictorAccessor<B: Basics, E> {
  pub soonest_prediction: RefCell<Option<(B::Time, E)>>,
  pub dependencies: RefCell<(Vec<FieldId>, SiphashIdGenerator)>,
}
impl<B: Basics, E> GenericPredictorAccessor<B, E> {
  pub fn new() -> Self {
    GenericPredictorAccessor {
      soonest_prediction: RefCell::new (None),
      dependencies: RefCell::new((Vec::new(), SiphashIdGenerator::new())),
    }
  }
}
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_accessor_methods_for_predictor_accessor {
  ($B: ty, $get: ident) => {
    fn generic_data_and_extended_last_change(&self,
                                             id: FieldId)
                                             -> Option<(&FieldRc, &ExtendedTime<$B>)> {
      let dependencies: &mut (Vec<FieldId>, SiphashIdGenerator) = &mut*self.generic.dependencies.borrow_mut();
      dependencies.0.push(id);
      self.$get(id).map(|p| {
        {
          use bincode;
          use serde::Serialize;
          let mut serializer = bincode::Serializer::new (&mut dependencies.1);
          p.1.id.serialize (&mut serializer).unwrap();
        }
        p
      })
    }
  }
}
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_predictor_accessor_methods_for_predictor_accessor {
  ($B: ty, $DynamicEventFn: ident) => {
    fn predict_at_time <E: $crate::Event <Basics = $B>> (&self, time: <$B as $crate::Basics>::Time, event: E) {
      $crate::implementation_support::list_of_types::assert_contains_event::<$B, E>();
      if time < *self.unsafe_now() {
        return;
      }
      let mut guard = self.generic.soonest_prediction.borrow_mut();
      if let Some((ref old_time, _)) = *guard {
        if old_time <= &time {
          return;
        }
      }
      *guard = Some((time, StewardRc::new ($DynamicEventFn ::new (event))));
    }
  }
}


pub struct GenericMutator<B: Basics> {
  pub now: ExtendedTime<B>,
  pub generator: ChaChaRng,
}
impl<B: Basics> GenericMutator<B> {
  pub fn new(now: ExtendedTime<B>) -> Self {
    let generator = generator_for_event(now.id);
    GenericMutator {
      now: now,
      generator: generator,
    }
  }
}
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_accessor_methods_for_mutator {
  ($B: ty) => {
    fn unsafe_now(& self)->& <$B as $crate::Basics>::Time {& self.generic.now.base}
  }
}
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_mutator_methods_for_mutator {
  ($B: ty) => {
    fn extended_now(& self)->& ExtendedTime <$B> {& self.generic.now}
    fn gen_id(&mut self) -> RowId {RowId::from_rng (&mut self.generic.generator)}
  }
}
#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_rng_methods_for_mutator {
  ($B: ty) => {
    fn next_u32(&mut self) -> u32 {self.generic.generator.next_u32()}

    fn next_f32(&mut self) -> f32 {
      assert!(<$B as $crate::Basics>::allow_floats_unsafe(), "Using floating point numbers in TimeSteward events is forbidden by default because it is nondeterministic across platforms. If you know you don't need to be consistent between different computers, or you otherwise know EXACTLY what you're doing, you may add fn allow_floats_unsafe() {true} to your Basics.");
      self.generic.generator.next_f32()
    }
    fn next_f64(&mut self) -> f64 {
      assert!(<$B as $crate::Basics>::allow_floats_unsafe(), "Using floating point numbers in TimeSteward events is forbidden by default because it is nondeterministic across platforms. If you know you don't need to be consistent between different computers, or you otherwise know EXACTLY what you're doing, you may add fn allow_floats_unsafe() {true} to your Basics.");
      self.generic.generator.next_f64()
    }
  }
}

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_mutator_set_prefix {
  ($B: ty, $C: ty, $self_hack: ident, $id: ident, $data: ident) => {
    $crate::implementation_support::list_of_types::assert_contains_column::<$B, $C>();
  }
}


#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_insert_fiat_event_prefix {
  ($B: ty, $self_hack: ident, $time: ident, $E: ty) => {
    $crate::implementation_support::list_of_types::assert_contains_event::<$B, $E>();
    if $self_hack.valid_since() > $time {
      return Err(FiatEventOperationError::InvalidTime);
    }
  }
}






pub fn extended_time_of_fiat_event<B: Basics>(time: B::Time,
                                                  id: TimeId)
                                                  -> ExtendedTime<B> {
  ExtendedTime {
    base: time,
    iteration: 0,
    id: id.for_fiat_event_internal(),
  }
}

pub fn time_id_for_predicted_event(predictor_id: PredictorId,
                                   row_id: RowId,
                                   iteration: IterationType,
                                   dependencies_hash: DeterministicRandomId)
                                   -> TimeId {
  TimeId::new(&(predictor_id, row_id, iteration, dependencies_hash))
}
pub fn next_extended_time_of_predicted_event<B: Basics>
  (predictor_id: PredictorId,
   row_id: RowId,
   dependencies_hash: DeterministicRandomId,
   event_base_time: B::Time,
   from: &ExtendedTime<B>)
   -> Option<ExtendedTime<B>> {
  let (iteration, id) = match event_base_time.cmp(&from.base) {
    Ordering::Less => return None, // short-circuit
    Ordering::Greater => {
      (0, time_id_for_predicted_event(predictor_id, row_id, 0, dependencies_hash))
    }
    Ordering::Equal => {
      let id = time_id_for_predicted_event(predictor_id, row_id, from.iteration, dependencies_hash);
      if id > from.id {
        (from.iteration, id)
      } else {
        if from.iteration >= B::max_iteration() {
          panic!("Too many iterations at the same base time; probably an infinite loop")
        }
        (from.iteration + 1,
         time_id_for_predicted_event(predictor_id, row_id, from.iteration + 1, dependencies_hash))
      }
    }
  };
  Some(ExtendedTime {
    base: event_base_time,
    iteration: iteration,
    id: id,
  })
}

time_steward_dynamic_fn! (pub fn fields_are_equal <B: Basics> (id: ColumnId of <C: Column>, first: &FieldRc, second: &FieldRc)->bool {
  ::unwrap_field::<C>(first) == ::unwrap_field::<C>(second)
});

pub fn field_options_are_equal<B: Basics>(column_id: ColumnId,
                                          first: Option<&FieldRc>,
                                          second: Option<&FieldRc>)
                                          -> bool {
  match (first, second) {
    (None, None) => true,
    (Some(first), Some(second)) => fields_are_equal::<B>(column_id, first, second),
    _ => false,
  }
}

use bincode;
time_steward_dynamic_fn! (pub fn serialize_event <B: Basics, [W: Any + Write], [S: Any + bincode::SizeLimit]> (id: EventId of <E: Event <Basics = B>>, writer: &mut W, data: & StewardRc <Any>, size_limit: S) ->bincode::internal::Result<()> {
  try! (bincode::serialize_into (writer, &id, bincode::Bounded (8)));
  try! (bincode::serialize_into (writer, data.downcast_ref::<E>().expect ("id and type don't match"), size_limit));
  Ok (())
});

time_steward_dynamic_fn! (pub fn serialize_field <B: Basics, [W: Any + Write], [S: Any + bincode::SizeLimit]> (id: ColumnId of <C: Column>, writer: &mut W, data: & FieldRc, size_limit: S) ->bincode::internal::Result<()> {
  try! (bincode::serialize_into (writer, ::unwrap_field::<C>(data), size_limit));
  Ok (())
});

time_steward_dynamic_fn! (pub fn deserialize_field <B: Basics, [R: Any + Read], [S: Any + bincode::SizeLimit]> (id: ColumnId of <C: Column>, reader: &mut R, size_limit: S) ->bincode::internal::Result<FieldRc> {
  Ok (StewardRc::new (try! (bincode::deserialize_from::<R, C::FieldType, S> (reader, size_limit))))
});
