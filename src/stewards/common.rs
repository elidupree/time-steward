use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use rand::{ChaChaRng, SeedableRng};
use {DeterministicRandomId, PredictorId, TimeId, RowId, FieldId, FieldRc, SiphashIdGenerator,
     IterationType, Basics, ExtendedTime, GenericExtendedTime, ColumnId, Column, ColumnList, ColumnListUser};

// https://github.com/rust-lang/rfcs/issues/1485
pub trait Filter<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self;
}
impl<T> Filter<T> for Option<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self {
    self.and_then(|x| { if predicate(&x) { Some(x) } else { None } })
  }
}

type EventRng = ChaChaRng;
fn generator_for_event(id: TimeId) -> EventRng {
  EventRng::from_seed(&[(id.data()[0] >> 32) as u32,
                        (id.data()[0] & 0xffffffff) as u32,
                        (id.data()[1] >> 32) as u32,
                        (id.data()[1] & 0xffffffff) as u32])
}

#[macro_export]
macro_rules! time_steward_common_dynamic_callback_structs {

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

#[derive (Clone)]
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

#[allow (unused_imports)]
use self::__time_steward_make_dynamic_callbacks_impl::DynamicEventFn as $DynamicEventFn;
#[allow (unused_imports)]
use self::__time_steward_make_dynamic_callbacks_impl::DynamicPredictor as $DynamicPredictor;
#[allow (unused_imports)]
use self::__time_steward_make_dynamic_callbacks_impl::DynamicPredictorFn as $DynamicPredictorFn;
pub use self::__time_steward_make_dynamic_callbacks_impl::StandardSettings as $StandardSettings;

}}

pub struct GenericPredictorAccessor<B: Basics, E> {
  pub soonest_prediction: Option<(B::Time, E)>,
  pub dependencies: RefCell<(Vec<FieldId>, SiphashIdGenerator)>,
}
impl<B: Basics, E> GenericPredictorAccessor<B, E> {
  pub fn new() -> Self {
    GenericPredictorAccessor {
      soonest_prediction: None,
      dependencies: RefCell::new((Vec::new(), SiphashIdGenerator::new())),
    }
  }
}
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
          let mut serializer = bincode::serde::Serializer::new (&mut dependencies.1);
          p.1.id.serialize (&mut serializer).unwrap();
        }
        p
      })
    }
  }
}
#[macro_export]
macro_rules! time_steward_common_predictor_accessor_methods_for_predictor_accessor {
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
#[macro_export]
macro_rules! time_steward_common_accessor_methods_for_mutator {
  ($B: ty) => {
    fn unsafe_now(& self)->& <$B as $crate::Basics>::Time {& self.generic.now.base}
  }
}
#[macro_export]
macro_rules! time_steward_common_mutator_methods_for_mutator {
  ($B: ty) => {
    fn extended_now(& self)->& ExtendedTime <$B> {& self.generic.now}
    fn gen_id(&mut self) -> RowId {RowId::from_rng (&mut self.generic.generator)}
  }
}
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





pub fn extended_time_of_fiat_event<BaseTime: Ord>(time: BaseTime,
                                                  id: TimeId)
                                                  -> GenericExtendedTime<BaseTime> {
  GenericExtendedTime {
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
pub fn next_extended_time_of_predicted_event<BaseTime: Ord>
  (predictor_id: PredictorId,
   row_id: RowId,
   dependencies_hash: DeterministicRandomId,
   event_base_time: BaseTime,
   from: &GenericExtendedTime<BaseTime>)
   -> Option<GenericExtendedTime<BaseTime>> {
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


fn check_equality<C: Column>(first: &FieldRc, second: &FieldRc)->bool {
  ::unwrap_field::<C>(first) == ::unwrap_field::<C>(second)
}
pub struct FieldEqualityTable (HashMap<ColumnId, fn(&FieldRc, &FieldRc)->bool>);
impl ColumnListUser for FieldEqualityTable{
  fn apply<C: Column>(&mut self) {
    self.0.insert(C::column_id(), check_equality::<C>);
  }
}
impl FieldEqualityTable{
  pub fn new <C: ColumnList>()->FieldEqualityTable{
    let mut result = FieldEqualityTable(HashMap::new());
    C::apply (&mut result);
    result
  }
  pub fn fields_are_equal(&self, column_id: ColumnId, first: & FieldRc, second: & FieldRc)->bool {
    (self.0.get (&column_id).expect ("Column missing from equality table; did you forget to list a column in Basics::Columns?")) (first, second)
  }
}