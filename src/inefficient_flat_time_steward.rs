//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, Column, ExtendedTime,
            EventRng, Basics, TimeSteward, FiatEventOperationResult, ValidSince,
            TimeStewardLifetimedMethods, TimeStewardStaticMethods};
use std::collections::{HashMap, BTreeMap};
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::RefCell;
use rand::Rng;

#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Rc<Any>,
}



#[derive (Clone)]
struct StewardState<B: Basics> {
  last_event: Option<ExtendedTime<B>>,
  field_states: HashMap<FieldId, Field<B>>,
  fiat_events: BTreeMap<ExtendedTime<B>, Event<B>>,
}

struct StewardSettings<B: Basics> {
  predictors: Vec<Predictor<B>>,
  constants: B::Constants,
}
#[derive (Clone)]
pub struct Steward<B: Basics> {
  state: StewardState<B>,
  settings: Rc<StewardSettings<B>>,
}
type StewardImpl<B> = Steward<B>;
pub struct Snapshot<B: Basics> {
  now: B::Time,
  state: StewardState<B>,
  settings: Rc<StewardSettings<B>>,
}
pub struct Mutator<'a, B: Basics> {
  now: ExtendedTime<B>,
  steward: &'a mut StewardImpl<B>,
  generator: EventRng,
}
pub struct PredictorAccessor<'a, B: Basics> {
  steward: &'a StewardImpl<B>,
  soonest_prediction: Option<(B::Time, Event<B>)>,
  dependencies_hasher: RefCell<SiphashIdGenerator>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = Rc<EventFn<B>>;
pub type Predictor<B> = super::Predictor<PredictorFn<B>>;
pub type PredictorFn<B> = for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId);



impl<B: Basics> super::Accessor<B> for Snapshot<B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    self.state.get::<C>(id).map(|p| (p.0, &p.1.base))
  }
  fn constants(&self) -> &B::Constants {
    &self.settings.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    self.steward.state.get::<C>(id).map(|p| (p.0, &p.1.base))
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    self.steward.state.get::<C>(id).map(|p| {
      p.1.id.hash(&mut*self.dependencies_hasher.borrow_mut());
      (p.0, &p.1.base)
    })
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
}

impl<B: Basics> super::MomentaryAccessor<B> for Snapshot<B> {
  fn now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> super::MomentaryAccessor<B> for Mutator<'a, B> {
  fn now(&self) -> &B::Time {
    &self.now.base
  }
}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn internal_now<'b>(&'b self) -> &'a ExtendedTime<B> {
    self.steward
        .state
        .last_event
        .as_ref()
        .expect("how can we be calling a predictor when there are no fields yet?")
  }
}
impl<'a, B: Basics> super::PredictorAccessor<B, EventFn<B>> for PredictorAccessor<'a, B> {
  fn predict_immediately(&mut self, event: Event<B>) {
    let t = &self.internal_now().base;
    self.predict_at_time(t, event);
  }
  fn predict_at_time(&mut self, time: &B::Time, event: Event<B>) {
    if time < &self.internal_now().base {
      return;
    }
    if let Some((ref old_time, _)) = self.soonest_prediction {
      if old_time <= time {
        return;
      }
    }
    self.soonest_prediction = Some((time.clone(), event));
  }
}
impl<B: Basics> super::Snapshot<B> for Snapshot<B> {}

impl<'a, B: Basics> super::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    self.steward.state.set_opt::<C>(id, data, &self.now);
  }
  fn rng(&mut self) -> &mut EventRng {
    &mut self.generator
  }
  fn random_id(&mut self) -> RowId {
    RowId { data: [self.generator.gen::<u64>(), self.generator.gen::<u64>()] }
  }
}


// https://github.com/rust-lang/rfcs/issues/1485
trait Filter<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self;
}
impl<T> Filter<T> for Option<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self {
    self.and_then(|x| {
      if predicate(&x) {
        Some(x)
      } else {
        None
      }
    })
  }
}


impl<B: Basics> StewardState<B> {
  fn get<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &ExtendedTime<B>)> {
    self.field_states
        .get(&FieldId {
          row_id: id,
          column_id: C::column_id(),
        })
        .map(|something| {
          (something.data
                    .downcast_ref::<C::FieldType>()
                    .expect("a field had the wrong type for its column")
                    .borrow(),
           &something.last_change)
        })
  }
  fn set<C: Column>(&mut self, id: RowId, value: C::FieldType, time: &ExtendedTime<B>) {
    self.field_states
        .insert(FieldId {
                  row_id: id,
                  column_id: C::column_id(),
                },
                Field {
                  data: Rc::new(value),
                  last_change: time.clone(),
                });
  }
  fn remove<C: Column>(&mut self, id: RowId) {
    self.field_states
        .remove(&FieldId {
          row_id: id,
          column_id: C::column_id(),
        });
  }
  fn set_opt<C: Column>(&mut self,
                        id: RowId,
                        value_opt: Option<C::FieldType>,
                        time: &ExtendedTime<B>) {
    if let Some(value) = value_opt {
      self.set::<C>(id, value, time);
    } else {
      self.remove::<C>(id);
    }
  }
}
impl<B: Basics> StewardImpl<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, Event<B>)> {
    let first_fiat_event_iter = self.state
                                    .fiat_events
                                    .iter()
                                    .map(|ev| (ev.0.clone(), ev.1.clone()));
    let predicted_events_iter = self.settings
                                    .predictors
                                    .iter()
                                    .flat_map(|predictor| {
                                      self.state.field_states.keys().filter_map(move |field_id| {
              if field_id.column_id != predictor.column_id {
                None
              } else {
                let mut pa = PredictorAccessor {
                  steward: self,
                  soonest_prediction: None,
                  dependencies_hasher: RefCell::new (SiphashIdGenerator::new()),
                };
                (predictor.function)(&mut pa, field_id.row_id);
                let dependencies_hash = pa.dependencies_hasher.borrow().generate();
                pa.soonest_prediction.and_then(|(event_base_time, event)| {
                  super::next_extended_time_of_predicted_event(predictor.predictor_id,
                                                               field_id.row_id,
                                                               dependencies_hash,
                                                               event_base_time,
                                                               &self.state
                                                                    .last_event
                                                                    .as_ref()
                                                                    .expect("how can we be \
                                                                             calling a predictor \
                                                                             when there are no \
                                                                             fields yet?"))
                    .map(|event_time| (event_time, event))
                })
              }
            })
                                    });
    let events_iter = first_fiat_event_iter.chain(predicted_events_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    event(&mut Mutator {
      now: event_time.clone(),
      steward: &mut *self,
      generator: super::generator_for_event(event_time.id),
    });
    // if it was a fiat event, clean it up:
    self.state.fiat_events.remove(&event_time);
    self.state.last_event = Some(event_time);
  }

  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    while let Some(ev) = self.next_event().filter(|ev| ev.0.base < *target_time) {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
}


impl<B: Basics> Steward<B> {}
impl<'a, B: Basics> TimeStewardLifetimedMethods<'a, B> for Steward<B> {
  type Mutator = Mutator <'a, B>;
  type PredictorAccessor = PredictorAccessor <'a, B>;
}
impl<B: Basics> TimeStewardStaticMethods<B> for Steward<B> {
  type EventFn = EventFn <B>;
  type PredictorFn = PredictorFn <B>;
  type Snapshot = Snapshot<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    match self.state.last_event {
      None => ValidSince::TheBeginning,
      Some(ref time) => ValidSince::After(time.base.clone()),
    }
  }

  fn new_empty(constants: B::Constants,
               predictors: Vec<super::Predictor<Self::PredictorFn>>)
               -> Self {
    StewardImpl {
      state: StewardState {
        last_event: None,
        field_states: HashMap::new(),
        fiat_events: BTreeMap::new(),
      },
      settings: Rc::new(StewardSettings {
        predictors: predictors,
        constants: constants,
      }),
    }
  }

  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Event<B>)
                       -> FiatEventOperationResult {
    if let Some(ref change) = self.state.last_event {
      if change.base >= time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.state.fiat_events.insert(super::extended_time_of_fiat_event(time, id), event) {
      None => FiatEventOperationResult::Success,
      Some(_) => FiatEventOperationResult::InvalidInput,
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    if let Some(ref change) = self.state.last_event {
      if change.base >= *time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.state.fiat_events.remove(&super::extended_time_of_fiat_event(time.clone(), id)) {
      None => FiatEventOperationResult::InvalidInput,
      Some(_) => FiatEventOperationResult::Success,
    }
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if let Some(ref change) = self.state.last_event {
      if change.base >= *time {
        return None;
      }
    }
    self.update_until_beginning_of(time);
    Some(Snapshot {
      now: time.clone(),
      state: self.state.clone(),
      settings: self.settings.clone(),
    })
  }
}
impl<B: Basics> TimeSteward<B> for Steward<B> {}
