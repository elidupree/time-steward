//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use::{DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, Column, ExtendedTime,
            Basics, TimeSteward, FiatEventOperationError, ValidSince, StewardRc,
            FieldRc, Accessor};
use stewards::common::{self, Filter};
use std::collections::{HashMap, BTreeMap};
use rand::Rng;
use std::cmp::max;

#[derive (Clone)]
struct Field<B: Basics> {
  data: FieldRc,
  last_change: ExtendedTime<B>,
}



#[derive (Clone)]
struct StewardState<B: Basics> {
  last_event: Option<ExtendedTime<B>>,
  invalid_before: ValidSince<B::Time>,
  field_states: HashMap<FieldId, Field<B>>,
  fiat_events: BTreeMap<ExtendedTime<B>, Event<B>>,
}

struct StewardSettings<B: Basics> {
  settings: Settings<B>,
  constants: B::Constants,
}
#[derive (Clone)]
pub struct Steward<B: Basics> {
  state: StewardState<B>,
  settings: StewardRc<StewardSettings<B>>,
}
type StewardImpl<B> = Steward<B>;
pub struct Snapshot<B: Basics> {
  now: B::Time,
  state: StewardState<B>,
  settings: StewardRc<StewardSettings<B>>,
}
pub struct Mutator<'a, B: Basics> {
  generic: common::GenericMutator<B>,
  steward: &'a mut StewardImpl<B>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  generic: common::GenericPredictorAccessor<B, Event<B>>,
  steward: &'a StewardImpl<B>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = StewardRc<EventFn<B>>;

time_steward_common_dynamic_callback_structs! (Mutator, PredictorAccessor, DynamicEventFn, DynamicPredictorFn, DynamicPredictor, Settings);

impl<B: Basics> ::Accessor<B> for Snapshot<B> {
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.state.get(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.settings.constants
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> ::Accessor<B> for Mutator<'a, B> {
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.steward.state.get(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
  time_steward_common_accessor_methods_for_mutator!(B);
}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn get_impl(&self, id: FieldId) -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.steward.state.get(id)
  }
}
impl<'a, B: Basics> ::Accessor<B> for PredictorAccessor<'a, B> {
  time_steward_common_accessor_methods_for_predictor_accessor!(B, get_impl);
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.internal_now().base
  }
}

impl<B: Basics> ::MomentaryAccessor<B> for Snapshot<B> {}
impl<'a, B: Basics> ::MomentaryAccessor<B> for Mutator<'a, B> {}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn internal_now<'b>(&'b self) -> &'a ExtendedTime<B> {
    self.steward
        .state
        .last_event
        .as_ref()
        .expect("how can we be calling a predictor when there are no fields yet?")
  }
}
impl<'a, B: Basics> ::PredictorAccessor<B> for PredictorAccessor<'a, B> {
  time_steward_common_predictor_accessor_methods_for_predictor_accessor!(B, DynamicEventFn);
}
impl<B: Basics>::Snapshot<B> for Snapshot<B> {
  fn num_fields(&self) -> usize {
    self.state.field_states.len()
  }
}
use std::collections::hash_map;
pub struct SnapshotIter<'a, B: Basics>(hash_map::Iter<'a, FieldId, Field<B>>);
impl<'a, B: Basics> Iterator for SnapshotIter<'a, B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  fn next(&mut self) -> Option<Self::Item> {
    (self.0).next().map(|(id, stuff)| (id.clone(), (&stuff.data, &stuff.last_change)))
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.0.size_hint()
  }
}
impl<'a, B: Basics> IntoIterator for &'a Snapshot<B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  type IntoIter = SnapshotIter <'a, B>;
  fn into_iter(self) -> Self::IntoIter {
    SnapshotIter(self.state.field_states.iter())
  }
}



impl<'a, B: Basics> ::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    self.steward.state.set_opt::<C>(id, data, &self.generic.now);
  }
  time_steward_common_mutator_methods_for_mutator!(B);
}
impl<'a, B: Basics> Rng for Mutator<'a, B> {
  time_steward_common_rng_methods_for_mutator!(B);
}


impl<B: Basics> StewardState<B> {
  fn get(&self, id: FieldId) -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.field_states
        .get(&id)
        .map(|something| (&something.data, &something.last_change))
  }
  fn set<C: Column>(&mut self, id: RowId, value: C::FieldType, time: &ExtendedTime<B>) {
    self.field_states
        .insert(FieldId::new(id, C::column_id()),
                Field {
                  data: StewardRc::new(value),
                  last_change: time.clone(),
                });
  }
  fn remove<C: Column>(&mut self, id: RowId) {
    self.field_states
        .remove(&FieldId::new(id, C::column_id()));
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
    let empty = Vec::new();
    let predicted_events_iter = self.state.field_states.keys().flat_map(|field_id| {
      let column = field_id.column_id;
      self.settings
          .settings
          .predictors_by_column
          .get(&column)
          .unwrap_or(&empty)
          .iter()
          .filter_map(move |predictor| {
            let generic;
            {
              let mut pa = PredictorAccessor {
                generic: common::GenericPredictorAccessor::new(),
                steward: self,
              };
              (predictor.function)(&mut pa, field_id.row_id);
              generic = pa.generic;
            }
            let dependencies_hash = generic.dependencies.borrow().1.generate();
            generic.soonest_prediction.map(|(event_base_time, event)| {
              let extended = common::next_extended_time_of_predicted_event(predictor.predictor_id,
                                                                          field_id.row_id,
                                                                          dependencies_hash,
                                                                          event_base_time,
                                                                          &self.state
                                                                               .last_event
                                                                               .as_ref()
                                                                               .expect("how can \
                                                                                        we be ca\
                                                                                        lling a \
                                                                                        predicto\
                                                                                        r when t\
                                                                                        here are \
                                                                                        no field\
                                                                                        s yet?"))
                               .expect("this should only fail if the time was in the past, a \
                                        case that was already ruled out");
              (extended, event)
            })
          })
    });
    let events_iter = first_fiat_event_iter.chain(predicted_events_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    event(&mut Mutator {
      generic: common::GenericMutator::new(event_time.clone()),
      steward: &mut *self,
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

impl<B: Basics> TimeSteward<B> for Steward<B> {
  type Snapshot = Snapshot<B>;
  type Settings = Settings<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max(self.state.invalid_before.clone(),
        match self.state.last_event {
          None => ValidSince::TheBeginning,
          Some(ref time) => ValidSince::After(time.base.clone()),
        })
  }

  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    StewardImpl {
      state: StewardState {
        last_event: None,
        invalid_before: ValidSince::TheBeginning,
        field_states: HashMap::new(),
        fiat_events: BTreeMap::new(),
      },
      settings: StewardRc::new(StewardSettings {
        settings: settings,
        constants: constants,
      }),
    }
  }

  fn from_snapshot<'a, S: ::Snapshot<B>>(snapshot: &'a S, settings: Self::Settings) -> Self
    where &'a S: IntoIterator<Item =::SnapshotEntry<'a, B>>
  {
    let mut result = StewardImpl {
      state: StewardState {
        last_event: None,
        invalid_before: ValidSince::Before(snapshot.now().clone()),
        field_states: HashMap::new(),
        fiat_events: BTreeMap::new(),
      },
      settings: StewardRc::new(StewardSettings {
        settings: settings,
        constants: snapshot.constants().clone(),
      }),
    };
    result.state.field_states = snapshot.into_iter()
                                        .map(|(id, stuff)| {
                                          if match result.state.last_event {
                                            None => true,
                                            Some(ref time) => stuff.1 > time,
                                          } {
                                            result.state.last_event = Some(stuff.1.clone());
                                          }
                                          (id,
                                           Field {
                                            data: stuff.0.clone(),
                                            last_change: stuff.1.clone(),
                                          })
                                        })
                                        .collect();
    result
  }

  fn insert_fiat_event<E: ::EventFn<B>>(&mut self,
                                             time: B::Time,
                                             id: DeterministicRandomId,
                                             event: E)
                                             -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.insert(common::extended_time_of_fiat_event(time, id),
                                        StewardRc::new(DynamicEventFn::new(event))) {
      None => Ok(()),
      Some(_) => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.remove(& common::extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(_) => Ok(()),
    }
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    self.update_until_beginning_of(time);
    Some(Snapshot {
      now: time.clone(),
      state: self.state.clone(),
      settings: self.settings.clone(),
    })
  }
}
