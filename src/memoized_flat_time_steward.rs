//! A flat TimeSteward implementation that has good asymptotic performance for all operations.
//!
//! This implementation demonstrates the basic principles of how a flat TimeSteward can be efficient, without delving into serious optimizations or multithreading.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, ColumnId, FieldId, PredictorId,
            Column, ExtendedTime, EventRng, Basics, TimeSteward, FiatEventOperationResult,
            ValidSince, TimeStewardLifetimedMethods, TimeStewardStaticMethods};

use std::collections::{HashMap, BTreeMap, HashSet};
use std::collections::hash_map::Entry;
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use std::ops::Drop;
use rand::Rng;
use insert_only;

type SnapshotIdx = u64;

#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Rc<Any>,
}
struct SnapshotField<B: Basics> {
  data: Option<Field<B>>,
  touched_by_steward: Cell<bool>,
}


type FieldsMap<B: Basics> = HashMap<FieldId, Field<B>>;

struct Fields<B: Basics> {
  field_states: FieldsMap<B>,
  changed_since_snapshots: BTreeMap<SnapshotIdx,
                                    Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>>,
}


#[derive (Clone)]
struct Prediction<B: Basics> {
  predictor_id: PredictorId,
  prediction_is_about_row_id: RowId,
  predictor_accessed: Vec<FieldId>,
  what_will_happen: Option<(ExtendedTime<B>, Event<B>)>,
}

struct StewardShared<B: Basics> {
  predictors_by_column: HashMap<ColumnId, Vec<Predictor<B>>>,
  predictors_by_id: HashMap<PredictorId, Predictor<B>>,
  constants: B::Constants,
  fields: RefCell<Fields<B>>,
}
#[derive (Clone)]
struct StewardOwned<B: Basics> {
  last_event: Option<ExtendedTime<B>>,
  fiat_events: BTreeMap<ExtendedTime<B>, Event<B>>,
  next_snapshot: SnapshotIdx,

  predictions_by_time: BTreeMap<ExtendedTime<B>, Rc<Prediction<B>>>,
  predictions_by_id: HashMap<(RowId, PredictorId), Rc<Prediction<B>>>,
  prediction_dependencies: HashMap<FieldId, HashSet<(RowId, PredictorId)>>,
}
#[derive (Clone)]
pub struct Steward<B: Basics> {
  owned: StewardOwned<B>,
  shared: Rc<StewardShared<B>>,
}
pub struct Snapshot<B: Basics> {
  now: B::Time,
  index: SnapshotIdx,
  field_states: Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>,
  shared: Rc<StewardShared<B>>,
}
pub struct Mutator<'a, B: Basics> {
  now: ExtendedTime<B>,
  steward: &'a mut StewardOwned<B>,
  shared: &'a StewardShared<B>,
  fields: &'a mut Fields<B>,
  generator: EventRng,
  predictions_needed: HashSet<(RowId, PredictorId)>,
}
struct PredictorAccessorResults<B: Basics> {
  soonest_prediction: Option<(B::Time, Event<B>)>,
  dependencies: Vec<FieldId>,
  dependencies_hasher: SiphashIdGenerator,
}
pub struct PredictorAccessor<'a, B: Basics> {
  predictor_id: PredictorId,
  about_row_id: RowId,
  internal_now: ExtendedTime<B>,
  steward: RefCell<&'a mut StewardOwned<B>>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  results: RefCell<PredictorAccessorResults<B>>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = Rc<EventFn<B>>;
pub type Predictor<B> = super::Predictor<PredictorFn<B>>;
pub type PredictorFn<B> = for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId);



impl<B: Basics> Drop for Snapshot<B> {
  fn drop(&mut self) {
    self.shared.fields.borrow_mut().changed_since_snapshots.remove(&self.index);
  }
}

impl<B: Basics> super::Accessor<B> for Snapshot<B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    let field_id = FieldId {
      row_id: id,
      column_id: C::column_id(),
    };
    extract_field_info::<B, C>(self.field_states
                                   .get_default(field_id, || {
                                     SnapshotField {
                                       data: self.shared
                                                 .fields
                                                 .borrow()
                                                 .field_states
                                                 .get(&field_id)
                                                 .cloned(),
                                       touched_by_steward: Cell::new(false),
                                     }
                                   })
                                   .data
                                   .as_ref())
      .map(|p| (p.0, &p.1.base))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    self.fields.get::<C>(id).map(|p| (p.0, &p.1.base))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    let field_id = FieldId {
      row_id: id,
      column_id: C::column_id(),
    };
    let mut results = self.results.borrow_mut();
    self.steward
        .borrow_mut()
        .prediction_dependencies
        .entry(field_id)
        .or_insert(HashSet::new())
        .insert((self.about_row_id, self.predictor_id));
    results.dependencies.push(field_id);
    self.fields.get::<C>(id).map(|p| {
      p.1.id.hash(&mut results.dependencies_hasher);
      (p.0, &p.1.base)
    })
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
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
impl<'a, B: Basics> super::PredictorAccessor<B, EventFn<B>> for PredictorAccessor<'a, B> {
  fn predict_immediately(&mut self, event: Event<B>) {
    let t = self.internal_now.base.clone();
    self.predict_at_time(&t, event);
  }
  fn predict_at_time(&mut self, time: &B::Time, event: Event<B>) {
    if time < &self.internal_now.base {
      return;
    }
    let mut results = self.results.borrow_mut();
    if let Some((ref old_time, _)) = results.soonest_prediction {
      if old_time <= time {
        return;
      }
    }
    results.soonest_prediction = Some((time.clone(), event));
  }
}
impl<B: Basics> super::Snapshot<B> for Snapshot<B> {}

impl<'a, B: Basics> super::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    let field_id = FieldId {
      row_id: id,
      column_id: C::column_id(),
    };
    let old_value = self.fields.field_states.get(&field_id).cloned();
    let existence_changed = self.fields.set_opt::<C>(id, data, &self.now);
    for snapshot_map in self.fields.changed_since_snapshots.iter().rev() {
      let info = snapshot_map.1.get_default(field_id, || {
        SnapshotField {
          data: old_value.clone(),
          touched_by_steward: Cell::new(false),
        }
      });
      if info.touched_by_steward.get() {
        break;
      }
      info.touched_by_steward.set(true);
    }

    if existence_changed {
      self.shared.predictors_by_column.get(&C::column_id()).map(|predictors| {
        for predictor in predictors {
          self.predictions_needed.insert((id, predictor.predictor_id));
        }
      });
    }
    if let Entry::Occupied(entry) = self.steward.prediction_dependencies.entry(field_id) {
      for prediction in entry.get() {
        self.predictions_needed.insert(prediction.clone());
      }
      entry.remove();
    }
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

fn extract_field_info<B: Basics, C: Column>(option: Option<&Field<B>>)
                                            -> Option<(&C::FieldType, &ExtendedTime<B>)> {
  option.map(|something| {
    (something.data
              .downcast_ref::<C::FieldType>()
              .expect("a field had the wrong type for its column")
              .borrow(),
     &something.last_change)
  })

}

fn get<C: Column, Value>(map: &HashMap<FieldId, Value>, id: RowId) -> Option<&Value> {
  map.get(&FieldId {
    row_id: id,
    column_id: C::column_id(),
  })

}

impl<B: Basics> Fields<B> {
  fn get<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &ExtendedTime<B>)> {
    let field_states = &self.field_states;
    extract_field_info::<B, C>(get::<C, Field<B>>(field_states, id))
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn set<C: Column>(&mut self, id: RowId, value: C::FieldType, time: &ExtendedTime<B>) -> bool {
    let field = Field {
      data: Rc::new(value),
      last_change: time.clone(),
    };
    match self.field_states
              .entry(FieldId {
                row_id: id,
                column_id: C::column_id(),
              }) {
      Entry::Occupied(mut entry) => {
        entry.insert(field);
        false
      }
      Entry::Vacant(entry) => {
        entry.insert(field);
        true
      }
    }
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn remove<C: Column>(&mut self, id: RowId) -> bool {
    self.field_states
        .remove(&FieldId {
          row_id: id,
          column_id: C::column_id(),
        })
        .is_some()
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn set_opt<C: Column>(&mut self,
                        id: RowId,
                        value_opt: Option<C::FieldType>,
                        time: &ExtendedTime<B>)
                        -> bool {
    if let Some(value) = value_opt {
      self.set::<C>(id, value, time)
    } else {
      self.remove::<C>(id)
    }
  }
}
impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, Event<B>)> {
    let first_fiat_event_iter = self.owned
                                    .fiat_events
                                    .iter()
                                    .map(|ev| (ev.0.clone(), ev.1.clone()))
                                    .take(1);
    let first_predicted_event_iter = self.owned
                                         .predictions_by_time
                                         .iter()
                                         .map(|pair| {
                                           (pair.0.clone(),
                                            pair.1
                                                .what_will_happen
                                                .as_ref()
                                                .expect("a prediction that predicted nothing was \
                                                         stored in predictions")
                                                .1
                                                .clone())
                                         })
                                         .take(1);
    /* let predicted_events_iter = self.settings
     * .predictors
     * .iter()
     * .flat_map(|predictor|
     * TODO change field_states
     * to separate by field type, for efficiency,
     * like the haskell does?
     * self.state.field_states.keys().filter_map(move |field_id|
     * if field_id.column_id != predictor.column_id {
     * None
     * } else {
     * let mut pa = PredictorAccessor{
     * steward: self,
     * soonest_prediction: None,
     * dependencies_hasher: SiphashIdGenerator::new(),
     * };
     * (predictor.function)(&mut pa, field_id.row_id);
     * let dependencies_hash = pa.dependencies_hasher.generate();
     * pa.soonest_prediction.and_then(|(event_base_time, event)|
     * super::next_extended_time_of_predicted_event(
     * predictor.predictor_id,
     * field_id.row_id,
     * dependencies_hash,
     * event_base_time,
     * &self.state.last_event.as_ref().expect ("how can we be calling a predictor when there are no fields yet?")
     * ).map(|event_time| (event_time, event)))})); */
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn get_predictor(&self, predictor_id: PredictorId) -> &Predictor<B> {
    self.shared
        .predictors_by_id
        .get(&predictor_id)
        .expect("somehow a PredictorId appeared with no associated predictor")
  }

  fn clear_prediction(&mut self, row_id: RowId, predictor_id: PredictorId) {
    if let Some(prediction) = self.owned.predictions_by_id.get(&(row_id, predictor_id)) {
      for field_id in prediction.predictor_accessed.iter() {
        if let Entry::Occupied(mut entry) = self.owned
                                                .prediction_dependencies
                                                .entry(field_id.clone()) {
          entry.get_mut().remove(&(row_id, predictor_id));
          if entry.get().is_empty() {
            entry.remove();
          }
        }
      }
      if let Some((ref when, _)) = prediction.what_will_happen {
        self.owned.predictions_by_time.remove(when).expect("prediction records were inconsistent");
      }
    }
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    let predictions_needed;

    {
      let field_ref = &mut *self.shared.fields.borrow_mut();
      let mut mutator = Mutator {
        now: event_time.clone(),
        steward: &mut self.owned,
        shared: &self.shared,
        fields: field_ref,
        generator: super::generator_for_event(event_time.id),
        predictions_needed: HashSet::new(),
      };
      event(&mut mutator);
      predictions_needed = mutator.predictions_needed;
    }
    // if it was a fiat event, clean it up:
    self.owned.fiat_events.remove(&event_time);
    self.owned.last_event = Some(event_time);

    for (row_id, predictor_id) in predictions_needed {
      self.clear_prediction(row_id, predictor_id);
      let now = self.owned
                    .last_event
                    .clone()
                    .expect("how can we be calling a predictor when there are no fields yet?");
      let function = self.get_predictor(predictor_id).function.clone();
      let results;
      {
        let field_ref = &*self.shared.fields.borrow();
        let mut pa = PredictorAccessor {
          predictor_id: predictor_id,
          about_row_id: row_id,
          internal_now: now,
          steward: RefCell::new(&mut self.owned),
          shared: &self.shared,
          fields: field_ref,
          results: RefCell::new(PredictorAccessorResults {
            soonest_prediction: None,
            dependencies: Vec::new(),
            dependencies_hasher: SiphashIdGenerator::new(),
          }),
        };
        (function)(&mut pa, row_id);
        results = pa.results.into_inner();
      }
      let dependencies_hash = results.dependencies_hasher.generate();
      let prediction = Rc::new(Prediction {
        predictor_id: predictor_id,
        prediction_is_about_row_id: row_id,
        predictor_accessed: results.dependencies,
        what_will_happen: results.soonest_prediction.and_then(|(event_base_time, event)| {
          super::next_extended_time_of_predicted_event(predictor_id,
                                                       row_id,
                                                       dependencies_hash,
                                                       event_base_time,
                                                       &self.owned
                                                            .last_event
                                                            .as_ref()
                                                            .expect("how can we be calling a \
                                                                     predictor when there are no \
                                                                     fields yet?"))
            .map(|event_time| (event_time, event))
        }),
      });
      self.owned.predictions_by_id.insert((row_id, predictor_id), prediction.clone());
      if let Some((ref time, _)) = prediction.what_will_happen {
        self.owned.predictions_by_time.insert(time.clone(), prediction.clone());
      }
    }
  }

  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    while let Some(ev) = self.next_event().filter(|ev| ev.0.base < *target_time) {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
}


impl<'a, B: Basics> TimeStewardLifetimedMethods<'a, B> for Steward<B> {
  type Mutator = Mutator <'a, B>;
  type PredictorAccessor = PredictorAccessor <'a, B>;
}
impl<B: Basics> TimeStewardStaticMethods<B> for Steward<B> {
  type EventFn = EventFn <B>;
  type PredictorFn = PredictorFn <B>;
  type Snapshot = Snapshot<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    match self.owned.last_event {
      None => ValidSince::TheBeginning,
      Some(ref time) => ValidSince::After(time.base.clone()),
    }
  }

  fn new_empty(constants: B::Constants,
               predictors: Vec<super::Predictor<Self::PredictorFn>>)
               -> Self {
    let mut predictors_by_id = HashMap::new();
    let mut predictors_by_column = HashMap::new();
    for predictor in predictors {
      predictors_by_id.insert(predictor.predictor_id, predictor.clone());
      predictors_by_column.entry(predictor.column_id).or_insert(Vec::new()).push(predictor);
    }

    Steward {
      owned: StewardOwned {
        last_event: None,
        fiat_events: BTreeMap::new(),
        next_snapshot: 0,
        predictions_by_time: BTreeMap::new(),
        predictions_by_id: HashMap::new(),
        prediction_dependencies: HashMap::new(),
      },
      shared: Rc::new(StewardShared {
        predictors_by_id: predictors_by_id,
        predictors_by_column: predictors_by_column,
        constants: constants,
        fields: RefCell::new(Fields {
          field_states: HashMap::new(),
          changed_since_snapshots: BTreeMap::new(),
        }),
      }),
    }
  }

  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Event<B>)
                       -> FiatEventOperationResult {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.owned.fiat_events.insert(super::extended_time_of_fiat_event(time, id), event) {
      None => FiatEventOperationResult::Success,
      Some(_) => FiatEventOperationResult::InvalidInput,
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= *time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.owned.fiat_events.remove(&super::extended_time_of_fiat_event(time.clone(), id)) {
      None => FiatEventOperationResult::InvalidInput,
      Some(_) => FiatEventOperationResult::Success,
    }
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= *time {
        return None;
      }
    }
    self.update_until_beginning_of(time);

    let result = Some(Snapshot {
      now: time.clone(),
      index: self.owned.next_snapshot,
      field_states: self.shared
                        .fields
                        .borrow_mut()
                        .changed_since_snapshots
                        .entry(self.owned.next_snapshot)
                        .or_insert(Rc::new(insert_only::HashMap::new()))
                        .clone(),
      shared: self.shared.clone(),
    });

    self.owned.next_snapshot += 1;
    result
  }
}
impl<B: Basics> TimeSteward<B> for Steward<B> {}
