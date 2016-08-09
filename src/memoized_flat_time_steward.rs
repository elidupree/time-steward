//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, PredictorId, Column, ExtendedTime,
            EventRng, Basics, TimeSteward, FiatEventOperationResult, ValidSince,
            TimeStewardLifetimedMethods};

use std::collections::{HashMap, BTreeMap};
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use rand::Rng;
use ::insert_only;

type SnapshotIdx = u64;

#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Rc<Any>,
}
struct SnapshotField<B: Basics> {
  data: Option <Field<B>>,
  touched_by_steward: Cell <bool>,
}


type FieldsMap<B: Basics>  =HashMap<FieldId, Field<B>>;

struct Fields<B: Basics>  {
  field_states: FieldsMap <B>,
  changed_since_snapshots: BTreeMap<SnapshotIdx, insert_only::HashMap<FieldId, SnapshotField <B> >>,
}


#[derive (Clone)]
struct Prediction<B: Basics> {
  predictor_idx: u32,
  predictor_id: PredictorId,
  prediction_is_about_row_id: RowId,
  predictor_accessed: Vec<FieldId>,
  what_will_happen: Option<(ExtendedTime<B>, Event<B>)>,
}

struct StewardShared <B: Basics> {
  predictors: Vec<Predictor<B>>,
  constants: B::Constants,
  fields: RefCell< Fields <B>>,
}
#[derive (Clone)]
pub struct Steward<B: Basics> {
  last_event: Option<ExtendedTime<B>>,
  fiat_events: BTreeMap<ExtendedTime<B>, Event<B>>,
  next_snapshot: SnapshotIdx,

  predictions: BTreeMap<ExtendedTime<B>, Prediction<B>>,
  prediction_dependencies: HashMap<FieldId, HashMap<PredictorId, Prediction <B>>>,
  shared: Rc<StewardShared <B>>,
}
pub struct Snapshot<'a, B: Basics> {
  now: B::Time,
  index: SnapshotIdx,
shared: &'a StewardShared <B>,
}
pub struct Mutator<'a, B: Basics> {
  now: ExtendedTime<B>,
  steward: &'a mut Steward<B>,
  fields: & 'a mut Fields <B>,
  generator: EventRng,
}
pub struct PredictorAccessor<'a, B: Basics> {
  steward: &'a mut Steward<B>,
  fields: &'a Fields <B>,
  soonest_prediction: Option<(B::Time, Event<B>)>,
  dependencies_hasher: SiphashIdGenerator,
}
pub type Event<B> = Rc<for<'d, 'e> Fn(&'d mut Mutator<'e, B>)>;
pub type Predictor<B> = super::Predictor<PredictorFn<B>>;
pub type PredictorFn<B> = Rc<for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId)>;



impl<'a, B: Basics> super::Accessor<B> for Snapshot<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    let field_id =FieldId {
        row_id: id,
        column_id: C::column_id(),
      };
    let fields = & self.shared.fields.borrow();
    let my_field_states = fields.changed_since_snapshots.get (& self.index).expect ("a map should exist for this snapshot");
    extract_field_info::<B, C> (my_field_states.get_default (field_id, | |
      SnapshotField {data: fields.field_states.get (& field_id).cloned(), touched_by_steward: Cell::new (false)}
    ).data.as_ref()).map(|p| p.0)
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    self.fields.get (id).map(|p| p.0)
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    let field_id =FieldId {
        row_id: id,
        column_id: C::column_id(),
      };
    self.steward.prediction_dependencies.entry (field_id).or_insert (HashMap::new()).insert (self.predictor_id, something);
    self.fields.get (id).map(|p| {
      p.1.id.hash(&mut self.dependencies_hasher);
      p.0
    })
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.shared.constants
  }
}

impl<'a, B: Basics> super::MomentaryAccessor<B> for Snapshot<'a, B> {
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
  fn internal_now<'b>(&'b self) -> &'b ExtendedTime<B> {
    self.steward
        .last_event
        .as_ref()
        .expect("how can we be calling a predictor when there are no fields \
                yet?")
  }
}
impl<'a, B: Basics> super::PredictorAccessor<B> for PredictorAccessor<'a, B> {
  type Event = Event<B>;
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
impl<'a, B: Basics> super::Snapshot<B> for Snapshot<'a, B> {}

impl<'a, B: Basics> super::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    let field_id =FieldId {
        row_id: id,
        column_id: C::column_id(),
      };
    let old_value = self.fields.field_states.get (& field_id).cloned();
    self.fields.set_opt::<C>(id, data, &self.now);
    for snapshot_map in self.fields.changed_since_snapshots.iter().rev() {
      let info = snapshot_map.1.get_default (field_id, | |
            SnapshotField {data: old_value.clone(), touched_by_steward: Cell::new (false)}
          );
      if info.touched_by_steward.get() {break;}
      info.touched_by_steward.set (true);
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
    self.and_then(|x| if predicate(&x) {
      Some(x)
    } else {
      None
    })
  }
}

fn extract_field_info<B: Basics, C: Column>(option: Option <& Field <B>>) -> Option<(&C::FieldType, &ExtendedTime<B>)> {
option.map(|something| {
        (something.data
          .downcast_ref::<C::FieldType>()
          .expect("a field had the wrong type for its column")
          .borrow(),
         &something.last_change)
      })

}

fn get<C: Column, Value>(map: & HashMap<FieldId, Value >, id: RowId)->Option <& Value> {
  map.get(&FieldId {
        row_id: id,
        column_id: C::column_id(),
      })

}

impl<B: Basics> Fields <B> {
  fn get<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &ExtendedTime<B>)> {
    let field_states =& self.field_states;
    extract_field_info::<B, C> (get::<C, Field <B>> (field_states, id))
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
impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, Event<B>)> {
    let first_fiat_event_iter =
    // range is unstable
    // https://doc.rust-lang.org/std/collections/struct.BTreeMap.html#method.range
    // and cannot be enabled in stable rustc
    // https://stackoverflow.com/questions/30975088/use-of-unstable-library-feature-how-can-i-fix-those
    // so use something else...
    //  self.fiat_events.range(Excluded(&self.last_event), Unbounded).next();
      self.fiat_events.iter().map(|ev| (ev.0.clone(), ev.1.clone())).take (1);
let first_predicted_event_iter =
      self.predictions.iter().map (| pair | (pair.0.clone(), pair.1.what_will_happen.as_ref().expect ("a prediction that predicted nothing was stored in predictions").1.clone())).take (1);
    /*let predicted_events_iter = self.settings
      .predictors
      .iter()
      .flat_map(|predictor|
      // TODO change field_states
      // to separate by field type, for efficiency,
      // like the haskell does?
      self.state.field_states.keys().filter_map(move |field_id|
        if field_id.column_id != predictor.column_id {
          None
        } else {
          let mut pa = PredictorAccessor{
              steward: self,
              soonest_prediction: None,
              dependencies_hasher: SiphashIdGenerator::new(),
            };
          (predictor.function)(&mut pa, field_id.row_id);
          let dependencies_hash = pa.dependencies_hasher.generate();
          pa.soonest_prediction.and_then(|(event_base_time, event)|
            super::next_extended_time_of_predicted_event(
              predictor.predictor_id,
              field_id.row_id,
              dependencies_hash,
              event_base_time,
              &self.state.last_event.as_ref().expect ("how can we be calling a predictor when there are no fields yet?")
            ).map(|event_time| (event_time, event)))}));*/
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    
    event(&mut Mutator {
      now: event_time.clone(),
      steward: &mut *self,
      fields: &mut *self.shared.fields.borrow_mut(),
      generator: super::generator_for_event(event_time.id),
    });
    // if it was a fiat event, clean it up:
    self.fiat_events.remove(&event_time);
    self.last_event = Some(event_time);
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
  type Snapshot = Snapshot<'a, B>;
  fn snapshot_before(&'a mut self, time: &B::Time) -> Option<Self::Snapshot> {
    if let Some(ref change) = self.last_event {
      if change.base >= *time {
        return None;
      }
    }
    self.update_until_beginning_of(time);
    
    let result = Some(Snapshot {
      now: time.clone(),
index: self.next_snapshot,
shared: self.shared.as_ref(),
    });
    self.shared.fields.borrow_mut().changed_since_snapshots.insert (self.next_snapshot, insert_only::HashMap::new());
    self.next_snapshot += 1;
    result
  }
}
impl<B: Basics> TimeSteward<B> for Steward<B> {
  type Event = Event<B>;
  type Predictor = Predictor<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    match self.last_event {
      None => ValidSince::TheBeginning,
      Some(ref time) => ValidSince::After(time.base.clone()),
    }
  }
  fn new_empty(constants: B::Constants, predictors: Vec<Self::Predictor>) -> Self {
    Steward{
        last_event: None,
        fiat_events: BTreeMap::new(),
next_snapshot: 0,
        predictions: BTreeMap::new(),
        prediction_dependencies: HashMap::new(),
shared: Rc::new(StewardShared {
        predictors: predictors,
        constants: constants,
        fields:RefCell::new (Fields {
        field_states: HashMap::new(),
        changed_since_snapshots: BTreeMap::new(),
      })}),
    }
  }

  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Self::Event)
                       -> FiatEventOperationResult {
    if let Some(ref change) = self.last_event {
      if change.base >= time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.fiat_events.insert(super::extended_time_of_fiat_event(time, id), event) {
      None => FiatEventOperationResult::Success,
      Some(_) => FiatEventOperationResult::InvalidInput,
    }
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    if let Some(ref change) = self.last_event {
      if change.base >= *time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.fiat_events.remove(&super::extended_time_of_fiat_event(time.clone(), id)) {
      None => FiatEventOperationResult::InvalidInput,
      Some(_) => FiatEventOperationResult::Success,
    }
  }
}
