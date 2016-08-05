
use super::{SiphashId, RowId, FieldId, Column, ExtendedTime, Basics, TimeSteward, FiatEventOperationResult};
use std::collections::{HashMap, BTreeMap};
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::{Borrow, Cow};
use std::rc::Rc;


#[derive (Clone)]
// struct StewardState<'a, B: Basics + 'a> {
struct StewardState<B: Basics> {
  last_change: ExtendedTime<B>, // B::Time,
  field_states: HashMap<FieldId, Rc<Any>>,
  // fiat_events: BTreeMap<ExtendedTime<B>, Event<'a, B>>,
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
type StewardImpl<B: Basics> = Steward<B>;
// pub struct Steward<'a, B: Basics + 'a> {
// steward: Cow<'a, StewardImpl<B>>,
// }
pub struct Snapshot<'a, B: Basics + 'a> {
  now: B::Time,
  steward: &'a StewardImpl<B>,
}
pub struct Mutator<'a, B: Basics + 'a> {
  now: B::Time,
  steward: &'a mut StewardImpl<B>,
}
pub struct PredictorAccessor<'a, B: Basics + 'a> {
  steward: &'a StewardImpl<B>,
  soonest_prediction: Option<(B::Time, Event<B>)>,
}
// pub type Event<'a, B: Basics + 'a> = super::Event<Mutator<'a, B>>;
pub type Event<B: Basics> = Rc<for<'d, 'e> Fn(&'d mut Mutator<'e, B>)>;
//type Prediction<B: Basics> = super::Prediction<B, Event<B>>;
// it seems impossible to write the lifetimes this way
// type Predictor<'a, B: Basics + 'a> = super::Predictor<B, Mutator<'a, B>, PredictorAccessor<'a, B>>;

type Predictor<B: Basics> = super::Predictor<PredictorFn<B>>;

type PredictorFn<B: Basics> = Rc<for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId)>;
//type PredictorFn<B: Basics> = Rc<for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId)
//                                              -> Prediction<B>>;
// -> Prediction<B, Event<B>>>;
// -> Prediction<B, Rc<for<'d, 'e> Fn(&'d mut Mutator<'e, B>)>>>;



impl<'a, B: Basics> super::Accessor<B> for Snapshot<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    self.steward.get::<C>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    self.steward.get::<C>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    self.steward.get::<C>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.steward.settings.constants
  }
}

impl<'a, B: Basics> super::MomentaryAccessor<B> for Snapshot<'a, B> {
  fn now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> super::MomentaryAccessor<B> for Mutator<'a, B> {
  fn now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> super::PredictorAccessor<B> for PredictorAccessor<'a, B> {
  type Event = Event<B>;
  fn predict_immediately(&mut self, event: Event<B>) {
    self.predict_at_time(&self.steward.state.last_change.base, event);
  }
  fn predict_at_time(&mut self, time: &B::Time, event: Event<B>) {
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
  fn get_mut<C: Column>(&mut self, id: RowId) -> Option<&mut C::FieldType>
    where C::FieldType: Clone
  {
    panic!("are we really doing this");
  }
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    self.steward.set_opt::<C>(id, data);
  }
  fn random_bits(&mut self, num_bits: u32) -> u64 {
    panic!("no randomness yet 1");
  }
  fn random_id(&mut self) -> SiphashId {
    panic!("no randomness yet 2");
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


impl<B: Basics> StewardImpl<B> {
  fn get<C: Column>(&self, id: RowId) -> Option<&C::FieldType> {
    self.state
      .field_states
      .get(&FieldId {
        row_id: id,
        column_id: C::column_id(),
      })
      .map(|something| something.downcast_ref::<C::FieldType>().unwrap().borrow())
  }
  fn set<C: Column>(&mut self, id: RowId, value: C::FieldType) {
    self.state
      .field_states
      .insert(FieldId {
                row_id: id,
                column_id: C::column_id(),
              },
              Rc::new(value));
  }
  fn remove<C: Column>(&mut self, id: RowId) {
    self.state
      .field_states
      .remove(&FieldId {
        row_id: id,
        column_id: C::column_id(),
      });
  }
  fn set_opt<C: Column>(&mut self, id: RowId, value_opt: Option<C::FieldType>) {
    if let Some(value) = value_opt {
      self.set::<C>(id, value);
    } else {
      self.remove::<C>(id);
    }
  }

  fn next_event(&self) -> Option<(ExtendedTime<B>, Event<B>)> {
    let first_fiat_event_iter =
    // range is unstable
    // https://doc.rust-lang.org/std/collections/struct.BTreeMap.html#method.range
    // and cannot be enabled in stable rustc
    // https://stackoverflow.com/questions/30975088/use-of-unstable-library-feature-how-can-i-fix-those
    // so use something else...
    //  self.state.fiat_events.range(Excluded(&self.state.last_change), Unbounded).next();
      self.state.fiat_events.iter().map(|ev| (ev.0.clone(), ev.1.clone()));
    let predicted_events_iter = self.settings
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
            };
          (predictor.function)(&mut pa, field_id.row_id);
          pa.soonest_prediction.and_then(|(event_base_time, event)|
            super::extended_time_of_predicted_event(
              predictor.predictor_id,
              field_id.row_id,
              event_base_time,
              &self.state.last_change
            ).map(|event_time| (event_time, event)))}));
    let events_iter = first_fiat_event_iter.chain(predicted_events_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    event(&mut Mutator {
      now: event_time.base.clone(),
      steward: &mut *self,
    });
    // if it was a fiat event, clean it up:
    self.state.fiat_events.remove(&event_time);
    self.state.last_change = event_time;
  }

  fn move_to_future_time(&mut self, future_t: ExtendedTime<B>) {
    if future_t < self.state.last_change {
      panic!("not defined for past times");
    }
    while let Some(ev) = self.next_event().filter(|ev| ev.0 < future_t) {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
}


// impl<'a, B: Basics> Steward<'a, B> {
impl<B: Basics> Steward<B> {
  fn new(constants: B::Constants, predictors: Vec<Predictor<B>>) -> Self {
    StewardImpl {
      state: StewardState {
        last_change: super::beginning_of_moment(super::BaseTime::min_time()),
        field_states: HashMap::new(),
        fiat_events: BTreeMap::new(),
      },
      settings: Rc::new(StewardSettings {
        predictors: predictors,
        constants: constants,
      }),
    }
  }
}
// impl<'a, B: Basics> super::TimeSteward<'a, B> for Steward<'a, B> {
// type S = Snapshot<'a, B>;
// type Event = Event<B>;
//
// fn valid_from(&self) -> B::Time {}
// fn insert_fiat_event(&mut self,
// time: B::Time,
// distinguisher: u64,
// event: Self::Event)
// -> FiatEventOperationResult {
// }
// fn erase_fiat_event(&mut self, time: B::Time, distinguisher: u64) -> FiatEventOperationResult {}
// fn snapshot_before(&mut self, time: B::Time) -> Option<Self::S> {}
// }
