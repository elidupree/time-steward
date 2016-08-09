//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, Column, ExtendedTime,
            EventRng, Basics, TimeSteward, FiatEventOperationResult, ValidSince,
            TimeStewardLifetimedMethods};
use std::collections::{HashMap, BTreeMap};
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::marker::PhantomData;

#[derive (Clone)]
pub struct Steward<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (Steward0, Steward1, PhantomData <B::Constants>);
pub struct Snapshot<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeStewardLifetimedMethods <'a, B>>::Snapshot,
  <Steward1 as TimeStewardLifetimedMethods <'a, B>>::Snapshot,
);
pub enum Mutator<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > where <Steward0 as TimeStewardLifetimedMethods <'a, B>>::Mutator: 'a, <Steward1 as TimeStewardLifetimedMethods <'a, B>>::Mutator: 'a {
  Form0 (& 'a mut <Steward0 as TimeStewardLifetimedMethods <'a, B>>::Mutator),
  Form1 (& 'a mut <Steward1 as TimeStewardLifetimedMethods <'a, B>>::Mutator),
}
pub enum PredictorAccessor<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > where <Steward0 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor: 'a, <Steward1 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor: 'a {
  Form0 (& 'a mut <Steward0 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor),
  Form1 (& 'a mut <Steward1 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor),
}
pub type Event<B, Steward0, Steward1> = Rc<for<'d, 'e> Fn(&'d mut Mutator<'e, B, Steward0, Steward1>)>;
pub type Predictor<B, Steward0, Steward1> = super::Predictor<PredictorFn<B, Steward0, Steward1>>;
pub type PredictorFn<B, Steward0, Steward1> = Rc<for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B, Steward0, Steward1>, RowId)>;



impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for Snapshot<'a, B, Steward0, Steward1> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    match (self.0.get::<C> (id), self.1.get::<C> (id)) {
      (None, None) => None,
      //we cannot actually check whether the values are the same, so just use one of them
      (Some (value_0), Some (value_1)) => Some (value_0),
      //however, we CAN tell the difference between something and nothing
      _=> panic! ("One snapshot returned a value and the other didn't; this proves that one or both of the stewards is buggy")
    }
  }
  fn constants(&self) -> &B::Constants {
    //constants are usually implemented trivially; we don't bother checking them
    self.0.constants()
  }
}
impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for Mutator<'a, B, Steward0, Steward1> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    match self {
      &mut Mutator::Form0 (other) => other.get::<C> (id),
      &mut Mutator::Form1 (other) => other.get::<C> (id),
    }
  }
  fn constants(&self) -> &B::Constants {
    match self {
      & Mutator::Form0 (other) => other.constants (),
      & Mutator::Form1 (other) => other.constants (),
    }
  }
}
impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for PredictorAccessor<'a, B, Steward0, Steward1> {
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {
    match self {
      &mut PredictorAccessor::Form0 (other) => other.get::<C> (id),
      &mut PredictorAccessor::Form1 (other) => other.get::<C> (id),
    }
  }
  fn constants(&self) -> &B::Constants {
    match self {
      & PredictorAccessor::Form0 (other) => other.constants (),
      & PredictorAccessor::Form1 (other) => other.constants (),
    }
  }
}

impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::MomentaryAccessor<B> for Snapshot<'a, B, Steward0, Steward1> {
  fn now(&self) -> &B::Time {
    let result = (self.0.now(), self.1.now());
    assert! (result.0 == result.1, "Snapshots returned different times; this is an egregious bug!");
    result.0
  }
}
impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::MomentaryAccessor<B> for Mutator<'a, B, Steward0, Steward1> {
  fn now(&self) -> &B::Time {
    match self {
      & Mutator::Form0 (other) => other.now(),
      & Mutator::Form1 (other) => other.now(),
    }
  }
}
impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::PredictorAccessor<B> for PredictorAccessor<'a, B, Steward0, Steward1> {
  type Event = Event<B, Steward0, Steward1>;
  fn predict_immediately(&mut self, event: Event<B, Steward0, Steward1>) {
    match self {
      &mut PredictorAccessor::Form0 (other) => other.predict_immediately(convert_event_0 (event)),
      &mut PredictorAccessor::Form1 (other) => other.predict_immediately(convert_event_1 (event)),
    }
  }
  fn predict_at_time(&mut self, time: &B::Time, event: Event<B, Steward0, Steward1>) {
    match self {
      &mut PredictorAccessor::Form0 (other) => other.predict_at_time(time, convert_event_0 (event)),
      &mut PredictorAccessor::Form1 (other) => other.predict_at_time(time, convert_event_1 (event)),
    }
  }
}
impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Snapshot<B> for Snapshot<'a, B, Steward0, Steward1> {}

impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Mutator<B> for Mutator<'a, B, Steward0, Steward1> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    match self {
      &mut Mutator::Form0 (other) => other.set::<C>(id, data),
      &mut Mutator::Form1 (other) => other.set::<C>(id, data),
    }
  }
  fn rng(&mut self) -> &mut EventRng {
    match self {
      &mut Mutator::Form0 (other) => other.rng (),
      &mut Mutator::Form1 (other) => other.rng (),
    }
  }
  fn random_id(&mut self) -> RowId {
    match self {
      &mut Mutator::Form0 (other) => other.random_id(),
      &mut Mutator::Form1 (other) => other.random_id(),
    }
  }
}


fn convert_event_0 <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> (event: Event <B, Steward0, Steward1>)-><Steward0 as TimeSteward <B>>::Event {
  Rc::new (| mutator | {event (&mut Mutator::Form0 (mutator))});
}
fn convert_event_1 <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> (event: Event <B, Steward0, Steward1>)-><Steward1 as TimeSteward <B>>::Event {
  Rc::new (| mutator | {event (&mut Mutator::Form1 (mutator))});
}



impl<'a, B: Basics, Steward0: 'a + TimeSteward<B>, Steward1: 'a + TimeSteward<B> > TimeStewardLifetimedMethods<'a, B> for Steward<B, Steward0, Steward1> {
  type Snapshot = Snapshot<'a, B, Steward0, Steward1>;
  type Mutator = Mutator <'a, B, Steward0, Steward1>;
  type PredictorAccessor = PredictorAccessor <'a, B, Steward0, Steward1>;
  
  fn snapshot_before(&'a mut self, time: &B::Time) -> Option<Self::Snapshot> {
    match (
      self.0.snapshot_before (time),
      self.1.snapshot_before (time)
    ) {
      (None, None) => None,
      (Some (snapshot_0), Some (snapshot_1)) => Some (Snapshot (snapshot_0, snapshot_1)),
      _=> panic! ("One steward returned a snapshot and the other didn't; this could be an error in the stewards or a user error (calling at a time that is valid for one steward but not the other)")
    }
  }
}
impl<B: Basics, Steward0: TimeSteward<B> , Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0, Steward1> where B::Constants: Clone {
  type Event = Event<B, Steward0, Steward1>;
  type Predictor = Predictor<B, Steward0, Steward1>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    unimplemented!()
  }
  fn new_empty(constants: B::Constants, predictors: Vec<Self::Predictor>) -> Self {
    Steward (
      Steward0::new_empty (constants.clone(), predictors.iter().map(| predictor | Steward0::Predictor {predictor_id: predictor. predictor_id, column_id: predictor.column_id, function: | accessor, row | (predictor.function) (&mut PredictorAccessor::Form0 (accessor), row)}).collect()),
      Steward1::new_empty (constants, predictors.iter().map(| predictor | Steward1::Predictor {predictor_id: predictor. predictor_id, column_id: predictor.column_id, function: | accessor, row | (predictor.function) (&mut PredictorAccessor::Form1 (accessor), row)}).collect()),
      PhantomData,
    )
  }

  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Self::Event)
                       -> FiatEventOperationResult {
    let result_0 = self.0.insert_fiat_event (time.clone(), id.clone(),convert_event_0 (event));
    let result_1 = self.1.insert_fiat_event (time, id, convert_event_1 (event));
    assert! (result_0 == result_1, "stewards returned different results for insert_fiat_event; this could be an error in the stewards or a user error (calling at a time that is valid for one steward but not the other)");
    result_0
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    let result_0 = self.0.erase_fiat_event (time, id.clone());
    let result_1 = self.1.erase_fiat_event (time, id);
    assert! (result_0 == result_1, "stewards returned different results for erase_fiat_event; this could be an error in the stewards or a user error (calling at a time that is valid for one steward but not the other)");
    result_0
  }
}
