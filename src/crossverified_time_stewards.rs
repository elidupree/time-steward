//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, Column, ExtendedTime,
            EventRng, Basics, FieldRc, TimeSteward, FiatEventOperationResult, ValidSince};
use std::collections::{HashMap, BTreeMap};
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::marker::PhantomData;

#[derive (Clone)]
pub struct Steward<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (Steward0, Steward1, PhantomData <B::Constants>);
pub struct Snapshot<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeSteward <B>>::Snapshot,
  <Steward1 as TimeSteward <B>>::Snapshot,
);
pub enum Mutator<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > where <Steward0 as TimeStewardLifetimedMethods <'a, B>>::Mutator: 'a, <Steward1 as TimeStewardLifetimedMethods <'a, B>>::Mutator: 'a {
  Form0 (& 'a mut <Steward0 as TimeSteward <B>>::Mutator),
  Form1 (& 'a mut <Steward1 as TimeSteward <B>>::Mutator),
}
pub enum PredictorAccessor<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > where <Steward0 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor: 'a, <Steward1 as TimeStewardLifetimedMethods <'a, B>>::PredictorAccessor: 'a {
  Form0 (& 'a mut <Steward0 as TimeSteward <B>>::PredictorAccessor),
  Form1 (& 'a mut <Steward1 as TimeSteward <B>>::PredictorAccessor),
}
pub type EventFn <B, Steward0, Steward1> = for<'d, 'e> Fn(&'d mut Mutator<'e, B, Steward0, Steward1>);
pub type Event<B, Steward0, Steward1> = Rc<EventFn <B, Steward0, Steward1>>;
//pub type Predictor<B, Steward0, Steward1> = super::Predictor<PredictorFn<B, Steward0, Steward1>>;
//pub type PredictorFn<B, Steward0, Steward1> = for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B, Steward0, Steward1>, RowId);



impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for Snapshot<'a, B, Steward0, Steward1> {
  //macro_rules! forward_snapshot_method ($method: ident ($self, $($argument_name: ident: $argument_type:ty),*)->$return_type:ty
  // TODO: forward all the methods properly
  // and check equality by serialization
  fn generic_data_and_extended_last_time (&self, id: FieldId)->Option <(& FieldRc, & ExtendedTime <B>)> {
    match (self.0.generic_data_and_extended_last_time (id), self.1.generic_data_and_extended_last_time (id)) {
      (None, None) => None,
      //we cannot actually check whether the values are the same, so just use one of them
      (Some (value_0), Some (value_1)) => Some (value_0),
      //however, we CAN tell the difference between something and nothing
      _=> panic! ("One snapshot returned a value and the other didn't; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types")
    }
  }
  fn constants(&self) -> &B::Constants {
    //constants are usually implemented trivially; we don't bother checking them
    self.0.constants()
  }
}
  macro_rules! forward_mutator_method {
  ($method: ident [$($generic_parameters:tt)*]=[$($specific_parameters:ty),*] ([$($self_reference:tt)*] self, $($argument_name: ident: $argument_type:ty),*)->$return_type:ty) => {
  fn $method <$($generic_parameters)*>($($self_reference)* self, $($argument_name: $argument_type)*) ->$return_type {
    match self {
      $($self_reference)* Mutator::Form0 (other) => other.$method::<$($specific_parameters)*> ($($argument_name)*),
      $($self_reference)* Mutator::Form1 (other) => other.$method::<$($specific_parameters)*> ($($argument_name)*),
    }
  }
}}

impl<'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for Mutator<'a, B, Steward0, Steward1> {
  forward_mutator_method! (generic_data_and_extended_last_change []=[] ([&] self, id: FieldId)->Option <(& FieldRc, & ExtendedTime <B>)>);
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
impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Snapshot<B> for Snapshot<B, Steward0, Steward1> {}

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


fn convert_event_0 <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> (event: Event <B, Steward0, Steward1>)->Rc<<Steward0 as TimeStewardStaticMethods <B>>::EventFn> {
  Rc::new (| mutator | {event (&mut Mutator::Form0 (mutator))});
}
fn convert_event_1 <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> (event: Event <B, Steward0, Steward1>)->Rc<<Steward1 as TimeStewardStaticMethods <B>>::EventFn> {
  Rc::new (| mutator | {event (&mut Mutator::Form1 (mutator))});
}
fn convert_predictor_0 <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> (predictor: Predictor <B, Steward0, Steward1>)
  ->super::Predictor <<Steward0 as TimeStewardStaticMethods <B>>::PredictorFn> {
  let inner_function =predictor.function .clone();
  let function: Rc<<Steward0 as TimeStewardStaticMethods <B>>::PredictorFn> = 
    Rc::new (move | access, row | {
      inner_function (&mut PredictorAccessor::Form0 (access), row)
    });
}



impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0, Steward1> where B::Constants: Clone {}
impl<B: Basics, Steward0: TimeSteward<B> , Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0, Steward1> {
  type Snapshot = Snapshot<B, Steward0, Steward1>;
  type Settings = Settings<B, Steward0, Steward1>;
  
  fn snapshot_before(&mut self, time: &B::Time) -> Option<Self::Snapshot> {
    match (
      self.0.snapshot_before (time),
      self.1.snapshot_before (time)
    ) {
      (None, None) => None,
      (Some (snapshot_0), Some (snapshot_1)) => Some (Snapshot (snapshot_0, snapshot_1)),
      _=> panic! ("One steward returned a snapshot and the other didn't; this could be an error in the stewards or a user error (calling at a time that is valid for one steward but not the other)")
    }
  }

  fn valid_since(&self) -> ValidSince<B::Time> {
    max (self.0.valid_since(), self.1.valid_since())
  }
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    Steward (
      TimeSteward::new_empty (constants.clone(), settings.0),
      TimeSteward::new_empty (constants, settings.1),
      PhantomData,
    )
  }

  fn from_snapshot<'a, S: super::Snapshot<B>>(snapshot: & 'a S,
                                              settings: Self::Settings)
                                              -> Self
                                              where & 'a S: IntoIterator <Item = super::SnapshotEntry <'a, B>> {
    Steward (
      TimeSteward::from_snapshot (snapshot, settings.0),
      TimeSteward::from_snapshot (snapshot, settings.1),
      PhantomData,
    )
  }
  fn insert_fiat_event <E: super::EventFn <B>> (&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: E)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match (
      self.0.insert_fiat_event (time.clone(), id.clone(), event),
      self.1.insert_fiat_event (time, id, event);
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)");
    }
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match (
      self.0.erase_fiat_event (time.clone(), id.clone()),
      self.1.erase_fiat_event (time, id);
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)");
    }
  }
}
impl<B: Basics, Steward0: TimeSteward<B> , Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0, Steward1> where B::Constants: Clone {}
