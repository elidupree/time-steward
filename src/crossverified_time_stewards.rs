//! The simplest possible implementation of the TimeSteward API.
//!
//! This implementation is unusably slow on large simulations. Its main use is to cross-check with other TimeSteward implementations to make sure they are implementing the API correctly.
//!
//!


use super::{DeterministicRandomId, SiphashIdGenerator, RowId, ColumnId, FieldId, PredictorId, Column, ExtendedTime, StewardRc,
             Basics, FieldRc, TimeSteward, FiatEventOperationError, ValidSince, PredictorFn, TimeStewardSettings, Accessor};
use std::collections::{HashMap, BTreeMap};
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::cmp::max;
use std::borrow::Borrow;
use std::rc::Rc;
use std::marker::PhantomData;

#[derive (Clone)]
pub struct Steward<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  Steward0,
  Steward1,
  StewardRc <HashMap<ColumnId, fn (&FieldRc, &FieldRc)>>,
  PhantomData <B::Constants>
);
pub struct Snapshot<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeSteward <B>>::Snapshot,
  <Steward1 as TimeSteward <B>>::Snapshot,
  StewardRc <HashMap<ColumnId, fn (&FieldRc, &FieldRc)>>,
);
pub struct Settings <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeSteward <B>>::Settings,
  <Steward1 as TimeSteward <B>>::Settings,
  pub HashMap<ColumnId, fn (&FieldRc, &FieldRc)>,
);

pub fn check_equality <C: Column> (first: &FieldRc, second: &FieldRc) {
  assert_eq!(::unwrap_field::<C>(first), ::unwrap_field::<C>(second), "Snapshots returned the same field with the same last change times but different data; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types");
}

#[macro_export]
macro_rules! __time_steward_insert_crossverification_function {
  ($column: ty, $table: expr) => {
    $table.insert (<$column as $crate::Column>::column_id(), $crate::crossverified_time_stewards::check_equality::<$column>);
  }
}

#[macro_export]
macro_rules! populate_crossverified_time_stewards_equality_table {
($settings: ident) => {
  for_all_columns! (__time_steward_insert_crossverification_function {Column, $settings.2});
}
}



impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Accessor<B> for Snapshot<B, Steward0, Steward1> {
  //macro_rules! forward_snapshot_method ($method: ident ($self, $($argument_name: ident: $argument_type:ty),*)->$return_type:ty
  // TODO: forward all the methods properly
  // and check equality by serialization
  fn generic_data_and_extended_last_change (&self, id: FieldId)->Option <(& FieldRc, & ExtendedTime <B>)> {
    match (
      self.0.generic_data_and_extended_last_change (id), 
      self.1.generic_data_and_extended_last_change (id)
    ) {
      (None, None) => None,
      (Some (value_0), Some (value_1)) => {
        assert_eq!(value_0.1, value_1.1, "Snapshots returned different last change times for the same field; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types");
        (self.2.get (& id.column_id).expect ("Column missing from crossverification table; did you forget to call populate_crossverified_time_stewards_equality_table!()? Or did you forget to list a column in for_all_columns!()?")) (value_0.0, value_1.0);
        Some (value_0)
      },
      //however, we CAN tell the difference between something and nothing
      _=> panic! ("One snapshot returned a value and the other didn't; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types")
    }
  }
  fn constants(&self) -> &B::Constants {
    //constants methods are usually implemented trivially; we don't bother checking them.
    //Since the user only gives you one set of constants, it's hard to return a wrong value
    self.0.constants()
  }
  fn unsafe_now(&self) -> &B::Time {
    let result = (self.0.unsafe_now(), self.1.unsafe_now());
    assert! (result.0 == result.1, "Snapshots returned different times; this is an egregious bug!");
    result.0
  }
}

impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::MomentaryAccessor<B> for Snapshot<B, Steward0, Steward1> {}

impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > super::Snapshot<B> for Snapshot<B, Steward0, Steward1> {
  fn num_fields(&self) -> usize {
    assert_eq!(self.0.num_fields(), self.1.num_fields());
    self.0.num_fields()
  }
}/*
use std::collections::hash_set;
pub struct SnapshotIter <'a, B: Basics, Steward0: TimeSteward<B> + 'a, Steward1: TimeSteward<B> + 'a>
where & 'a Steward0::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>>
{
  set: hash_set::HashSet <FieldId>,
  iter: hash_set::Iter <'a, FieldId>,
  snapshot: & 'a Snapshot <B, Steward0, Steward1>,
}
impl <'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> Iterator for SnapshotIter<'a, B, Steward0, Steward1>
where & 'a Steward0::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  fn next (&mut self)->Option <Self::Item> {
    (self. iter).next().map (| id | (id.clone(), (self. snapshot).generic_data_and_extended_last_change (id.clone()).expect ("the snapshot thinks a FieldId exists when it doesn't")))
  }
  fn size_hint (&self)->(usize, Option <usize>) {self. iter.size_hint()}
}
impl <'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> IntoIterator for & 'a Snapshot <B, Steward0, Steward1>
where & 'a Steward0::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = super::SnapshotEntry <'a, B>>,

 {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  type IntoIter = SnapshotIter <'a, B, Steward0, Steward1>;
  fn into_iter (self)->Self::IntoIter {
    let mut field_ids = hash_set::HashSet::new();
    for entry in (& self.0) {
      field_ids.insert (entry.0.clone());
    }
    for entry in (& self.0) {
      let redundant =!field_ids.insert (entry.0.clone());
      assert!(redundant, "field existed in Steward1 snapshot but not Steward0 snapshot: {:?}", entry.0.clone());
    }
    unimplemented!()//SnapshotIter {set: field_ids, iter: field_ids.iter(), snapshot: self}
  }
}
*/
//TODO: implement snapshot iterating

/*
//TODO: should we just require PredictorFn to implement Clone?
#[derive (Serialize, Deserialize)]
struct ShareablePredictorFn <B: Basics, P: PredictorFn <B>> (StewardRc <P>, PhantomData <B>);
impl <B: Basics, P: PredictorFn <B>> PredictorFn <B> for ShareablePredictorFn <B, P> {
  fn call <PA: super::PredictorAccessor <B>> (&self, accessor: &mut PA, id: RowId) {
    self.0.call (accessor, id);
  }
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
*/


impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > TimeStewardSettings <B> for Settings <B, Steward0, Steward1> {
  fn new()->Self {
    Settings (<Steward0::Settings as TimeStewardSettings <B>>::new(), <Steward1::Settings as TimeStewardSettings <B>>::new(), HashMap::new())
  }
  fn insert_predictor <P: PredictorFn <B>> (&mut self, predictor_id: PredictorId, column_id: ColumnId, function: P) {
    //let function = ShareablePredictorFn::<B, P> (StewardRc::new (function), PhantomData);
    self.0.insert_predictor (predictor_id, column_id, function.clone());
    self.1.insert_predictor (predictor_id, column_id, function);
  }
}

impl<B: Basics, Steward0: TimeSteward<B> , Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0, Steward1> {
  type Snapshot = Snapshot<B, Steward0, Steward1>;
  type Settings = Settings<B, Steward0, Steward1>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max (self.0.valid_since(), self.1.valid_since())
  }
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    Steward (
      TimeSteward::new_empty (constants.clone(), settings.0),
      TimeSteward::new_empty (constants, settings.1),
      StewardRc::new (settings.2),
      PhantomData,
    )
  }

  fn from_snapshot<'a, S: super::Snapshot<B>>(snapshot: & 'a S,
                                              settings: Self::Settings)
                                              -> Self
                                              where & 'a S: IntoIterator <Item = super::SnapshotEntry <'a, B>> {
    Steward (
      Steward0::from_snapshot (snapshot, settings.0),
      Steward1::from_snapshot (snapshot, settings.1),
      StewardRc::new (settings.2),
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
      self.0.insert_fiat_event (time.clone(), id, event.clone()),
      self.1.insert_fiat_event (time, id, event)
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)"),
    }
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match (
      self.0.erase_fiat_event (time, id),
      self.1.erase_fiat_event (time, id)
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)"),
    }
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    match (
      self.0.snapshot_before (time),
      self.1.snapshot_before (time)
    ) {
      (Some (snapshot_0), Some (snapshot_1)) => Some (Snapshot (snapshot_0, snapshot_1, self.2.clone())),
      (None, _) => panic! ("Steward0 failed to return a snapshot at a time it claims to be valid"),
      (_, None) => panic! ("Steward1 failed to return a snapshot at a time it claims to be valid"),
    }
  }
}

