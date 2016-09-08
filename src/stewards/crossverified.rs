//! A wrapper around two different TimeSteward types, to verify that they behave consistently.
//!
//!
//!


use {DeterministicRandomId, ColumnId, FieldId, PredictorId, Column, ExtendedTime, StewardRc,
     Basics, FieldRc, TimeSteward, IncrementalTimeSteward, FiatEventOperationError, ValidSince,
     PredictorFn, TimeStewardSettings, ColumnList, ColumnListUser};
use std::collections::HashMap;
use std::cmp::max;
use std::marker::PhantomData;
use Snapshot as SuperSnapshot;

pub struct Steward<B: Basics, C: ColumnList, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  Steward0,
  Steward1,
  StewardRc <HashMap<ColumnId, fn (&FieldRc, &FieldRc)>>,
  PhantomData <B::Constants>,
  PhantomData <C>,
);
pub struct Snapshot<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeSteward <B>>::Snapshot,
  <Steward1 as TimeSteward <B>>::Snapshot,
  StewardRc <HashMap<ColumnId, fn (&FieldRc, &FieldRc)>>,
);
pub struct Settings <B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > (
  <Steward0 as TimeSteward <B>>::Settings,
  <Steward1 as TimeSteward <B>>::Settings,
);
impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> Clone for Settings<B,
                                                                                       Steward0,
                                                                                       Steward1> {
  fn clone(&self) -> Self {
    Settings(self.0.clone(), self.1.clone())
  }
}

fn check_equality<C: Column>(first: &FieldRc, second: &FieldRc) {
  assert_eq!(::unwrap_field::<C>(first), ::unwrap_field::<C>(second), "Snapshots returned the same field with the same last change times but different data; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types");
}


impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > ::Accessor<B> for Snapshot<B, Steward0, Steward1> {
// macro_rules! forward_snapshot_method ($method: ident ($self, $($argument_name: ident: $argument_type:ty),*)->$return_type:ty
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
      _=> panic! ("One snapshot returned a value and the other didn't; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types")
    }
  }
  fn constants(&self) -> &B::Constants {
// constants methods are usually implemented trivially; we don't bother checking them.
// Since the user only gives you one set of constants, it's hard to return a wrong value
    self.0.constants()
  }
  fn unsafe_now(&self) -> &B::Time {
    let result = (self.0.unsafe_now(), self.1.unsafe_now());
    assert! (result.0 == result.1, "Snapshots returned different times; this is an egregious bug!");
    result.0
  }
}

impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > ::MomentaryAccessor<B> for Snapshot<B, Steward0, Steward1> {}

impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > ::Snapshot<B> for Snapshot<B, Steward0, Steward1> {
  fn num_fields(&self) -> usize {
    assert_eq!(self.0.num_fields(), self.1.num_fields());
    self.0.num_fields()
  }
}

pub struct SnapshotIter <'a, B: Basics, Steward0: TimeSteward<B> + 'a, Steward1: TimeSteward<B> + 'a>
where & 'a Steward0::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>
{
  iter: <& 'a Steward0::Snapshot as IntoIterator>::IntoIter,
  #[allow (dead_code)]
  snapshot: & 'a Snapshot <B, Steward0, Steward1>,
}
impl <'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> Iterator for SnapshotIter<'a, B, Steward0, Steward1>
where & 'a Steward0::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  fn next (&mut self)->Option <Self::Item> {
    self. iter.next()
  }
  fn size_hint (&self)->(usize, Option <usize>) {self. iter.size_hint()}
}
impl <'a, B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B>> IntoIterator for & 'a Snapshot <B, Steward0, Steward1>
where & 'a Steward0::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>,
& 'a Steward1::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>,

 {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  type IntoIter = SnapshotIter <'a, B, Steward0, Steward1>;
  fn into_iter (self)->Self::IntoIter {
    assert_eq! (self.0.num_fields(), self.1.num_fields());
    let mut fields = HashMap::new();
    for (id, data) in & self.0 {
      fields.insert (id, (data.0.clone(), data.1.clone()));
    }
    for (id, data) in & self.1 {
      let other_data = fields.get (& id).expect ("field existed in Steward1 snapshot but not Steward0 snapshot");
      assert_eq!(*data .1, other_data .1, "Snapshots returned different last change times for the same field; one or both of the stewards is buggy, or the caller submitted very nondeterministic event/predictor types");
      (self.2.get (& id.column_id).expect ("Column missing from crossverification table; did you forget to call populate_crossverified_time_stewards_equality_table!()? Or did you forget to list a column in for_all_columns!()?")) (data .0, & other_data .0);
    }
    SnapshotIter:: <'a, B, Steward0, Steward1> {
      iter: (& self.0).into_iter(),
      snapshot: self
    }
  }
}

impl ColumnListUser for HashMap<ColumnId, fn(&FieldRc, &FieldRc)> {
  fn apply<C: Column>(&mut self) {
    self.insert(C::column_id(), check_equality::<C>);
  }
}


impl<B: Basics, Steward0: TimeSteward<B>, Steward1: TimeSteward<B> > TimeStewardSettings <B> for Settings <B, Steward0, Steward1> {
  fn new()->Self {
    Settings (<Steward0::Settings as TimeStewardSettings <B>>::new(), <Steward1::Settings as TimeStewardSettings <B>>::new())
  }
  fn insert_predictor <P: PredictorFn <B>> (&mut self, predictor_id: PredictorId, column_id: ColumnId, function: P) {
    self.0.insert_predictor (predictor_id, column_id, function.clone());
    self.1.insert_predictor (predictor_id, column_id, function);
  }
}

impl<B: Basics, C: ColumnList, Steward0: TimeSteward<B> , Steward1: TimeSteward<B> > TimeSteward<B> for Steward<B, C, Steward0, Steward1> {
  type Snapshot = Snapshot<B, Steward0, Steward1>;
  type Settings = Settings<B, Steward0, Steward1>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max (self.0.valid_since(), self.1.valid_since())
  }
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    let mut equality_table = HashMap::new();
    C::apply (&mut equality_table);
    let result = Steward::<B, C, Steward0, Steward1> (
      TimeSteward::new_empty (constants.clone(), settings.0),
      TimeSteward::new_empty (constants, settings.1),
      StewardRc::new (equality_table),
      PhantomData, PhantomData,
    );
    assert!(result.0.valid_since() == ValidSince::TheBeginning, "Steward0 broke the ValidSince rules");
    assert!(result.1.valid_since() == ValidSince::TheBeginning, "Steward1 broke the ValidSince rules");
    result
  }

  fn from_snapshot<'a, S: ::Snapshot<B>>(snapshot: & 'a S,
                                              settings: Self::Settings)
                                              -> Self
                                              where & 'a S: IntoIterator <Item = ::SnapshotEntry <'a, B>> {
    let mut equality_table = HashMap::new();
    C::apply (&mut equality_table);
    let result = Steward (
      Steward0::from_snapshot::<'a, S>(snapshot, settings.0),
      Steward1::from_snapshot::<'a, S>(snapshot, settings.1),
      StewardRc::new (equality_table),
      PhantomData, PhantomData,
    );
    assert!(result.0.valid_since() == ValidSince::Before (snapshot.now().clone()), "Steward0 broke the ValidSince rules");
    assert!(result.1.valid_since() == ValidSince::Before (snapshot.now().clone()), "Steward1 broke the ValidSince rules");
    result
  }
  fn insert_fiat_event <E: ::EventFn <B>> (&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: E)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let old_valid_since = (self.0.valid_since(), self.1.valid_since());
    let result = match (
      self.0.insert_fiat_event (time.clone(), id, event.clone()),
      self.1.insert_fiat_event (time, id, event)
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)"),
    };
    assert!(self .0.valid_since() == old_valid_since.0, "Steward0 broke the ValidSince rules");
    assert!(self .1.valid_since() == old_valid_since.1, "Steward1 broke the ValidSince rules");
    result
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let old_valid_since = (self.0.valid_since(), self.1.valid_since());
    let result = match (
      self.0.erase_fiat_event (time, id),
      self.1.erase_fiat_event (time, id)
    ){
      (Ok (()), Ok (())) => Ok (()),
      (Err (FiatEventOperationError::InvalidTime),_) => panic!("Steward0 returned InvalidTime after its own ValidSince"),
      (_,Err (FiatEventOperationError::InvalidTime)) => panic!("Steward1 returned InvalidTime after its own ValidSince"),
      (Err (FiatEventOperationError::InvalidInput),Err (FiatEventOperationError::InvalidInput)) => Err (FiatEventOperationError::InvalidInput),
      _=> panic!("stewards returned different results for insert_fiat_event; I believe this is ALWAYS a bug in one of the stewards (that is, it cannot be caused by invalid input)"),
    };
    assert!(self .0.valid_since() == old_valid_since.0, "Steward0 broke the ValidSince rules");
    assert!(self .1.valid_since() == old_valid_since.1, "Steward1 broke the ValidSince rules");
    result
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    let result = match (
      self.0.snapshot_before (time),
      self.1.snapshot_before (time)
    ) {
      (Some (snapshot_0), Some (snapshot_1)) => Some (Snapshot (snapshot_0, snapshot_1, self.2.clone())),
      (None, _) => panic! ("Steward0 failed to return a snapshot at a time it claims to be valid"),
      (_, None) => panic! ("Steward1 failed to return a snapshot at a time it claims to be valid"),
    };
    assert!(self .0.valid_since() <*time, "Steward0 broke the ValidSince rules");
    assert!(self .1.valid_since() <*time, "Steward1 broke the ValidSince rules");
    result
  }
}


impl<B: Basics, C: ColumnList, Steward0: IncrementalTimeSteward <B>, Steward1: IncrementalTimeSteward <B>> ::IncrementalTimeSteward<B> for Steward<B, C, Steward0, Steward1> {
  fn step(&mut self) {
    if self.0.updated_until_before() <self.1.updated_until_before() {
      //printlnerr!("stepping 0");
      self.0.step();
    } else {
      //printlnerr!("stepping 1");
      self.1.step();
    }
  }
  fn updated_until_before (&self)->Option <B::Time> {
    match (self.0.updated_until_before(), self.1.updated_until_before()) {
      (None, None) => None,
      (Some (time_0), None) => {
        Some (time_0)
      },
      (None, Some (time_1)) => {
        Some (time_1)
      },
      (Some (time_0), Some (time_1)) => {
        if time_0 <= time_1 {
          Some (time_0)
        } else {
          Some (time_1)
        }
      },
    }
  }
}
