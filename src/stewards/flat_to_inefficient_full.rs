//! A wrapper that turns a flat TimeSteward into an inefficient fall TimeSteward.
//!
//! This is done by storing snapshots and reverting to the previous snapshot whenever you try to query the past.
//!


use {DeterministicRandomId, ColumnId, FieldId, PredictorId, Column, ExtendedTime, StewardRc,
             Basics, FieldRc, TimeSteward, FiatEventOperationError, ValidSince, PredictorFn, TimeStewardSettings,ColumnList, ColumnListUser};
use std::collections::{HashMap, BTreeMap};
use std::cmp::max;
use std::marker::PhantomData;
use Snapshot as SuperSnapshot;

pub struct Steward<B: Basics, Steward0: TimeSteward<B> > {
  steward: Steward0,
  invalid_before: ValidSince <B::Time>,
  constants: B::Constants,
  settings: Steward0::Settings,
  fiat_events: BTreeMap<B::Time, i32>,
  snapshots: Vec<Steward0::Snapshot>,
}


impl<B: Basics, Steward0: TimeSteward<B> > TimeSteward<B> for Steward<B, Steward0> {
  type Snapshot = Steward0::Snapshot;
  type Settings = Steward0::Settings;

  fn valid_since(&self) -> ValidSince<B::Time> {
    self.invalid_before.clone()
  }
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    Steward::<B, Steward0> {
      steward: TimeSteward::new_empty (constants.clone(), settings.clone()),
      invalid_before: ValidSince::TheBeginning,
      constants: constants,
      settings: settings,
      fiat_events: BTreeMap::new(),
      snapshots: Vec::new(),
    }
  }

  fn from_snapshot<'a, S: ::Snapshot<B>>(snapshot: & 'a S,
                                              settings: Self::Settings)
                                              -> Self
                                              where & 'a S: IntoIterator <Item = ::SnapshotEntry <'a, B>> {
    let steward = TimeSteward::from_snapshot::<'a, S> (snapshot, settings.clone());
    Steward::<B, Steward0> {
      steward: steward,
      invalid_before: ValidSince::Before (snapshot.now().clone()),
      constants: snapshot.constants().clone(),
      settings: settings,
      fiat_events: BTreeMap::new(),
      snapshots: vec![steward.snapshot_before (snapshot.now())],
    }
  }
  fn insert_fiat_event <E: ::EventFn <B>> (&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: E)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    if self.steward.valid_since() > *time {
      while self.snapshots.last().map_or (false, | snapshot | snapshot.now() > time) {
        self.snapshots.pop();
      }
      self.steward = match self.snapshots.last() {
        None => TimeSteward::new_empty (self.constants.clone(), self.settings.clone()),
        Some (snapshot) => TimeSteward::from_snapshot::<Self::Snapshot> (snapshot, self.settings.clone()),
      }
    }
    self.snapshots.push (self.steward.snapshot_before (time));
    self.steward.snapshot_before (time)
  }
}

