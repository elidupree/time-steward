//! A wrapper that turns a flat TimeSteward into an inefficient fall TimeSteward.
//!
//! This is done by storing snapshots and reverting to the previous snapshot whenever you try to query the past.
//!


use {DeterministicRandomId, Basics, TimeSteward, FiatEventOperationError, ValidSince, Accessor,
     MomentaryAccessor};
use std::collections::HashMap;

pub struct Steward<B: Basics, Steward0: TimeSteward<B>> {
  steward: Steward0,
  invalid_before: ValidSince<B::Time>,
  constants: B::Constants,
  settings: Steward0::Settings,
  fiat_events: HashMap<DeterministicRandomId,
                       (B::Time, Box<Fn(&mut Steward0, B::Time, DeterministicRandomId)>)>,
  snapshots: Vec<Steward0::Snapshot>,
}


impl<B: Basics, Steward0: TimeSteward<B>> TimeSteward<B> for Steward<B, Steward0> {
  type Snapshot = Steward0::Snapshot;
  type Settings = Steward0::Settings;

  fn valid_since(&self) -> ValidSince<B::Time> {
    self.invalid_before.clone()
  }
  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {
    Steward::<B, Steward0> {
      steward: TimeSteward::new_empty(constants.clone(), settings.clone()),
      invalid_before: ValidSince::TheBeginning,
      constants: constants,
      settings: settings,
      fiat_events: HashMap::new(),
      snapshots: Vec::new(),
    }
  }

  fn from_snapshot<'a, S: ::Snapshot<B>>(snapshot: &'a S, settings: Self::Settings) -> Self
    where &'a S: IntoIterator<Item = ::SnapshotEntry<'a, B>>
  {
    let mut steward: Steward0 = TimeSteward::from_snapshot::<'a, S>(snapshot, settings.clone());
    let snapshot = steward.snapshot_before(snapshot.now()).unwrap();
    Steward::<B, Steward0> {
      steward: steward,
      invalid_before: ValidSince::Before(snapshot.now().clone()),
      constants: snapshot.constants().clone(),
      settings: settings,
      fiat_events: HashMap::new(),
      snapshots: vec![snapshot],
    }
  }
  fn insert_fiat_event<E: ::EventFn<B>>(&mut self,
                                        time: B::Time,
                                        id: DeterministicRandomId,
                                        event: E)
                                        -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    if self.fiat_events.contains_key(&id) {
      return Err(FiatEventOperationError::InvalidInput);
    }
    let _ = self.steward.insert_fiat_event(time.clone(), id, event.clone());
    self.fiat_events.insert(id,
                            (time,
                             Box::new(move |steward, time, id| {
                              let _ = steward.insert_fiat_event(time, id, event.clone());
                            })));
    Ok(())
  }
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    if self.fiat_events.remove(&id).is_none() {
      return Err(FiatEventOperationError::InvalidInput);
    }
    let _ = self.steward.erase_fiat_event(time, id);
    Ok(())
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    if self.steward.valid_since() > *time {
      while self.snapshots.last().map_or(false, |snapshot| snapshot.now() > time) {
        self.snapshots.pop();
      }
      self.steward = //match self.snapshots.last() {
        //None =>
        TimeSteward::new_empty (self.constants.clone(), self.settings.clone())
        //,
        //Some (snapshot) => TimeSteward::from_snapshot::<Self::Snapshot> (snapshot, self.settings.clone()),
      //}
      ;
      for (id, &(ref time, ref insert_event)) in self.fiat_events.iter() {
        insert_event(&mut self.steward, time.clone(), id.clone());
      }
    }
    self.snapshots.push(self.steward
      .snapshot_before(time)
      .expect("reloading from an earlier snapshot was supposed to make this work!"));
    self.steward.snapshot_before(time)
  }
}


impl<B: Basics, Steward0: ::IncrementalTimeSteward <B>> ::IncrementalTimeSteward<B> for Steward<B, Steward0>
where for <'a> & 'a Steward0::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>{
  fn step(&mut self) {
    self. steward.step();
  }
  fn updated_until_before (&self)->Option <B::Time> {
    self. steward.updated_until_before()
  }
}
