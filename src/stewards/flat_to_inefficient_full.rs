//! A wrapper that turns a flat TimeSteward into an inefficient fall TimeSteward.
//!
//! This is done by storing snapshots and reverting to the previous snapshot whenever you try to query the past.
//!


use {DeterministicRandomId, Basics, TimeSteward, FiatEventOperationError, ValidSince,
     MomentaryAccessor, FiatSnapshot};
use std::collections::HashMap;

pub struct Steward<B: Basics, Steward0: TimeSteward<Basics = B>> {
  steward: Steward0,
  invalid_before: ValidSince<B::Time>,
  fiat_events: HashMap<DeterministicRandomId,
                       (B::Time, Box<Fn(&mut Steward0, B::Time, DeterministicRandomId)>)>,
  reset_to_original: Box<Fn() -> Steward0>,
  snapshots: Vec<Steward0::Snapshot>,
}

impl<B: Basics, Steward0: TimeSteward<Basics = B>> Steward<B, Steward0> {
  fn reset_if_needed(&mut self, time: &B::Time) {
    if self.steward.valid_since() > *time {
      // println!("whoops");
      while self.snapshots.last().map_or(false, |snapshot| snapshot.now() > time) {
        self.snapshots.pop();
      }
      self.steward = //match self.snapshots.last() {
        //None =>
          (self.reset_to_original) ()
        //,
        //Some (snapshot) => TimeSteward::from_snapshot::<Self::Snapshot> (snapshot, self.settings.clone()),
      //}
      ;
      for (id, &(ref time, ref insert_event)) in self.fiat_events.iter() {
        insert_event(&mut self.steward, time.clone(), id.clone());
      }
    }
  }
}


impl<B: Basics, Steward0: TimeSteward<Basics = B>> TimeSteward for Steward<B, Steward0> {
  type Basics = B;
  type Snapshot = Steward0::Snapshot;

  fn valid_since(&self) -> ValidSince<B::Time> {
    self.invalid_before.clone()
  }
  fn new_empty(constants: B::Constants) -> Self {
    let reset_constants = constants.clone();
    let reset_to_original = Box::new(move || TimeSteward::new_empty(reset_constants.clone()));
    Steward::<B, Steward0> {
      steward: TimeSteward::new_empty(constants.clone()),
      invalid_before: ValidSince::TheBeginning,
      fiat_events: HashMap::new(),
      reset_to_original: reset_to_original,
      snapshots: Vec::new(),
    }
  }

  fn from_snapshot<'a, S: ::Snapshot<Basics = B>>(snapshot: &'a S) -> Self
    where &'a S: IntoIterator<Item = ::SnapshotEntry<'a, B>>
  {
    let reset_snapshot = FiatSnapshot::<B>::from_snapshot::<'a, S>(snapshot);
    let reset_to_original =
      Box::new(move || TimeSteward::from_snapshot::<FiatSnapshot<B>>(&reset_snapshot));
    let mut steward: Steward0 = reset_to_original();
    let snapshot = steward.snapshot_before(snapshot.now()).unwrap();
    Steward::<B, Steward0> {
      steward: steward,
      invalid_before: ValidSince::Before(snapshot.now().clone()),
      fiat_events: HashMap::new(),
      reset_to_original: reset_to_original,
      snapshots: vec![snapshot],
    }
  }
  fn insert_fiat_event<E: ::Event<Basics = B>>(&mut self,
                                               time: B::Time,
                                               id: DeterministicRandomId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    self.reset_if_needed(&time);
    if self.fiat_events.contains_key(&id) {
      return Err(FiatEventOperationError::InvalidInput);
    }
    self.steward.insert_fiat_event(time.clone(), id, event.clone()).unwrap();
    self.fiat_events.insert(id,
                            (time,
                             Box::new(move |steward, time, id| {
                              let _ = steward.insert_fiat_event(time, id, event.clone());
                            })));
    Ok(())
  }
  fn remove_fiat_event(&mut self,
                       time: &B::Time,
                       id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    self.reset_if_needed(time);
    if self.fiat_events.remove(&id).is_none() {
      return Err(FiatEventOperationError::InvalidInput);
    }
    self.steward.remove_fiat_event(time, id).unwrap();
    Ok(())
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    // println!("snapshot?");
    self.reset_if_needed(time);
    self.snapshots.push(self.steward
      .snapshot_before(time)
      .expect("reloading from an earlier snapshot was supposed to make this work!"));
    self.steward.snapshot_before(time)
  }
}


impl<B: Basics, Steward0: ::IncrementalTimeSteward <Basics = B>> ::IncrementalTimeSteward for Steward<B, Steward0>
where for <'a> & 'a Steward0::Snapshot: IntoIterator <Item = ::SnapshotEntry <'a, B>>{
  fn step(&mut self) {
    self.steward.step();
  }
  fn updated_until_before (&self)->Option <B::Time> {
    self.steward.updated_until_before()
  }
}

impl<B: Basics, Steward0: TimeSteward<Basics = B>> ::FullTimeSteward for Steward<B, Steward0> {}
