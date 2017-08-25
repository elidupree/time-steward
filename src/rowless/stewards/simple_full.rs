macro_rules! simple_full {
  ($($auditing:tt)*) => {

use std::mem;
use std::cell::{Cell, RefCell, Ref, RefMut};
use std::collections::{BTreeMap, BTreeSet, HashMap, Bound};
use std::cmp::{Ordering, max};
use std::borrow::Borrow;
use std::any::Any;
use std::io::{Read, Write};
use std::rc::Rc;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use super::super::api::*;
use super::super::implementation_support::common::*;
use {DeterministicRandomId};

time_steward_steward_specific_api!();

thread_local! {
  static NEXT_SERIAL_NUMBER: Cell <usize> = Cell::new (0);
}
fn new_serial_number()->usize {
  NEXT_SERIAL_NUMBER.with (| cell | {
    let result = cell.get();
    cell.set (result + 1);
    result
  })
}

#[derive (Debug)]
pub struct DataTimelineCell <T: DataTimeline> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  data: RefCell<T>,
  #[cfg($($auditing)*)]
  queries: BTreeMap<ExtendedTime<T::Basics>, usize>,
}
type DataTimelineCellReadGuard<'a, T> = Ref<'a, T>;
type DataTimelineCellWriteGuard<'a, T> = RefMut<'a, T>;
#[derive (Debug)]
struct ExecutionState {
  valid: bool,
  execution_data: Box <Any>,
}
#[derive (Debug)]
struct EventInner <B: Basics> {
  time: ExtendedTime <B>,
  data: Box <EventInnerTrait<B>>,
  should_be_executed: Cell<bool>,
  is_prediction: bool,
  prediction_destroyed_by: RefCell<Option <EventHandle <B>>>,
  execution_state: RefCell<Option <ExecutionState>>,
}
trait EventInnerTrait <B: Basics>: Any + Debug {
  fn execute (&self, self_handle: & EventHandle <B>, steward: &mut Steward <B>);
  fn undo (&self, self_handle: & EventHandle <B>, steward: &mut Steward <B>);
  fn re_execute (&self, self_handle: & EventHandle <B>, steward: &mut Steward <B>);
}
impl <B: Basics, T: Event <Steward = Steward <B>>> EventInnerTrait <B> for T {
  fn execute (&self, self_handle: & EventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      steward: RefCell::new (steward),
    };
    let result = <T as Event>::execute (self, &mut accessor);
    mem::replace (&mut*self_handle.data.execution_state.borrow_mut(), Some (ExecutionState {
      valid: true,
      execution_data: Box::new (result),
    }));
  }
  fn undo (&self, self_handle: & EventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      steward: RefCell::new (steward),
    };
    <T as Event>::undo (self, &mut accessor, *self_handle.data.execution_state.borrow_mut().take().unwrap().execution_data.downcast().unwrap());
  }
  fn re_execute (&self, self_handle: & EventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      steward: RefCell::new (steward),
    };
    let result = <T as Event>::re_execute (self, &mut accessor, *self_handle.data.execution_state.borrow_mut().take().unwrap().execution_data.downcast().unwrap());
    
    mem::replace (&mut*self_handle.data.execution_state.borrow_mut(), Some (ExecutionState {
      valid: true,
      execution_data: Box::new (result),
    }));
  }
}


#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataHandle <T: StewardData> {
  data: Rc<T>
}

#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct EventHandle <B: Basics> {
  data: Rc <EventInner<B>>
}


impl <B: Basics> EventHandleTrait<B> for EventHandle <B> {
  fn extended_time (& self)->& ExtendedTime <B> {& self.data.time}
  fn downcast_ref <T: Any> (&self)->Option<&T> {
    downcast_ref!(&*self.data.data, T, EventInnerTrait<B>)
  }
}

impl <T: StewardData> DataHandleTrait <T> for DataHandle <T> {
  fn new(data: T)->Self {
    DataHandle { data: Rc::new(data) }
  }
}
impl <T: DataTimeline> DataTimelineCellTrait <T> for DataTimelineCell <T> {
  fn new(data: T)->Self {
    DataTimelineCell {
      serial_number: new_serial_number(),
      first_snapshot_not_updated: Cell::new (0),
      data: RefCell::new (data),
      #[cfg($($auditing)*)]
      queries: BTreeMap::new(),
    }
  }
}
impl <T: DataTimeline> Clone for DataTimelineCell <T> {
  fn clone(&self)->Self {
    Self::new(self.data.borrow().clone())
  }
}

impl <T: StewardData> Deref for DataHandle <T> {
  type Target = T;
  fn deref (&self) -> &T {
    &*self.data
  }
}

time_steward_common_impls_for_handles!();
time_steward_common_impls_for_uniquely_identified_handle! ([T: StewardData] [DataHandle <T>] self => (&*self.data as *const T): *const T);
time_steward_common_impls_for_uniquely_identified_handle! ([T: DataTimeline] [DataTimelineCell <T>] self => (self.serial_number): usize);

time_steward_serialization_impls_for_handle!(
  [T: DataTimeline] [DataTimelineCell <T>]
  (&self) Data located at (| handle | &mut handle.data)
);
time_steward_serialization_impls_for_handle!(
  [B: Basics] [EventHandle <B>]
  (&self) Data located at (| handle | &mut unimplemented!())
);
time_steward_serialization_impls_for_handle!(
  [T: StewardData] [DataHandle <T>]
  (&self) Data located at (| handle | &mut*handle.data)
);

#[derive (Debug)]
pub struct EventAccessorStruct <'a, B: Basics> {
  handle: EventHandle <B>,
  globals: Rc<B::Globals>,
  steward: RefCell<&'a mut Steward<B>>,
}
#[derive (Debug)]
pub struct SnapshotInner <B: Basics> {
  index: usize,
  time: ExtendedTime <B>,
  globals: Rc<B::Globals>,
  clones: RefCell<HashMap<usize, Box<Any>>>,
  snapshots_tree: Rc<RefCell<SnapshotsTree<B>>>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <B: Basics> {
  data: Rc <SnapshotInner <B>>,
}

type SnapshotsTree<B> = BTreeMap<usize, SnapshotHandle <B>>;

impl <B: Basics> Drop for SnapshotHandle <B> {
  fn drop (&mut self) {
    assert!(Rc::strong_count(&self.data) >= 2);
    // if we are the last one dropped, our data still exists, and so does the entry in the tree
    if Rc::strong_count(&self.data) == 2 {
      // when we drop the one from the map recursively, that one will also observe a strong count of 2, so short-circuit it
      if let Ok (mut map) = self.data.snapshots_tree.try_borrow_mut() {
        map.remove (&self.data.index);
      }
    }
  }
}

impl <'a, B: Basics> Accessor for EventAccessorStruct <'a, B> {
  type Steward = Steward <B>;
  fn globals (&self)->&B::Globals {&*self.globals}
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
    self.handle().extended_time()
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = B>> (&self, timeline: & DataTimelineCell <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    DataTimelineQueriableWith::<Query>::query (&*timeline.data.borrow(), query, self.extended_now(), offset)
  }
}
impl <B: Basics> Accessor for SnapshotHandle <B> {
  type Steward = Steward <B>;
  fn globals (&self)->&B::Globals {&*self.data.globals}
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
    & self.data.time
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, timeline: & DataTimelineCell <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    let mut guard = self.data.clones.borrow_mut();
    let entry = guard.entry (timeline.serial_number);
    let boxref = entry.or_insert_with (| | Box::new (
      timeline.data.borrow().clone_for_snapshot (self.extended_now())
    ));
    let typed = boxref.downcast_ref::<T>().unwrap();
    DataTimelineQueriableWith::<Query>::query(typed, query, self.extended_now(), offset)
  }
}
impl <'a, B: Basics> EventAccessor for EventAccessorStruct <'a, B> {
  fn handle (&self)->& EventHandle <B> {
    &self.handle
  }
  
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineCell <T>, modification: F) {
    {
      let index = timeline.first_snapshot_not_updated.get ();
      let steward = self.steward.borrow();
      let guard = (*steward.snapshots).borrow();
      let map: &SnapshotsTree<B> = &*guard;
      for (_,snapshot) in map.range ((Bound::Included(index), Bound::Unbounded)) {
        let mut guard = snapshot.data.clones.borrow_mut();
        let entry = guard.entry (timeline.serial_number);
        entry.or_insert_with (| | Box::new (timeline.data.borrow().clone_for_snapshot (self.extended_now())));
      }
      timeline.first_snapshot_not_updated.set (steward.next_snapshot_index);
    }
    
    let mut modify_guard = timeline.data.borrow_mut();
    modification (&mut*modify_guard);
    match &self.steward.borrow().invalid_before {
      &ValidSince::Before (ref time) => modify_guard.forget_before(&ExtendedTime::beginning_of (time.clone())),
      &ValidSince::After (ref time) => modify_guard.forget_before(&ExtendedTime::end_of(time.clone())),
      &ValidSince::TheBeginning => (),
    }
  }
  
  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)->EventHandle <B> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::Basics> (time, id, self.extended_now()).expect("You can't create a prediction in the past.");
    let handle = EventHandle {
      data: Rc::new (EventInner {
        time: time,
        data: Box::new (event),
        should_be_executed: Cell::new(true),
        is_prediction: true,
        prediction_destroyed_by: RefCell::new (None),
        execution_state: RefCell::new (None),
      })
    };
    assert!(self.steward.borrow_mut().events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}), "Created a prediction at the same time as one that already existed and has not yet been destroyed.");
    handle
  }
  fn destroy_prediction (&self, prediction: &EventHandle<B>) {
    assert!(prediction.data.is_prediction, "Attempted to destroy a fiat event as if it was a prediction.");
    let mut guard = prediction.data.prediction_destroyed_by.borrow_mut();
    if let Some (old_destroyer) = guard.as_ref() {
      assert!(self.handle() < old_destroyer, "You can't destroy a prediction that was already destroyed. (A prediction is supposed to be destroyed exactly when it's no longer accessible in the simulation data. Double-destroying it implies that you held onto a handle to it somewhere, which is probably a bug.)");
    }
    mem::replace (&mut*guard, Some(self.handle().clone()));
    if prediction != self.handle() {
      self.steward.borrow_mut().event_shouldnt_be_executed (prediction);
    }
  }
  
  type FutureCleanupAccessor = Self;
  fn future_cleanup(&self)->Option<&Self::FutureCleanupAccessor> {
    // We're always ALLOWED to return Some, even if it would be more optimal not to.
    Some(self)
  }
}

// EventAccessorStruct is also the FutureCleanupAccessor – its functionality is only restricted by what bounds the client code is allowed to place on it
impl <'a, B: Basics> FutureCleanupAccessor for EventAccessorStruct <'a, B> {
  fn peek <'c, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'c self, timeline: &'b DataTimelineCell<T>)->DataTimelineCellReadGuard<'b, T> {
    timeline.data.borrow()
  }
  fn peek_mut <'c, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'c self, timeline: &'b DataTimelineCell<T>)->DataTimelineCellWriteGuard<'b, T> {
    timeline.data.borrow_mut()
  }
  fn change_prediction_destroyer (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle, destroyer: Option <&<Self::Steward as TimeSteward>::EventHandle>) {
    //TODO assertions
    if let Some(destroyer) = destroyer {
      assert!(destroyer >= self.handle(), "Tried to set of prediction's destruction time to a time in the past");
    }
    mem::replace (&mut*prediction.data.prediction_destroyed_by.borrow_mut(), destroyer.cloned());
    if prediction != self.handle() {
      if let Some(destroyer) = destroyer {
        assert!(destroyer < prediction, "Tried to set of prediction's destruction time to after the prediction is supposed to be executed");
        self.steward.borrow_mut().event_shouldnt_be_executed(prediction);
      }
      else {
        self.steward.borrow_mut().event_should_be_executed(prediction);
      }
    }
  }
  fn invalidate_execution (&self, handle: & <Self::Steward as TimeSteward>::EventHandle) {
    assert!(handle > self.handle(), "An event at {:?} tried to invalidate one at {:?}. Only future events can be invalidated.", self.extended_now(), handle.extended_time());
    self.steward.borrow_mut().invalidate_event_execution (handle);
  }
}

impl <B: Basics> SnapshotAccessor for SnapshotHandle <B> {
  fn serialize_into <W: Write> (&self, writer: W) {
    unimplemented!()
  }
}



      // when you delete an event and then re-create it, we want
      // undoing the deleted event to come BEFORE executing the new one.
      // (false comes before true)
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct EventNeedingAttention<B: Basics> {
  handle: EventHandle<B>,
  should_be_executed: bool,
}

#[derive (Debug)]
pub struct Steward <B: Basics> {
  globals: Rc<B::Globals>,
  invalid_before: ValidSince <B::Time>,
  events_needing_attention: BTreeSet<EventNeedingAttention<B>>,
  fiat_events: BTreeSet<EventHandle <B>>,
  snapshots: Rc<RefCell<SnapshotsTree<B>>>,
  next_snapshot_index: usize,
}


impl<B: Basics> Steward<B> {
  fn next_event_needing_attention (&self) -> Option<&EventNeedingAttention<B>> {
    self.events_needing_attention.iter().next()
  }
  
  fn do_event (&mut self, event: & EventNeedingAttention<B>) {
    self.events_needing_attention.remove (event);
    let event = &event.handle;
    if event.data.should_be_executed.get() {
      let currently_executed = match event.data.execution_state.borrow().as_ref() {
        Some (state) => {
          assert! (!state.valid);
          true
        },
        None => false,
      };
      if currently_executed {
        event.data.data.re_execute (event, &mut*self);
      }
      else {
        event.data.data.execute (event, &mut*self);
      }
      if event.data.is_prediction {
        assert!(event.data.prediction_destroyed_by.borrow().as_ref() == Some(event), "An event at {:?} should have destroyed itself, but its destruction time was {:?} instead. All predicted events must destroy the prediction that predicted them. (It's ambiguous what should happen if the prediction isn't destroyed. There are two natural meanings: either it continues existing meaninglessly, or it gets executed repeatedly until it destroys itself. Neither of these seems especially desirable, so we take the conservative approach and forbid the whole situation from arising. This is also future-proofing in case we choose a specific behavior later.", event.extended_time(), event.data.prediction_destroyed_by.borrow().as_ref().map (| destroyer | destroyer.extended_time()))
      }
    }
    else {
      assert! (event.data.execution_state.borrow().as_ref().is_some());
      event.data.data.undo (event, &mut*self);
    }
  }
  
  
  
  fn invalidate_event_execution (&mut self, handle: & EventHandle<B>) {
    if let Some(state) = handle.data.execution_state.borrow_mut().as_mut() {
      if handle.data.should_be_executed.get() && state.valid {
        assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
      state.valid = false;
    }
  }
  fn event_should_be_executed (&mut self, handle: & EventHandle<B>) {
    if !handle.data.should_be_executed.get() {
      if let Some(state) = handle.data.execution_state.borrow().as_ref() {
        assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: false}));
        if !state.valid {
          assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
        }
      }
      else {
        assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
      handle.data.should_be_executed.set(true);
    }
  }
  fn event_shouldnt_be_executed (&mut self, handle: & EventHandle<B>) {
    if handle.data.should_be_executed.get() {
      if let Some(state) = handle.data.execution_state.borrow().as_ref() {
        assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: false}));
        if !state.valid {
          assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
        }
      }
      else {
        assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
      handle.data.should_be_executed.set(false);
    }
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type SnapshotAccessor = SnapshotHandle <B>;
  type EventHandle = EventHandle <B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    self.invalid_before.clone()
  }
  
  fn insert_fiat_event<E: Event<Steward = Self>>(&mut self,
                                               time: B::Time,
                                               id: DeterministicRandomId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let handle = EventHandle {data: Rc::new (EventInner {
        time: extended_time_of_fiat_event(time, id),
        data: Box::new (event),
        should_be_executed: Cell::new(true),
        is_prediction: false,
        prediction_destroyed_by: RefCell::new (None),
        execution_state: RefCell::new (None),
      })};
    match self.fiat_events.insert(handle.clone()) {
      false => Err(FiatEventOperationError::InvalidInput),
      true => {
        self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true});
        Ok(())
      },
    }
  }

  fn remove_fiat_event(&mut self,
                       time: &B::Time,
                       id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.fiat_events.take(&extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(handle) => {
        self.event_shouldnt_be_executed (&handle);
        Ok(())
      },
    }
  }
  
  fn snapshot_before (&mut self, time: & B::Time)->Option <Self::SnapshotAccessor> {
    // NOT self.valid_since(); this Steward can continue recording snapshots from earlier than the earliest time it can accept fiat event input
    if self.invalid_before > *time { return None; }
    while let Some (updated) = self.updated_until_before () {
      if updated >= *time {break;}
      self.step();
    }
    let handle = SnapshotHandle {
      data: Rc::new (SnapshotInner {
        index: self.next_snapshot_index,
        globals: self.globals.clone(),
        time: ExtendedTime::beginning_of(time.clone()),
        clones: RefCell::new (HashMap::new()),
        snapshots_tree: self.snapshots.clone(),
      })
    };
    self.snapshots.borrow_mut().insert (self.next_snapshot_index, handle.clone());
    self.next_snapshot_index += 1;
    Some (handle)
  }
  
  fn forget_before (&mut self, time: & B::Time) {
    self.invalid_before = max (self.invalid_before.clone(), ValidSince::Before(time.clone()));
    
  }
}


impl <B: Basics> ConstructibleTimeSteward for Steward <B> {
  fn from_globals (globals: <Self::Basics as Basics>::Globals)->Self {
    Steward {
      globals: Rc::new (globals),
      invalid_before: ValidSince::TheBeginning,
      events_needing_attention: BTreeSet::new(),
      fiat_events: BTreeSet::new(),
      snapshots: Rc::new (RefCell::new (BTreeMap::new())),
      next_snapshot_index: 0,
    }
  }
  
  fn deserialize_from <R: Read> (data: &mut R)->Self {
    unimplemented!()
  }
}

impl<B: Basics> IncrementalTimeSteward for Steward<B> {
  fn step(&mut self) {
    if let Some(event) = self.next_event_needing_attention().cloned() {
      self.do_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event_needing_attention().map(|event| event.handle.extended_time().base.clone())
  }
}
impl<B: Basics> CanonicalTimeSteward for Steward<B> {}

time_steward_define_simple_timeline!();




#[cfg($($auditing)*)]
mod audits {
  use super::*;
  fn audit_timeline<T: DataTimeline> (timeline: & DataTimelineCell <T>) {
    for query in timeline.queries.borrow() {
      
    }
  }
}

  };
}

simple_full!(any());
