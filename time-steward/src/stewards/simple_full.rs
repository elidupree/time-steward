macro_rules! simple_full {
  ($($auditing:tt)*) => {

use std::mem;
use std::cell::{Cell, RefCell, Ref, RefMut};
use std::collections::{BTreeMap, BTreeSet, HashMap, Bound};
use std::cmp::{Ordering, max};
use std::borrow::Borrow;
use std::any::{Any, TypeId};
use std::io::{Read, Write};
use std::rc::Rc;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use super::super::api::*;
use super::super::implementation_support::common::*;
use {EntityId};
use type_utils::{PersistentlyIdentifiedType, DynamicPersistentlyIdentifiedType};

use implementation_support::insert_only;

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
pub struct EntityCell <T: Entity> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  data: RefCell<T>,
  #[cfg($($auditing)*)]
  queries: BTreeMap<EventHandle<T::SimulationSpec>, Vec<usize>>,
  //#[cfg($($auditing)*)]
  //modifiers: BTreeSet<EventHandle<T::SimulationSpec>>,
}
pub type EntityCellReadGuard<'a, T> = Ref<'a, T>;
pub type EntityCellWriteGuard<'a, T> = RefMut<'a, T>;
#[derive (Debug)]
struct ExecutionState {
  valid: bool,
  execution_data: Box <Any>,
}
#[derive (Debug)]
struct EventInner <S: SimulationSpec> {
  time: ExtendedTime <S>,
  data: Box <EventInnerTrait<S>>,
  links: Cell<usize>,
  is_prediction: bool,
  execution_state: RefCell<Option <ExecutionState>>,
}
trait EventInnerTrait <S: SimulationSpec>: Any + Debug + SerializeInto + DynamicPersistentlyIdentifiedType {
  fn execute (&self, self_handle: & EventHandle <S>, steward: &mut Steward <S>);
  fn undo (&self, self_handle: & EventHandle <S>, steward: &mut Steward <S>);
  fn re_execute (&self, self_handle: & EventHandle <S>, steward: &mut Steward <S>);
  fn get_type_id(&self)->TypeId;
}
impl <S: SimulationSpec, T: Event <Steward = Steward <S>>> EventInnerTrait <S> for T {
  fn execute (&self, self_handle: & EventHandle<S>, steward: &mut Steward <S>) {
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
  fn undo (&self, self_handle: & EventHandle<S>, steward: &mut Steward <S>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      steward: RefCell::new (steward),
    };
    <T as Event>::undo (self, &mut accessor, *self_handle.data.execution_state.borrow_mut().take().unwrap().execution_data.downcast().unwrap());
  }
  fn re_execute (&self, self_handle: & EventHandle<S>, steward: &mut Steward <S>) {
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
  fn get_type_id(&self)->TypeId {TypeId::of::<T>()}
}


#[derive (Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataHandle <T: SimulationStateData + PersistentlyIdentifiedType> {
  data: Rc<T>
}

#[derive (Derivative)]
#[derivative (Clone (bound = ""))]
pub struct EventHandle <S: SimulationSpec> {
  data: Rc <EventInner<S>>
}


impl <S: SimulationSpec> EventHandleTrait<S> for EventHandle <S> {
  fn extended_time (& self)->& ExtendedTime <S> {& self.data.time}
  fn downcast_ref <T: Any> (&self)->Option<&T> {
    downcast_ref!(&*self.data.data, T, EventInnerTrait<S>)
  }
}

impl <T: SimulationStateData + PersistentlyIdentifiedType> DataHandleTrait <T> for DataHandle <T> {
  fn new_for_globals(data: T)->Self {
    DataHandle { data: Rc::new(data) }
  }
}
impl <T: Entity> EntityCellTrait <T> for EntityCell <T> {
  fn new(data: T)->Self {
    EntityCell {
      serial_number: new_serial_number(),
      first_snapshot_not_updated: Cell::new (0),
      data: RefCell::new (data),
      #[cfg($($auditing)*)]
      queries: BTreeMap::new(),
    }
  }
}
impl <T: Entity> Clone for EntityCell <T> {
  fn clone(&self)->Self {
    Self::new(self.data.borrow().clone())
  }
}

impl <T: SimulationStateData + PersistentlyIdentifiedType> Deref for DataHandle <T> {
  type Target = T;
  fn deref (&self) -> &T {
    &*self.data
  }
}

time_steward_common_impls_for_handles!();
time_steward_common_impls_for_uniquely_identified_handle! ([T: SimulationStateData + PersistentlyIdentifiedType] [DataHandle <T>] self => (&*self.data as *const T): *const T);
time_steward_common_impls_for_uniquely_identified_handle! ([T: Entity] [EntityCell <T>] self => (self.serial_number): usize);

time_steward_serialization_impls!();
fn deserialization_create_event_inner <S: SimulationSpec, T: Event<Steward = Steward<S>>> (time: ExtendedTime <S>, data: T, in_future: bool, links: Cell<usize>)->EventInner<S> {
  EventInner {
    time: time,
    data: Box::new (data),
    links: links,
    is_prediction: in_future,
    execution_state: RefCell::new (None),
  }
}
fn deserialization_create_prediction <S: SimulationSpec> (steward: &mut Steward <S>, prediction: EventHandle <S>) {
  steward.events_needing_attention.insert (EventNeedingAttention {handle: prediction, should_be_executed: true});
}

#[derive (Debug)]
pub struct EventAccessorStruct <'a, S: SimulationSpec> {
  handle: EventHandle <S>,
  globals: Rc<S::Globals>,
  steward: RefCell<&'a mut Steward<S>>,
}
#[derive (Debug)]
pub struct SnapshotInner <S: SimulationSpec> {
  index: usize,
  time: ExtendedTime <S>,
  globals: Rc<S::Globals>,
  clones: insert_only::HashMap<usize, Box<Any>>,
  snapshots_tree: Rc<RefCell<SnapshotsTree<S>>>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <S: SimulationSpec> {
  data: Rc <SnapshotInner <S>>,
}

impl <S: SimulationSpec> SnapshotHandle <S> {
  fn get_clone <T: Entity <SimulationSpec = S>> (&self, timeline: & EntityCell <T>)->& EntityCell <T> {
    self.data.clones.get_default (timeline.serial_number, | | Some(Box::new (
      EntityCell::new (timeline.data.borrow().clone_for_snapshot (self.extended_now()))
    ))).unwrap ().downcast_ref::<EntityCell <T>>().expect("A clone in a snapshot was a different type than what it was supposed to be a clone of; maybe two different timelines got the same serial number somehow")
  }
}

type SnapshotsTree<S> = BTreeMap<usize, SnapshotHandle <S>>;

impl <S: SimulationSpec> Drop for SnapshotHandle <S> {
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

impl <'a, S: SimulationSpec> Accessor for EventAccessorStruct <'a, S> {
  type Steward = Steward <S>;
  fn globals (&self)->&S::Globals {&*self.globals}
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::SimulationSpec> {
    self.this_event().extended_time()
  }
  fn query <Q: Query, T: EntityQueriableWith<Q, SimulationSpec = S>> (&self, timeline: & EntityCell <T>, query: &Q)-> T::QueryResult {
    EntityQueriableWith::<Q>::query (&*timeline.data.borrow(), query, self.extended_now())
  }
  fn query_ref <'timeline, Q: Query, T: EntityQueryRefableWith<Q, SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>> (&'timeline self, timeline: &'timeline EntityCell<T>, query: &Q)-> EntityCellReadGuard<'timeline, T::QueryResult> {
    Ref::map(timeline.data.borrow(), |timeline| EntityQueryRefableWith::<Q>::query_ref (timeline, query, self.extended_now()))
  }
}
impl <S: SimulationSpec> Accessor for SnapshotHandle <S> {
  type Steward = Steward <S>;
  fn globals (&self)->&S::Globals {&*self.data.globals}
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::SimulationSpec> {
    & self.data.time
  }
  fn query <Q: Query, T: EntityQueriableWith<Q, SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>> (&self, timeline: & EntityCell <T>, query: &Q)-> T::QueryResult {
    EntityQueriableWith::<Q>::query(&*self.get_clone (timeline).data.borrow(), query, self.extended_now())
  }
  fn query_ref <'timeline, Q: Query, T: EntityQueryRefableWith<Q, SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>> (&'timeline self, timeline: &'timeline EntityCell<T>, query: &Q)-> EntityCellReadGuard<'timeline, T::QueryResult> {
    Ref::map(self.get_clone (timeline).data.borrow(), |timeline| EntityQueryRefableWith::<Q>::query_ref (timeline, query, self.extended_now()))
  }
}
impl <'a, S: SimulationSpec> EventAccessor for EventAccessorStruct <'a, S> {
  fn this_event (&self)->& EventHandle <S> {
    &self.handle
  }

  fn new_handle<T: SimulationStateData + PersistentlyIdentifiedType> (&self, data: T)->DataHandle <T> {
    DataHandle {data: Rc::new(data)}
  }

  fn modify <T: Entity<SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>, F: FnOnce(&mut T)> (&self, timeline: &EntityCell <T>, modification: F) {
    {
      let index = timeline.first_snapshot_not_updated.get ();
      let steward = self.steward.borrow();
      let guard = (*steward.snapshots).borrow();
      let map: &SnapshotsTree<S> = &*guard;
      for (_,snapshot) in map.range ((Bound::Included(index), Bound::Unbounded)) {
        snapshot.get_clone (timeline) ;
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

  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: <<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Time, id: EntityId, event: E)->EventHandle <S> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::SimulationSpec> (time, id, self.extended_now()).expect("You can't create a prediction in the past.");
    let handle = EventHandle {
      data: Rc::new (EventInner {
        time: time,
        data: Box::new (event),
        links: Cell::new(0),
        is_prediction: true,
        execution_state: RefCell::new (None),
      })
    };
    //assert!(self.steward.borrow_mut().events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}), "Created a prediction at the same time as one that already existed and has not yet been destroyed.");
    handle
  }
  fn link_prediction (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    assert!(prediction.data.is_prediction, "Attempted to link a fiat event as if it was a prediction.");
    assert!(prediction.extended_time() > self.extended_now(), "Attempted to link a prediction in the past.");
    let previous = prediction.data.links.get();
    if previous == 0 {
      self.steward.borrow_mut().event_should_be_executed (prediction);
    }
    prediction.data.links.set(previous + 1);
  }
  fn unlink_prediction (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    assert!(prediction.data.is_prediction, "Attempted to unlink a fiat event as if it was a prediction.");
    assert!(prediction.extended_time() > self.extended_now(), "Attempted to unlink a prediction in the past.");
    let previous = prediction.data.links.get();
    assert!(previous > 0, "unlinked a prediction more times than it was linked");
    if previous == 1 {
      self.steward.borrow_mut().event_shouldnt_be_executed (prediction);
    }
    prediction.data.links.set(previous - 1);
  }

  type FutureCleanupAccessor = Self;
  fn future_cleanup(&self)->Option<&Self::FutureCleanupAccessor> {
    // We're always ALLOWED to return Some, even if it would be more optimal not to.
    Some(self)
  }
}

// EventAccessorStruct is also the FutureCleanupAccessor – its functionality is only restricted by what bounds the client code is allowed to place on it
impl <'a, S: SimulationSpec> FutureCleanupAccessor for EventAccessorStruct <'a, S> {
  fn peek <'c, 'b, T: Entity<SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>> (&'c self, timeline: &'b EntityCell<T>)->EntityCellReadGuard<'b, T> {
    timeline.data.borrow()
  }
  fn peek_mut <'c, 'b, T: Entity<SimulationSpec = <Self::Steward as TimeSteward>::SimulationSpec>> (&'c self, timeline: &'b EntityCell<T>)->EntityCellWriteGuard<'b, T> {
    timeline.data.borrow_mut()
  }
  fn invalidate_execution (&self, handle: & <Self::Steward as TimeSteward>::EventHandle) {
    assert!(handle > self.this_event(), "An event at {:?} tried to invalidate one at {:?}. Only future events can be invalidated.", self.extended_now(), handle.extended_time());
    self.steward.borrow_mut().invalidate_event_execution (handle);
  }
}

impl <S: SimulationSpec> SnapshotAccessor for SnapshotHandle <S> {
  fn serialize_into <W: Write> (&self, writer: &mut W)->::bincode::Result <()> {
    serialize_snapshot (writer, self.clone())
  }
}



      // when you delete an event and then re-create it, we want
      // undoing the deleted event to come BEFORE executing the new one.
      // (false comes before true)
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct EventNeedingAttention<S: SimulationSpec> {
  handle: EventHandle<S>,
  should_be_executed: bool,
}

#[derive (Debug)]
pub struct Steward <S: SimulationSpec> {
  globals: Rc<S::Globals>,
  invalid_before: ValidSince <S::Time>,
  events_needing_attention: BTreeSet<EventNeedingAttention<S>>,
  fiat_events: BTreeSet<EventHandle <S>>,
  snapshots: Rc<RefCell<SnapshotsTree<S>>>,
  next_snapshot_index: usize,
}


impl<S: SimulationSpec> Steward<S> {
  fn next_event_needing_attention (&self) -> Option<&EventNeedingAttention<S>> {
    self.events_needing_attention.iter().next()
  }

  fn do_event (&mut self, event: & EventNeedingAttention<S>) {
    self.events_needing_attention.remove (event);
    assert_eq!(event.handle.data.links.get() > 0, event.should_be_executed);
    let event = &event.handle;
    if event.data.links.get() > 0 {
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
    }
    else {
      assert! (event.data.execution_state.borrow().as_ref().is_some());
      event.data.data.undo (event, &mut*self);
    }
  }



  fn invalidate_event_execution (&mut self, handle: & EventHandle<S>) {
    if let Some(state) = handle.data.execution_state.borrow_mut().as_mut() {
      if handle.data.links.get() > 0 && state.valid {
        assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
      state.valid = false;
    }
  }
  fn event_should_be_executed (&mut self, handle: & EventHandle<S>) {
    if let Some(state) = handle.data.execution_state.borrow().as_ref() {
      assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: false}));
      if !state.valid {
        assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
    }
    else {
      assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
    }
  }
  fn event_shouldnt_be_executed (&mut self, handle: & EventHandle<S>) {
    if let Some(state) = handle.data.execution_state.borrow().as_ref() {
      assert! (self.events_needing_attention.insert (EventNeedingAttention {handle: handle.clone(), should_be_executed: false}));
      if !state.valid {
        assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
      }
    }
    else {
      assert! (self.events_needing_attention.remove (&EventNeedingAttention {handle: handle.clone(), should_be_executed: true}));
    }
  }
}


impl<S: SimulationSpec> TimeSteward for Steward<S> {
  type SimulationSpec = S;
  type SnapshotAccessor = SnapshotHandle <S>;
  type EventHandle = EventHandle <S>;

  fn valid_since(&self) -> ValidSince<S::Time> {
    self.invalid_before.clone()
  }

  fn insert_fiat_event<E: Event<Steward = Self>>(&mut self,
                                               time: S::Time,
                                               id: EntityId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let handle = EventHandle {data: Rc::new (EventInner {
        time: extended_time_of_fiat_event(time, id),
        data: Box::new (event),
        links: Cell::new(1),
        is_prediction: false,

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
                       time: &S::Time,
                       id: EntityId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.fiat_events.take(&extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(handle) => {
        handle.data.links.set(0);
        self.event_shouldnt_be_executed (&handle);
        Ok(())
      },
    }
  }

  fn snapshot_before (&mut self, time: & S::Time)->Option <Self::SnapshotAccessor> {
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
        clones: insert_only::HashMap::new(),
        snapshots_tree: self.snapshots.clone(),
      })
    };
    self.snapshots.borrow_mut().insert (self.next_snapshot_index, handle.clone());
    self.next_snapshot_index += 1;
    Some (handle)
  }

  fn forget_before (&mut self, time: & S::Time) {
    self.invalid_before = max (self.invalid_before.clone(), ValidSince::Before(time.clone()));

  }
}


impl <S: SimulationSpec> ConstructibleTimeSteward for Steward <S> {
  fn from_globals (globals: <Self::SimulationSpec as SimulationSpec>::Globals)->Self {
    Steward {
      globals: Rc::new (globals),
      invalid_before: ValidSince::TheBeginning,
      events_needing_attention: BTreeSet::new(),
      fiat_events: BTreeSet::new(),
      snapshots: Rc::new (RefCell::new (BTreeMap::new())),
      next_snapshot_index: 0,
    }
  }

  fn deserialize_from <R: Read> (data: &mut R)->::bincode::Result <Self> {
    deserialize_something (data)
  }
}

impl<S: SimulationSpec> IncrementalTimeSteward for Steward<S> {
  fn step(&mut self) {
    if let Some(event) = self.next_event_needing_attention().cloned() {
      self.do_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<S::Time> {
    self.next_event_needing_attention().map(|event| event.handle.extended_time().base.clone())
  }
}
impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}

time_steward_define_simple_timeline!();
time_steward_define_bbox_collision_detection!();




#[cfg($($auditing)*)]
mod audits {
  use super::*;

  impl<S: SimulationSpec> Steward <S> {
    pub fn audit_timeline<T: Entity> (&self, timeline: & EntityCell <T>) {
      for _query in timeline.queries.borrow() {

      }
    }
    pub fn audit_after_event (&self, _event: & EventHandle <S>) {
      /*for timeline_id in event.modified.iter() {

      }*/
    }
  }
}

  };
}

simple_full!(any());
