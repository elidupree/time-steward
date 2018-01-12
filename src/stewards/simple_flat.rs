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
use std::mem;

use super::super::api::*;
use super::super::implementation_support::common::*;
use {DeterministicRandomId};

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
pub struct DataTimelineCell <T: DataTimeline> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  data: RefCell<T>,
}
pub type DataTimelineCellReadGuard<'a, T> = Ref<'a, T>;
pub type DataTimelineCellWriteGuard<'a, T> = RefMut<'a, T>;
#[derive (Debug)]
struct EventInner <B: Basics> {
  links: Cell<usize>,
  time: ExtendedTime <B>,
  data: Box <EventInnerTrait<B>>,
}
trait EventInnerTrait <B: Basics>: Any + Debug + SerializeInto + DynamicPersistentlyIdentifiedType {
  fn execute (&self, self_handle: & EventHandle <B>, steward: &mut Steward <B>);
}
impl <B: Basics, T: Event <Steward = Steward <B>>> EventInnerTrait <B> for T {
  fn execute (&self, self_handle: & EventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      steward: RefCell::new (steward),
    };
    <T as Event>::execute (self, &mut accessor);
  }
}


#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataHandle <T: SimulationStateData + PersistentlyIdentifiedType> {
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

impl <T: SimulationStateData + PersistentlyIdentifiedType> DataHandleTrait <T> for DataHandle <T> {
  fn new_for_globals(data: T)->Self {
    DataHandle { data: Rc::new(data) }
  }
}
impl <T: DataTimeline> DataTimelineCellTrait <T> for DataTimelineCell <T> {
  fn new(data: T)->Self {
    DataTimelineCell {
      serial_number: new_serial_number(),
      first_snapshot_not_updated: Cell::new (0),
      data: RefCell::new (data),
    }
  }
}
impl <T: DataTimeline> Clone for DataTimelineCell <T> {
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
time_steward_common_impls_for_uniquely_identified_handle! ([T: DataTimeline] [DataTimelineCell <T>] self => (self.serial_number): usize);

time_steward_serialization_impls!();
fn deserialization_create_event_inner <B: Basics, T: Event<Steward = Steward<B>>> (time: ExtendedTime <B>, data: T, _in_future: bool, links: Cell<usize>)->EventInner<B> {
  EventInner {
    links: links,
    time: time,
    data: Box::new (data),
  }
}
fn deserialization_create_prediction <B: Basics> (steward: &mut Steward <B>, prediction: EventHandle <B>) {
  steward.existent_predictions.insert (prediction);
}

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
  clones: insert_only::HashMap<usize, Box<Any>>,
  snapshots_tree: Rc<RefCell<SnapshotsTree<B>>>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <B: Basics> {
  data: Rc <SnapshotInner <B>>,
}

impl <B: Basics> SnapshotHandle <B> {
  fn get_clone <T: DataTimeline <Basics = B>> (&self, timeline: & DataTimelineCell <T>)->& DataTimelineCell <T> {
    self.data.clones.get_default (timeline.serial_number, | | Some(Box::new (
      DataTimelineCell::new (timeline.data.borrow().clone_for_snapshot (self.extended_now()))
    ))).unwrap ().downcast_ref::<DataTimelineCell <T>>().expect("A clone in a snapshot was a different type than what it was supposed to be a clone of; maybe two different timelines got the same serial number somehow")
  }
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
    self.this_event().extended_time()
  }
  fn query <Q: Query, T: DataTimelineQueriableWith<Q, Basics = B>> (&self, timeline: & DataTimelineCell <T>, query: &Q)-> T::QueryResult {
    DataTimelineQueriableWith::<Q>::query (&*timeline.data.borrow(), query, self.extended_now())
  }
  fn query_ref <'timeline, Q: Query, T: DataTimelineQueryRefableWith<Q, Basics = <Self::Steward as TimeSteward>::Basics>> (&'timeline self, timeline: &'timeline DataTimelineCell<T>, query: &Q)-> DataTimelineCellReadGuard<'timeline, T::QueryResult> {
    Ref::map(timeline.data.borrow(), |timeline| DataTimelineQueryRefableWith::<Q>::query_ref (timeline, query, self.extended_now()))
  }
}
impl <B: Basics> Accessor for SnapshotHandle <B> {
  type Steward = Steward <B>;
  fn globals (&self)->&B::Globals {&self.data.globals}
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
    & self.data.time
  }
  fn query <Q: Query, T: DataTimelineQueriableWith<Q, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, timeline: & DataTimelineCell <T>, query: &Q)-> T::QueryResult {
    DataTimelineQueriableWith::<Q>::query(&*self.get_clone(timeline).data.borrow(), query, self.extended_now())
  }
  fn query_ref <'timeline, Q: Query, T: DataTimelineQueryRefableWith<Q, Basics = <Self::Steward as TimeSteward>::Basics>> (&'timeline self, timeline: &'timeline DataTimelineCell<T>, query: &Q)-> DataTimelineCellReadGuard<'timeline, T::QueryResult> {
    Ref::map(self.get_clone (timeline).data.borrow(), |timeline| DataTimelineQueryRefableWith::<Q>::query_ref (timeline, query, self.extended_now()))
  }
}
impl <'a, B: Basics> EventAccessor for EventAccessorStruct <'a, B> {
  fn this_event (&self)->& EventHandle <B> {
    &self.handle
  }
  
  fn new_handle<T: SimulationStateData + PersistentlyIdentifiedType> (&self, data: T)->DataHandle <T> {
    DataHandle {data: Rc::new(data)}
  }
  
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineCell <T>, modification: F) {
    {
      let index = timeline.first_snapshot_not_updated.get ();
      let steward = self.steward.borrow();
      let guard = (*steward.snapshots).borrow();
      let map: &SnapshotsTree<B> = &*guard;
      for (_,snapshot) in map.range ((Bound::Included(index), Bound::Unbounded)) {
        snapshot.get_clone(timeline);
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
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::Basics> (time, id, self.extended_now()).unwrap();
    let handle = EventHandle {
      data: Rc::new (EventInner {
        links: Cell::new (0),
        time: time,
        data: Box::new (event),
      })
    };
    //printlnerr!("at {:?}, creating prediction at {:?}", self.extended_now(), handle.extended_time());
    handle
  }
  fn link_prediction (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    let previous = prediction.data.links.get();
    if previous == 0 {
      assert!(self.steward.borrow_mut().existent_predictions.insert (prediction.clone()), "created a prediction that already existed?!");
    }
    prediction.data.links.set(previous + 1);
  }
  fn unlink_prediction (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    let previous = prediction.data.links.get();
    assert!(previous > 0, "unlinked a prediction more times than it was linked");
    if previous == 1 {
      assert!(self.steward.borrow_mut().existent_predictions.remove (prediction));
    }
    prediction.data.links.set(previous - 1);
  }
  
  type FutureCleanupAccessor = Self;
  fn future_cleanup(&self)->Option<&Self::FutureCleanupAccessor> {
    // There are never any future events to clean up.
    None
  }
}

impl <'a, B: Basics> FutureCleanupAccessor for EventAccessorStruct <'a, B> {
  fn peek <'c, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'c self, _: &'b DataTimelineCell<T>)->DataTimelineCellReadGuard<'b, T> {unreachable!()}
  fn peek_mut <'c, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'c self, _: &'b DataTimelineCell<T>)->DataTimelineCellWriteGuard<'b, T> {unreachable!()}
  fn invalidate_execution (&self, _: & <Self::Steward as TimeSteward>::EventHandle) {unreachable!()}
}

impl <B: Basics> SnapshotAccessor for SnapshotHandle <B> {
  fn serialize_into <W: Write> (&self, writer: &mut W)->::bincode::Result <()> {
    serialize_snapshot (writer, self.clone())
  }
}

#[derive (Debug)]
pub struct Steward <B: Basics> {
  globals: Rc<B::Globals>,
  invalid_before: ValidSince <B::Time>,
  last_event: Option <ExtendedTime <B>>,
  upcoming_fiat_events: BTreeSet<EventHandle<B>>,
  existent_predictions: BTreeSet <EventHandle<B>>,
  snapshots: Rc<RefCell<SnapshotsTree<B>>>,
  next_snapshot_index: usize,
}


impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<&EventHandle<B>> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take (1);
    let first_predicted_event_iter = self.existent_predictions.iter().take (1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min()
  }

  fn execute_event(&mut self, event: &EventHandle <B>) {
    event.data.data.execute (event, &mut*self);
    // clean it up:
    self.upcoming_fiat_events.remove(event);
    self.existent_predictions.remove(event);
    self.last_event = Some(event.extended_time().clone());
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type SnapshotAccessor = SnapshotHandle <B>;
  type EventHandle = EventHandle <B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max(self.invalid_before.clone(),
        match self.last_event {
          None => ValidSince::TheBeginning,
          Some(ref time) => ValidSince::After(time.base.clone()),
        })
  }
  
  fn insert_fiat_event<E: Event<Steward = Self>>(&mut self,
                                               time: B::Time,
                                               id: DeterministicRandomId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.upcoming_fiat_events.insert(EventHandle {data: Rc::new (EventInner {links: Cell::new(1), time: extended_time_of_fiat_event(time, id), data: Box::new (event)})}) {
      true => Ok(()),
      false => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn remove_fiat_event(&mut self,
                       time: &B::Time,
                       id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.upcoming_fiat_events.remove(&extended_time_of_fiat_event(time.clone(), id)) {
      false => Err(FiatEventOperationError::InvalidInput),
      true => Ok(()),
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
        clones: insert_only::HashMap::new(),
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
      last_event: None,
      upcoming_fiat_events: BTreeSet::new(),
      existent_predictions: BTreeSet::new(),
      snapshots: Rc::new (RefCell::new (BTreeMap::new())),
      next_snapshot_index: 0,
    }
  }
  
  fn deserialize_from <R: Read> (data: &mut R)->::bincode::Result <Self> {
    deserialize_something (data)
  }
}

impl<B: Basics> IncrementalTimeSteward for Steward<B> {
  fn step(&mut self) {
    if let Some(event) = self.next_event().cloned() {
      self.execute_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event().map(|event| event.extended_time().base.clone())
  }
}
impl<B: Basics> CanonicalTimeSteward for Steward<B> {}

time_steward_define_simple_timeline!();
time_steward_define_bbox_collision_detection!();
