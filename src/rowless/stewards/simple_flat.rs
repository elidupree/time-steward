use std::mem;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap, Bound};
use std::cmp::{Ordering, max};
use std::borrow::Borrow;
use std::any::Any;
use std::io::{Read, Write};
use std::rc::Rc;
use std::marker::PhantomData;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use rand::Rng;

use super::super::api::*;
use super::super::implementation_support::common::*;
use implementation_support::common::split_off_greater_set;
use {DeterministicRandomId};

time_steward_steward_specific_api!();

thread_local! {
  static NEXT_SERIAL_NUMBER: Cell <usize> = Cell::new (0);
}
#[derive (Debug, Serialize, Deserialize)]
struct DataTimelineCell <T: DataTimeline> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")] 
  data: RefCell<T>,
}
#[derive (Debug)]
struct EventInner <B: Basics> {
  time: ExtendedTime <B>,
  data: Box <EventInnerTrait<B>>,
}
trait EventInnerTrait <B: Basics>: Any + Debug {
  fn execute (&self, self_handle: & EventHandle <B>, steward: &mut Steward <B>);
}
impl <B: Basics, T: Event <Steward = Steward <B>>> EventInnerTrait <B> for T {
  fn execute (&self, self_handle: & EventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      generic: GenericEventAccessor::new(&self_handle.extended_time()),
      handle: self_handle.clone(),
      steward: steward,
    };
    <T as Event>::execute (self, &mut accessor);
  }
}
/*trait DataTimelineInnerTrait <B: Basics>: Any + Debug {
  fn shared (&self)->& DataTimelineInnerShared;
  fn clone_for_snapshot (&self, time: & ExtendedTime <B>)->DynamicDataTimelineHandle <B>;
}
impl <T: DataTimeline> DataTimelineInnerTrait <T::Basics> for DataTimelineInner <T> {
  fn shared (&self)->& DataTimelineInnerShared {&self.shared}
  fn clone_for_snapshot (&self, time: & ExtendedTime <T::Basics>)->DynamicDataTimelineHandle <T::Basics> {
    DataTimelineHandle {
      data: Rc::new (DataTimelineInner {
        shared: self.shared.clone(),
        data: RefCell::new (self.data.borrow().clone_for_snapshot(time)),
      })
    }.erase_type()
  }
}*/


#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataHandle <T: StewardData> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")] 
  data: Rc<T>
}

#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct EventHandle <B: Basics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")] 
  data: Rc <EventInner<B>>
}


impl <B: Basics> EventHandleTrait for EventHandle <B> {
  type Basics = B;
  fn extended_time (& self)->& ExtendedTime <B> {& self.data.time}
  fn downcast_ref <T: Any> (&self)->Option<&T> {
    downcast_ref!(self.data.data, T, EventInnerTrait<B>)
  }
}

//impl <T: DataTimeline> DataTimelineHandleTrait for DataTimelineHandle <T> {}
//impl <B: Basics> DataTimelineHandleTrait for DynamicDataTimelineHandle <B> {}
impl <T: StewardData> DataHandleTrait <T> for DataHandle <T> {
  fn new(data: T)->Self {
    DataHandle {
      data: Rc::new(data)
    }
  }
}
impl <T: DataTimeline> DataTimelineCellTrait <T> for DataTimelineCell <T> {
  fn new(data: T)->Self {
    DataTimelineCell {
      serial_number: NEXT_SERIAL_NUMBER.with (| cell | {
        let result = cell.get();
        cell.set (result + 1) ;
        result
      }),
      first_snapshot_not_updated: Cell::new (0),
      data: RefCell::new (data),
    }
  }
}
impl <T: DataTimeline> Clone for DataTimelineCell <T> {
  fn clone(&self)->Self {
    DataTimelineCell {
      serial_number: NEXT_SERIAL_NUMBER.with (| cell | {
        let result = cell.get();
        cell.set (result + 1) ;
        result
      }),
      first_snapshot_not_updated: Cell::new (0),
      data: self.data.clone(),
    }
  }
}


impl <T: StewardData> Deref for DataHandle <T> {
  type Target = T;
  fn deref (&self) -> &T {
    &*self.data
  }
}

impl <T: StewardData> Hash for DataHandle <T> {
  fn hash <H: Hasher> (&self, state: &mut H) {
    (&*self.data as *const T).hash (state);
  }
}
impl <B: Basics> Hash for EventHandle <B>{
  fn hash <H: Hasher> (&self, state: &mut H) {
    (&*self.data as *const EventInner<B>).hash (state);
  }
}
impl <T: StewardData> Eq for DataHandle <T> {}
impl <T: StewardData> PartialEq for DataHandle <T> {
  fn eq(&self, other: &Self) -> bool {
    (&*self.data as *const T) == (&*other.data as *const T)
  }
}
impl <B: Basics> Eq for EventHandle <B> {}
impl <B: Basics> PartialEq for EventHandle <B> {
  fn eq(&self, other: &Self) -> bool {
    (&*self.data as *const EventInner<B>) == (&*other.data as *const EventInner<B>)
  }
}


impl <T: DataTimeline> Hash for DataTimelineCell <T> {
  fn hash <H: Hasher> (&self, state: &mut H) {
    self.serial_number.hash (state);
  }
}
impl <T: DataTimeline> Eq for DataTimelineCell <T> {}
impl <T: DataTimeline> PartialEq for DataTimelineCell <T> {
  fn eq(&self, other: &Self) -> bool {
    self.serial_number == other.serial_number
  }
}


time_steward_common_impls_for_handles!();

/*time_steward_serialization_impls_for_handle!(
  [T: DataTimeline]
  [DataTimelineHandle <T>]
  (&self)
  Uniquely identified by ((self.data.shared.serial_number): usize)
  Data located at (| handle | &mut handle.data.data)
);
time_steward_serialization_impls_for_handle!(
  [B: Basics]
  [DynamicDataTimelineHandle <B>]
  (&self)
  Uniquely identified by ((self.data.shared().serial_number): usize)
  Data located at (| handle | &mut unimplemented!())
);
time_steward_serialization_impls_for_handle!(
  [T: Event]
  [EventHandle <T>]
  (&self)
  Uniquely identified by ((self.data.shared.time.id): DeterministicRandomId)
  Data located at (| handle | &mut handle.data.data)
);
time_steward_serialization_impls_for_handle!(
  [B: Basics]
  [DynamicEventHandle <B>]
  (&self)
  Uniquely identified by ((self.data.shared().time.id): DeterministicRandomId)
  Data located at (| handle | &mut unimplemented!())
);
time_steward_serialization_impls_for_handle!(
  [T: Event]
  [PredictionHandle <T>]
  (&self)
  Uniquely identified by ((self.data.shared.time.id): DeterministicRandomId)
  Data located at (| handle | &mut handle.data.data)
);*/


#[derive (Debug)]
pub struct EventAccessorStruct <'a, B: Basics> {
  generic: GenericEventAccessor,
  handle: EventHandle <B>,
  steward: &'a mut Steward<B>,
}
#[derive (Debug)]
pub struct SnapshotInner <B: Basics> {
  index: usize,
  time: ExtendedTime <B>,
  globals: B::Globals,
  clones: RefCell<HashMap<usize, Box<Any>>>,
  snapshots_tree: Rc<RefCell<SnapshotsTree<B>>>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <B: Basics> {
  data: Rc <SnapshotInner <B>>,
}
#[allow (unreachable_patterns, unreachable_code)]
#[derive (Debug)]
pub struct InvalidationAccessorStruct <B: Basics> (!, PhantomData <B>);

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
  fn globals (&self)->&B::Globals {&self.steward.globals}
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = B>> (&self, timeline: & DataTimelineCell <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    DataTimelineQueriableWith::<Query>::query (&*timeline.data.borrow(), query, self.extended_now(), offset)
  }
}
impl <B: Basics> Accessor for SnapshotHandle <B> {
  type Steward = Steward <B>;
  fn globals (&self)->&B::Globals {&self.data.globals}
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
impl <B: Basics> MomentaryAccessor for SnapshotHandle <B> {
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
    & self.data.time
  }
}
impl <'a, B: Basics> EventAccessor for EventAccessorStruct <'a, B> {
  fn handle (&self)->& EventHandle <B> {
    &self.handle
  }
  
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineCell <T>, modification: F) {
    let index = timeline.first_snapshot_not_updated.get ();
    let guard = (*self.steward.snapshots).borrow();
    let map: &SnapshotsTree<B> = &*guard;
    for (_,snapshot) in map.range ((Bound::Included(index), Bound::Unbounded)) {
      let mut guard = snapshot.data.clones.borrow_mut();
      let entry = guard.entry (timeline.serial_number);
      entry.or_insert_with (| | Box::new (timeline.data.borrow().clone_for_snapshot (self.extended_now())));
    }
    timeline.first_snapshot_not_updated.set (self.steward.next_snapshot_index);
    
    let mut modify_guard = timeline.data.borrow_mut();
    modification (&mut*modify_guard);
    match &self.steward.invalid_before {
      &ValidSince::Before (ref time) => modify_guard.forget_before(&ExtendedTime::beginning_of (time.clone())),
      &ValidSince::After (ref time) => modify_guard.forget_before(&ExtendedTime::end_of(time.clone())),
      &ValidSince::TheBeginning => (),
    }
  }
  
  fn create_prediction <E: Event <Steward = Self::Steward>> (&mut self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)->EventHandle <B> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::Basics> (time, id, self.extended_now()).unwrap();
    let handle = EventHandle {
      data: Rc::new (EventInner {
        time: time,
        data: Box::new (event),
      })
    };
    assert!(self.steward.existent_predictions.insert (handle.clone()), "created a prediction that already existed?!");
    handle
  }
  fn destroy_prediction (&mut self, prediction: &EventHandle<B>) {
    assert!(self.steward.existent_predictions.remove (& prediction.clone()), "destroyed a prediction doesn't exist? Probably one that was already destroyed");
  }
  
  fn invalidate <F: FnOnce(&<Self::Steward as TimeSteward>::InvalidationAccessor)> (&self, _: F) {
    // There are never any future events. Do nothing.
  }
}
impl <'a, B: Basics> Rng for EventAccessorStruct <'a, B> {
  fn next_u32(&mut self) -> u32 {self.generic.generator.next_u32()}
    fn next_f32(&mut self) -> f32 {
      panic!("Using floating point numbers in TimeSteward events is forbidden because it is nondeterministic across platforms.")
    }
    fn next_f64(&mut self) -> f64 {
      panic!("Using floating point numbers in TimeSteward events is forbidden because it is nondeterministic across platforms.")
    }
}

impl <B: Basics> SnapshotAccessor for SnapshotHandle <B> {
  fn serialize_into <W: Write> (&self, writer: W) {
    unimplemented!()
  }
}

impl <B: Basics> Accessor for InvalidationAccessorStruct <B> {
  type Steward = Steward <B>;
  fn globals (&self)->&<<Self::Steward as TimeSteward>::Basics as Basics>::Globals { unreachable!() }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, _: & DataTimelineCell <T>, _: &Query, _: QueryOffset)-> T::QueryResult { unreachable!() }
}
impl <B: Basics> MomentaryAccessor for InvalidationAccessorStruct <B> {
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> { unreachable!() }
}
impl <B: Basics> PeekingAccessor for InvalidationAccessorStruct <B> {
  fn peek <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&self, _: & DataTimelineCell <T>)->& T { unreachable!() }
}
impl <B: Basics> InvalidationAccessor for InvalidationAccessorStruct <B> {
  fn invalidate (&self, _: & EventHandle <B>) { unreachable!() }
}

#[derive (Debug)]
pub struct Steward <B: Basics> {
  globals: B::Globals,
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
    // if it was a fiat event, clean it up:
    self.upcoming_fiat_events.remove(event);
    self.last_event = Some(event.extended_time().clone());
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type SnapshotAccessor = SnapshotHandle <B>;
  type InvalidationAccessor = InvalidationAccessorStruct <B>;
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
    match self.upcoming_fiat_events.insert(EventHandle {data: Rc::new (EventInner {time: extended_time_of_fiat_event(time, id), data: Box::new (event)})}) {
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
      globals: globals,
      invalid_before: ValidSince::TheBeginning,
      last_event: None,
      upcoming_fiat_events: BTreeSet::new(),
      existent_predictions: BTreeSet::new(),
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
    if let Some(event) = self.next_event().cloned() {
      self.execute_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event().map(|event| event.extended_time().base.clone())
  }
}
impl<B: Basics> CanonicalTimeSteward for Steward<B> {}

//time_steward_define_simple_timeline!();
