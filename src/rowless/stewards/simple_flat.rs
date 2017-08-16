use std::mem;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::cmp::{Ordering, max};
use std::borrow::Borrow;
use std::any::Any;
use std::io::{Read, Write};
use std::rc::Rc;
use std::marker::PhantomData;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use rand::Rng;

use super::super::api::*;
use super::super::implementation_support::common::*;
use implementation_support::common::split_off_greater_set;
use {DeterministicRandomId};

time_steward_steward_specific_api!();

#[derive (Debug, Clone)]
struct DataTimelineInnerShared {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
}
#[derive (Debug)]
struct DataTimelineInner <T: DataTimeline> {
  shared: DataTimelineInnerShared,
  data: RefCell<T>,
}
#[derive (Debug)]
struct EventInnerShared <B: Basics> {
  time: ExtendedTime <B>,
}
#[derive (Debug)]
struct EventInner <T: Event> {
  shared: EventInnerShared <<T::Steward as TimeSteward>::Basics>,
  data: T,
}
trait EventInnerTrait <B: Basics>: Any + Debug {
  fn shared (&self)->& EventInnerShared <B>;
  fn execute (&self, self_handle: & DynamicEventHandle <B>, steward: &mut Steward <B>);
}
impl <B: Basics, T: Event <Steward = Steward <B>>> EventInnerTrait <B> for EventInner <T> {
  fn shared (&self)->& EventInnerShared <B> {&self.shared}
  fn execute (&self, self_handle: & DynamicEventHandle<B>, steward: &mut Steward <B>) {
    let mut accessor = EventAccessorStruct {
      generic: GenericEventAccessor::new(&self.shared.time),
      handle: self_handle.clone(),
      steward: steward,
    };
    self.data.execute (&mut accessor);
  }
}
trait DataTimelineInnerTrait <B: Basics>: Any + Debug {
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
}


#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataTimelineHandle <T: DataTimeline> {
  data: Rc<DataTimelineInner<T>>
}
#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DynamicDataTimelineHandle <B: Basics> {
  data: Rc <DataTimelineInnerTrait <B>>
}

#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct EventHandle <T: Event> {
  data: Rc <EventInner<T>>
}

#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DynamicEventHandle <B: Basics> {
  data: Rc <EventInnerTrait<B>>
}
#[derive (Debug, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct PredictionHandle <T: Event> {
  data: Rc <EventInner<T>>
}

impl <T: DataTimeline> DataTimelineHandle <T> {
  pub fn erase_type (self)->DynamicDataTimelineHandle<T::Basics> {
    DynamicDataTimelineHandle {
      data: self.data as Rc <DataTimelineInnerTrait <T::Basics>>
    }
  }
}
impl <B: Basics> DynamicDataTimelineHandle<B> {
  pub fn downcast <T: DataTimeline <Basics = B>> (self)->Option <DataTimelineHandle<T>> {
    unimplemented!()
  }
}
impl <B: Basics, T: Event <Steward = Steward <B>>> EventHandle <T> {
  pub fn erase_type (self)->DynamicEventHandle<B> {
    DynamicEventHandle {
      data: self.data as Rc <EventInnerTrait<B>>
    }
  }
}
impl <B: Basics> DynamicEventHandle <B> {
  pub fn downcast <T: Event> (self)->Option <EventHandle<T>> {
    unimplemented!() 
    /*self.data.downcast_ref::<EventInner <T>> ().map (DynamicEventHandle {
      data: self.data as Rc <EventInnerTrait<<T::Steward as TimeSteward>::Basics>>
    })*/
  }
}
impl <B: Basics, T: Event <Steward = Steward <B>>> PredictionHandle <T> {
  pub fn as_dynamic_event (self)->DynamicEventHandle<B> {
    DynamicEventHandle {
      data: self.data as Rc <EventInnerTrait<B>>
    }
  }
}

impl <T: Event> EventHandleTrait for EventHandle <T> {
  type Basics = <T::Steward as TimeSteward>::Basics;
  fn extended_time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared.time}
}
impl <T: Event> EventHandleTrait for PredictionHandle <T> {
  type Basics = <T::Steward as TimeSteward>::Basics;
  fn extended_time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared.time}
}
impl <B: Basics> EventHandleTrait for DynamicEventHandle<B> {
  type Basics = B;
  fn extended_time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared().time}
}

impl <T: DataTimeline> DataTimelineHandleTrait for DataTimelineHandle <T> {}
impl <B: Basics> DataTimelineHandleTrait for DynamicDataTimelineHandle <B> {}
impl <T: DataTimeline> TypedDataTimelineHandleTrait <T> for DataTimelineHandle <T> {
  fn new(data: T)->Self {
    thread_local! {
      static NEXT_SERIAL_NUMBER: Cell <usize> = Cell::new (0);
    }
    DataTimelineHandle {
      data: Rc::new(DataTimelineInner {
        shared: DataTimelineInnerShared {
          serial_number: NEXT_SERIAL_NUMBER.with (| cell | {
            let result = cell.get();
            cell.set (result + 1) ;
            result
          }),
          first_snapshot_not_updated: Cell::new (0),
        },
        data: RefCell::new (data),
      })
    }
  }
}

impl <T: DataTimeline> Hash for DataTimelineHandle <T> {
  fn hash <H: Hasher> (&self, state: &mut H) {
    self.data.shared().serial_number.hash (state);
  }
}
impl <T: DataTimeline> Eq for DataTimelineHandle <T> {}
impl <T: DataTimeline> PartialEq for DataTimelineHandle <T> {
  fn eq(&self, other: &Self) -> bool {
    self.data.shared().serial_number == other.data.shared().serial_number
  }
}
impl <B: Basics> Hash for DynamicDataTimelineHandle <B>{
  fn hash <H: Hasher> (&self, state: &mut H) {
    self.data.shared().serial_number.hash (state);
  }
}
impl <B: Basics> Eq for DynamicDataTimelineHandle <B> {}
impl <B: Basics> PartialEq for DynamicDataTimelineHandle <B> {
  fn eq(&self, other: &Self) -> bool {
    self.data.shared().serial_number == other.data.shared().serial_number
  }
}


time_steward_common_impls_for_handles!();

time_steward_serialization_impls_for_handle!(
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
);


#[derive (Debug)]
pub struct EventAccessorStruct <'a, B: Basics> {
  generic: GenericEventAccessor,
  handle: DynamicEventHandle <B>,
  steward: &'a mut Steward<B>,
}
#[derive (Debug)]
pub struct SnapshotInner <B: Basics> {
  time: ExtendedTime <B>,
  global_timeline: DataTimelineHandle <B::GlobalTimeline>,
  clones: RefCell<HashMap<DynamicDataTimelineHandle <B>, DynamicDataTimelineHandle <B> >>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <B: Basics> {
  data: Rc <SnapshotInner <B>>,
}
#[allow (unreachable_patterns, unreachable_code)]
#[derive (Debug)]
pub struct InvalidationAccessorStruct <B: Basics> (!, PhantomData <B>);


impl <'a, B: Basics> Accessor for EventAccessorStruct <'a, B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->&DataTimelineHandle <B::GlobalTimeline> {&self.steward.global_timeline}
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = B>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    DataTimelineQueriableWith::<Query>::query (&*handle.data.data.borrow(), query, self.extended_now(), offset)
  }
}
impl <B: Basics> Accessor for SnapshotHandle <B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->&DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> {
    &self.data.global_timeline
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    let mut guard = self.data.clones.borrow_mut();
    let entry = guard.entry (handle.clone().erase_type());
    let handle = entry.or_insert_with (| | DataTimelineHandle::new (handle.data.data.borrow().clone_for_snapshot (self.extended_now())).erase_type());
    let typed = handle.clone().downcast::<T>().unwrap();
    let timeline_guard = typed.data.data.borrow();
    DataTimelineQueriableWith::<Query>::query(
      &*timeline_guard, query, self.extended_now(), offset)
  }
}
impl <B: Basics> MomentaryAccessor for SnapshotHandle <B> {
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
    & self.data.time
  }
}
impl <'a, B: Basics> EventAccessor for EventAccessorStruct <'a, B> {
  fn handle (&self)->& DynamicEventHandle <B> {
    &self.handle
  }
  
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineHandle <T>, modification: F) {
    modification (&mut*timeline.data.data.borrow_mut());
  }
  
  fn create_prediction <E: Event <Steward = Self::Steward>> (&mut self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)->PredictionHandle<E> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::Basics> (time, id, self.extended_now()).unwrap();
    let handle = PredictionHandle {
      data: Rc::new (EventInner {
        data: event,
        shared: EventInnerShared {

          time: time,
        }
      })
    };
    self.steward.existent_predictions.insert (handle.clone().as_dynamic_event());
    handle
  }
  fn destroy_prediction <E: Event <Steward = Self::Steward>> (&mut self, prediction: &PredictionHandle<E>) {
    assert!(self.steward.existent_predictions.remove (& prediction.clone().as_dynamic_event()));
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
  fn global_timeline (&self)->&DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> { unimplemented!() }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, _: & DataTimelineHandle <T>, _: &Query, _: QueryOffset)-> T::QueryResult { unimplemented!() }
}
impl <B: Basics> MomentaryAccessor for InvalidationAccessorStruct <B> {
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> { unimplemented!() }
}
impl <B: Basics> PeekingAccessor for InvalidationAccessorStruct <B> {
  fn peek <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&self, _: & DataTimelineHandle <T>)->& T { unimplemented!() }
}
impl <B: Basics> InvalidationAccessor for InvalidationAccessorStruct <B> {
  fn invalidate <T: Event <Steward = Self::Steward>> (&self, _: & EventHandle <T>) { unimplemented!() }
  fn invalidate_dynamic (&self, _: & DynamicEventHandle<<Self::Steward as TimeSteward>::Basics>) { unimplemented!() }
}

#[derive (Debug)]
pub struct Steward <B: Basics> {
  global_timeline: DataTimelineHandle <B::GlobalTimeline>,
  invalid_before: ValidSince <B::Time>,
  last_event: Option <ExtendedTime <B>>,
  upcoming_fiat_events: BTreeSet<DynamicEventHandle<B>>,
  existent_predictions: BTreeSet <DynamicEventHandle<B>>,
  snapshots: BTreeMap<usize, SnapshotHandle <B>>,
  next_snapshot_index: usize,
}


impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<&DynamicEventHandle<B>> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take (1);
    let first_predicted_event_iter = self.existent_predictions.iter().take (1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min()
  }

  fn execute_event(&mut self, event: &DynamicEventHandle <B>) {
    event.data.execute (event, &mut*self);
    // if it was a fiat event, clean it up:
    self.upcoming_fiat_events.remove(event);
    self.last_event = Some(event.extended_time().clone());
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type SnapshotAccessor = SnapshotHandle <B>;
  type InvalidationAccessor = InvalidationAccessorStruct <B>;

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
    match self.upcoming_fiat_events.insert(EventHandle {data: Rc::new(EventInner {shared: EventInnerShared {time:extended_time_of_fiat_event(time, id)}, data: event})}.erase_type()) {
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
    if self.valid_since() > *time { return None; }
    while let Some (updated) = self.updated_until_before () {
      if updated >= *time {break;}
      self.step();
    }
    self.next_snapshot_index += 1;
    let handle = SnapshotHandle {
      data: Rc::new (SnapshotInner {
        global_timeline: self.global_timeline.clone(),
        time: unimplemented!(),
        clones: RefCell::new (HashMap::new()),
      })
    };
    self.snapshots.insert (self.next_snapshot_index, handle.clone());
    Some (handle);
  }
  
  fn forget_before (&mut self, _: & B::Time) {}
}


impl <B: Basics> ConstructibleTimeSteward for Steward <B> {
  fn from_global_timeline (timeline: <Self::Basics as Basics>::GlobalTimeline)->Self {
    Steward {
      global_timeline: DataTimelineHandle::new (timeline),
      invalid_before: ValidSince::TheBeginning,
      last_event: None,
      upcoming_fiat_events: BTreeSet::new(),
      existent_predictions: BTreeSet::new(),
      snapshots: BTreeMap::new(),
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

time_steward_define_simple_timeline!();
