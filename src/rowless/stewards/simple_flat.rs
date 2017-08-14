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

use super::super::api::*;
use super::super::implementation_support::common::*;
use implementation_support::common::split_off_greater_set;
use {DeterministicRandomId};

time_steward_steward_specific_api!();

#[derive (Debug)]
struct DataTimelineInner <T: DataTimeline> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  data: RefCell<T>,
}
#[derive (Debug)]
struct EventInnerShared <B: Basics> {
  serial_number: usize,
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
    self.data.execute (&accessor);
  }
}

#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DataTimelineHandle <T: DataTimeline> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Rc<DataTimelineInner<T>>
}

#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct EventHandle <T: Event> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Rc <EventInner<T>>
}

#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct DynamicEventHandle <B: Basics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Rc <EventInnerTrait<B>>
}
#[derive (Debug, Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound = ""))]
pub struct PredictionHandle <T: Event> {#[serde(deserialize_with = "::serde::Deserialize::deserialize")] data: Rc <EventInner<T>>}

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

impl <T: Event> EventHandleTrait for EventHandle <T> {
  type Basics = <T::Steward as TimeSteward>::Basics;
  fn time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared.time}
}
impl <T: Event> EventHandleTrait for PredictionHandle <T> {
  type Basics = <T::Steward as TimeSteward>::Basics;
  fn time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared.time}
}
impl <B: Basics> EventHandleTrait for DynamicEventHandle<B> {
  type Basics = B;
  fn time (&self)->& ExtendedTime <Self::Basics> {&self.data.shared().time}
}

impl <T: DataTimeline> DataTimelineHandleTrait for DataTimelineHandle <T> {
  fn new(data: T)->Self {
    DataTimelineHandle {
      data: Rc::new(DataTimelineInner {
        data: RefCell::new (data),
      })
    }
  }
}

time_steward_common_impls_for_handles!();


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
  clones: RefCell<HashMap<DynamicDataTimelineHandle, DynamicDataTimelineHandle>>,
}
#[derive (Debug, Clone)]
pub struct SnapshotHandle <B: Basics> {
  data: Rc <SnapshotInner <B>>,
}
#[derive (Debug)]
pub struct InvalidationAccessorStruct <B: Basics> (!, PhantomData <B>);


impl <'a, B: Basics> Accessor for EventAccessorStruct <'a, B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->&DataTimelineHandle <B::GlobalTimeline> {&self.steward.global_timeline}
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = B>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    DataTimelineQueriableWith::<Query>::query (handle.data.borrow(), query, self.now(), offset)
  }
}
impl <B: Basics> Accessor for SnapshotHandle <B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->&DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> {
    &self.data.global_timeline
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    self.data.clones.borrow_mut().entry (handle.clone()).or_insert_with (| | handle.data.data.borrow().clone_for_snapshot (self.now())).query (query, offset)
  }
}
impl <B: Basics> MomentaryAccessor for SnapshotHandle <B> {
  fn now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
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
  
  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)->PredictionHandle<E> {
    let time = extended_time_of_fiat_event::<<Self::Steward as TimeSteward>::Basics> (time, id);
    let handle = PredictionHandle {
      data: Rc::new (EventInner {
        data: event,
        shared: EventInnerShared {
          serial_number: panic!(),
          time: time,
        }
      })
    };
    self.steward.existent_predictions.insert (handle.clone());
    handle
  }
  fn destroy_prediction <E: Event <Steward = Self::Steward>> (&self, prediction: &PredictionHandle<E>) {
    self.steward.existent_predictions.remove (prediction.clone());
  }
  
  fn invalidate <F: FnOnce(&<Self::Steward as TimeSteward>::InvalidationAccessor)> (&self, invalidator: F) {
    // There are never any future events. Do nothing.
  }
}

impl <B: Basics> Snapshot for SnapshotHandle <B> {
  fn serialize_into <W: Write> (&self, writer: W) {
    unimplemented!()
  }
}

impl <B: Basics> Accessor for InvalidationAccessorStruct <B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->&DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> { unimplemented!() }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult { unimplemented!() }
}
impl <B: Basics> MomentaryAccessor for InvalidationAccessorStruct <B> {
  fn now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> { unimplemented!() }
}
impl <B: Basics> PeekingAccessor for InvalidationAccessorStruct <B> {
  fn peek <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>)->& T { unimplemented!() }
}
impl <B: Basics> InvalidationAccessor for InvalidationAccessorStruct <B> {
  fn invalidate <T: Event <Steward = Self::Steward>> (&self, handle: & EventHandle <T>) { unimplemented!() }
  fn invalidate_dynamic (&self, handle: & DynamicEventHandle<<Self::Steward as TimeSteward>::Basics>) { unimplemented!() }
}

#[derive (Debug)]
struct Steward <B: Basics> {
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
    let first_predicted_event_iter = self.existent_predictions.iter().map(| prediction | prediction.as_dynamic_event).take (1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min()
  }

  fn execute_event(&mut self, event: &DynamicEventHandle <B>) {
    event.data.execute (event, &mut*self);
    // if it was a fiat event, clean it up:
    self.upcoming_fiat_events.remove(event);
    self.last_event = Some(event.time().clone());
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type Snapshot = SnapshotHandle <B>;
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
    match self.upcoming_fiat_events.insert(extended_time_of_fiat_event(time, id),
                                        EventHandle {data: Rc::new(event)}.erase_type()) {
      false => Ok(()),
      true => Err(FiatEventOperationError::InvalidInput),
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
  
  fn snapshot_before (&mut self, time: & B::Time)->Option <Self::Snapshot> {
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
        clones: BTreeSet::new(),
      })
    };
    self.snapshots.insert (self.next_snapshot_index, handle.clone());
    Some (handle);
  }
  
  fn forget_before (&mut self, time: & B::Time) {}
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
    if let Some(event) = self.next_event() {
      self.execute_event(event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event().map(|event| event.time().base)
  }
}
impl<B: Basics> CanonicalTimeSteward for Steward<B> {}

time_steward_define_simple_timeline!();
