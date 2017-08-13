use std::mem;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::cmp::{Ordering, max};
use std::borrow::Borrow;
use std::any::Any;
use std::io::{Read, Write};
use std::rc::Rc;
use std::marker::PhantomData;

use super::super::api::*;
use super::super::implementation_support::common::*;
use implementation_support::common::split_off_greater_set;
use {DeterministicRandomId};

time_steward_steward_specific_api!();

struct DataTimelineInner <T: DataTimeline> {
  serial_number: usize,
  first_snapshot_not_updated: Cell<usize>,
  data: RefCell<T>,
}
struct EventInnerShared <B: Basics> {
  serial_number: usize,
  time: ExtendedTime <B>,
}
struct EventInner <T: Event> {
  shared: EventInnerShared <<T::Steward as TimeSteward>::Basics>,
  data: T,
}
trait EventInnerTrait <B: Basics>: Any {
  fn shared (&self)->& EventInnerShared <B>;
  fn execute (&self, self_handle: & DynamicEventHandle <B>, steward: &mut Steward <B>);
}
impl <T: Event> EventInnerTrait <<T::Steward as TimeSteward>::Basics> for EventInner <T> {
  fn shared (&self)->& EventInnerShared <<T::Steward as TimeSteward>::Basics> {&self.shared}
  fn execute (&self, self_handle: & DynamicEventHandle<<T::Steward as TimeSteward>::Basics>, steward: &mut Steward <<T::Steward as TimeSteward>::Basics>) {
    let mut accessor = EventAccessorStruct {
      generic: GenericEventAccessor::new(self.shared.time.clone()),
      handle: self_handle,
      steward: steward,
    };
    self.data.execute (accessor);
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

impl <T: Event> EventHandle <T> {
  pub fn erase_type (self)->DynamicEventHandle<<T::Steward as TimeSteward>::Basics> {
    DynamicEventHandle {
      data: self.data as Rc <EventInnerTrait<<T::Steward as TimeSteward>::Basics>>
    }
  }
}
impl <B: Basics> DynamicEventHandle <B> {
  pub fn downcast <T: Event> (self)->Option <EventHandle<T>> {
    self.data.downcast_ref::<EventInner <T>> ().map (DynamicEventHandle {
      data: self.data as Rc <EventInnerTrait<<T::Steward as TimeSteward>::Basics>>
    })
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
        data: data
      })
    }
  }
}

time_steward_common_impls_for_handles!();



pub struct EventAccessorStruct <'a, T: Event> {
  generic: GenericEventAccessor,
  handle: EventHandle <T>,
  steward: &'a mut Steward<<T::Steward as TimeSteward>::Basics>,
}
pub struct SnapshotStruct <B: Basics> {
  time: ExtendedTime <B>,
  clones: RefCell<HashMap<DynamicDataTimelineHandle, DynamicDataTimelineHandle>>,
}
pub struct InvalidationAccessorStruct <B: Basics> (!, PhantomData <B>);


impl <'a, T: Event> Accessor for EventAccessorStruct <'a, T> {
  type Steward = Steward <<T::Steward as TimeSteward>::Basics>;
  fn global_timeline (&self)->DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> {self.steward.global_timeline}
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    handle.data.borrow().query (query, self.now(), offset)
  }
}
impl <B: Basics> Accessor for SnapshotStruct <B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> {
    
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
  
  }
}
impl <B: Basics> MomentaryAccessor for SnapshotStruct <B> {
  fn now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics> {
  
  }
}
impl <'a, T: Event> EventAccessor for EventAccessorStruct <'a, T> {
  type Event = T;
  fn handle (&self)->& EventHandle <Self::Event> {
    self.event
  }
  
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineHandle <T>, modification: F) {
    modification (&mut*timeline.data.borrow_mut());
  }
  
  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)->PredictionHandle<E> {
    let time = extended_time_of_fiat_event::<<Self::Steward as TimeSteward>::Basics> (time, id);
    let handle = PredictionHandle {
      data: Rc::new (EventInner {
        data: event,
        shared: EventInnerShared {
          serialize_number: panic!(),
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

impl <B: Basics> Snapshot for SnapshotStruct <B> {
  fn serialize_into <W: Write> (&self, writer: W) {
    unimplemented!()
  }
}

impl <B: Basics> Accessor for InvalidationAccessorStruct <B> {
  type Steward = Steward <B>;
  fn global_timeline (&self)->DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline> {
    unimplemented!()
  }
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, handle: & DataTimelineHandle <T>, query: &Query, offset: QueryOffset)-> T::QueryResult {
    unimplemented!()
  }
}
impl <B: Basics> MomentaryAccessor for InvalidationAccessorStruct <B> {}
impl <B: Basics> PeekingAccessor for InvalidationAccessorStruct <B> {}
impl <B: Basics> InvalidationAccessor for InvalidationAccessorStruct <B> {}


struct Steward <B: Basics> {
  global_timeline: DataTimelineHandle <B::GlobalTimeline>,
  invalid_before: ValidSince <B::Time>,
  last_event: Option <ExtendedTime <B>>,
  upcoming_fiat_events: BTreeSet<DynamicEventHandle<B>>,
  existent_predictions: BTreeSet <DynamicEventHandle<B>>,
  snapshots: BTreeMap<usize, SnapshotStruct <B>>,
}


impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, DynamicEventHandle<B>)> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take (1);
    let first_predicted_event_iter = self.existent_predictions.iter().map(| prediction | prediction.as_dynamic_event).take (1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min_by_key(|ev| &ev.0)
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: DynamicEventHandle <B>) {
    event.data.execute (event, &mut*self);
    // if it was a fiat event, clean it up:
    self.upcoming_fiat_events.remove(&event_time);
    self.last_event = Some(event_time);
  }
}


impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type Snapshot = SnapshotStruct <B>;
  type InvalidationAccessor = InvalidationAccessorStruct <B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max(self.state.invalid_before.clone(),
        match self.state.last_event {
          None => ValidSince::TheBeginning,
          Some(ref time) => ValidSince::After(time.base.clone()),
        })
  }
  
  fn insert_fiat_event<E: Event<Steward = Self>>(&mut self,
                                               time: B::Time,
                                               id: DeterministicRandomId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.insert(extended_time_of_fiat_event(time, id),
                                        EventHandle {data: Rc::new(event)}.erase_type()) {
      None => Ok(()),
      Some(_) => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn remove_fiat_event(&mut self,
                       time: &B::Time,
                       id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.remove(&extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(_) => Ok(()),
    }
  }
  
  fn snapshot_before (&mut self, time: & B::Time)->Option <Self::Snapshot> {
    if self.valid_since() > *time { return None; }
    while let Some (updated) = self.updated_until_before () {
      if updated >= time {break;}
      self.step();
    }
    Some (self.snapshots.make_new (time))
  }
  
  fn forget_before (&mut self, time: & B::Time) {}
}


impl <B: Basics> ConstructibleTimeSteward for Steward <B> {
  fn from_global_timeline (timeline: <Self::Basics as Basics>::GlobalTimeline)->Self {
    Steward {
      global_timeline: timeline,
      invalid_before: ValidSince::TheBeginning,
      last_event: None,
      upcoming_fiat_events: BTreeSet::new(),
      upcoming_predictions: BTreeSet::new(),
      snapshots: BTreeMap::new(),
    }
  }
  
  fn deserialize_from <R: Read> (data: &mut R)->Self {
    unimplemented!()
  }
}

impl<B: Basics> IncrementalTimeSteward for Steward<B> {
  fn step(&mut self) {
    if let Some(ev) = self.next_event() {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event().map(|(time, _)| time.base)
  }
}
impl<B: Basics> CanonicalTimeSteward for Steward<B> {}

time_steward_define_simple_timeline!();
