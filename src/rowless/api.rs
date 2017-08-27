use ::DeterministicRandomId;
use std::hash::Hash;
use serde::Serialize;
use serde::de::DeserializeOwned;
//use std::io::{Read, Write};
use std::any::Any;
use std::fmt::Debug;
//use std::cmp::Ordering;
use std::borrow::Borrow;
use std::ops::Deref;

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// TimeSteward has strong requirements for serializability. In addition to implementing these traits, a StewardData must have all its behavior be invariant under cloning and serialize-deserialize cycles, and identical to that of any other object that is == to it.
/// We used to require `Send + Sync` for this, but now that DataTimelineHandles can be part of StewardData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
pub trait StewardData: Any + Clone + Eq + Serialize + DeserializeOwned + Debug {}

/// DataTimeline operations must be auditable. Their arguments have to be StewardData,
/// so that they can be clone, stored, and compared to other values.
///
/// However, for optimization, we might want to pass a &StewardData instead of a StewardData.
/// This trait includes both, and also complex such that as (&A, B, &C) where A, B, and C are StewardData.
/// For auditing purposes, it must be able to convert back and forth from the underlying StewardData,
/// so that the system can check that it is actually equivalent in information.
pub trait PossiblyBorrowedStewardData <'a, Owned: StewardData> {
  fn to_owned (self)->Owned;
  fn clone_to_owned (&self)->Owned;
  fn from_ref (source: &'a Owned)->Self;
}



// Model: events interact with the physics only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time, and the events must use invalidate() to collect future events that must be invalidated. (Although, for instance, modifications made for dependency tracking purposes might not change any results any queries, so they wouldn't create the need for any invalidation.)
//To audit, we record all of the queries and query results. Then after each event that modifies one or more DataTimelines, we rerun all queries to those timelines made by still-valid future events. If any query has a different result than before, it's an error.

// Given a query input, the function (time->query output) must be piecewise constant, changing only at times when modifications have been inserted.
// Event must implement undo. After an event is undone, there may not remain any modifications at the time of that event. It follows that after doing and then undoing an event, all query results immediately before the event are equal to the corresponding results immediately after the event.

//These would be associated type constructors if Rust supported those: DataTimelineHandle, EventHandle, DynamicEventHandle, PredictionHandle


pub enum QueryOffset {
  Before, After
}

pub trait DataTimeline: Any + Clone + Serialize + DeserializeOwned + Debug {
  type Basics: Basics;

  /// Make a clone of only the data necessary to report accurately at a specific time.
  // audit: the clone yields the same query results immediately before and after the time
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self;
  
  // audit: forget functions don't change any query results except those forgotten
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>);
}
pub trait DataTimelineQueriableWith<'a, Owned: StewardData, Query: PossiblyBorrowedStewardData <'a, Owned>>: DataTimeline {
  type QueryResultOwned: StewardData;
  type QueryResult: PossiblyBorrowedStewardData <'a, Self::QueryResultOwned>;
  
  // audit: queries must not have side effects (do a separate action for manual dependency tracking)
  // audit: cloning and re-borrowing the
  // audit: queries don't return PredictionHandles that don't exist at the time
  fn query (&self, query: Query, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult;
}



/**
This is intended to be implemented on an empty struct. Requiring Clone etc. is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait Basics
  : Any + Send + Sync + Copy + Clone + Ord + Hash + Serialize + DeserializeOwned + Debug + Default {
  type Time: StewardData + Ord + Hash;
  type Globals: StewardData;
  const MAX_ITERATION: IterationType = 65535;
}

pub type IterationType = u32;
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ExtendedTime<B: Basics> {
  pub base: B::Time,
  pub iteration: IterationType,
  pub id: DeterministicRandomId,
}

impl <B: Basics> ExtendedTime<B> {
  pub fn beginning_of (time: B::Time) -> ExtendedTime<B> {
    ExtendedTime { base: time, iteration: 0, id: DeterministicRandomId::MIN }
  }
  pub fn end_of (time: B::Time) -> ExtendedTime<B> {
    ExtendedTime { base: time, iteration: B::MAX_ITERATION, id: DeterministicRandomId::MAX }
  }
}

#[derive (Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

pub trait EventHandleTrait <B: Basics>: StewardData + Ord + Hash + Borrow<ExtendedTime <B>> {
  fn extended_time (&self)->& ExtendedTime <B>;
  fn time (&self)->& B::Time {& self.extended_time().base}
  fn downcast_ref <T: Any> (&self)->Option<&T>;
}
pub trait DataHandleTrait <T: StewardData>: StewardData + Hash + Deref<Target = T> {
  fn new(data: T)->Self;
}
pub trait DataTimelineCellTrait <T: DataTimeline>: StewardData + Hash {
  fn new(data: T)->Self;
}

/*pub struct TimeRange <Time> {
  pub start: Time,
  pub end: Option <Time>,
}*/

// This exists to support a variety of time stewards
// along with allowing BaseTime to be dense (e.g. a
// rational number rather than an integer).
// It is an acceptable peculiarity that even for integer times,
// After(2) < Before(3).
// #[derive (Copy, Clone, PartialEq, Eq, Hash)]
#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ValidSince<BaseTime> {
  TheBeginning,
  Before(BaseTime),
  After(BaseTime),
}


#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_steward_specific_api {
  () => {

pub trait Event: StewardData {
  type Steward: TimeSteward;
  type ExecutionData;
  // audit all functions: calls invalidate_event for everything whose queries would be changed
  // audit all functions: doesn't change any query results in the past
  fn execute<Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor)->Self::ExecutionData;
  // audit: leaves self in its original state??
  // audit: after undoing, all query results immediately before and after the event are identical to each other (if the previous audit passes, this is more an audit of the DataTimeline types than this event type)
  fn undo<Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, execution_data: Self::ExecutionData);
  // audit: should produce the same subsequent query results as doing undo() and then execute()
  // implementing this is simply an optimization that may allow you to invalidate fewer things, so we default-implement it
  fn re_execute<Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, execution_data: Self::ExecutionData) {
    self.undo (accessor, execution_data);
    self.execute (accessor);
  }
}

pub trait Accessor {
  type Steward: TimeSteward;
  fn globals (&self)->&<<Self::Steward as TimeSteward>::Basics as Basics>::Globals;
  fn extended_now(&self) -> & ExtendedTime <<Self::Steward as TimeSteward>::Basics>;
  fn now(&self) -> & <<Self::Steward as TimeSteward>::Basics as Basics>::Time {&self.extended_now().base}
  fn id(&self) -> DeterministicRandomId {self.extended_now().id}
  fn query <'a, Owned: StewardData, Query: PossiblyBorrowedStewardData <'a, Owned>, T: DataTimelineQueriableWith<'a, Owned, Query, Basics = <Self::Steward as TimeSteward>::Basics>> (&self, timeline: & DataTimelineCell<T>, query: Query, offset: QueryOffset)-> T::QueryResult;
}

pub trait EventAccessor: Accessor {
  fn handle (&self)->& <Self::Steward as TimeSteward>::EventHandle;
  
  // modification is done within a closure, to help prevent the event from extracting any information from DataTimelines except by querying. I'd like to make this a Fn instead of FnOnce, to prevent the user from putting &mut in it that could communicate back to the outer function, but it may be useful for optimization to be able move owned objects into the closure.
  // audit: the event does the same thing if the closure isn't called, as long as we feed it the same query results after that
  fn modify <T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>, F: FnOnce(&mut T)> (&self, timeline: &DataTimelineCell<T>, modification: F);
  
  // audit: whenever an event is executed or undone, it creates/destroys the exact predictions that become existent/nonexistent between the serializations of the physics immediately before and after the event.
  // audit: never generates two predictions with the same id, except when rerunning the same event
  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: <<Self::Steward as TimeSteward>::Basics as Basics>::Time, id: DeterministicRandomId, event: E)-><Self::Steward as TimeSteward>::EventHandle;
  // audit: predicted events must destroy themselves
  // audit: you can't destroy a fiat event as if it's a prediction
  fn destroy_prediction (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle);
  
  type FutureCleanupAccessor: FutureCleanupAccessor<Steward = Self::Steward>;
  fn future_cleanup(&self)->Option<&Self::FutureCleanupAccessor>;
}

// Querying accessors are generally for things that can affect the canonical physics. Querying uses an exact interface that can be tracked and audited in various ways to make sure the physics stays consistent.
// `FutureCleanupAccessor`s don't have any leeway to change the canonical physics. We allow them full access with no tracking, and merely audit that they did the job they were supposed to. They are also allowed to use the querying interface for convenience (so that they can call generic functions that take a regular accessor).
pub trait FutureCleanupAccessor: EventAccessor {
  // note that, when undoing events, query results don't necessarily correspond to those observed by the original execution in any way
  fn peek <'a, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'a self, timeline: &'b DataTimelineCell<T>)->DataTimelineCellReadGuard<'b, T>;
  fn peek_mut <'a, 'b, T: DataTimeline<Basics = <Self::Steward as TimeSteward>::Basics>> (&'a self, timeline: &'b DataTimelineCell<T>)->DataTimelineCellWriteGuard<'b, T>;
  // audit: can't change things in the past relative to the current event
  fn change_prediction_destroyer (&self, prediction: &<Self::Steward as TimeSteward>::EventHandle, destroyer: Option <&<Self::Steward as TimeSteward>::EventHandle>);
  // audit: can't invalidate things in the past relative to the current event
  fn invalidate_execution (&self, handle: & <Self::Steward as TimeSteward>::EventHandle);
}

pub trait SnapshotAccessor: Accessor {
  /// note: SnapshotAccessor::serialize() matches TimeSteward::deserialize()
  fn serialize_into <W: Write> (&self, writer: W);
}


pub trait TimeSteward: Any + Sized + Debug {
  type Basics: Basics;
  type SnapshotAccessor: SnapshotAccessor <Steward = Self>;
  type EventHandle: EventHandleTrait <Self::Basics>;
  
  fn insert_fiat_event<E: Event <Steward = Self>>(&mut self, time: <Self::Basics as Basics>::Time, id: DeterministicRandomId, event: E)
                                               -> Result<(), FiatEventOperationError>;
  fn remove_fiat_event(&mut self, time: &<Self::Basics as Basics>::Time, id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError>;
  fn snapshot_before (&mut self, time: &<Self::Basics as Basics>::Time)->Option <Self::SnapshotAccessor>;
  
  fn valid_since(&self) -> ValidSince<<Self::Basics as Basics>::Time>;
  fn forget_before (&mut self, time: &<Self::Basics as Basics>::Time);
}

/// A trait for TimeSteward types that can be initialized from just the initial physics data.
/// Most TimeSteward types should implement this. Exceptions are types that can't function without certain extra runtime metadata
pub trait ConstructibleTimeSteward: TimeSteward {
  fn from_globals (globals: <Self::Basics as Basics>::Globals)->Self;
  /// note: SnapshotAccessor::serialize() matches TimeSteward::deserialize()
  fn deserialize_from <R: Read> (data: &mut R)->Self;
}

pub trait IncrementalTimeSteward: TimeSteward {
  fn step(&mut self);
  fn updated_until_before(&self) -> Option<<Self::Basics as Basics>::Time>;
}

pub trait CanonicalTimeSteward: TimeSteward {}


  };
}
