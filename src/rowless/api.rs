use ::DeterministicRandomId;
use std::hash::Hash;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::io::{Read, Write};
use std::any::Any;
use std::fmt::{self, Debug};

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// TimeSteward has strong requirements for serializability. In addition to implementing these traits, a StewardData must have Eq be equivalent to equality of its serialization, and all its behavior must be invariant under cloning and serialize-deserialize cycles.
pub trait StewardData: Any + Send + Sync + Clone + Eq + Serialize + DeserializeOwned + Debug {}


// Model: events interact only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time, and may use query_range on other timelines to collect events/predictions that must be invalidated. (Although for instance, modifications made for dependency tracking purposes don't change any results of "real" queries (i.e. those by events and predictors), so they never need to invalidate anything themselves.)
//To audit, we record all of the queries, query results, and modifications. Then after each modification of a DataTimeline, we rerun all queries to that timeline made by still-valid predictions/events. If any query has a different result than before, it's an error. (I guess in order to rerun modifications, we have to feed them the old results of their queries instead of having them do real queries?)

// Given a query input, the function (time->query output) must be piecewise constant, changing only at times when modifications have been inserted.
// Event must implement undo. That must call the undo function matching all modifications it made. After an event is undone, there may not remain any modifications at the time of that event. It follows that after doing and then undoing an event, all queries IMMEDIATELY after the event time are restored to their former value. However, queries in the future might not be restored because modifications are permitted to mess up the future(?)

// Defined by each TimeSteward type; would be associated type constructors if Rust supported those; temporarily defining them here to allow the API to compile by itself
pub struct DataTimelineHandle <T: DataTimeline> {t:T}
pub struct EventHandle <T: Event> {t:T}
pub struct PredictorHandle <T: Predictor> {t:T}
impl <T: Event> EventHandleTrait for EventHandle <T> {
  type Steward = T::Steward;
  fn time (&self)->& ExtendedTime <<Self::Steward as TimeSteward>::Basics> {unimplemented!()}
}

pub trait DataTimeline: Any {
  type Steward: TimeSteward;
  // audit: result NEVER changes unless forget_snapshot() is called
  // audit: restoring from version serialized using fresh snapshot doesn't change any query results afterwards
  fn serialize <Context: SerializationContext> (&self, serializer: &mut Context, snapshot: & <Self::Steward as TimeSteward>::Snapshot);
  fn deserialize <Context: DeserializationContext> (deserializer: &mut Context, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>)->Self;
  
  // audit: forget functions don't change any query results except those forgotten
  fn forget_before (&mut self, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>);
  fn forget_snapshot (&mut self, snapshot: & <Self::Steward as TimeSteward>::Snapshot);
}
pub trait DataTimelineQueriableWith<Query: StewardData>: DataTimeline {
  type QueryResult: StewardData;
  
  // audit all functions: must be consistent with each other
  // audit: queries must not have side effects (do a separate action for manual dependency tracking)
  fn query (&self, query: Query, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>)->Self::QueryResult;
  // permitted to record the dependency
  fn prediction_query<P: Predictor <Steward = Self::Steward>> (&mut self, query: Query, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>, predictor: PredictorHandle<P>)->(Self::QueryResult, Option <ExtendedTime <<Self::Steward as TimeSteward>::Basics>>);
  // TODO: is this necessary? Or is it only used in invalidation code, which can peek anyway?
  // fn query_range (&self, query: Query, time_range: TimeRange)->impl Iter <Item = (TimeRange, QueryResult)>;
  
  // audit: NEVER changes unless forget_snapshot() is called
  fn snapshot_query (&self, query: Query, snapshot: & <Self::Steward as TimeSteward>::Snapshot)->Self::QueryResult;
}
pub trait DataTimelineModifiableWith<Modification: StewardData>: DataTimeline {
  type DataToUndoModification: StewardData;
  
  // audit both functions: calls invalidate_[thing] for everything whose queries would be changed
  // audit both functions: doesn't change any snapshot_query results for snapshots in the argument
  // audit both functions: doesn't change any query results in the past
  fn modify<E: Event <Steward = Self::Steward>, Accessor: InvalidationAccessor<Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, accessor: &Accessor, modification: Modification, event: EventHandle<E>, snapshots: &Snapshots)->Self::DataToUndoModification;
  // audit: if all modifications from a certain event are undone, all query results immediately before and after that event are identical
  fn undo<E: Event <Steward = Self::Steward>, Accessor: InvalidationAccessor <Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, accessor: &Accessor, modification: Self::DataToUndoModification, event: EventHandle<E>, snapshots: &Snapshots);
}
pub trait Event: StewardData {
  type Steward: TimeSteward;
  fn execute<Accessor: EventAccessor <Steward = Self::Steward, Event = Self>> (&mut self, accessor: &Accessor);
  //audit: undoes every action done by execute(), and leaves self in its original state
  fn undo<Accessor: UndoEventAccessor <Steward = Self::Steward, Event = Self>> (&mut self, accessor: &Accessor);
}
pub trait Predictor: StewardData {
  type Steward: TimeSteward;
  fn execute <Accessor: PredictorAccessor <Steward = Self::Steward>> (&mut self, accessor: &Accessor);
}

pub trait Accessor {
  type Steward: TimeSteward;
  fn global_timeline (&self)->DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline>;
}
// Querying versus peeking:
// Querying accessors are generally for things that can affect the physics. Querying uses an exact interface that can be tracked and audited in various ways to make sure the physics stays consistent.
// Peeking accessors are generally for things that are required to do a specific job and don't have any leeway to change the physics. We allow them full read-only access with no tracking, and merely audit that they did the job they were asked to. Peeking accessors can also be allowed to use the querying interface for convenience (so that they can call generic functions that take a QueryingAccessor), but this currently is only possible for snapshots.
pub trait QueryingAccessor: Accessor {
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query>> (&self, handle: & DataTimelineHandle <T>, query: Query)-> T::QueryResult;
}
pub trait PeekingAccessor: Accessor {
  fn peek <T: DataTimeline> (&self, handle: & DataTimelineHandle <T>)->& T;
}
pub trait MomentaryAccessor: Accessor {
  fn now(&self) -> &<<Self::Steward as TimeSteward>::Basics as Basics>::Time;
}
pub trait EventSpecificAccessor: MomentaryAccessor {
  type Event: Event <Steward = Self::Steward>;
  fn event (&self)->& EventHandle <Self::Event>;
}
pub trait EventAccessor: QueryingAccessor + EventSpecificAccessor {
  fn modify <Modification: StewardData, T: DataTimelineModifiableWith<Modification>> (&self, handle: & DataTimelineHandle <T>, modification: Modification)-> T::DataToUndoModification;
}
pub trait UndoEventAccessor: PeekingAccessor + EventSpecificAccessor {
  // no querying because query results wouldn't necessarily correspond to those observed by the original execution in any way
  fn undo <Modification: StewardData, T: DataTimelineModifiableWith<Modification>> (&self, handle: & DataTimelineHandle <T>, data: T::DataToUndoModification);
}
pub trait PredictorAccessor: QueryingAccessor {
  // this one may record dependencies when querying
  // and intentionally doesn't expose the time
  fn predict_at_time <E: Event <Steward = Self::Steward>> (&self, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>, event: E);
  fn predict_immediately <E: Event <Steward = Self::Steward>> (&self, event: E);
}
pub trait InvalidationAccessor: PeekingAccessor {
  // no querying because there may be multiple relevant times
  // audit: can't invalidate things in the past relative to the modification
  fn peek <T: DataTimeline> (&self, handle: & DataTimelineHandle <T>)->& T;
  fn invalidate_predictions <T: Predictor> (&self, handle: & PredictorHandle <T>, time_range: TimeRange<&ExtendedTime <<Self::Steward as TimeSteward>::Basics>>);
  fn invalidate_event <T: Event> (&self, handle: & EventHandle <T>) ;
}

pub trait Snapshot: QueryingAccessor + PeekingAccessor + MomentaryAccessor {
  /// note: Snapshot::serialize() matches TimeSteward::deserialize()
  fn serialize_into <W: Write> (&self, writer: W);
  
  // for DataTimelines to request notification so they can drop data related to the snapshot
  fn notify_on_drop<T: DataTimeline> (&self, timeline: & DataTimelineHandle <T>);
}

impl <T: EventSpecificAccessor> MomentaryAccessor for T {
  fn now(&self) -> &<<Self::Steward as TimeSteward>::Basics as Basics>::Time {
    &self.event().time().base
  }
}

pub trait SnapshotTree {
  //fn iterate_snapshots (&self, time_range: TimeRange, skip: ?????)->impl Iter<Item = (&Time, &Snapshot)>;
}

pub trait EventHandleTrait {
  type Steward: TimeSteward;
  fn time (&self)->& ExtendedTime <<Self::Steward as TimeSteward>::Basics>;
}

pub trait SerializationContext {
  fn serialize_data <T: StewardData> (&mut self, data: T);
  fn serialize_timeline_handle <T: DataTimeline> (&mut self, timeline: DataTimelineHandle <T>);
  fn serialize_predictor_handle <T: Predictor> (&mut self, predictor: PredictorHandle <T>);
}
pub trait DeserializationContext {
  fn deserialize_data <T: StewardData> (&mut self)->T;
  fn deserialize_timeline_handle <T: DataTimeline> (&mut self)->DataTimelineHandle <T>;
  fn deserialize_predictor_handle <T: Predictor> (&mut self)->PredictorHandle <T>;
}

pub trait TimeSteward {
  type Basics: Basics;
  type Snapshot: Snapshot;
  
  fn from_global_timeline (timeline: <Self::Basics as Basics>::GlobalTimeline)->Self;
  /// note: Snapshot::serialize() matches TimeSteward::deserialize()
  fn deserialize_from <R: Read> (data: &mut R)->Self;
  
  fn insert_fiat_event<E: Event>(&mut self, time: <Self::Basics as Basics>::Time, id: DeterministicRandomId, event: E)
                                               -> Result<(), FiatEventOperationError>;
  fn remove_fiat_event(&mut self, time: &<Self::Basics as Basics>::Time, id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError>;
  fn snapshot_before (&mut self, time: &<Self::Basics as Basics>::Time)->Option <Self::Snapshot>;
  
  fn valid_since(&self) -> ValidSince<<Self::Basics as Basics>::Time>;
  fn forget_before (&mut self, time: &<Self::Basics as Basics>::Time);
}

pub trait Basics {
  type Time: StewardData + Ord + Hash;
  type GlobalTimeline: DataTimeline;
  const MAX_ITERATION: IterationType = 65535;
}

pub type IterationType = u32;
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ExtendedTime<B: Basics> {
  pub base: B::Time,
  pub iteration: IterationType,
  pub id: DeterministicRandomId,
}

#[derive (Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

pub struct TimeRange <Time> {
  pub start: Time,
  pub end: Option <Time>,
}


use std::cmp::Ordering;

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
impl<B: fmt::Display> fmt::Display for ValidSince<B> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &ValidSince::TheBeginning => write!(f, "TheBeginning"),
      &ValidSince::Before(ref something) => write!(f, "Before({})", something),
      &ValidSince::After(ref something) => write!(f, "After({})", something),
    }
  }
}

impl<T: Ord> Ord for ValidSince<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&ValidSince::TheBeginning, &ValidSince::TheBeginning) => Ordering::Equal,
      (&ValidSince::TheBeginning, _) => Ordering::Less,
      (_, &ValidSince::TheBeginning) => Ordering::Greater,
      (&ValidSince::Before(ref something), &ValidSince::Before(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::After(ref something), &ValidSince::After(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::Before(ref something), &ValidSince::After(ref anything)) => {
        if something <= anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      (&ValidSince::After(ref something), &ValidSince::Before(ref anything)) => {
        if something < anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T> PartialEq<T> for ValidSince<T> {
  fn eq(&self, _: &T) -> bool {
    false
  }
}

impl<T: Ord> PartialOrd<T> for ValidSince<T> {
  fn partial_cmp(&self, other: &T) -> Option<Ordering> {
    Some(match self {
      &ValidSince::TheBeginning => Ordering::Less,
      &ValidSince::Before(ref something) => {
        if something <= other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      &ValidSince::After(ref something) => {
        if something < other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    })
  }
}
impl<T: Ord> PartialOrd for ValidSince<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
// impl <T: Ord> PartialOrd <ValidSince <T>> for T {
//  fn partial_cmp (&self, other: & ValidSince <T>)->Option <Ordering> {
//    Some (other.partial_cmp (self).unwrap().reverse());
//  }
// }


/*
/////////////////
// old brainstorms below this line
/////////////////

trait TimeStewardModificationReverter {
  fn revert (event: ?????)->invalidated stuff etc., like the return value of the original modify functions;
}

/**

Loosely speaking, these must be retroactive data structures.

Specifically, I'll call them **stack-retroactive data structures**. They are like fully retroactive data structures, except that operations in the past are also permitted to delete arbitrary subsets of the existing operations that are later in time. This leniency is because:
* Data structures can be more efficient if they don't have to support insertion or deletion in the middle, and
* In many cases, the later operations would almost certainly be invalidated anyway if the earlier one was invalidated.

They must obey these conditions:
* The result of a query from a non-snapshot accessor must depend only on the initial snapshot and currently existing operations, and not, for instance, on the order the operations were added. It MAY depend on the order of operations among operations at the exact same ExtendedTime. Query results must also be invariant under taking snapshots at earlier historical times and restoring from them.
* Also, the existing predictors are implicit in the current state. They may not change over serializing and deserializing through a snapshot.
* The results of a query/snapshot from a *snapshot accessor* must depend only on the initial snapshot and operations that existed *at the time the snapshot accessor was created*. (In order to maintain this condition, the structure may have to examine the supplied snapshots structure during insert_operation().) This condition may be violated if forget_snapshot() has been called for that snapshot.
* If a change to the operations would change the result of an earlier query (earlier in program time, >= in historical time), and that earlier query submitted a InvalidationHandle, that update operation must invalidate it by including an equal InvalidationHandle in its return value. (False-positives are permitted, though undesirable). It need only return each InvalidationHandle once. Note that it does NOT have to retain copies of every handle submitted, if it has some way of looking them up, for example by querying other DataTimelines.
* Any method called at a historical time > self.valid_since() must return Ok. self.valid_since() must be TheBeginning when a with_steward() is created, and After (time) when a from_snapshot() is created. It must not increase except from calls to discard_before or insert_operation, and if it implements FullDataTimeline, it must not increase from calls to insert_operation either.


They also *should* obey these conditions:
* whenever the structure stores obsolete information to maintain a snapshot, it should notify the snapshot using ????? so that the snapshot may free up the memory using forget_snapshot() when it is dropped.

*/
struct QueryResult <Timeline: DataTimeline> {
  data: Timeline::QueryResultData;
  valid_until: something < ExtendedTime <Whatever::Steward::Basics>>;
}
struct OperationResult <Timeline: DataTimeline> {
  invalidated_queries: Vec<InvalidationHandle>;
  deleted_future_operations: Vec<(EventHandle, Operation)>; ?????
  created_predictors: Vec<PredictorHandle>;
  deleted_predictors: Vec<PredictorHandle>;
}
struct FromSnapshotResult <Timeline: DataTimeline> {
  created_predictors: Vec<PredictorHandle>;
}
// DataTimeline handles serialize to DataTimelineId
//but the id probably won't be stored in the trait implementor
 // fn id (&self)->DataTimelineId;
trait DataTimeline: Any + UniquelyIdentifiedType {
  
  
  type Steward: TimeSteward;
  type Constants: StewardData;
  
  // All inputs are required to have a standard format, so that the structure can be audited.
  type Query: StewardData;
  type QueryResultData: StewardData;
  type Operation: StewardData;
  type Snapshot: StewardData;
  
  fn from_constants (steward: StewardHandle <Self::Steward>, constants: Self::Constants)->Self;
  fn from_snapshot (steward: StewardHandle <Self::Steward>, snapshot: &Self::Snapshot)->Self;
  
  
  fn constants (&self)->Result <QueryResult<Self>,>;
  fn query <A: Accessor> (&self, query: Self::Query, accessor: &A)->Result <QueryResult<Self>,>;
  
  // note: I'm not sure this needs an error mode, because having a snapshot prior to the valid_since of a data timeline might be a caller error in a TimeSteward
  fn snapshot (snapshot: &Self::Steward::Snapshot)->Snapshot;
  
  /// requires a &Steward only so that you can't do it in a predictor or event function.
  //Maybe this should also return a range of times changed so you know what snapshots need to be taken?
  fn insert_operation <Snapshots: SnapshotTree> (&mut self, time: ExtendedTime <Self::Steward::Basics>, operation: Self::Operation, snapshots_to_maintain: & Snapshots, & Steward)->Result <,>;
  
  fn valid_since (&self, & Steward)->ValidSince <ExtendedTime <Self::Steward::Basics>>;
  
  
  fn forget_before (&mut self, time: ExtendedTime <Self::Steward::Basics>, & Steward);
  fn forget_snapshot (&mut self, StewardRc <Steward::Snapshot>);

  // query functions, which take an accessor and possibly a prediction handle, which may be stored in self for future invalidation
  // modify functions, which take a mutator and
  // - possibly return invalidated prediction handles
  // – possibly return spawned predictions, as from creating a new entity
  // - assuming this is for a full TimeSteward, return a StewardRc <TimeStewardModificationReverter> (which, for the simple case, may be a Self). This makes each potential reversion take at least a fat-pointer to store, unless we can make StewardRc be a thin pointer with the vtable at the destination.
  
  //An operation at time T may create new predictors from T to other times, destroy predictors from T to other times, or invalidate predictions that include After (T).
}



struct DataTimelineHandle <GlobalLists: GlobalLists, Timeline: DataTimeline> {
  data: DynamicArc <GlobalLists, StewardsTimesDataTimelines, DataTimelineId>,
}
impl<GlobalLists: GlobalLists, Timeline: DataTimeline> DataTimeline for DataTimelineHandle <GlobalLists, Timeline> {
  // forward all??
  fn query <A: Accessor> (&self, query: Self::Query, accessor: &A)->Result <QueryResult<Self>,> {
    self.data.differentiated<Times<A::Steward, Timeline>>().query(query, accessor);
  }
}

trait Mutator : ??? + Rng {
  type Steward: Steward;
  fn create <T: DataTimeline> (&mut self, constants: T::Constants)->DataTimelineHandle <T, Self::Steward>;
  // not needed because they can invoke on the DataTimelineHandle directly, passing in this accessor
  //fn query <T: DataTimeline> (&self, t: DataTimelineHandle<T, Self::Steward>, query: T::Query)->Result <QueryResult<Self>,>{t.data.get (...).downcast_ref().expect().query(), }
  ;
}

trait TimeSteward {
  type Basics: Basics;
  type Mutator: Mutator <Steward = Self>;
  type DataTimelineHandle: DataTimelineHandleData <Steward = Self>;
}

trait TimeSteward {
  fn initialized_with <I: FnOnce (&mut Mutator)->Self::Basics::GlobalTimeline> (time: Self::Basics::Time, initializer: I)->Self;
}



impl Event for Struct {
  ...
  fn call <M: Mutator <...>> (& self, mutator: M) {
    let timeline: DataTimelineHandle <GlobalLists, MyType> = self.timeline;
    let value: &MyType = timeline.query (mutator, MyValue);
    ...
    let new_timeline = mutator.create <MyType>(constants);
  }
}

*/





