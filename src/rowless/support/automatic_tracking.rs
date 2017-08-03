use ::DeterministicRandomId;
use std::hash::Hash;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::io::{Read, Write};
use std::any::Any;
use std::fmt::{self, Debug};
use super::api::*;

struct RecordDependency {}
struct GetValue {}
struct SimpleTimeline <Data: StewardData, Steward: TimeSteward> {
  changes: Vec<(DynamicEventHandle, Option <Data>)>,
  other_dependent_events: RefCell<Vec<DynamicEventHandle>>,
  snapshots_data: (),
}

trait DynamicInvalidate {
  fn invalidate_with (&self, Steward::InvalidationAccessor)
}

impl <Steward: TimeSteward, E: Event <Steward = Steward>> DynamicInvalidate for EventHandle <E>

impl <Data: StewardData, Steward: TimeSteward> DataTimeline for SimpleTimeline <Data, Steward> {
  fn invalidate_after <Accessor: InvalidationAccessor> (&self, time: ExtendedTime <Steward::Basics>, accessor: & Accessor) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    while let Some (event) = dependencies.pop() {
      if event.time() <= time {
        dependencies.push (event);
        return
      }
      event.invalidate_with (accessor);
    }
  }
}

impl <Data: StewardData, Steward: TimeSteward> DataTimeline for SimpleTimeline <Data, Steward> {
  type Steward: TimeSteward;
  
  fn serialize <Context: SerializationContext> (&self, serializer: &mut Context, snapshot: & <Self::Steward as TimeSteward>::Snapshot) {
    unimplemented!()
  )
  fn deserialize <Context: DeserializationContext> (deserializer: &mut Context, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>)->Self {
    unimplemented!()
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>) {
    
  }
  fn forget_snapshot (&mut self, snapshot: & <Self::Steward as TimeSteward>::Snapshot) {
    unimplemented!()//snapshots_data.remove (snapshot);
  }
}
impl <Data: StewardData, Steward: TimeSteward> DataTimelineQueriableWith<GetValue> for SimpleTimeline <Data, Steward> {
  type QueryResult: Option <(ExtendedTime <Steward::Basics>, Option <Data>)>;

  fn query (&self, query: &Query, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.changes.binary_search_by_key (time, | change | &change.0) {
      Ok(index) => match offset {After => index, Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    }
    self.changes.get (index).map (| (event, data) | (event.time(), data))
  }
  
  fn snapshot_query (&self, query: &Query, snapshot: & <Self::Steward as TimeSteward>::Snapshot)->Self::QueryResult {
    unimplemented!()
    //self.query (query, snapshot)
  }
}
impl <Data: StewardData, Steward: TimeSteward> DataTimelineModifiableWith<Option <Data>> for SimpleTimeline <Data, Steward> {
  type DataToUndoModification: ();
  
  fn modify<E: Event <Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, modification: Modification, event: &EventHandle<E>, snapshots: &Snapshots)->Self::DataToUndoModification {
    ...
    self.changes.push ((event, modification));
  }
  
  fn undo<E: Event <Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, modification: &Self::DataToUndoModification, event: &EventHandle<E>, snapshots: &Snapshots)
  {
    ...
  }
}
impl <Data: StewardData, Steward: TimeSteward> DataTimelineModifiableWith<RecordDependency> for SimpleTimeline <Data, Steward> {
  type DataToUndoModification: ();
  
  fn modify<E: Event <Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, modification: Modification, event: &EventHandle<E>, snapshots: &Snapshots)->Self::DataToUndoModification {
    self.other_dependent_events...
  }
  
  fn undo<E: Event <Steward = Self::Steward>, Snapshots: SnapshotTree> (&mut self, modification: &Self::DataToUndoModification, event: &EventHandle<E>, snapshots: &Snapshots)
  {
    self.other_dependent_events...
  }
}


fn query_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: ExecuteEventAccessor <Steward = Steward> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, Option <Data>)> {
  accessor.modify (handle, RecordDependency);
  accessor.query (handle, GetValue, offset)
}
fn modify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: ExecuteEventAccessor <Steward = Steward> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>, modification: Option <Data>) {
  accessor.invalidate (| invalidator | {
  
  });
  accessor.modify (handle, modification);
}
fn unmodify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: ExecuteEventAccessor <Steward = Steward> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>, modification: Option <Data>) {
  accessor.invalidate (| invalidator | {
  
  });
  accessor.undo (handle, ());
}


pub trait Event: StewardData {
  type Steward: TimeSteward;
  // audit both functions: calls invalidate_event for everything whose queries would be changed
  // audit both functions: doesn't change any snapshot_query results for snapshots in the argument
  // audit both functions: doesn't change any query results in the past
  fn execute<Accessor: EventAccessor <Steward = Self::Steward, Event = Self>> (&mut self, accessor: &Accessor);
  // audit: undoes exactly every action done by execute() and nothing else, and leaves self in its original state
  // audit: after undoing, all query results immediately before and after the event are identical to each other (if the previous audit passes, this is more an audit of the DataTimeline types than this event type)
  fn undo<Accessor: UndoEventAccessor <Steward = Self::Steward, Event = Self>> (&mut self, accessor: &Accessor);
}

pub trait Accessor {
  type Steward: TimeSteward;
  fn global_timeline (&self)->DataTimelineHandle <<<Self::Steward as TimeSteward>::Basics as Basics>::GlobalTimeline>;
}
// Querying versus peeking:
// Querying accessors are generally for things that can affect the physics. Querying uses an exact interface that can be tracked and audited in various ways to make sure the physics stays consistent.
// Peeking accessors are generally for things that are required to do a specific job and don't have any leeway to change the physics. We allow them full read-only access with no tracking, and merely audit that they did the job they were asked to. Peeking accessors can also be allowed to use the querying interface for convenience (so that they can call generic functions that take a QueryingAccessor), but this currently is only possible for snapshots.
pub trait QueryingAccessor: Accessor {
  fn query <Query: StewardData, T: DataTimelineQueriableWith<Query>> (&self, handle: & DataTimelineHandle <T>, query: &Query)-> T::QueryResult;
}
pub trait PeekingAccessor: Accessor {
  fn peek <T: DataTimeline> (&self, handle: & DataTimelineHandle <T>)->& T;
}
pub trait MomentaryAccessor: Accessor {
  fn now(&self) -> &<<Self::Steward as TimeSteward>::Basics as Basics>::Time;
}
pub trait EventAccessor: MomentaryAccessor {
  type Event: Event <Steward = Self::Steward>;
  fn event (&self)->& EventHandle <Self::Event>;
  // invalidation is done within a Fn so that the event can't extract any information from the PeekingAccessor used for invalidation.
  fn invalidate <A: InvalidationAccessor, F: Fn(&A)> (&self, invalidator: &F);
}
pub trait ExecuteEventAccessor: QueryingAccessor + EventAccessor {
  fn modify <Modification: StewardData, T: DataTimelineModifiableWith<Modification>> (&self, handle: & DataTimelineHandle <T>, modification: Modification)-> T::DataToUndoModification;
  fn create_prediction <E: Event <Steward = Self::Steward>> (&self, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>, event: E)->PredictionHandle<E>;
  fn destroy_prediction <E: Event <Steward = Self::Steward>> (&self, &PredictionHandle<E>);
}
pub trait UndoEventAccessor: PeekingAccessor + EventAccessor {
  // no querying because query results wouldn't necessarily correspond to those observed by the original execution in any way
  fn undo <Modification: StewardData, T: DataTimelineModifiableWith<Modification>> (&self, handle: & DataTimelineHandle <T>, data: &T::DataToUndoModification);
  fn uncreate_prediction <E: Event <Steward = Self::Steward>> (&self, &PredictionHandle<E>);
  fn undestroy_prediction <E: Event <Steward = Self::Steward>> (&self, &PredictionHandle<E>);
}
pub trait InvalidationAccessor: PeekingAccessor {
  // no querying because there may be multiple relevant times and this might be in an undo (see above)
  fn peek <T: DataTimeline> (&self, handle: & DataTimelineHandle <T>)->& T;
  // audit: can't invalidate things in the past relative to the current event
  fn invalidate <T: Event> (&self, handle: & EventHandle <T>);
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
  fn serialize_prediction_handle <T: Event> (&mut self, prediction: PredictionHandle <T>);
  // event handles can't be serialized because they're momentary and serialization never takes place at one of those moments.
}
pub trait DeserializationContext {
  fn deserialize_data <T: StewardData> (&mut self)->T;
  fn deserialize_timeline_handle <T: DataTimeline> (&mut self)->DataTimelineHandle <T>;
  fn deserialize_prediction_handle <T: Event> (&mut self)->PredictionHandle <T>;
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





