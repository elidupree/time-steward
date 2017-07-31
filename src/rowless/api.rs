/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// TimeSteward has strong requirements for serializability. In addition to implementing these traits, a StewardData must have Eq be equivalent to equality of its serialization, and all its behavior must be invariant under cloning and serialize-deserialize cycles.
pub trait StewardData: Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug {}

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


trait SnapshotTree {
  type Steward: TimeSteward;
  fn iterate_snapshots (&self, time_range, since_snapshot_number)->impl Iter<Item = (& Self::Steward::Basics::Time, &StewardRc <Self::Steward::Snapshot>)>;
}


// Model: events interact only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time, and may use query_range on other timelines to collect events/predictions that must be invalidated. (Although for instance, modifications made for dependency tracking purposes don't change any results of "real" queries (i.e. those by events and predictors), so they never need to invalidate anything themselves.)
// Predictors cannot modify and can only query??? (The data necessary to invalidate them has to be placed by the events that created them?)
//To audit, we record all of the queries, query results, and modifications. Then after each modification of a DataTimeline, we rerun all queries to that timeline made by still-valid predictions/events. If any query has a different result than before, it's an error. (I guess in order to rerun modifications, we have to feed them the old results of their queries instead of having them do real queries?)
// Also audit: a modification can never change results of queries in the past

trait DataTimelineQueryInterface: Any + {
  type Query: StewardData;
  type QueryResult: StewardData;
  
  // audit: these must be consistent
  // audit: queries must not have side effects (do a separate action for manual dependency tracking)
  fn query (query: Query, time: Time)->QueryResult;
  fn query_range (query: Query, time_range: TimeRange)->impl Iter <Item = (TimeRange, QueryResult)>;
  
}
trait DataTimelineModifyInterface: Any + {
  type Modification: StewardData;
  
  fn modify (modification: Modification, time: Time);
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







