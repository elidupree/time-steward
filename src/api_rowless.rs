/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// TimeSteward has strong requirements for serializability. In addition to implementing these traits, a StewardData must have Eq be equivalent to equality of its serialization, and invariant under cloning and serialize-deserialize cycles.
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
* The result of a query from a non-snapshot accessor must depend only on the initial snapshot and currently existing operations, and not, for instance, on the order the operations were added. It must also be invariant under taking snapshots at earlier historical times and restoring from them.
* Also, the existing predictors are implicit in the current state. They may not change over serializing and deserializing through a snapshot.
* The results of a query/snapshot from a *snapshot accessor* must depend only on the initial snapshot and operations that existed *at the time the snapshot accessor was created*. (In order to maintain this condition, the structure may have to examine its StewardHandle.snapshots() during insert_operation().) This condition may be violated if forget_snapshot() has been called for that snapshot.
* If a change to the operations would change the result of an earlier query (earlier in program time, >= in historical time), and that earlier query submitted a InvalidationHandle, that update operation must invalidate it by including an equal InvalidationHandle in its return value. (False-positives are permitted, though undesirable). It need only return each InvalidationHandle once. Note that it does NOT have to retain copies of every handle submitted, if it has some way of looking them up, for example by querying other DataTimelines.
* Any method called at a historical time > self.valid_since() must return Ok. self.valid_since() must be TheBeginning when a with_steward() is created, and After (time) when a from_snapshot() is created. It must not increase except from calls to discard_before or insert_operation, and if it implements FullDataTimeline, it must not increase from calls to insert_operation either.


They also *should* obey these conditions:
* whenever the structure stores obsolete information to maintain a snapshot, it should notify the snapshot using snapshot.????? so that the snapshot may free up the memory using forget_snapshot() when it is dropped.

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
  fn insert_operation (&mut self, time: ExtendedTime <Self::Steward::Basics>, operation: Self::Operation, & Steward)->Result <,>;
  
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

#[derive (Clone)]
struct StewardHandle <Steward: TimeSteward> {

}
impl <Steward: TimeSteward> StewardHandle <Steward> {
  fn snapshots (&self)->& SnapshotTree <Steward>;
}
impl <Steward: TimeSteward> SnapshotTree <Steward> {
  fn iterate_snapshots (time_range, since_snapshot_number)->impl Iter<Item = (&Steward::Basics::Time, &StewardRc <Steward::Snapshot>)>;
}




type DataTimelineHandle = ThinRc<(DataTimelineId, DataTimeline)>;

struct TypedDataTimelineHandle<T: DataTimeline> {
  data: DataTimelineHandle;
}
impl<T: DataTimeline> TypedDataTimelineHandle<T> {
  fn get (m: & S::Mutator) {self.data.get (m).downcast_ref().expect()}
}


trait Entity {
  type Varying = Self;
  type Constants = ();
}

trait Mutator : ??? + Rng {
  type Steward: Steward;
  fn create <T: DataTimeline> (&mut self, constants: T::Constants)->DataTimelineHandle <E, B::S>;
  fn query <T: DataTimeline> (&self, t: TypedDataTimelineHandle<T>, query: T::Query)->????{t.data.get (...).downcast_ref().expect().query(), }
  ;
}

trait Steward {
  type Mutator: Mutator <Steward = Self>;
  type EntityHistory: EntityHistory <Steward = Self>;
}

impl Event for Struct {
  ...
  fn call <M: Mutator <...>> (& self, mutator: M) {
    let history: TypedEntityHistory <MyType, M::Steward> = self.entity_history;
    let value: &MyType = history.get (mutator);
    
    let new_history = mutator.create <MyType>(constants);
  }
}







