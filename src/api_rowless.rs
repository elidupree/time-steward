/// In addition to implementing these traits, a StewardData must have Eq be equivalent to the quality of its serialization, and invariant under cloning and serialize-deserialize cycles.
trait StewardData: Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug {

}

trait TimeStewardModificationReverter {
  fn revert (event: ?????)->invalidated stuff etc., like the return value of the original modify functions;
}

/*

Loosely speaking, these must be retroactive data structures. Specifically, I'll call them **stack-retroactive data structures**. They are like fully retroactive data structures, except that operations in the past are also permitted to delete arbitrary subsets of the existing operations that are later in time.

They must obey these conditions:
* The result of a query from a non-snapshot accessor must depend only on the initial snapshot and currently existing operations, and not, for instance, on the order the operations were added. It must also be invariant under taking snapshots at earlier historical times and restoring from them.
* The results of a query/snapshot from a *snapshot accessor* must depend only on the initial snapshot and operations that existed *at the time the snapshot accessor was created*. (In order to maintain this condition, the structure may have to examine its StewardHandle.snapshots() during insert_operation().) This condition may be violated if forget_snapshot() has been called for that snapshot.
* If a change to the operations would change the result of an earlier query (earlier in program time, >= in historical time), and that earlier query submitted a InvalidationHandle, that update operation must invalidate it by including the InvalidationHandle in its return value. (False-positives are permitted, though undesirable). It need only return each InvalidationHandle once.
* Any method called at a historical time > self.valid_since() must return Ok. self.valid_since() must be TheBeginning when a with_steward() is created, and After (time) when a from_snapshot() is created. It must not increase except from calls to discard_before or insert_operation, and if it implements FullTimeStewardDataThingy, it must not increase from calls to insert_operation either.

They also *should* obey these conditions:
* whenever the structure stores obsolete information to maintain a snapshot, it should notify the snapshot using snapshot.????? so that the snapshot may free up the memory using forget_snapshot() when it is dropped.

*/
struct QueryResult <Whatever: TimeStewardDataThingy> {
  data: Whatever::QueryResultData;
  valid_until: something < ExtendedTime <Whatever::Steward::Basics>>;
}
trait TimeStewardDataThingy {
  
  
  type Steward: TimeSteward;
  
  // All inputs are required to have a standard format, so that the structure can be audited.
  type Query: StewardData;
  type QueryResultData: StewardData;
  type Operation: StewardData;
  type Snapshot: StewardData;
  
  fn with_steward (steward: StewardHandle <Self::Steward>)->Self;
  fn from_snapshot (steward: StewardHandle <Self::Steward>, snapshot: &Self::Snapshot)->Self;
  
  
  fn query <A: Accessor> (&self, query: Self::Query, accessor: A)->Result <QueryResult<Self>,>;
  
  fn snapshot (snapshot: Self::Steward::Snapshot)->Snapshot;
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





trait EntityHistory {
  type Steward: Steward;
  fn get (m: & Steward::Mutator) {
}
struct TypedEntityHistory<E:Entity, S: Steward> {
  data: Steward::EntityHistory;
}
impl TypedEntityHistory {
  fn get (m: & S::Mutator) {self.data.get (m).downcast_ref().expect()}
}


trait Entity {
  type Varying = Self;
  type Constants = ();
}

trait Mutator : ??? + Rng {
  type Steward: Steward;
  fn create <E: Entity> (&mut self, constants: E::Constants)->TypedEntityHistory <E, B::S>;
  fn get <E: Entity> (&self, t: TypedEntityHistory <E, B::S>)->E::Varying {t.data.get (...).downcast_ref().expect()}
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







