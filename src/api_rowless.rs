



trait TimeStewardModificationReverter {
  fn revert (event: ?????)->invalidated stuff etc., like the return value of the original modify functions;
}
trait TimeStewardDataThingy {
  // query functions, which take an accessor and possibly a prediction handle, which may be stored in self for future invalidation
  // modify functions, which take a mutator and
  // - possibly return invalidated prediction handles
  // – possibly return spawned predictions, as from creating a new entity
  // - assuming this is for a full TimeSteward, return a StewardRc <TimeStewardModificationReverter> (which, for the simple case, may be a Self). This makes each potential reversion take at least a fat-pointer to store, unless we can make StewardRc be a thin pointer with the vtable at the destination.
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







