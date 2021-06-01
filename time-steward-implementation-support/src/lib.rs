use serde::Serialize;
use time_steward_api::EntityId;

pub mod accessor_cell;

#[derive(Debug)]
pub struct EventChildrenIdGenerator {
  next: Option<EntityId>,
}

impl Default for EventChildrenIdGenerator {
  fn default() -> Self {
    Self::new()
  }
}
impl EventChildrenIdGenerator {
  pub fn new() -> EventChildrenIdGenerator {
    EventChildrenIdGenerator { next: None }
  }
  pub fn generate_id<Time: Serialize>(&mut self, waker_id: &EntityId, time: &Time) -> EntityId {
    let result = match self.next {
      None => EntityId::hash_of(&(waker_id, time)),
      Some(next) => next,
    };
    self.next = Some(EntityId::from_raw([
      result.data()[0],
      result.data()[1].wrapping_add(1),
    ]));
    result
  }
}
#[derive(Debug)]
pub struct GlobalsConstructionIdGenerator {
  previous: EntityId,
}

impl Default for GlobalsConstructionIdGenerator {
  fn default() -> Self {
    Self::new()
  }
}
impl GlobalsConstructionIdGenerator {
  pub fn new() -> GlobalsConstructionIdGenerator {
    GlobalsConstructionIdGenerator {
      previous: EntityId::from_raw([0xbad_c0de, 0xbad_c0de]),
    }
  }
  pub fn generate_id(&mut self) -> EntityId {
    self.previous = EntityId::from_raw([
      self.previous.data()[0],
      self.previous.data()[1].wrapping_add(1),
    ]);
    self.previous
  }
}
