use serde::Serialize;
//use std::any::{Any, TypeId};
use std::borrow::Borrow;
//use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet /*, HashMap*/};
use std::fmt::Debug;

//use crate::api::*;
use crate::EntityId;

pub fn split_off_greater<K: Ord + Borrow<Q> + Clone, V, Q: Ord + ?Sized>(
  input: &mut BTreeMap<K, V>,
  split: &Q,
) -> BTreeMap<K, V> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  let mut transfer = None;
  if let Some(whoops) = result.iter().next() {
    if whoops.0.borrow() == split {
      transfer = Some(whoops.0.clone());
    }
  }
  if let Some(key) = transfer {
    input.insert(key, result.remove(split).unwrap());
  }
  result
}

pub fn split_off_greater_set<K: Ord + Borrow<Q>, Q: Ord + ?Sized>(
  input: &mut BTreeSet<K>,
  split: &Q,
) -> BTreeSet<K> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  if let Some(whoops) = result.take(split) {
    input.insert(whoops);
  }
  #[cfg(debug_assertions)]
  {
    if result.take(split).is_some() {
      panic!("Some code broke the Ord/Borrow rules for BTreeSet calls")
    }
  }
  result
}

#[doc(hidden)]
#[macro_export]
macro_rules! delegate {
  (Ord, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> Ord for $($concrete)* {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        let my_target = { let $this = self; $target };
        let other_target = { let $this = other; $target };
        my_target.cmp(other_target)
      }
    }
  };
  (PartialOrd, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> PartialOrd for $($concrete)* {
      fn partial_cmp(&self, other: &Self) ->Option <::std::cmp::Ordering> {
        let my_target = { let $this = self; $target };
        let other_target = { let $this = other; $target };
        my_target.partial_cmp(other_target)
      }
    }
  };
  (Eq, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> Eq for $($concrete)* {}
  };
  (PartialEq, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> PartialEq for $($concrete)* {
      fn eq(&self, other: &Self) -> bool {
        let my_target = { let $this = self; $target };
        let other_target = { let $this = other; $target };
        my_target.eq(other_target)
      }
    }
  };
  (Hash, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> ::std::hash::Hash for $($concrete)* {
      fn hash <H: ::std::hash::Hasher> (&self, state: &mut H) {
        let my_target = { let $this = self; $target };
        my_target.hash (state);
      }
    }
  };
  (Serialize, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> ::serde::ser::Serialize for $($concrete)* {
      fn serialize<Ser: ::serde::ser::Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
        let my_target = { let $this = self; $target };
        my_target.serialize(serializer)
      }
    }
  };
  ([$($bounds:tt)*] [$Trait1: tt, $($Traits:tt),*$(,)*] for [$($concrete:tt)*] to [$this: ident => $target: expr]) => {
    delegate! ($Trait1, $this => $target, [$($bounds)*], [$($concrete)*]);
    delegate! ([$($bounds)*] [$($Traits,)*] for [$($concrete)*] to [$this => $target]);
  };
  ([$($bounds:tt)*] [] for [$($concrete:tt)*] to [$this: ident => $target: expr]) => {};
}

/*
impl<
    PublicImmutableData: SimulationStateData + Default,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait + Default,
  > Default for DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn default() -> Self {
    struct InProgress;
    thread_local! {
      static DEFAULTS: RefCell<HashMap <TypeId, Box <dyn Any>>> = RefCell::new (HashMap::new());
    }
    DEFAULTS.with(|defaults| {
      let id = TypeId::of::<Self>();
      if let Some(existing) = defaults.borrow().get(&id) {
        if existing.is::<InProgress>() {
          panic!(
            "Infinite recursion in Default impl of TimeSteward simulation data type {}",
            std::any::type_name::<Self>()
          )
        } else if let Some(concrete) = existing.downcast_ref::<Self>() {
          concrete.clone()
        } else {
          unreachable!("Wrong type stored in DataHandle defaults map")
        }
      } else {
        defaults.borrow_mut().insert(id, Box::new(InProgress));
        let handle = Self::new_nonreplicable(
          PublicImmutableData::default(),
          PrivateTimeStewardData::default(),
        );
        defaults.borrow_mut().insert(id, Box::new(handle.clone()));
        handle
      }
    })
  }
}
 */

/*pub trait DeserializationContext {
fn deserialize_data <T: DeserializeOwned> (&mut self)->T;
fn deserialize_timeline_handle <T: Entity> (&mut self)->EntityHandle <T>;
fn deserialize_prediction_handle <T: Event> (&mut self)->PredictionHandle <T>;
fn deserialize_event_handle <T: Event> (&mut self)->EventHandle <T>;
fn deserialize_dynamic_event_handle (&mut self)->DynamicEventHandle;
}*/

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
  pub fn next<Time: Serialize>(&mut self, waker_id: &EntityId, time: &Time) -> EntityId {
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
  pub fn next(&mut self) -> EntityId {
    self.previous = EntityId::from_raw([
      self.previous.data()[0],
      self.previous.data()[1].wrapping_add(1),
    ]);
    self.previous
  }
}
