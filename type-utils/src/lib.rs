#![feature(never_type, specialization, generic_associated_types)]
#![allow(incomplete_features)]

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

mod lens;
pub mod list_of_types;
pub mod visit_serialized;

pub use self::lens::*;

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// We used to require `Send + Sync` for SimulationStateData, but now that EntityHandles can be part of SimulationStateData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
///
/// Requiring DeserializeOwned is improper because you can't deserialize EntityHandles without more
///  information; the current approach is a hack where Deserialize
/// uses a thread-local context for that; it may later be replaced with a proper custom derive of my own.
pub trait SimulationStateData: 'static + Clone + Eq + Serialize + DeserializeOwned + Debug {}
impl<T: 'static + Clone + Eq + Serialize + DeserializeOwned + Debug> SimulationStateData for T {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug)]
pub struct PersistentTypeId(pub u64);
pub trait PersistentlyIdentifiedType {
  const ID: PersistentTypeId;
}
pub trait DynamicPersistentlyIdentifiedType {
  fn persistent_type_id(&self) -> PersistentTypeId;
}
impl<T: PersistentlyIdentifiedType> DynamicPersistentlyIdentifiedType for T {
  fn persistent_type_id(&self) -> PersistentTypeId {
    <T as PersistentlyIdentifiedType>::ID
  }
}

impl PersistentlyIdentifiedType for () {
  const ID: PersistentTypeId = PersistentTypeId(0x8087a0d59fb441b6);
}

trait TryIdentity<T> {
  fn try_identity(self) -> Option<T>;
}
impl<T> TryIdentity<T> for T {
  #[inline(always)]
  fn try_identity(self) -> Option<T> {
    Some(self)
  }
}
impl<T, U> TryIdentity<T> for U {
  #[inline(always)]
  default fn try_identity(self) -> Option<T> {
    None
  }
}

/// Convert between two types if they are actually the same type.
///
///
#[deprecated]
#[inline(always)]
pub fn static_downcast<T, U>(input: T) -> Option<U> {
  TryIdentity::<U>::try_identity(input)
}
#[inline(always)]
pub fn try_identity<T, U>(input: T) -> Option<U> {
  TryIdentity::<U>::try_identity(input)
}

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
      fn hash <__DELEGATE_HASHER: ::std::hash::Hasher> (&self, state: &mut __DELEGATE_HASHER) {
        let my_target = { let $this = self; $target };
        my_target.hash (state);
      }
    }
  };
  (Serialize, $this: ident => $target: expr, [$($bounds:tt)*], [$($concrete:tt)*]) => {
    impl<$($bounds)*> ::serde::ser::Serialize for $($concrete)* {
      fn serialize<__DELEGATE_SERIALIZER: ::serde::ser::Serializer>(&self, serializer: __DELEGATE_SERIALIZER) -> Result<__DELEGATE_SERIALIZER::Ok, __DELEGATE_SERIALIZER::Error> {
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_try_identity() {
    assert_eq!(try_identity::<i32, i32>(5i32), Some(5i32));
    assert_eq!(try_identity::<i32, i16>(5i32), None);
  }
}
