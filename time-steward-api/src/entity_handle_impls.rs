use crate::entity_handles::*;
use crate::EntityId;
use scopeguard::defer;
use serde::{Deserialize, Deserializer};
use std::cell::Cell;
use std::fmt::Debug;
use time_steward_type_utils::delegate;

impl EntityHandle for EntityId {
  fn id(&self) -> EntityId {
    *self
  }
}

delegate! (
  [E: EntityKind, H: EntityHandleKind]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [TypedHandle<E, H>]
  to [this => &this.wrapped_gat().id()]
);
delegate! (
  [H: EntityHandleKind]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [DynHandle<H>]
  to [this => &this.wrapped_gat().id()]
);
delegate! (
  ['a, E: EntityKind, H: EntityHandleKindDeref]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [TypedHandleRef<'a, E, H>]
  to [this => &this.into_wrapped_gat().id()]
);
delegate! (
  ['a, H: EntityHandleKindDeref]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [DynHandleRef<'a, H>]
  to [this => &this.into_wrapped_gat().id()]
);

impl<'de, E: EntityKind, H: EntityHandleKind> Deserialize<'de> for TypedHandle<E, H> {
  fn deserialize<D: Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
    todo!()
  }
}
impl<'de, H: EntityHandleKind> Deserialize<'de> for DynHandle<H> {
  fn deserialize<D: Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
    todo!()
  }
}

impl<E: EntityKind, H: EntityHandleKind> Debug for TypedHandle<E, H> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    thread_local! {
      static RECURSIVE: Cell<bool> = Cell::new(false);
    }
    RECURSIVE.with(|recursive| {
      if recursive.get() {
        write!(f, "TypedHandle({})", self.id())
      } else {
        recursive.set(true);
        defer! { recursive.set(false) }
        write!(f, "TypedHandle({:?})", self.wrapped_gat())
      }
    })
  }
}

impl<H: EntityHandleKind> Debug for DynHandle<H> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "DynHandle({})", self.id())
  }
}

impl<'a, E: EntityKind, H: EntityHandleKindDeref> Debug for TypedHandleRef<'a, E, H> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "TypedHandleRef({:?})", self.into_wrapped_gat())
  }
}

impl<'a, H: EntityHandleKindDeref> Debug for DynHandleRef<'a, H> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "DynHandleRef({})", self.id())
  }
}

macro_rules! cross_handle_impls {
  ([$($bounds:tt)*] [$($concrete1:tt)*] [$($concrete2:tt)*]) => {
    impl<$($bounds)*> PartialOrd<$($concrete2)*> for $($concrete1)* {
      fn partial_cmp(&self, other: &$($concrete2)*) ->Option <::std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
      }
    }
    impl<$($bounds)*> PartialEq<$($concrete2)*> for $($concrete1)* {
      fn eq(&self, other: &$($concrete2)*) -> bool {
        self.id().eq(&other.id())
      }
    }
  };
}

cross_handle_impls!([E: EntityKind, H: EntityHandleKind] [TypedHandle<E, H>] [DynHandle<H>]);
cross_handle_impls!([E: EntityKind, H: EntityHandleKind] [DynHandle<H>] [TypedHandle<E, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [TypedHandleRef<'a, E, H>] [DynHandleRef<'a, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [DynHandleRef<'a, H>] [TypedHandleRef<'a, E, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [TypedHandle<E, H>] [TypedHandleRef<'a, E, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [TypedHandleRef<'a, E, H>] [TypedHandle<E, H>]);
cross_handle_impls!(['a, H: EntityHandleKindDeref] [DynHandle<H>] [DynHandleRef<'a, H>]);
cross_handle_impls!(['a, H: EntityHandleKindDeref] [DynHandleRef<'a, H>] [DynHandle<H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [TypedHandle<E, H>] [DynHandleRef<'a, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [DynHandleRef<'a, H>] [TypedHandle<E, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [DynHandle<H>] [TypedHandleRef<'a, E, H>]);
cross_handle_impls!(['a, E: EntityKind, H: EntityHandleKindDeref] [TypedHandleRef<'a, E, H>] [DynHandle<H>]);
