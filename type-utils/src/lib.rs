#![feature(never_type, specialization)]
#![allow(incomplete_features)]

use serde::{Deserialize, Serialize};

pub mod list_of_types;
pub mod visit_serialized;

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

pub trait ChoiceOfObjectContainedIn<T>: Copy + 'static {
  type Target;
  fn get(self, object: &T) -> &Self::Target;
  fn get_mut(self, object: &mut T) -> &mut Self::Target;
}

pub trait GetContained<Choice> {
  type Target;
  fn get_contained(&self, choice: Choice) -> &Self::Target;
  fn get_contained_mut(&mut self, choice: Choice) -> &mut Self::Target;
}

impl<T, C: ChoiceOfObjectContainedIn<T>> GetContained<C> for T {
  type Target = C::Target;
  fn get_contained(&self, choice: C) -> &Self::Target {
    choice.get(self)
  }
  fn get_contained_mut(&mut self, choice: C) -> &mut Self::Target {
    choice.get_mut(self)
  }
}

// macro for implementing n-ary tuple functions and operations, adapted from libcore
macro_rules! tuple_impls {
    ($(
        $Tuple:ident {
            $First: ident
            ($($T:ident $Choice:ident $U:ident,)*)
            $Last: ident
        }
    )+) => {
        $(
            #[allow(non_snake_case)]
            impl<$($T: 'static,)* $Last: 'static, $($Choice: ChoiceOfObjectContainedIn<$T, Target=$U>,)* > ChoiceOfObjectContainedIn<$First> for ($($Choice,)*) {
              type Target= $Last;
              fn get(self, object: &T) -> &Self::Target {
                let $First = object;
                let ($($Choice,)*) = self;
                $(let $U = $Choice.get($T);)*
                $Last
              }
              fn get_mut(self, object: &mut T) -> &mut Self::Target {
                let $First = object;
                let ($($Choice,)*) = self;
                $(let $U = $Choice.get_mut($T);)*
                $Last
              }
            }
        )+
    }
}

tuple_impls! {
    Tuple1 {
        T (T TU U,) U
    }
    Tuple2 {
        T (T TU U, U UV V,) V
    }
    Tuple3 {
        T (T TU U, U UV V, V VW W,) W
    }
    Tuple4 {
        T (T TU U, U UV V, V VW W, W WX X,) X
    }
    Tuple5 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z,) Z
    }
    Tuple6 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A,) A
    }
    Tuple7 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A, A AB B,) B
    }
    Tuple8 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A, A AB B, B BC C,) C
    }
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
