#![feature(never_type, specialization)]

use serde::{Deserialize, Serialize};

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
  fn try_identity(self) -> Option<T> {
    Some(self)
  }
}
impl<T, U> TryIdentity<T> for U {
  default fn try_identity(self) -> Option<T> {
    None
  }
}

/// Convert between two types if they are actually the same type.
///
///
#[deprecated]
pub fn static_downcast<T, U>(input: T) -> Option<U> {
  TryIdentity::<U>::try_identity(input)
}
pub fn try_identity<T, U>(input: T) -> Option<U> {
  TryIdentity::<U>::try_identity(input)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_static_downcast() {
    assert_eq!(try_identity::<i32, i32>(5i32), Some(5i32));
    assert_eq!(try_identity::<i32, i16>(5i32), None);
  }
}

pub mod list_of_types;
pub mod visit_serialized;
