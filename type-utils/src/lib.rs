#![feature(never_type, specialization)]

use serde::{Serialize, Deserialize};

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



trait StaticDowncast <T> {
    fn static_downcast (self)->Option <T>;
  }
  impl <T> StaticDowncast <T> for T {
    fn static_downcast (self)->Option <T> {Some (self)}
  }
  impl <T, U> StaticDowncast <T> for U {
    default fn static_downcast (self)->Option <T> {None}
  }
  

/// Convert between two types if they are actually the same type.
///
/// 
pub fn static_downcast <T, U> (input: T)->Option <U> {
  StaticDowncast::<U>::static_downcast (input)
}

#[cfg (test)]
mod tests {
  use super::*;
  
  #[test]
  fn test_static_downcast() {
    assert_eq! (static_downcast::<i32, i32> (5i32), Some (5i32)) ;
    assert_eq! (static_downcast::<i32, i16> (5i32), None) ;
  }
}

pub mod list_of_types;
