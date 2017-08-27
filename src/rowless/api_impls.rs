use std::fmt;
use std::collections::BTreeSet;

use super::api::*;


macro_rules! StewardData_tuple_impls {
  ($TL: ident) => {};
  ($TL: ident $(, $T: ident)*) => {
    impl<$($T,)* $TL> StewardData for ($TL $(, $T)*)
      where $($T: StewardData,)* $TL: StewardData {}
    StewardData_tuple_impls! ($($T),*);
  };
}
StewardData_tuple_impls!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
macro_rules! StewardData_array_impls {
  () => {};
  ($num: expr $(, $rest: expr)*) => {
    impl<T: StewardData + Copy> StewardData for [T; $num] {}
    StewardData_array_impls! ($($rest),*);
  };
}
StewardData_array_impls! (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);
macro_rules! StewardData_primitive_impls {
  () => {};
  ($prim: ty $(, $rest: ty)*) => {
    impl StewardData for $prim {}
    StewardData_primitive_impls! ($($rest),*);
  };
}
StewardData_primitive_impls!((), u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);

impl <T: StewardData> StewardData for Option <T> {}
impl <T: StewardData> StewardData for Vec<T> {}
impl <T: StewardData + Ord> StewardData for BTreeSet<T> {}



impl <T: Basics> StewardData for ExtendedTime <T> {}

impl <'a, Owned: StewardData> PossiblyBorrowedStewardData <'a, Owned> for Owned {
  fn to_owned (self)->Owned {self}
  fn clone_to_owned (&self)->Owned {self.clone()}
  fn from_ref (source: &'a Owned)->Self {source.clone()}
}
impl <'a, Owned: StewardData> PossiblyBorrowedStewardData <'a, Owned> for & 'a Owned {
  fn to_owned (self)->Owned {self.clone()}
  fn clone_to_owned (&self)->Owned {(*self).clone()}
  fn from_ref (source: &'a Owned)->Self {source}
}


use std::cmp::Ordering;

impl<B: fmt::Display> fmt::Display for ValidSince<B> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &ValidSince::TheBeginning => write!(f, "TheBeginning"),
      &ValidSince::Before(ref something) => write!(f, "Before({})", something),
      &ValidSince::After(ref something) => write!(f, "After({})", something),
    }
  }
}

impl<T: Ord> Ord for ValidSince<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&ValidSince::TheBeginning, &ValidSince::TheBeginning) => Ordering::Equal,
      (&ValidSince::TheBeginning, _) => Ordering::Less,
      (_, &ValidSince::TheBeginning) => Ordering::Greater,
      (&ValidSince::Before(ref something), &ValidSince::Before(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::After(ref something), &ValidSince::After(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::Before(ref something), &ValidSince::After(ref anything)) => {
        if something <= anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      (&ValidSince::After(ref something), &ValidSince::Before(ref anything)) => {
        if something < anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T> PartialEq<T> for ValidSince<T> {
  fn eq(&self, _: &T) -> bool {
    false
  }
}

impl<T: Ord> PartialOrd<T> for ValidSince<T> {
  fn partial_cmp(&self, other: &T) -> Option<Ordering> {
    Some(match self {
      &ValidSince::TheBeginning => Ordering::Less,
      &ValidSince::Before(ref something) => {
        if something <= other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      &ValidSince::After(ref something) => {
        if something < other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    })
  }
}
impl<T: Ord> PartialOrd for ValidSince<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
// impl <T: Ord> PartialOrd <ValidSince <T>> for T {
//  fn partial_cmp (&self, other: & ValidSince <T>)->Option <Ordering> {
//    Some (other.partial_cmp (self).unwrap().reverse());
//  }
// }


