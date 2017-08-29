use std::fmt;
use std::collections::BTreeSet;

use super::api::*;


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


impl<T> ListOfTypes for ListedType <T> {
  fn visit_all <Visitor: ListOfTypesVisitor>(visitor: &mut Visitor) {
    visitor.visit::<T>();
  }
}


// macro for implementing n-ary tuple functions and operations, adapted from libcore
macro_rules! tuple_impls {
    ($(
        $Tuple:ident {
            $(($idx:tt) -> $T:ident $U:ident)+
        }
    )+) => {
        $(
            impl<$($T:StewardData),+> StewardData for ($($T,)+) {}
            impl<$($T:ListOfTypes),+> ListOfTypes for ($($T,)+) {
              fn visit_all <Visitor: ListOfTypesVisitor>(visitor: &mut Visitor) {
                $($T::visit_all(visitor);)*
              }
            }
        )+
    }
}

tuple_impls! {
    Tuple1 {
        (0) -> A AA
    }
    Tuple2 {
        (0) -> A AA
        (1) -> B BB
    }
    Tuple3 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
    }
    Tuple4 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
    }
    Tuple5 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
    }
    Tuple6 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
    }
    Tuple7 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
    }
    Tuple8 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
        (7) -> H HH
    }
    Tuple9 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
        (7) -> H HH
        (8) -> I II
    }
    Tuple10 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
        (7) -> H HH
        (8) -> I II
        (9) -> J JJ
    }
    Tuple11 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
        (7) -> H HH
        (8) -> I II
        (9) -> J JJ
        (10) -> K KK
    }
    Tuple12 {
        (0) -> A AA
        (1) -> B BB
        (2) -> C CC
        (3) -> D DD
        (4) -> E EE
        (5) -> F FF
        (6) -> G GG
        (7) -> H HH
        (8) -> I II
        (9) -> J JJ
        (10) -> K KK
        (11) -> L LL
    }
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


