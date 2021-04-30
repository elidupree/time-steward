use std::marker::PhantomData;

pub struct ListedType<T>(PhantomData<T>, !);
pub trait ListOfTypesVisitor {
  fn visit<T>(&mut self);
}
pub trait ListOfTypes {
  fn visit_all<Visitor: ListOfTypesVisitor>(visitor: &mut Visitor);
}

impl<T> ListOfTypes for ListedType<T> {
  fn visit_all<Visitor: ListOfTypesVisitor>(visitor: &mut Visitor) {
    visitor.visit::<T>();
  }
}

impl ListOfTypes for () {
  fn visit_all<Visitor: ListOfTypesVisitor>(_visitor: &mut Visitor) {}
}

// macro for implementing n-ary tuple functions and operations, adapted from libcore
macro_rules! tuple_impls {
    ($(
        $Tuple:ident {
            $(($idx:tt) -> $T:ident $U:ident)+
        }
    )+) => {
        $(
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
