//! Allows dynamic dispatch to object-unsafe traits, with some caveats.
//!
//! `list_of_types` works around Rust's lack of generic/associated statics by requiring the program to specify a single "list of types" at compile time, which must include everything that you want to call into dynamically. This must be globally unique within the program. That is, if a *library* wants to use these features, it must rely on being passed the global list as a generic parameter.

use std::marker::PhantomData;

pub struct DeterministicallyRandomlyIdentifiedTypeId(u64);
pub trait DeterministicallyRandomlyIdentifiedType {
  const ID: DeterministicallyRandomlyIdentifiedTypeId;
}

macro_rules! make_visitor {
  (pub trait $Visitor: ident visits $T: ident where $($where:tt)*) => {
    pub trait $Visitor {
      fn visit<$T>(&mut self) where $($where)*;
    }
    trait Visitable {
      fn visit_all<V: $Visitor>(visitor: &mut V);
      fn count()->usize;
      fn count_before<T>()->usize;
      fn count_before_id (id: DeterministicallyRandomlyIdentifiedTypeId)->usize;
    }
    impl <T> Visitable for ListedType<T> {
      #[inline(always)]
      default fn visit_all<V: $Visitor>(visitor: &mut V) {}
      #[inline(always)]
      default fn count()->usize {0}
      #[inline(always)]
      default fn count_before<T>()->usize {panic!("invoked count_before on a list that doesn't contain the type")}
    }
    impl <$T> Visitable for ListedType<$T> where $($where)* {
      #[inline(always)]
      fn visit_all<V: $Visitor>(visitor: &mut V) {
        visitor.visit<$T>();
      }
      #[inline(always)]
      fn count()->usize {1}
      #[inline(always)]
      fn count_before<T>()->usize {0}
    }
    tuple_impls(Visitable);
  }
}
pub struct ListedType<T>(PhantomData <T>, !);


macro_rules! tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    impl<$($T,)* $TL> Visitable for ($($T,)* $TL,)
      where $($T: Visitable,)* $TL: Visitable
    {
      #[inline(always)]
      fn visit_all<V: $Visitor>(visitor: &mut V) {
        $($T::visit_all(visitor);)*
        $TL::visit_all(visitor);
      }
      #[inline(always)]
      fn count()->usize {
        $($T::count() + )* $TL::count();
      }
      #[inline(always)]
      fn count_before<T>()->usize {
        let mut count = 0;
        if $TL::includes::<T> () {return $TL::count_before::<T>()}
        count += $TL::count();
        $(if $T::includes::<T> () {return $T::count_before::<T>()}
        count += $T::count();)*
        panic!("invoked count_before on a list that doesn't contain the type")
      }
    }
    tuple_impls! ($($T),*);
  };
  () => {};
}

pub trait List {
  fn includes <T> ()->bool;
}

pub trait AmI <T> {fn am_i()->bool;}
impl <T, U> AmI <T> for U {default fn am_i()->bool {false}}
impl <T> AmI <T> for T {fn am_i()->bool {true}}

impl <T> List for ListedType<T> {
  #[inline(always)]
  fn includes<U>()->bool {<T as AmI<U>>::am_i()}
}

macro_rules! tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    impl<$($T,)* $TL> List for ($($T,)* $TL,)
      where $($T: List,)* $TL: List
    {
      #[inline(always)]
      fn includes<T>()->bool {
        $($T::includes::<T>() || )* $TL::includes::<T>()
      }
    }
    tuple_impls! ($($T),*);
  };
  () => {};
}


tuple_impls! (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);

