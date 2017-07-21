//! Allows dynamic dispatch to object-unsafe traits, with some caveats.
//!
//! `list_of_types` works around Rust's lack of generic/associated statics by requiring the program to specify a single "list of types" at compile time, which must include everything that you want to call into dynamically. This must be globally unique within the program. That is, if a *library* wants to use these features, it must rely on being passed the global list as a generic parameter.
//!
//! Creating a list of types is simple: you wrap each type `T` into `ListedType<T>`, then put the `ListedType`s into tuples (possibly nested tuples). All the traits necessary to use a list for automatically implemented for all nested tuples of `ListedType`s. For instance, ((ListedType<i64>, ListedType<String>, ListedType<Vec<usize>>), ListedType<bool>) is a valid list with 4 types in it.

use std::marker::PhantomData;

#[macro_export]
macro_rules! time_steward_make_sublist {
  (pub trait $SubList; pub trait $Visitor: ident visits $T: ident where $($where:tt)*) => {
    pub trait $Visitor {
      fn visit<$T>(&mut self) where $($where)*;
    }
    pub unsafe trait $SubList {
      fn visit_all<V: $Visitor>(visitor: &mut V);
      fn count()->usize;
      fn count_before<T>()->usize;
    }
    unsafe impl <T> $SubList for ListedType<T> {
      #[inline(always)]
      default fn visit_all<V: $Visitor>(visitor: &mut V) {}
      #[inline(always)]
      default fn count()->usize {0}
      #[inline(always)]
      default fn count_before<T>()->usize {panic!("invoked count_before on a list that doesn't contain the type")}
    }
    unsafe impl <$T> $SubList for ListedType<$T> where $($where)* {
      #[inline(always)]
      fn visit_all<V: $Visitor>(visitor: &mut V) {
        visitor.visit<$T>();
      }
      #[inline(always)]
      fn count()->usize {1}
      #[inline(always)]
      fn count_before<T>()->usize {0}
    }
    __time_steward_internal_sublist_tuple_impls! ($SubList tuples T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
  }
}
pub struct ListedType<T>(PhantomData <T>, !);

#[macro_export]
#[doc(hidden)]
macro_rules! __time_steward_internal_sublist_tuple_impls {
  ($SubList: ident tuples $TL: ident $(, $T: ident)*) => {
    unsafe impl<$($T,)* $TL> $SubList for ($($T,)* $TL,)
      where $($T: $SubList,)* $TL: $SubList
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
        if $TL::includes::<T> () {return count + $TL::count_before::<T>()}
        count += $TL::count();
        $(if $T::includes::<T> () {return count + $T::count_before::<T>()}
        count += $T::count();)*
        panic!("invoked count_before on a list that doesn't contain the type")
      }
    }
    __time_steward_internal_sublist_tuple_impls! ($SubList tuples $($T),*);
  };
  () => {};
}

pub unsafe trait List {
  fn includes <T> ()->bool;
}

pub unsafe trait AmI <T> {fn am_i()->bool;}
unsafe impl <T, U> AmI <T> for U {default fn am_i()->bool {false}}
unsafe impl <T> AmI <T> for T {fn am_i()->bool {true}}

unsafe impl <T> List for ListedType<T> {
  #[inline(always)]
  fn includes<U>()->bool {<T as AmI<U>>::am_i()}
}

macro_rules! tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    unsafe impl<$($T,)* $TL> List for ($($T,)* $TL,)
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

#[derive (Copy, Clone, Hash, Serialize,
pub struct DeterministicallyRandomlyIdentifiedTypeId(u64);
pub trait DeterministicallyRandomlyIdentifiedType {
  const ID: DeterministicallyRandomlyIdentifiedTypeId;
}


time_steward_make_sublist! (DeterministicallyRandomlyIdentifiedTypesList, DeterministicallyRandomlyIdentifiedTypesVisitor visits T where T: DeterministicallyRandomlyIdentifiedType);

fn listed_type_id <GlobalList> (index: usize)->Option <DeterministicallyRandomlyIdentifiedTypeId> {
  struct Visitor (Vec<DeterministicallyRandomlyIdentifiedTypeId>);
  impl DeterministicallyRandomlyIdentifiedTypesVisitor for Visitor {
    fn visit<T>(&mut self) where T: DeterministicallyRandomlyIdentifiedType {self.0.push (T::ID);}
  }
  thread_local! {
    static LIST_ID: TypeId = TypeId::of::<GlobalList>();
    static TABLE: Vec<DeterministicallyRandomlyIdentifiedTypeId> = {
      let mut visitor = Visitor (Vec::with_capacity(<GlobalList as DeterministicallyRandomlyIdentifiedTypesList>::count()));
      <GlobalList as DeterministicallyRandomlyIdentifiedTypesList>::visit_all (&mut visitor);
      visitor.0
    };
  }
  LIST_ID.with (| id | {
    assert!(id, TypeId::of::<GlobalList>(), "invoked dynamic function with two different global lists (see the list_of_types documentation for why this is wrong")
  });
  TABLE.with (| table | {table.get (index)})
}

