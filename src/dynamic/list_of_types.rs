//! Allows dynamic dispatch to object-unsafe traits, with some caveats.
//!
//! `list_of_types` works around Rust's lack of generic/associated statics by requiring the program to specify a single "list of types" at compile time, which must include everything that you want to call into dynamically. This must be globally unique within the program. That is, if a *library* wants to use these features, it must rely on being passed the global list as a generic parameter.
//!
//! Creating a list of types is simple: you wrap each type `T` into `ListedType<T>`, then put the `ListedType`s into tuples (possibly nested tuples). All the traits necessary to use a list for automatically implemented for all nested tuples of `ListedType`s. For instance, ((ListedType<i64>, ListedType<String>, ListedType<Vec<usize>>), ListedType<bool>) is a valid list with 4 types in it.

use std::marker::PhantomData;

#[macro_export]
macro_rules! time_steward_make_sublist {
  (mod $mod: ident visits $T: ident where $($where:tt)*) => {mod $mod {
    use std::marker::PhantomData;
    use std::any::Any;
    use $crate::dynamic::list_of_types::{List, SubListRepresentative, ListedType};
    pub trait Visitor {
      fn visit<$T>(&mut self) where $($where)*;
    }
    pub unsafe trait SubList: List {
      fn visit_all<V: Visitor>(visitor: &mut V);
      fn count()->usize;
      fn count_before<T>()->usize;
    }
    unsafe impl <T: Any> SubList for ListedType<T> {
      #[inline(always)]
      default fn visit_all<V: Visitor>(_: &mut V) {}
      #[inline(always)]
      default fn count()->usize {0}
      #[inline(always)]
      default fn count_before<U>()->usize {panic!("invoked count_before on a list that doesn't contain the type")}
    }
    unsafe impl <$T: Any> SubList for ListedType<$T> where $($where)* {
      #[inline(always)]
      fn visit_all<V: Visitor>(visitor: &mut V) {
        visitor.visit::<$T>();
      }
      #[inline(always)]
      fn count()->usize {1}
      #[inline(always)]
      fn count_before<U>()->usize {0}
    }
    __time_steward_internal_sublist_tuple_impls! (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
    
    pub struct Representative <GlobalList: SubList> (PhantomData <GlobalList>,!);
    unsafe impl <GlobalList: SubList> SubListRepresentative for Representative <GlobalList> {
      fn count()->usize {
        $crate::dynamic::list_of_types::assert_unique_global_list::<GlobalList>();
        GlobalList::count()
      }
      fn count_before<T>()->usize {
        $crate::dynamic::list_of_types::assert_unique_global_list::<GlobalList>();
        GlobalList::count_before::<T>()
      }
    }/*{
      fn visit_all<V: Visitor>(visitor: &mut V) {
        GlobalList::visit_all(visitor);
      }
    }*/
  }}
}
pub struct ListedType<T: Any>(PhantomData <T>, !);

#[macro_export]
#[doc(hidden)]
macro_rules! __time_steward_internal_sublist_tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    unsafe impl<$($T,)* $TL> SubList for ($($T,)* $TL,)
      where $($T: SubList,)* $TL: SubList
    {
      #[inline(always)]
      fn visit_all<V: Visitor>(visitor: &mut V) {
        $($T::visit_all(visitor);)*
        $TL::visit_all(visitor);
      }
      #[inline(always)]
      fn count()->usize {
        $($T::count() + )* $TL::count()
      }
      #[inline(always)]
      fn count_before<T>()->usize {
        let count = 0;
        $(if $T::includes::<T> () {return count + $T::count_before::<T>()}
        let count = count + $T::count();)*
        if $TL::includes::<T> () {return count + $TL::count_before::<T>()}
        panic!("invoked count_before on a list that doesn't contain the type")
      }
    }
    __time_steward_internal_sublist_tuple_impls! ($($T),*);
  };
  () => {};
}

pub unsafe trait List: Any {
  fn includes <T> ()->bool;
}
pub unsafe trait SubListRepresentative {
  fn count()->usize;
  fn count_before<T>()->usize;
}

pub unsafe trait AmI <T> {fn am_i()->bool;}
unsafe impl <T, U> AmI <T> for U {default fn am_i()->bool {false}}
unsafe impl <T> AmI <T> for T {fn am_i()->bool {true}}

unsafe impl <T: Any> List for ListedType<T> {
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

#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DeterministicallyRandomlyIdentifiedTypeId(u64);
pub trait DeterministicallyRandomlyIdentifiedType {
  const ID: DeterministicallyRandomlyIdentifiedTypeId;
}

fn assert_unique_global_list <GlobalList: List>() {
  use std::any::TypeId;
  use std::cell::Cell;
  thread_local! {
    static LIST_ID: Cell<Option <TypeId>> = Cell::new (None);
  }
  LIST_ID.with (| id_cell | {
    let id = TypeId::of::<GlobalList>();
    if let Some (existing) = id_cell.get() {
      assert!(id == existing, "invoked dynamic function with two different global lists (see the list_of_types documentation for why this is wrong");
      return
    }
    id_cell.set (Some(id));
  });
}


time_steward_make_sublist! (
mod whatever visits T where T: super::DeterministicallyRandomlyIdentifiedType);
/*
#[macro_export]
macro_rules! time_steward_sublist_fn {
  (static $visitor_data: ident: $VisitorData: ty: $VisitorTrait: ident = $initial_value: expr followed by visit<$T> (&mut $self_hack) $visitation_effect: expr; fn $function_name: ident <$GlobalList: $SubList: ident> ($($argument_name: ident: $argument_type:ty),*)->$return_type:ty $($body:tt)*) => {
  
    fn $function_name <GlobalList: $List> $($argument_name: $argument_type),*)->$return_type:ty {
      struct Visitor ($VisitorData);
      impl VisitorTrait for Visitor {
        fn visit<$T>(&mut $self_hack) where  {$visitation_effect}
      }
      thread_local! {
        static DATA: $VisitorData = {
          let mut visitor = Visitor ($initial_value);
          <GlobalList as DeterministicallyRandomlyIdentifiedTypesList>::visit_all (&mut visitor);
          visitor.0
        };
      }
      DATA.with (| $visitor_data | {$($body)*})
  }
}
  */


#[macro_export]
macro_rules! time_steward_visit_sublist {
  (&mut $object: ident: $Object: ty, $GlobalList: ident, $mod: ident, $($method: tt)*) => {
    /*struct Visitor <'a> (&mut 'a $Object);
    impl <'a> $mod::Visitor for Visitor <'a> {
      $method
    }*/
    impl $mod::Visitor for $Object {
      $($method)*
    }
    <$GlobalList as $mod::SubList>::visit_all (&mut $object);
  }
}

use std::any::Any ;
  
fn listed_type_id <GlobalList: whatever::SubList> (index: usize)->Option <DeterministicallyRandomlyIdentifiedTypeId> {
  assert_unique_global_list::<GlobalList>();
  use std::cell::RefCell;
  use std::mem;
  struct Visitor (Vec<DeterministicallyRandomlyIdentifiedTypeId>);
  impl whatever::Visitor for Visitor {
    fn visit<T>(&mut self) where T: DeterministicallyRandomlyIdentifiedType {self.0.push (T::ID);}
  }
  thread_local! {
    static TABLE: RefCell<Vec<DeterministicallyRandomlyIdentifiedTypeId>> = RefCell::new(Vec::new());
  }
  TABLE.with (| table | {
    let mut guard = table.borrow_mut();
    if guard.is_empty() {
      mem::replace (&mut*guard, {
        let mut result = Vec::with_capacity(whatever::Representative::<GlobalList>::count());
        time_steward_visit_sublist! (&mut result: Vec<DeterministicallyRandomlyIdentifiedTypeId>, GlobalList, whatever, fn visit<T>(&mut self) where T: DeterministicallyRandomlyIdentifiedType {self.push (T::ID);});
        result
      });
    }
    guard.get (index).cloned()
  })
}

