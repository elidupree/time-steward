//! `list_of_types` works around Rust's lack of generic/associated statics by requiring the program to specify a single "list of types" at compile time, which must include everything that you want to call into dynamically. This must be globally unique within the program. That is, if a *library* wants to use these features, it must rely on being passed the global list as a generic parameter.
//!
//! Creating a list of types is simple: you wrap each type `T` into `ListedType<T>`, then put the `ListedType`s into tuples (possibly nested tuples). All the traits necessary to use a list for automatically implemented for all nested tuples of `ListedType`s. For instance, ((ListedType<i64>, ListedType<String>, ListedType<Vec<usize>>), ListedType<bool>) is a valid list with 4 types in it.

use std::any::Any;
use std::marker::PhantomData;

#[derive (Derivative)]
#[derivative (Copy, Clone, PartialEq, Eq, Hash)] //  PartialOrd, Ord,
pub struct ListedTypeIndex <S: ListTrait> (usize, PhantomData <S>);

#[macro_export]
macro_rules! time_steward_make_sublist {
  (mod $mod: ident visits $T: ident where $($where:tt)*) => {mod $mod {
    use std::marker::PhantomData;
    use std::any::Any;
    use $crate::dynamic::list_of_types::{ListTrait, ListedType, GlobalListConscious};
    
    unsafe trait SublistGeneratable {
      type Result: SublistTrait;
    }
    unsafe impl <$T: Any, Tail: SublistGeneratable> SublistGeneratable for (ListedType<$T>, Tail) {
      default type Result = <Tail as SublistGeneratable>::Result;
    }
    unsafe impl <$T: Any, Tail: SublistGeneratable> SublistGeneratable for (ListedType<$T>, Tail) where $($where)* {
      type Result = (ListedType<T>, <Tail as SublistGeneratable>::Result);
    }
    unsafe impl SublistGeneratable for ! {
      type Result = !;
    }
    
    pub trait Visitor {
      fn visit<$T>(&mut self) where $($where)*;
    }
    pub unsafe trait SublistTrait: ListTrait {
      fn visit_all<V: Visitor>(visitor: &mut V);
    }
    unsafe impl <$T: Any, Tail: SublistTrait> SublistTrait for (ListedType<T>, Tail) where $($where)* {
      #[inline(always)]
      fn visit_all<V: Visitor>(visitor: &mut V) {
        visitor.visit::<$T>();
      }
    }
    unsafe impl SublistTrait for ! {
      #[inline(always)]
      fn visit_all<V: Visitor>(_: &mut V) {}
    }
    type MakeSublist <List: SublistGeneratable> = List::Result;
    
    /*pub struct Sublist <GlobalList: ListTrait> (PhantomData <GlobalList>,!);
    impl <GlobalList: ListTrait> GlobalListConscious for Sublist <GlobalList> {
      type GlobalList = GlobalList;
    }*/

  }}
}
pub struct ListedType<T: Any>(PhantomData <T>, !);
pub unsafe trait ListTrait: Any + Sized {
  fn count()->usize;
  fn contains<T> ()->bool;
  fn index <T>()->ListedTypeIndex <Self>;
}

unsafe impl <T: Any, Tail: ListTrait> ListTrait for (ListedType<T>, Tail) {
  #[inline(always)]
  fn count()->usize {1 + Tail::count()}
  #[inline(always)]
  default fn contains <U>()->bool {<T as AmI<U>>::am_i() || Tail::contains::<U>()}
  #[inline(always)]
  default fn index <U>()->ListedTypeIndex <Self> {
    if <T as AmI<U>>::am_i() {
      ListedTypeIndex::<Self>(0, PhantomData)
    }
    else {
      ListedTypeIndex::<Self>(1 + Tail::index::<U>().0, PhantomData)
    }
  }
}
unsafe impl ListTrait for ! {
  #[inline(always)]
  fn count()->usize {0}
  #[inline(always)]
  fn contains <T>()->bool {false}
  #[inline(always)]
  fn index <T>()->ListedTypeIndex <Self> {panic!("trying to get the index of a type that's not in the list")}
}
pub trait GlobalListConscious {
  type GlobalList: ListTrait;
}

pub unsafe trait AmI <T> {fn am_i()->bool;}
unsafe impl <T, U> AmI <T> for U {default fn am_i()->bool {false}}
unsafe impl <T> AmI <T> for T {fn am_i()->bool {true}}


unsafe trait Canonicalizable {
  type Head;
  type Tail: Canonicalizable;
  type Result;
}

unsafe impl <T: Any> Canonicalizable for ListedType<T> {
  type Head = ListedType<T>;
  type Tail = !;
  type Result = (Self::Head, !);
}
unsafe impl Canonicalizable for ! {
  type Head = !;
  type Tail = !;
  type Result = !;
}

macro_rules! Canonicalizable_tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    unsafe impl<$($T,)* $TL> Canonicalizable for ($TL $(, $T)*)
      where $($T: Canonicalizable,)* $TL: Canonicalizable
    {
      default type Head = $TL::Head;
      default type Tail = ($TL::Tail $(, $T)*);
      default type Result = (Self::Head, <Self::Tail as Canonicalizable>::Result);
    }
    unsafe impl<$($T,)* $TL> Canonicalizable for (!, $TL $(, $T)*)
      where $($T: Canonicalizable,)* $TL: Canonicalizable
    {
      type Head = <($TL $(, $T)*) as Canonicalizable>::Head;
      type Tail = <($TL $(, $T)*) as Canonicalizable>::Tail;
      type Result = <($TL $(, $T)*) as Canonicalizable>::Result;
    }
    Canonicalizable_tuple_impls! ($($T),*);
  };
  () => {};
}

Canonicalizable_tuple_impls!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);


/*unsafe trait Contains <Test> {
  type Result;
}

struct Yes(!); struct No(!);

unsafe impl <Test> Contains <Test> for ! {
  type Result = No;
}
unsafe impl <Test, List: Uniquable> Contains <Test> for List {
  default type Result = <List::UnprocessedTail as Contains <Test>>::Result;
}
unsafe impl <Test: Any, Tail: Uniquable> Contains <Test> for (ListedType<Test>, Tail) {
  type Result = Yes;
}*/

unsafe trait IfContainsThenTailElseBoth<Test, Head, Tail> {
  type Result;
}

unsafe trait Uniquable {
  type UnprocessedHead;
  type UnprocessedTail : Uniquable;
  type Result;
}


unsafe impl <Test: Any, Head, Tail, List: Uniquable> IfContainsThenTailElseBoth<Test, Head, Tail> for List {
  default type Result = <List::UnprocessedTail as IfContainsThenTailElseBoth<Test, Head, Tail>>::Result;
}
unsafe impl <Test: Any, Head, Tail, ListTail: Uniquable> IfContainsThenTailElseBoth<Test, Head, Tail> for (ListedType<Test>, ListTail) {
  type Result = Tail;
}
unsafe impl <Test, Head, Tail> IfContainsThenTailElseBoth<Test, Head, Tail> for ! {
  type Result = (Head, Tail);
}

/*unsafe impl <T: Any, Tail: Uniquable> Uniquable for (ListedType<T>, Tail) {
  default type Result = <Tail as Uniquable>::Result;
  default type UnprocessedHead = ListedType<T>;
  default type UnprocessedTail = Tail;
}

unsafe impl <T: Any, Tail: Uniquable> Uniquable for (ListedType<T>, Tail) where Tail: Contains <T, Result=No> {
  type Result = (ListedType<T>, <Tail as Uniquable>::Result);
  type UnprocessedHead = ListedType<T>;
  type UnprocessedTail = Tail;
}*/

unsafe impl <T: Any, Tail: Uniquable> Uniquable for (ListedType<T>, Tail) {
  type Result = <Tail as IfContainsThenTailElseBoth<T, ListedType<T>, <Tail as Uniquable>::Result>>::Result;
  type UnprocessedHead = ListedType<T>;
  type UnprocessedTail = Tail;
}
unsafe impl Uniquable for ! {
  type UnprocessedHead = !;
  type UnprocessedTail = !;
  type Result = !;
}




#[cfg (test)]
mod tests {
  use super::*;
  use ::std;
  use ::dynamic;
  
  type Test = (ListedType <usize>, (ListedType <usize>, (ListedType <u64>, !)));
  type Test2 = <Test as Uniquable>::Result;
  
  type Test3 = (ListedType <usize>, ListedType <f32>, ListedType <f32>, ListedType <u64>, ListedType <usize>, ListedType <Vec<usize>>);
  type Test4 = ((Test3), ((Test3), Test3), Test3);
  type Test5 = <Test4 as Canonicalizable>::Result;
  type Test55 = (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, !))))))))))))))))))))))));
  type Test6 = <Test55 as Uniquable>::Result;
  
  #[test]
  fn test_unique() {
    use std::intrinsics::type_name;
    unsafe {
      /*assert_eq! (type_name::<<Test as Contains<u64>>::Result>(), "dynamic::list_of_types::Yes");
      assert_eq! (type_name::<<Test as Contains<usize>>::Result>(), "dynamic::list_of_types::Yes");
      assert_eq! (type_name::<<Test as Contains<f64>>::Result>(), "dynamic::list_of_types::No");
      assert_eq! (type_name::<<<Test as Uniquable>::UnprocessedTail as Contains<usize>>::Result>(), "dynamic::list_of_types::Yes");
      assert_eq! (type_name::<<<<Test as Uniquable>::UnprocessedTail as Uniquable>::UnprocessedTail as Contains<usize>>::Result>(), "dynamic::list_of_types::No");
      assert_eq! (type_name::<<(ListedType <u64>, !) as Contains<usize>>::Result>(), "dynamic::list_of_types::No");*/
      
      assert_eq! (type_name::<<(ListedType <usize>, (ListedType <u64>, !)) as Uniquable>::Result>(), "(dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<u64>, !))");
      
      assert_eq! (type_name::<<<Test as Uniquable>::UnprocessedTail as Uniquable>::Result>(), "(dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<u64>, !))");
      assert_eq! (type_name::<Test2>(), "(dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<u64>, !))");
      
      
      assert_eq! (type_name::<Test6>(), "(dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, !))))");
      assert_eq! (type_name::<Test5>(), "(dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<f32>, (dynamic::list_of_types::ListedType<u64>, (dynamic::list_of_types::ListedType<usize>, (dynamic::list_of_types::ListedType<std::vec::Vec<usize>>, !))))))))))))))))))))))))");
    }
  }
}



#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DeterministicallyRandomlyIdentifiedTypeId(u64);
pub trait DeterministicallyRandomlyIdentifiedType {
  const ID: DeterministicallyRandomlyIdentifiedTypeId;
}

fn assert_unique_global_list <GlobalList: ListTrait>() {
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
  (static $visitor_data: ident: $VisitorData: ty: $VisitorTrait: ident = $initial_value: expr followed by visit<$T> (&mut $self_hack) $visitation_effect: expr; fn $function_name: ident <$GlobalList: $SublistTrait: ident> ($($argument_name: ident: $argument_type:ty),*)->$return_type:ty $($body:tt)*) => {
  
    fn $function_name <GlobalList: $SublistTrait> $($argument_name: $argument_type),*)->$return_type:ty {
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
  (&mut $object: ident: $Object: ty, $GlobalList: ident, [$($mod: tt)*], $($method: tt)*) => {
    impl $($mod)*::Visitor for $Object {
      $($method)*
    }
    <$GlobalList as $($mod)*::SublistTrait>::visit_all (&mut $object);
  }
}
#[macro_export]
macro_rules! time_steward_with_sublist_table_entry {
  ($GlobalList: ident, [$($mod: tt)*], Vec<$Entry: ty> [$T: ident => {$entry: expr} where $($where: tt)*] [$index: expr], | $entry_variable: ident | $($closure: tt)*) => {{
    assert_unique_global_list::<$GlobalList>();
    use std::cell::RefCell;
    use std::mem;
    thread_local! {
      static TABLE: RefCell<Vec<$Entry>> = RefCell::new(Vec::new());
    }
    TABLE.with (| table | {
      let mut guard = table.borrow_mut();
      if guard.is_empty() {
        mem::replace (&mut*guard, {
          let mut result = Vec::with_capacity($($mod)*::Representative::<$GlobalList>::count());
          time_steward_visit_sublist! (
            &mut result: Vec<$Entry>,
            $GlobalList, [$($mod)*],
            fn visit<$T>(&mut self) where $($where)* {self.push ($entry);}
          );
          result
        });
      }
      let $entry_variable = guard.get ($index.0).expect("an invalid ListedTypeIndex exists");
      $($closure)*
    })
  }}
}


#[macro_export]
macro_rules! time_steward_with_sublist_table_entry {
  ($GlobalList: ident, [$($mod: tt)*], Vec<$Entry: ty> [$T: ident => {$entry: expr} where $($where: tt)*] [$index: expr], | $entry_variable: ident | $($closure: tt)*) => {{
    assert_unique_global_list::<$GlobalList>();
    use std::cell::RefCell;
    use std::mem;
    thread_local! {
      static TABLE: RefCell<Vec<$Entry>> = RefCell::new(Vec::new());
    }
    TABLE.with (| table | {
      let mut guard = table.borrow_mut();
      if guard.is_empty() {
        mem::replace (&mut*guard, {
          let mut result = Vec::with_capacity($($mod)*::Representative::<$GlobalList>::count());
          time_steward_visit_sublist! (
            &mut result: Vec<$Entry>,
            $GlobalList, [$($mod)*],
            fn visit<$T>(&mut self) where $($where)* {self.push ($entry);}
          );
          result
        });
      }
      let $entry_variable = guard.get ($index.0).expect("an invalid ListedTypeIndex exists");
      $($closure)*
    })
  }}
}


/*


  
fn index_to_id <GlobalList: whatever::SublistTrait> (index: ListedTypeIndex <whatever::Representative<GlobalList>>)->DeterministicallyRandomlyIdentifiedTypeId {
  time_steward_with_sublist_table_entry! (
    GlobalList, [whatever],
    Vec<DeterministicallyRandomlyIdentifiedTypeId> [T => {T::ID} where T: DeterministicallyRandomlyIdentifiedType] [index],
    | entry | *entry
  )
}

#[macro_export]
macro_rules! time_steward_dynamic_sublist_fn {
  (fn $function_name: ident, GlobalList, [$($mod: tt)*], (index: ListedTypeIndex <...>)->{$inner_function: ident::<Type(index)> ($($arguments: tt)*)->$Return:ty} where $($where: tt)*) => {
    fn $function_name <GlobalList: $($mod)*::SublistTrait> (index: ListedTypeIndex <$($mod)*::Representative>)->fn ($($arguments)*)->$Return {
      time_steward_with_sublist_table_entry! (
        <GlobalList as whatever::SublistTrait>,
        Vec<DeterministicallyRandomlyIdentifiedTypeId> [T => {$inner_function::<T>} where $($where)*] [index],
        | entry | *entry
      )
    }
  }
}



use serde::{Serialize, Deserialize, Serializer, Deserializer};*/

/*impl <S: Sublist> Serialize for ListedTypeIndex <S> {
  fn serialize <S: Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
    index_to_id::<S::GlobalList> (*self).serialize (serializer);
  }
}*/

#[macro_export]
macro_rules! time_steward_dynamic_sublist_uhhhhh {
  () => {
  
    fn $function_name ????? (?????)->????? {
    
    }
  
  
  }
}
