use std::collections::HashMap;
use serde::{Serialize, Serializer, Error, de};

use {ExtendedTime, StewardRc, FieldRc, ColumnId, Column, Basics};

macro_rules! type_list_definitions {
($module: ident, $Trait: ident, $IdType: ident, $get_id: ident) => {
pub mod $module {
use std::any::Any;
use std::marker::PhantomData;
use {$Trait,$IdType};

pub type Id = $IdType;
pub fn get_id <T: $Trait>()->Id {T::$get_id()}

enum Void {}
pub struct Item <T: $Trait>(PhantomData <T>, Void);
pub trait User {
  fn apply<T: $Trait>(&mut self);
}
pub trait List: Any {
  fn apply<U: User>(user: &mut U);
}
impl<T: $Trait> List for Item <T> {
  #[inline]
  fn apply<U: User>(user: &mut U) {
    user.apply::<T>();
  }
}
tuple_impls! (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
}
};

($module: ident, $Trait: ident<B>, $IdType: ident, $get_id: ident) => {
pub mod $module {
use Basics;
use std::any::Any;
use std::marker::PhantomData;
use {$Trait,$IdType};

pub type Id = $IdType;
pub fn get_id <B: Basics, T: $Trait <B>>()->Id {T::$get_id()}

enum Void {}
pub struct Item <B: Basics, T: $Trait <B>>(PhantomData <B>, PhantomData <T>, Void);
pub trait User <B: Basics> {
  fn apply<T: $Trait <B>>(&mut self);
}
pub trait List <B: Basics>: Any {
  fn apply<U: User <B>>(user: &mut U);
}
impl<B: Basics, T: $Trait <B>> List <B> for Item <B, T> {
  #[inline]
  fn apply<U: User <B>>(user: &mut U) {
    user.apply::<T>();
  }
}
tuple_impls! (B- T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
}
};

}
macro_rules! tuple_impls {
  ($TL: ident $(, $T: ident)*) => {
    impl<$($T,)* $TL> List for ($($T,)* $TL,)
      where $($T: List,)* $TL: List
    {
      #[inline]
      fn apply <U: User> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! ($($T),*);
  };
  (B- $TL: ident $(, $T: ident)*) => {
    impl<B: Basics, $($T,)* $TL> List <B> for ($($T,)* $TL,)
      where $($T: List <B>,)* $TL: List <B>
    {
      #[inline]
      fn apply <U: User <B>> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! (B- $($T),*);
  };
  () => {};
  (B-) => {};
}
macro_rules! pair_null_impls {
([$module0: ident, $Trait0: ident][$module1: ident $Trait1: ident]) => {
impl<T: $Trait0> $module1::List for $module0::Item <T> {
  #[inline]
  fn apply<U: $module1::User>(_: &mut U) {}
}
impl<T: $Trait1> $module0::List for $module1::Item <T> {
  #[inline]
  fn apply<U: $module0::User>(_: &mut U) {}
}
};

([$module0: ident, $Trait0: ident][$module1: ident, $Trait1 <B>]) => {
impl<B: Basics, T: $Trait0> $List1 <B> for $module0::Item <T> {
  #[inline]
  fn apply<U: $module1::User <B>>(_: &mut U) {}
}
impl<B: Basics, T: $Trait1 <B>> $List0 for $module1::Item <B, T> {
  #[inline]
  fn apply<U: $module0::User>(_: &mut U) {}
}
};

([$module0: ident, $Trait0: ident <B>][$module1: ident, $Trait1: ident <B>]) => {
impl<B: Basics, T: $Trait0 <B>> $List1 <B> for $module0::Item <B, T> {
  #[inline]
  fn apply<U: $module1::User <B>>(_: &mut U) {}
}
impl<B: Basics, T: $Trait1 <B>> $List0 <B> for $module1::Item <B, T> {
  #[inline]
  fn apply<U: $module0::User <B>>(_: &mut U) {}
}
};
}
macro_rules! all_null_impls {
($info0:tt $($info:tt)*) => {
  $(pair_null_impls! ($info0 $info);)*
  all_null_impls! ($($info)*);
};
() => {};
}
macro_rules! all_list_definitions {
($([$($info:tt)*])*) => {
  $(type_list_definitions! ($($info)*);)*
  //all_null_impls! ($([$($info)*])*);
};
}

// Today I Learned that macro hygiene is not applied to type parameter lists
//
// macro_rules! escalate {
// ([$first:tt $($whatever:tt)*] $($T: ident)*) => {escalate! ([$($whatever)*] foo $($T)*);};
// ([] $($T: ident)*) => {tuple_impls! ($($T),*);};
// }
// escalate! ([!!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!!]);
//

all_list_definitions! (
  [column_list, Column, ColumnId, column_id]
  [event_list, EventFn<B>, EventId, event_id]
  [predictor_list, PredictorFn<B>, PredictorId, predictor_id]
);

pub use column_list::List as ColumnList;
pub use column_list::Item as ColumnType;


#[macro_export]
macro_rules! time_steward_make_function_table_type {
  ($Struct: ident, $module: ident, fn $function: ident <$T: ident: $Trait: ident $(, [$Parameter: ident $($bounds:tt)*])*> ($($argument_name: ident: $argument_type:ty),*)->$return_type:ty) => {
pub struct $Struct <$($Parameter $($bounds)*),*> (HashMap<$module::Id, fn($($argument_name: $argument_type),*)-> $return_type>);
impl<$($Parameter $($bounds)*),*> $module::User for $Struct<$($Parameter),*> {
  fn apply<$T: $Trait>(&mut self) {
    self.0.insert($module::get_id:: <$T>(), $function::<$T $(, $Parameter)*>);
  }
}
impl<$($Parameter $($bounds)*),*> $Struct<$($Parameter),*> {
  pub fn new <L: $module::List>()->$Struct<$($Parameter),*> {
    let mut result = $Struct (HashMap::new());
    L::apply (&mut result);
    result
  }
  pub fn call (&self, id: $module::Id $(, $argument_name: $argument_type)*)->$return_type {
    (self.0.get (&id).expect ("Type missing from function table; did you forget to list it in Basics::IncludedTypes?")) ($($argument_name),*)
  }
}
}

}


fn check_equality<C: Column>(first: &FieldRc, second: &FieldRc)->bool {
  ::unwrap_field::<C>(first) == ::unwrap_field::<C>(second)
}
time_steward_make_function_table_type! (FieldEqualityTable, column_list, fn check_equality<C: Column>(first: &FieldRc, second: &FieldRc)->bool);

impl FieldEqualityTable{
  pub fn fields_are_equal(&self, column_id: ColumnId, first: & FieldRc, second: & FieldRc)->bool {
    self.call (column_id, first, second)
  }
  pub fn options_are_equal(&self, column_id: ColumnId, first: Option <& FieldRc>, second: Option <& FieldRc>)->bool {
    match (first, second) {
      (None, None) => true,
      (Some (first), Some (second)) => self.fields_are_equal (column_id, first, second),
      _ => false,
    }
  }
}

fn serialize_field <C: Column, S: Serializer>(field: &FieldRc,
                                              serializer: &mut S)
                                              -> Result<(), S::Error> {
  try!(::unwrap_field::<C>(field).serialize(serializer));
  Ok(())
}
fn deserialize_field_from_map <C: Column, B: Basics, M: de::MapVisitor>
  (visitor: &mut M)
   -> Result<(FieldRc, ExtendedTime<B>), M::Error> {
  let (data, time) = try!(visitor.visit_value::<(C::FieldType, ExtendedTime<B>)>());
  Ok((StewardRc::new(data), time))
}
time_steward_make_function_table_type! (FieldSerializationTable, column_list, fn serialize_field <C: Column, [S: Serializer]>(field: &FieldRc,
                                                serializer: &mut S)
                                                -> Result<(), S::Error> );
time_steward_make_function_table_type! (MappedFieldDeserializationTable, column_list, fn deserialize_field_from_map <C: Column, [B: Basics], [M: de::MapVisitor]>(visitor: &mut M)
                                                -> Result<(FieldRc, ExtendedTime<B>), M::Error>  );

impl<S: Serializer> FieldSerializationTable<S> {
  pub fn serialize_field(&self, column_id: ColumnId, first: & FieldRc,
                                                  serializer: &mut S)->Result<(), S::Error>{
    use serde::ser::Error;
    try!(self.0.get (&column_id).ok_or (S::Error::custom ("Column missing from serialization table; did you forget to list a column in Basics::Columns?"))) (first, serializer)
  }
}
impl<B: Basics, M: de::MapVisitor> MappedFieldDeserializationTable<B, M> {
  pub fn deserialize_field(&self, column_id: ColumnId, visitor: &mut M)->Result<(FieldRc, ExtendedTime<B>), M::Error>{
    try!(self.0.get (&column_id).ok_or (M::Error::custom ("Column missing from deserialization table; did you forget to list a column in Basics::Columns?"))) (visitor)
  }
}


