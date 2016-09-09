use std::collections::HashMap;
use std::any::Any;
use std::marker::PhantomData;
use serde::{Serialize, Serializer, Error, de};

use {ExtendedTime, StewardRc, FieldRc, ColumnId, Column, Basics, EventFn, PredictorFn,};

enum Void {}

macro_rules! type_list_definitions {
($Trait: ident [$($bounded:tt)*] [$($unbounded: ident),*], $Wrapper: ident, $List: ident, $User: ident) => {
pub struct $Wrapper <$($bounded)* T: $Trait<$($unbounded),*>>($(PhantomData <$unbounded>,)* PhantomData <T>, Void);
pub trait $User<$($bounded)*> {
  fn apply<C: $Trait <$($unbounded),*>>(&mut self);
}
pub trait $List<$($bounded)*>: Any {
  fn apply<U: $User>(user: &mut U);
}
impl<$($bounded)* T: $Trait <$($unbounded),*>> $List <$($unbounded),*> for $Wrapper<$($unbounded,)* T> {
  #[inline]
  fn apply<U: $User>(user: &mut U) {
    user.apply::<T>();
  }
}
tuple_impls! ([$($bounded)*] [$($unbounded),*], $List, $User, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
};
}
macro_rules! tuple_impls {
  ([$($bounded:tt)*] [$unbounded: ident], $List: ident, $User: ident, $TL: ident $(, $T: ident)*) => {
    impl<$($bounded)* $($T,)* $TL> $List<$unbounded> for ($($T,)* $TL,)
      where $($T: $List<$unbounded>,)* $TL: $List<$unbounded>
    {
      #[inline]
      fn apply <U: $User<$unbounded>> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! ([$($bounded)*] [$unbounded], $List, $User, $($T),*);
  };
  ([$($bounded:tt)*] [], $List: ident, $User: ident, $TL: ident $(, $T: ident)*) => {
    impl<$($bounded)* $($T,)* $TL> $List<> for ($($T,)* $TL,)
      where $($T: $List<>,)* $TL: $List<>
    {
      #[inline]
      fn apply <U: $User> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! ([$($bounded)*] [], $List, $User, $($T),*);
  };
  ([$($bounded:tt)*] [$($unbounded: ident),*], $List: ident, $User: ident,) => {};
}
macro_rules! pair_null_impls {
([$($bounded0:tt)*] [$($unbounded0: ident),*], $Wrapper0: ident, $List0: ident, $User0: ident, [$($bounded1:tt)*] [$($unbounded1: ident),*], $Wrapper1: ident, $List1: ident, $User1: ident) => {
impl<$($bounded1)* T> $List1 for $Wrapper0<$($bounded)* T> {
  #[inline]
  fn apply<U: $User1>(_: &mut U) {}
}
impl<$($bounded0)* T> $List0 for $Wrapper1<$($bounded)* T> {
  #[inline]
  fn apply<U: $User0>(_: &mut U) {}
}
};
}
macro_rules! all_null_impls {
([$Wrapper0: ident, $List0: ident, $User0: ident] $(, [$Wrapper: ident, $List: ident, $User: ident])*) => {
  $(pair_null_impls! ($Wrapper0, $List0, $User0, $Wrapper, $List, $User);)*
  all_null_impls! ($([$Wrapper, $List, $User]),*);
};
() => {};
}
macro_rules! all_list_definitions {
($([$Trait: ident [$($bounded:tt)*] [$($unbounded: ident),*],  $Wrapper: ident, $List: ident, $User: ident],)*) => {
  $(type_list_definitions! ($Trait[$($bounded)*] [$($unbounded),*], $Wrapper, $List, $User);)*
  all_null_impls! ($([$Wrapper, $List, $User]),*);
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
  [Column[][], ColumnType, ColumnList, ColumnListUser],
  [EventFn[B: Basics,][B], EventType, EventList, EventListUser],
);



fn check_equality<C: Column>(first: &FieldRc, second: &FieldRc)->bool {
  ::unwrap_field::<C>(first) == ::unwrap_field::<C>(second)
}
pub struct FieldEqualityTable (HashMap<ColumnId, fn(&FieldRc, &FieldRc)->bool>);
impl ColumnListUser for FieldEqualityTable{
  fn apply<C: Column>(&mut self) {
    self.0.insert(C::column_id(), check_equality::<C>);
  }
}
impl FieldEqualityTable{
  pub fn new <C: ColumnList>()->FieldEqualityTable{
    let mut result = FieldEqualityTable(HashMap::new());
    C::apply (&mut result);
    result
  }
  pub fn fields_are_equal(&self, column_id: ColumnId, first: & FieldRc, second: & FieldRc)->bool {
    (self.0.get (&column_id).expect ("Column missing from equality table; did you forget to list a column in Basics::Columns?")) (first, second)
  }
  pub fn options_are_equal(&self, column_id: ColumnId, first: Option <& FieldRc>, second: Option <& FieldRc>)->bool {
    match (first, second) {
      (None, None) => true,
      (Some (first), Some (second)) => self.fields_are_equal (column_id, first, second),
      _ => false,
    }
  }
}


  fn serialize_column<C: Column, S: Serializer>(field: &FieldRc,
                                                serializer: &mut S)
                                                -> Result<(), S::Error> {
    try!(::unwrap_field::<C>(field).serialize(serializer));
    Ok(())
  }

  fn deserialize_column<B: Basics, C: Column, M: de::MapVisitor>
    (visitor: &mut M)
     -> Result<(FieldRc, ExtendedTime<B>), M::Error> {
    let (data, time) = try!(visitor.visit_value::<(C::FieldType, ExtendedTime<B>)>());
    Ok((StewardRc::new(data), time))
  }

  pub struct FieldSerializationTable<S: Serializer>(HashMap<ColumnId,
                                                           fn(&FieldRc, &mut S)
                                                              -> Result<(), S::Error>>);
  pub struct FieldDeserializationTable <B: Basics, M: de::MapVisitor> (HashMap<ColumnId, fn (&mut M)->Result <(FieldRc, ExtendedTime <B>), M::Error>>);

  impl<S: Serializer> ColumnListUser for FieldSerializationTable<S> {
    fn apply<C: Column>(&mut self) {
      self.0.insert(C::column_id(), serialize_column::<C, S>);
    }
  }
  impl<B: Basics, M: de::MapVisitor> ColumnListUser for FieldDeserializationTable <B, M> {
    fn apply<C: Column>(&mut self) {
      self.0.insert(C::column_id(), deserialize_column::<B, C, M>);
    }
  }
  impl<S: Serializer> FieldSerializationTable<S> {
  pub fn new <C: ColumnList>()->FieldSerializationTable <S> {
    let mut result = FieldSerializationTable(HashMap::new());
    C::apply (&mut result);
    result
  }
  pub fn serialize_field(&self, column_id: ColumnId, first: & FieldRc,
                                                  serializer: &mut S)->Result<(), S::Error>{
    use serde::ser::Error;
    try!(self.0.get (&column_id).ok_or (S::Error::custom ("Column missing from serialization table; did you forget to list a column in Basics::Columns?"))) (first, serializer)
  }
  }
  impl<B: Basics, M: de::MapVisitor> FieldDeserializationTable <B, M> {
  pub fn new <C: ColumnList>()->FieldDeserializationTable <B, M> {
    let mut result = FieldDeserializationTable (HashMap::new());
    C::apply (&mut result);
    result
  }
  pub fn deserialize_field(&self, column_id: ColumnId, visitor: &mut M)->Result<(FieldRc, ExtendedTime<B>), M::Error>{
    try!(self.0.get (&column_id).ok_or (M::Error::custom ("Column missing from deserialization table; did you forget to list a column in Basics::Columns?"))) (visitor)
  }
  }


