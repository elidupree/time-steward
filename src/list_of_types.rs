use std::collections::HashMap;
use std::any::Any;
use std::marker::PhantomData;
use serde::{Serialize, Serializer, Error, de};

use {ExtendedTime, StewardRc, FieldRc, ColumnId, Column, Basics, EventFn, PredictorFn,};

enum Void {}

macro_rules! type_list_definitions {
($Trait: ident, $Wrapper: ident, $List: ident, $User: ident) => {
pub struct $Wrapper<T: $Trait>(PhantomData <T>, Void);
pub trait $User {
  fn apply<C: $Trait>(&mut self);
}
pub trait $List: Any {
  fn apply<U: $User>(user: &mut U);
}
impl<T: $Trait> $List for $Wrapper<T> {
  #[inline]
  fn apply<U: $User>(user: &mut U) {
    user.apply::<T>();
  }
}
tuple_impls! ($List, $User, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
};

($Trait: ident<B>, $Wrapper: ident, $List: ident, $User: ident) => {
pub struct $Wrapper<B: Basics, T: $Trait <B>>(PhantomData <B>, PhantomData <T>, Void);
pub trait $User <B: Basics> {
  fn apply<C: $Trait <B>>(&mut self);
}
pub trait $List <B: Basics>: Any {
  fn apply<U: $User <B>>(user: &mut U);
}
impl<B: Basics, T: $Trait <B>> $List <B> for $Wrapper<B, T> {
  #[inline]
  fn apply<U: $User <B>>(user: &mut U) {
    user.apply::<T>();
  }
}
tuple_impls! (B $List, $User, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31);
};

}
macro_rules! tuple_impls {
  ($List: ident, $User: ident, $TL: ident $(, $T: ident)*) => {
    impl<$($T,)* $TL> $List for ($($T,)* $TL,)
      where $($T: $List,)* $TL: $List
    {
      #[inline]
      fn apply <U: $User> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! ($List, $User, $($T),*);
  };
  (B $List: ident, $User: ident, $TL: ident $(, $T: ident)*) => {
    impl<B: Basics, $($T,)* $TL> $List <B> for ($($T,)* $TL,)
      where $($T: $List <B>,)* $TL: $List <B>
    {
      #[inline]
      fn apply <U: $User <B>> (user: &mut U) {
        $($T::apply(user);)*
        $TL::apply(user);
      }
    }
    tuple_impls! (B $List, $User, $($T),*);
  };
  ($List: ident, $User: ident,) => {};
  (B $List: ident, $User: ident,) => {};
}
macro_rules! pair_null_impls {
([$Trait0: ident, $Wrapper0: ident, $List0: ident, $User0: ident][$Trait1: ident, $Wrapper1: ident, $List1: ident, $User1: ident]) => {
impl<T: $Trait0> $List1 for $Wrapper0<T> {
  #[inline]
  fn apply<U: $User1>(_: &mut U) {}
}
impl<T: $Trait1> $List0 for $Wrapper1<T> {
  #[inline]
  fn apply<U: $User0>(_: &mut U) {}
}
};

([$Trait0: ident, $Wrapper0: ident, $List0: ident, $User0: ident][$Trait1: ident <B>, $Wrapper1: ident, $List1: ident, $User1: ident]) => {
impl<B: Basics, T: $Trait0> $List1 <B> for $Wrapper0<T> {
  #[inline]
  fn apply<U: $User1 <B>>(_: &mut U) {}
}
impl<B: Basics, T: $Trait1 <B>> $List0 for $Wrapper1<B, T> {
  #[inline]
  fn apply<U: $User0>(_: &mut U) {}
}
};

([$Trait0: ident <B>, $Wrapper0: ident, $List0: ident, $User0: ident][$Trait1: ident <B>, $Wrapper1: ident, $List1: ident, $User1: ident]) => {
impl<B: Basics, T: $Trait0 <B>> $List1 <B> for $Wrapper0<B, T> {
  #[inline]
  fn apply<U: $User1 <B>>(_: &mut U) {}
}
impl<B: Basics, T: $Trait1 <B>> $List0 <B> for $Wrapper1<B, T> {
  #[inline]
  fn apply<U: $User0 <B>>(_: &mut U) {}
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
  all_null_impls! ($([$($info)*])*);
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
  [Column, ColumnType, ColumnList, ColumnListUser]
  [EventFn<B>, EventType, EventList, EventListUser]
  [PredictorFn<B>, PredictorType, PredictorList, PredictorListUser]
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


