//! Piggyback on the Serialize trait to scan an object for members of specific types.
//!
//! This can only visit specific types but implement VisitTarget and use a custom Serialize impl to make it possible. Probably, this should be eventually replaced by my own custom derive.

use serde::{ser, Serialize, Serializer};
use std::fmt::{self,Display};
use std::marker::PhantomData;


trait MaybeVisitAny {
  fn visit_any<T: Serialize + ?Sized>(&mut self, value: & T)->bool;
}
pub trait Visit<T: Serialize + ?Sized> {
  fn visit(&mut self, value: & T);
}
trait MaybeVisit<T: Serialize + ?Sized> {
  fn maybe_visit(&mut self, value: & T)->bool;
}

impl <V: ?Sized> MaybeVisitAny for V {
  fn visit_any<T: Serialize + ?Sized>(&mut self, value: & T)->bool {
    self.maybe_visit(value)
  }
}
impl <V: ?Sized, T: Serialize + ?Sized> MaybeVisit<T> for V {
  default fn maybe_visit(&mut self, _value: & T)->bool {
    false
  }
}
impl <V: Visit<T> + ?Sized, T: Serialize + ?Sized> MaybeVisit<T> for V {
  fn maybe_visit(&mut self, value: & T)->bool {
    self.visit(value);
    true
  }
}

pub trait VisitTarget: Serialize {}



trait MaybeVisitTarget: Serialize {
  fn verify (&self);
}


impl <T: Serialize + ?Sized> MaybeVisitTarget for T {
  default fn verify (&self) {}
}

impl <T: VisitTarget + ?Sized> MaybeVisitTarget for T {
  fn verify (&self) {
    struct Visitor <'a, U: ?Sized> (& 'a mut i32, PhantomData <*const U>);
    impl <'a, U: Serialize + ?Sized> Visit<U> for Visitor <'a, U> {
      fn visit(&mut self, _value: &U) {
        *self.0 += 1;
      }
    }
    eprintln!(" {:?} ",()) ;
    let mut count: i32 = 0;
    let mut visitor: Visitor <T> = Visitor (&mut count, PhantomData);
    visit_all(&mut visitor, self);
    assert_eq!(count, 1, "a type implemented VisitTarget without handling the serialization correctly");
  }
}

trait MaybeVisitingSerializerHack: Serializer {
  fn maybe_apply_visitor<T: VisitTarget + ?Sized>(&mut self, value: & T)->Option<Result<Self::Ok, Self::Error>>;
}

impl <S: Serializer> MaybeVisitingSerializerHack for S {
  default fn maybe_apply_visitor<T: VisitTarget + ?Sized>(&mut self, _value: & T)->Option<Result<Self::Ok, Self::Error>> {None}
}

impl <'a, V: ?Sized> MaybeVisitingSerializerHack for VisitingSerializerHack<'a, V> {
  fn maybe_apply_visitor<T: VisitTarget + ?Sized>(&mut self, value: & T)->Option<Result<Self::Ok, Self::Error>> {
    if self.0.maybe_visit (value) {
      Some(Ok(()))
    } else {
      None
    }
  }
}

pub fn maybe_apply_visitor<S: Serializer, T: VisitTarget + ?Sized>(serializer: &mut S, value: & T)->Option<Result<S::Ok, S::Error>> {
  serializer.maybe_apply_visitor(value)
}

pub fn visit_all<T: Serialize + ?Sized, V: ?Sized>(visitor: &mut V, value: & T) {
  value.serialize (VisitingSerializerHack(visitor)).unwrap();
}

#[macro_export]
macro_rules! maybe_apply_visitor {
  ($value: expr, $serializer: expr) => {
    if let Some(a) = maybe_apply_visitor(&mut $serializer, $value) {return a}
  };
}

struct VisitingSerializerHack<'a, V: ?Sized>(&'a mut V);



#[allow (unreachable_code, unreachable_patterns)]
#[derive (Debug)]
struct NeverError(!);
impl ser::Error for NeverError {
  fn custom<T: Display>(_msg: T) -> Self {
      panic!("TimeSteward doesn't know how to handle errors in serialization")
  }
}
impl ::std::error::Error for NeverError {
  fn description(&self) -> &str {
      unreachable!()
  }
}
impl Display for NeverError {
  fn fmt(&self, _formatter: &mut fmt::Formatter) -> fmt::Result {
      unreachable!()
  }
}
impl<'a, V: ?Sized> VisitingSerializerHack<'a, V> {
  fn handle<T: Serialize + ?Sized> (&mut self, value: & T) -> Result<(),NeverError> {
    value.verify();
    visit_all(self.0, value);
    Ok(())
  }
}
impl<'a, V: ?Sized> ser::Serializer for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;

  type SerializeSeq = Self;
  type SerializeTuple = Self;
  type SerializeTupleStruct = Self;
  type SerializeTupleVariant = Self;
  type SerializeMap = Self;
  type SerializeStruct = Self;
  type SerializeStructVariant = Self;

  fn serialize_bool(self, _: bool) -> Result<(),NeverError> { Ok(()) }
  fn serialize_i8(self, _: i8) -> Result<(),NeverError> { Ok(()) }
  fn serialize_i16(self, _: i16) -> Result<(),NeverError> { Ok(()) }
  fn serialize_i32(self, _: i32) -> Result<(),NeverError> { Ok(()) }
  fn serialize_i64(self, _: i64) -> Result<(),NeverError> { Ok(()) }
  fn serialize_u8(self, _: u8) -> Result<(),NeverError> { Ok(()) }
  fn serialize_u16(self, _: u16) -> Result<(),NeverError> { Ok(()) }
  fn serialize_u32(self, _: u32) -> Result<(),NeverError> { Ok(()) }
  fn serialize_u64(self, _: u64) -> Result<(),NeverError> { Ok(()) }
  fn serialize_f32(self, _: f32) -> Result<(),NeverError> { Ok(()) }
  fn serialize_f64(self, _: f64) -> Result<(),NeverError> { Ok(()) }
  fn serialize_char(self, _: char) -> Result<(),NeverError> { Ok(()) }
  fn serialize_str(self, _: &str) -> Result<(),NeverError> { Ok(()) }
  fn serialize_bytes(self, _: &[u8]) -> Result<(),NeverError> { Ok(()) }
  fn serialize_none(self) -> Result<(),NeverError> { Ok(()) }
  fn serialize_some<T>(mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn serialize_unit(self) -> Result<(),NeverError> { Ok(()) }
  fn serialize_unit_struct(self, _name: &'static str) -> Result<(),NeverError> { Ok(()) }
  fn serialize_unit_variant(
      self,
      _name: &'static str,
      __variant_index: u32,
      _variant: &'static str
  ) -> Result<(),NeverError> { Ok(()) }
  fn serialize_newtype_struct<T>(mut self, _name: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize{ self.handle(value) }
  fn serialize_newtype_variant<T>(
      mut self,
      _name: &'static str,
      _variant_index: u32,
      _variant: &'static str,
      value: &T
  )  -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }

  fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq,NeverError> { Ok(self) }
  fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple,NeverError> { Ok(self) }
  fn serialize_tuple_struct(
      self,
      _name: &'static str,
      _len: usize
  ) -> Result<Self::SerializeTupleStruct,NeverError> { Ok(self) }
  fn serialize_tuple_variant(
      self,
      _name: &'static str,
      _variant_index: u32,
      _variant: &'static str,
      _len: usize
  ) -> Result<Self::SerializeTupleVariant,NeverError> { Ok(self) }
  fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap,NeverError> { Ok(self) }
  fn serialize_struct(
      self,
      _name: &'static str,
      _len: usize
  ) -> Result<Self::SerializeStruct,NeverError> { Ok(self) }
  fn serialize_struct_variant(
      self,
      _name: &'static str,
      _variant_index: u32,
      _variant: &'static str,
      _len: usize
  ) -> Result<Self::SerializeStructVariant,NeverError> { Ok(self) }
}
impl<'a, V: ?Sized> ser::SerializeSeq for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeTuple for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeTupleStruct for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeTupleVariant for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeMap for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_key<T>(&mut self, key: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(key) }
  fn serialize_value<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeStruct for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: ?Sized> ser::SerializeStructVariant for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.handle(value) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}

#[cfg (test)]
mod tests {
  use super::*;

  #[test]
  fn test_visit_serialized() {
    #[derive (PartialEq, Eq)]
    struct ToVisit(i32);
    impl VisitTarget for ToVisit {}
    impl Serialize for ToVisit {
      fn serialize<S: Serializer>(&self, mut serializer: S) -> Result<S::Ok, S::Error> {
        maybe_apply_visitor! (self, serializer);
        panic!()
      }
    }
    
    #[derive (Serialize)]
    struct Wrapper(ToVisit);
    #[derive (PartialEq, Eq)]
    struct WrapperThatIsAlsoUnusedTarget<T>(T);
    impl<T: Serialize> VisitTarget for WrapperThatIsAlsoUnusedTarget<T> {}
    impl<T: Serialize> Serialize for WrapperThatIsAlsoUnusedTarget<T> {
      fn serialize<S: Serializer>(&self, mut serializer: S) -> Result<S::Ok, S::Error> {
        maybe_apply_visitor! (self, serializer);
        self.0.serialize(serializer)
      }
    }
    
    struct Visitor <'a> (& 'a mut Vec<i32>);
    impl <'a> Visit<ToVisit> for Visitor <'a> {
      fn visit(&mut self, value: &ToVisit) {
        //eprintln!("q");
        self.0.push (value.0);
      }
    }
    
    let mut result = Vec::new() ;
    let mut visitor = Visitor (&mut result);
    visit_all(&mut visitor, &(ToVisit(0), 1i64, 2i32, vec![ToVisit(3), ToVisit(4)], vec![Some(ToVisit(5)), None, Some(ToVisit(6))], Some(ToVisit(7)), &&&&mut&& ToVisit(8), Box::new(ToVisit(9)), Wrapper(ToVisit(10)), WrapperThatIsAlsoUnusedTarget(ToVisit(11))));
    
    assert_eq!(result, vec![0, 3, 4, 5, 6, 7, 8, 9, 10, 11])
  }
  
  #[test]
  #[should_panic (expected ="a type implemented VisitTarget without handling the serialization correctly")]
  fn test_catching_visit_target_implemented_wrongly() {
    #[derive (PartialEq, Eq, Serialize)]
    struct ToVisit(i32);
    impl VisitTarget for ToVisit {}

    struct Visitor <'a> (& 'a mut Vec<i32>);
    impl <'a> Visit<ToVisit> for Visitor <'a> {
      fn visit(&mut self, value: &ToVisit) {
        //eprintln!("q");
        self.0.push (value.0);
      }
    }
    
    let mut result = Vec::new() ;
    let mut visitor = Visitor (&mut result);
    visit_all(&mut visitor, &(ToVisit(0), 1i64, 2i32, vec![ToVisit(3), ToVisit(4)], vec![Some(ToVisit(5)), None, Some(ToVisit(6))], Some(ToVisit(7)), &&&&mut&& ToVisit(8), Box::new(ToVisit(9))));
  }
}
