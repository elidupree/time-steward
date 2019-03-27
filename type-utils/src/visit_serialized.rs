//! Piggyback on the Serialize trait to scan an object for members of specific types.
//!
//! This is not rigorous! If the serialize impl of the target object is called by another serialize impl rather than implicitly through the serializer, the object is missed! We account the only builtin case of this (deref impls), but custom impls could easily violate this condition. Probably, this should be eventually replaced by my own custom derive.

use serde::{ser, Serialize};
use std::fmt::{self,Display};
use std::ops::Deref;


pub trait VisitAny {
  fn visit<T: Serialize + ?Sized>(&mut self, value: & T) {
    self.visit_specific(value);
  }
}
pub trait VisitSpecific<T: Serialize + ?Sized> {
  fn visit_specific(&mut self, value: & T);
}

impl <V: VisitAny + ?Sized, T: Serialize + ?Sized> VisitSpecific<T> for V {
  default fn visit_specific(&mut self, value: & T) {
    //eprintln!("{}", unsafe{std::intrinsics::type_name::<T>()});
    value.serialize(VisitingSerializerHack(self)).unwrap();
  }
}
impl <'a, V: VisitAny + ?Sized, T: Serialize + Deref + ?Sized> VisitSpecific<T> for V where T::Target: Serialize {
  default fn visit_specific(&mut self, value: &T) {
    //eprintln!("deref {}", unsafe{std::intrinsics::type_name::<T>()});
    self.visit_specific(&**value);
  }
}

struct VisitingSerializerHack<'a, V: VisitAny + ?Sized>(&'a mut V);



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
impl<'a, V: VisitAny + ?Sized> ser::Serializer for VisitingSerializerHack<'a, V> {
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
  fn serialize_some<T>(self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn serialize_unit(self) -> Result<(),NeverError> { Ok(()) }
  fn serialize_unit_struct(self, _name: &'static str) -> Result<(),NeverError> { Ok(()) }
  fn serialize_unit_variant(
      self,
      _name: &'static str,
      __variant_index: u32,
      _variant: &'static str
  ) -> Result<(),NeverError> { Ok(()) }
  fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize{ self.0.visit(value); Ok(()) }
  fn serialize_newtype_variant<T>(
      self,
      _name: &'static str,
      _variant_index: u32,
      _variant: &'static str,
      value: &T
  )  -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }

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
impl<'a, V: VisitAny + ?Sized> ser::SerializeSeq for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeTuple for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeTupleStruct for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeTupleVariant for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeMap for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_key<T>(&mut self, key: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(key); Ok(())  }
  fn serialize_value<T>(&mut self, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeStruct for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, V: VisitAny + ?Sized> ser::SerializeStructVariant for VisitingSerializerHack<'a, V> {
  type Ok = ();
  type Error = NeverError;
  fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
      where T: ?Sized + Serialize { self.0.visit(value); Ok(()) }
  fn end(self) -> Result<(),NeverError> { Ok(()) }
}

#[cfg (test)]
mod tests {
  use super::*;

  #[test]
  fn test_visit_serialized() {
    #[derive (PartialEq, Eq, Serialize)]
    struct ToVisit(i32);
    #[derive (Serialize)]
    struct Wrapper(ToVisit);
    struct Visitor <'a> (& 'a mut Vec<i32>);
    impl <'a> VisitAny for Visitor <'a> {}
    impl <'a> VisitSpecific<ToVisit> for Visitor <'a> {
      fn visit_specific(&mut self, value: &ToVisit) {
        //eprintln!("q");
        self.0.push (value.0);
      }
    }
    
    let mut result = Vec::new() ;
    let mut visitor = Visitor (&mut result);
    visitor.visit (&(ToVisit(0), 1i64, 2i32, vec![ToVisit(3), ToVisit(4)], vec![Some(ToVisit(5)), None, Some(ToVisit(6))], Some(ToVisit(7)), &&&&mut&& ToVisit(8), Box::new(ToVisit(9)), Wrapper(ToVisit(10))));
    
    assert_eq!(result, vec![0, 3, 4, 5, 6, 7, 8, 9, 10])
  }
}
