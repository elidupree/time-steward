//! 

use super::list_of_types::{ListedTypeIndex, Sublist};
use std::ptr::{self, Shared};
use std::sync::atomic::AtomicUsize;
use std::any::Any;
use std::mem;

struct ArcInnerCommon <List: Sublist, CommonData: Any> {
  index: ListedTypeIndex <List>,
  references: AtomicUsize,
  data: CommonData,
}
#[repr(C)]
struct ArcInner <List: Sublist, CommonData: Any, SpecificData: Any> {
  common: ArcInnerAlways <List, CommonData>,
  data: SpecificData,
}
pub struct DynamicArc <List: Sublist, CommonData: Any> {
  pointer: Shared <ArcInnerCommon <List, CommonData>>,
}
pub struct TypedArc <List: Sublist, CommonData: Any, SpecificData: Any> {
  pointer: Shared <ArcInner <List, CommonData, SpecificData>>,
}

impl <List: Sublist, CommonData: Any, SpecificData: Any> TypedArc <List, CommonData, SpecificData> {
  fn erase_type (self)->DynamicArc <List, CommonData> {
    DynamicArc {pointer: unsafe {mem::transmute (self.pointer)}}
  }
}
impl <List: Sublist, CommonData: Any> DynamicArc <List, CommonData> {
  /// If it's not the correct type, return the original DynamicArc 
  fn downcast <SpecificData: Any> (self)->Result <TypedArc <List, CommonData, SpecificData>, DynamicArc <List, CommonData>> {
    if unsafe {self.pointer.as_ref().index} == List::index:: <SpecificData> () {
      Ok (TypedArc {pointer: unsafe {mem::transmute (self.pointer)}})
    }
    else {
      Err (self)
    }
  }
}
