//! 

use super::list_of_types::{ListedTypeIndex, Sublist};
use std::ptr::{self, Shared};
use std::sync::atomic::AtomicUsize;
use std::any::Any;
use std::mem;

struct ArcInnerAlways <List: Sublist, AlwaysData: Any> {
  index: ListedTypeIndex <List>,
  references: AtomicUsize,
  data: AlwaysData,
}
#[repr(C)]
struct ArcInner <List: Sublist, AlwaysData: Any, SpecificData: Any> {
  always: ArcInnerAlways <List, AlwaysData>,
  data: SpecificData,
}
pub struct DynamicArc <List: Sublist, AlwaysData: Any> {
  pointer: Shared <ArcInnerAlways <List, AlwaysData>>,
}
pub struct TypedArc <List: Sublist, AlwaysData: Any, SpecificData: Any> {
  pointer: Shared <ArcInner <List, AlwaysData, SpecificData>>,
}

impl <List: Sublist, AlwaysData: Any, SpecificData: Any> TypedArc <List, AlwaysData, SpecificData> {
  fn erase_type (self)->DynamicArc <List, AlwaysData> {
    DynamicArc {pointer: unsafe {mem::transmute (self.pointer)}}
  }
}
impl <List: Sublist, AlwaysData: Any> DynamicArc <List, AlwaysData> {
  /// If it's not the correct type, return the original DynamicArc 
  fn downcast <SpecificData: Any> (self)->Result <TypedArc <List, AlwaysData, SpecificData>, DynamicArc <List, AlwaysData>> {
    if unsafe {self.pointer.as_ref().index} == List::index:: <SpecificData> () {
      Ok (TypedArc {pointer: unsafe {mem::transmute (self.pointer)}})
    }
    else {
      Err (self)
    }
  }
}
