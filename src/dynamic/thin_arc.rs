//! 

use super::list_of_types::{ListedTypeIndex, Sublist};
use std::ptr::{self, Shared};
use std::sync::atomic::AtomicUsize;
use std::any::Any;
use std::mem;

struct ThinArcInnerAlways <List: Sublist, AlwaysData: Any> {
  index: ListedTypeIndex <List>,
  references: AtomicUsize,
  data: AlwaysData,
}
#[repr(C)]
struct ThinArcInner <List: Sublist, AlwaysData: Any, SpecificData: Any> {
  always: ThinArcInnerAlways <List, AlwaysData>,
  data: SpecificData,
}
pub struct DynamicThinArc <List: Sublist, AlwaysData: Any> {
  pointer: Shared <ThinArcInnerAlways <List, AlwaysData>>,
}
pub struct ThinArc <List: Sublist, AlwaysData: Any, SpecificData: Any> {
  pointer: Shared <ThinArcInner <List, AlwaysData, SpecificData>>,
}

impl <List: Sublist, AlwaysData: Any, SpecificData: Any> ThinArc <List, AlwaysData, SpecificData> {
  fn erase_type (self)->DynamicThinArc <List, AlwaysData> {
    DynamicThinArc {pointer: unsafe {mem::transmute (self.pointer)}}
  }
}
impl <List: Sublist, AlwaysData: Any> DynamicThinArc <List, AlwaysData> {
  /// If it's not the correct type, return the original DynamicThinArc 
  fn downcast <SpecificData: Any> (self)->Result <ThinArc <List, AlwaysData, SpecificData>, DynamicThinArc <List, AlwaysData>> {
    if unsafe {self.pointer.as_ref().index} == List::index:: <SpecificData> () {
      Ok (ThinArc {pointer: unsafe {mem::transmute (self.pointer)}})
    }
    else {
      Err (self)
    }
  }
}
