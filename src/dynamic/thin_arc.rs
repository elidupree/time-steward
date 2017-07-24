//! 

use super::list_of_types::{ListedTypeIndex, Sublist};
use std::ptr::{self, Shared};
use std::sync::atomic::AtomicUsize;
use std::any::Any;

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
pub struct ThinArc <List: Sublist, AlwaysData: Any> {
  pointer: Shared <ThinArcInnerAlways <List, AlwaysData>>,
}
