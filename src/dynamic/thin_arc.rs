//! 

use super::list_of_types::{ListedTypeIndex, SubListRepresentative};
use std::ptr::{self, Shared};
use std::sync::atomic::AtomicUsize;

struct ThinArcInnerAlways <List: SubListRepresentative, AlwaysData: Any> {
  index: ListedTypeIndex <List>,
  references: atomic::AtomicUsize;
  data: AlwaysData,
}
#[repr(C)]
struct ThinArcInner <List: SubListRepresentative, AlwaysData: Any, SpecificData: Any> {
  always: ThinArcInnerAlways <List>,
  data: SpecificData,
}
pub struct ThinArc <List: SubListRepresentative, AlwaysData: Any> {
  pointer: Shared <ThinArcInnerAlways <List, AlwaysData>>;
}
