/*!

Interior mutability based on the guarantee that only one Accessor (or only immutable Accessors) exist within each thread at a time.

To access the contents of a AccessorCell, you must be holding a ReadGuard or WriteGuard.
The guard types are !Send and !Sync, and constructing one performs runtime checks against a thread_local.
AccessorCell is !Sync, meaning that if you're holding a guard and have a &AccessorCell,
it is safe to access the contents of the cell, because there cannot be any other guard
that would allow conflicting access. On the other hand, AccessorCell *can* be Send.

*/

use std::cell::{Cell, UnsafeCell};
use std::fmt::Debug;

pub struct AccessorCell<T: ?Sized> {
  value: UnsafeCell<T>,
}

unsafe impl<T: ?Sized> Send for AccessorCell<T> {}

#[derive(Debug)]
pub struct ReadGuard {
  _private: (),
}
#[derive(Debug)]
pub struct WriteGuard {
  _private: (),
}
pub trait ReadAccess {
  fn read<'a, T: ?Sized>(&'a self, cell: &'a AccessorCell<T>) -> &'a T;
}

impl<T> AccessorCell<T> {
  pub fn new(value: T) -> Self {
    AccessorCell {
      value: UnsafeCell::new(value),
    }
  }
}

thread_local! {
  static ACCESSOR_BORROW_STATE: Cell<usize> = Cell::new(0);
}
const WRITING: usize = usize::MAX;

impl ReadGuard {
  pub fn claim() -> Self {
    ACCESSOR_BORROW_STATE.with(|borrow_state| {
      let old = borrow_state.get();
      if old >= WRITING - 1 {
        if old == WRITING {
          panic!("tried to construct an accessor_cell::ReadGuard when there was already a WriteGuard in the same thread")
        } else {
          panic!("overflowed the accessor_cell read count")
        }
      }
      borrow_state.set(old + 1);
      ReadGuard { _private: () }
    })
  }
}
impl ReadAccess for ReadGuard {
  #[inline(always)]
  fn read<'a, T: ?Sized>(&'a self, cell: &'a AccessorCell<T>) -> &'a T {
    unsafe { &*cell.value.get() }
  }
}

impl Drop for ReadGuard {
  fn drop(&mut self) {
    ACCESSOR_BORROW_STATE.with(|borrow_state| {
      let old = borrow_state.get();
      debug_assert!(old > 0);
      debug_assert_ne!(old, WRITING);
      borrow_state.set(old - 1);
    })
  }
}

impl WriteGuard {
  pub fn claim() -> Self {
    ACCESSOR_BORROW_STATE.with(|borrow_state| {
      let old = borrow_state.get();
      if old != 0 {
        if old == WRITING {
          panic!("tried to construct an accessor_cell::WriteGuard when there was already a WriteGuard in the same thread")
        }
        else {
          panic!("tried to construct an accessor_cell::WriteGuard when there was already a ReadGuard in the same thread")
        }
      }
      borrow_state.set(WRITING);
      WriteGuard{ _private: () }
    })
  }
  #[inline(always)]
  pub fn write<'a, T: ?Sized>(&'a mut self, cell: &'a AccessorCell<T>) -> &'a mut T {
    unsafe { &mut *cell.value.get() }
  }
}
impl ReadAccess for WriteGuard {
  #[inline(always)]
  fn read<'a, T: ?Sized>(&'a self, cell: &'a AccessorCell<T>) -> &'a T {
    unsafe { &*cell.value.get() }
  }
}

impl Drop for WriteGuard {
  fn drop(&mut self) {
    ACCESSOR_BORROW_STATE.with(|borrow_state| {
      let old = borrow_state.get();
      debug_assert_eq!(old, WRITING);
      borrow_state.set(0);
    })
  }
}

impl<T: Debug + ?Sized> Debug for AccessorCell<T> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    ACCESSOR_BORROW_STATE.with(|borrow_state| {
      if borrow_state.get() == WRITING {
        write!(f, "AccessorCell(mutably borrowed)")
      } else {
        write!(f, "AccessorCell({:?})", ReadGuard::claim().read(self))
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn valid_use() {
    let cell = AccessorCell::new(4);
    let a = ReadGuard::claim();
    ReadGuard::claim();
    assert_eq!(a.read(&cell), &4);
    drop(a);
    *WriteGuard::claim().write(&cell) = 5;
    assert_eq!(ReadGuard::claim().read(&cell), &5);
    assert_eq!(WriteGuard::claim().read(&cell), &5);
  }

  #[test]
  #[should_panic(expected = "accessor_cell")]
  fn read_write() {
    let _a = ReadGuard::claim();
    WriteGuard::claim();
  }

  #[test]
  #[should_panic(expected = "accessor_cell")]
  fn write_read() {
    let _a = WriteGuard::claim();
    ReadGuard::claim();
  }
}
