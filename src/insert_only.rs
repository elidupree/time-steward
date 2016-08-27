use std::boxed::Box;
use std::collections::HashMap as Interior;
use std::hash::Hash;
use std::cmp::Eq;
use std::borrow::Borrow;
use std::cell::{Cell, UnsafeCell};
use std::collections::hash_map::Entry;

pub struct HashMap<K: Eq + Hash, V> {
  data: UnsafeCell<Interior<K, Box<V>>>,
  inserting: Cell<bool>,
}

impl<K: Eq + Hash, V> HashMap<K, V> {
  pub fn new() -> HashMap<K, V> {
    HashMap {
      data: UnsafeCell::new(Interior::new()),
      inserting: Cell::new(false),
    }
  }
  pub fn get_default<F>(&self, key: K, default_function: F) -> Option <&V>
    where F: FnOnce() -> Option <V>
  {
    assert!(!self.inserting.get(),
            "Attempt to call get_default on a insert_only::HashMap within the default_function \
             callback for another get_default on the same map");
    self.inserting.set(true);
    let result = match unsafe { (*self.data.get()).entry(key) } {
      Entry::Vacant (entry) => match default_function() {
        None => None,
        Some (value) => Some ((*entry.insert (Box::new (value))).borrow()),
      },
      Entry::Occupied (entry) => Some ((*entry.into_mut()).borrow()),
    };
    self.inserting.set (false);
    result
  }
}
