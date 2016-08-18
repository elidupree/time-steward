use std::boxed::Box;
use std::collections::HashMap as Interior;
use std::hash::Hash;
use std::cmp::Eq;
use std::borrow::Borrow;
use std::cell::UnsafeCell;

pub struct HashMap<K: Eq + Hash, V> {
  data: UnsafeCell<Interior<K, Box<V>>>,
  inserting: bool,
}

impl<K: Eq + Hash, V> HashMap<K, V> {
  pub fn new() -> HashMap<K, V> {
    HashMap { data: UnsafeCell::new(Interior::new()), inserting: false }
  }
  pub fn get_default<F>(&self, key: K, default_function: F) -> &V
    where F: FnOnce() -> V
  {
    assert!(!self.inserting, "Attempt to call get_default on a insert_only::HashMap within the default_function callback for another get_default on the same map");
    self.inserting = true;
    let entry = unsafe { (*self.data.get()).entry(key) };
    let result = (*entry.or_insert_with(|| Box::new(default_function()))).borrow();
    self.inserting = false;
    result
  }
}
