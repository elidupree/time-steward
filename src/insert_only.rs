use std::boxed::Box;
use std::collections::HashMap as Interior;
use std::hash::Hash;
use std::cmp::Eq;
use std::borrow::Borrow;
use std::cell::UnsafeCell;

pub struct HashMap<K: Eq + Hash, V> {
  data: UnsafeCell<Interior<K, Box<V>>>,
}

impl<K: Eq + Hash, V> HashMap<K, V> {
  pub fn new() -> HashMap<K, V> {
    HashMap { data: UnsafeCell::new(Interior::new()) }
  }
  pub fn get_default<F>(&self, key: K, default_function: F) -> &V
    where F: FnOnce() -> V
  {
    let entry = unsafe { (*self.data.get()).entry(key) };
    (*entry.or_insert_with(|| Box::new(default_function()))).borrow()
  }
}
