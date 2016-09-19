use std::boxed::Box;
use std::collections::HashMap as Interior;
use std::hash::Hash;
use std::cmp::Eq;
use std::borrow::Borrow;
use std::cell::{Cell, UnsafeCell};
use std::collections::hash_map::{self, Entry};

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
  pub fn with_capacity(capacity: usize) -> HashMap<K, V> {
    HashMap {
      data: UnsafeCell::new(Interior::with_capacity(capacity)),
      inserting: Cell::new(false),
    }
  }
  pub fn get_default<F>(&self, key: K, default_function: F) -> Option<&V>
    where F: FnOnce() -> Option<V>
  {
    assert!(!self.inserting.get(),
            "Attempt to call get_default() on a insert_only::HashMap within the default_function \
             callback for another get_default() on the same map");
    self.inserting.set(true);
    let result = match unsafe { (*self.data.get()).entry(key) } {
      Entry::Vacant(entry) => {
        match default_function() {
          None => None,
          Some(value) => Some((*entry.insert(Box::new(value))).borrow()),
        }
      }
      Entry::Occupied(entry) => Some((*entry.into_mut()).borrow()),
    };
    self.inserting.set(false);
    result
  }

  // if you are holding a &mut HashMap, you can also use it like a regular map
  pub fn insert(&mut self, key: K, value: V) -> Option<V> {
    unsafe { (*self.data.get()).insert(key, Box::new(value)) }.map(|something| *something)
  }

  pub fn len(&self) -> usize {
    assert!(!self.inserting.get(),
                "Attempt to call len() on a insert_only::HashMap within the default_function \
                 callback for a get_default() on the same map");
    unsafe { (*self.data.get()).len() }
  }
}

pub struct IntoIter<K, V> {
  data: hash_map::IntoIter<K, Box<V>>,
}
impl<K, V> Iterator for IntoIter<K, V> {
  type Item = (K, V);
  fn next(&mut self) -> Option<Self::Item> {
    self.data.next().map(|(key, value)| (key, *value))
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.data.size_hint()
  }
}

impl<K: Eq + Hash, V> IntoIterator for HashMap<K, V> {
  type Item = (K, V);
  type IntoIter = IntoIter<K, V>;
  fn into_iter(self) -> Self::IntoIter {
    IntoIter { data: unsafe { self.data.into_inner().into_iter() } }
  }
}
