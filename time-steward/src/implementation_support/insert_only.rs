use std::borrow::Borrow;
use std::boxed::Box;
use std::cell::{Cell, UnsafeCell};
use std::cmp::Eq;
use std::collections::hash_map::{self, Entry};
use std::collections::HashMap as Interior;
use std::hash::{BuildHasher, Hash};

#[derive(Debug)]
pub struct HashMap<K: Eq + Hash, V, S: BuildHasher = ::std::collections::hash_map::RandomState> {
  data: UnsafeCell<Interior<K, Box<V>, S>>,
  inserting: Cell<bool>,
}

impl<K: Eq + Hash, V> HashMap<K, V, ::std::collections::hash_map::RandomState> {
  pub fn new() -> HashMap<K, V, ::std::collections::hash_map::RandomState> {
    HashMap {
      data: UnsafeCell::new(Interior::new()),
      inserting: Cell::new(false),
    }
  }
}
impl<K: Eq + Hash, V, S: BuildHasher> HashMap<K, V, S> {
  pub fn with_hasher(hash_builder: S) -> HashMap<K, V, S> {
    HashMap {
      data: UnsafeCell::new(Interior::with_hasher(hash_builder)),
      inserting: Cell::new(false),
    }
  }
  pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> HashMap<K, V, S> {
    HashMap {
      data: UnsafeCell::new(Interior::with_capacity_and_hasher(capacity, hash_builder)),
      inserting: Cell::new(false),
    }
  }
  pub fn get_default<F>(&self, key: K, default_function: F) -> Option<&V>
  where
    F: FnOnce() -> Option<V>,
  {
    assert!(
      !self.inserting.get(),
      "Attempt to call get_default() on a insert_only::HashMap within the default_function \
       callback for another get_default() on the same map"
    );
    self.inserting.set(true);
    let result = match unsafe { (*self.data.get()).entry(key) } {
      Entry::Vacant(entry) => match default_function() {
        None => None,
        Some(value) => Some((*entry.insert(Box::new(value))).borrow()),
      },
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
    assert!(
      !self.inserting.get(),
      "Attempt to call len() on a insert_only::HashMap within the default_function \
       callback for a get_default() on the same map"
    );
    unsafe { (*self.data.get()).len() }
  }
}

impl<K: Eq + Hash, V, S: BuildHasher + Default> Default for HashMap<K, V, S> {
  fn default() -> HashMap<K, V, S> {
    HashMap::with_hasher(Default::default())
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

impl<K: Eq + Hash, V, S: BuildHasher> IntoIterator for HashMap<K, V, S> {
  type Item = (K, V);
  type IntoIter = IntoIter<K, V>;
  fn into_iter(self) -> Self::IntoIter {
    IntoIter {
      data: self.data.into_inner().into_iter(),
    }
  }
}
