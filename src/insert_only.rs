use std::boxed::Box;
use std::collections::HashMap as Interior;
use std::hash::Hash;
use std::cmp::Eq;
use std::marker;
use std::borrow::Borrow;

pub struct HashMap <'a, K: Eq + Hash, V: 'a> {
  data: Interior <K, Box <V>>,
  _marker: marker::PhantomData <& 'a V>,
}

impl<'a, K: Eq + Hash, V> HashMap<'a, K, V> {
  pub fn new()->HashMap <'a, K, V> {
    HashMap {data: Interior::new(),_marker:marker::PhantomData }
  }
  pub fn insert (&mut self, key: K, value: V)->Option <V> {
    self.data.insert (key, Box::new (value)).map (| failure | *failure)
  }
  pub fn get <'b, Q: ?Sized> (& 'b self, key: & Q)->Option <& 'a V> where K: Borrow <Q>, Q: Hash + Eq {
    self.data.get (key).map (| box_ref | unsafe {(&**box_ref as *const V).as_ref().expect("box is supposed to be non-nullable")})
  }
}
