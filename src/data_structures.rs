mod PartiallyPersistentNonindexedSet {
  use std::sync::{Arc, RwLock};
  use std::cmp::min;
  use std::collections::HashSet;
  use std::hash::Hash;
  use std::mem;
  use std::cell::{Cell, UnsafeCell};
  #[derive (Clone)]
  struct Entry <K: Clone + Eq> {
    key: K,
    insertion: bool,
  }
  //using Vec< in this buffer type has unnecessary indirection, but I was unable to find
  //a library that does the job safely without it.
  //Compare Arc<[Entry<K>]>, which could behave unsafely if I
  //didn't initialize it, but would waste time if I did.
  //
  //Even though I am technically using the RwLock type,
  //this is a lock-free data structure because we never
  //actually call RwLock::write(), only new() and read().
  //(TODO: is there a better primitive for this?)
  #[derive (Clone)]
  struct Buffer <K: Clone + Eq> {
    data: Arc <UnsafeCell <Vec<RwLock <Entry <K>>>>>,
    deletions: usize,
  }
  unsafe impl<K: Clone + Eq> Send for Buffer <K>{}
  unsafe impl<K: Clone + Eq> Sync for Buffer <K>{}
  
  pub struct Snapshot <K: Clone + Eq> {
    buffer: Buffer <K>,
    used_length: usize,
  }
  pub struct SnapshotIter <'a, K: Clone + Eq + Hash + 'a> {
    snapshot: & 'a Snapshot <K>,
    position: usize,
    ignore: HashSet <K>,
  }

  pub struct Set <K: Clone + Eq> {
    live_buffer: Buffer <K>,
    next_buffer: Buffer <K>,
    next_transfer_index: usize,
    operating: bool,
  }
  const MAX_TRANSFER_SPEED: usize = 8;
  
  impl<K: Clone + Eq + Hash> Snapshot <K> {
    pub fn iter <'a> (& 'a self)->SnapshotIter <'a, K> {SnapshotIter {snapshot: self, position: self.used_length, ignore: HashSet::with_capacity(self.buffer.deletions),}}
  }
  impl<'a, K: Clone + Eq + Hash> SnapshotIter <'a, K> {
    /// Do a single iteration step, which MAY return an iteration result,
    /// and is guaranteed to finish in O(1) time (worst-case except for bad luck with hashing).
    pub fn step (&mut self)->Option <Option <K>> {
      if self.position == 0 {return Some (None);}
      self.position -= 1;
      let entry = unsafe {(*self.snapshot.buffer.data.get()) [self.position].read().unwrap()};
      if entry.insertion == false {
        self.ignore.insert (entry.key.clone());
      }
      else if self.ignore.get (& entry.key).is_none() {
        return Some (Some (entry.key.clone()));
      }
      None
    }
  }
  impl<'a, K: Clone + Eq + Hash > Iterator for SnapshotIter <'a, K> {
    type Item = K;
    fn next (&mut self)->Option <K> {
      loop {
        if let Some (result) = self.step() {return result;}
      }
    }
  }
  impl<K: Clone + Eq> Set <K> {
    pub fn new()->Set <K> {
      Set {
        live_buffer: Buffer {data: Arc::new (UnsafeCell::new (Vec::new())), deletions: 0},
        next_buffer: Buffer {data: Arc::new (UnsafeCell::new (Vec::new())), deletions: 0},
        next_transfer_index: 0,
        operating: false,
      }
    }
    /// Adds a value to the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn insert <F: Fn (& K)->bool> (&mut self, key: K, currently_existent: & F) {
      assert!(!self.operating, "Attempt to call PartiallyPersistentNonindexedSet::insert() from inside its own existence checker callback; what ghastly code led to this situation?");
      self.operating = true;
      self.make_room (currently_existent);
      unsafe {(*self.live_buffer.data.get()).push (RwLock::new (Entry {key: key, insertion: true}));}
      self.operating = false;
    }
    /// Removes a value from the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn remove <F: Fn (& K)->bool> (&mut self, key: K, currently_existent: & F) {
      assert!(!self.operating, "Attempt to call PartiallyPersistentNonindexedSet::insert() from inside its own existence checker callback; what ghastly code led to this situation?");
      self.operating = true;
      self.make_room (currently_existent);
      self.live_buffer.deletions += 1;
      unsafe {(*self.live_buffer.data.get()).push (RwLock::new (Entry {key: key.clone(), insertion: false}));}
      // the next buffer doesn't necessarily have an insertion
      // corresponding to this deletion, so this could be wasteful. However,
      // it would also be wasteful to look up whether the insertion is there
      // (we currently get away with not even being ABLE to do so)
      self.next_buffer.deletions += 1;
      unsafe {(*self.next_buffer.data.get()).push (RwLock::new (Entry {key: key, insertion: false}));}
      self.operating = false;
    }
    /// Takes out a snapshot to the current state of the set.
    ///
    /// These snapshots can safely be passed to other threads
    /// even as you continue to modify the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn snapshot (&self)->Snapshot <K> {
      Snapshot {buffer: self.live_buffer.clone(), used_length: unsafe {(*self.live_buffer.data.get()).len()}}
    }
    
    fn make_room <F: Fn (& K)->bool> (&mut self, currently_existent: F) {
      let live_data = unsafe {self.live_buffer.data.get().as_ref().unwrap()};
      let next_data = unsafe {self.next_buffer.data.get().as_mut().unwrap()};
      let buffer_pushes_before_reset_needed = min (
        live_data.capacity() - live_data.len(),
        next_data.capacity()/2 - next_data.len(),
      );
      let current_insertions = live_data.len() - self.live_buffer.deletions;
      let tolerable_deletions = (current_insertions*5)/12;
      let deletions_before_reset_needed = tolerable_deletions - self.live_buffer.deletions;
      let operations_before_reset_possibly_needed = min (buffer_pushes_before_reset_needed, deletions_before_reset_needed);
      let transfer_steps_needed_before_reset_possible = live_data.len() + operations_before_reset_possibly_needed - self.next_transfer_index;
      assert!(transfer_steps_needed_before_reset_possible <= (operations_before_reset_possibly_needed + 1) * MAX_TRANSFER_SPEED);
      if transfer_steps_needed_before_reset_possible <= operations_before_reset_possibly_needed * MAX_TRANSFER_SPEED {
        for _ in 0..MAX_TRANSFER_SPEED {
          if let Some (item) = live_data.get (self.next_transfer_index) {
            let entry = item.read().unwrap();
            if entry.insertion && currently_existent (& entry.key) {
              next_data.push (RwLock::new ((*entry).clone()));
            }
            self.next_transfer_index += 1;
          }
          else {break;}
        }
      }
      if operations_before_reset_possibly_needed == 0 {
        mem::swap (&mut self.live_buffer, &mut self.next_buffer);
        self.next_buffer = Buffer {
          data: Arc::new (UnsafeCell::new (Vec::with_capacity ((next_data.len() + 1)*2))),
          deletions: 0,
        };
        self.next_transfer_index = 0;
      }
      unsafe {
        assert!((*self.live_buffer.data.get()).len() < (*self.live_buffer.data.get()).capacity());
        assert!((*self.next_buffer.data.get()).len() < (*self.next_buffer.data.get()).capacity()/2);
      }
    }
  }
}
