mod partially_persistent_nonindexed_set {
  use std::sync::{Arc};
  use std::sync::atomic::{AtomicBool, Ordering};
  use std::cmp::min;
  use std::collections::HashSet;
  use std::hash::Hash;
  use std::mem;
  use std::cell::{UnsafeCell};
  use std::marker;use std::fmt::Debug;
use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


  #[derive (Clone, Debug)]
  struct Entry <K: Clone + Eq> {
    key: K,
    insertion: bool,
  }
  //using Vec< in this buffer type has unnecessary indirection, but I was unable to find
  //a library that does the job safely without it.
  //Compare Arc<[Entry<K>]>, which could behave unsafely if I
  //didn't initialize it, but would waste time if I did.
  #[derive (Debug)]
  struct BufferInner <K: Clone + Eq> {
    data: Vec<Entry <K>>,
    marker: AtomicBool,
  }
  #[derive (Clone, Debug)]
  struct Buffer <K: Clone + Eq> {
    data: Arc <UnsafeCell <BufferInner <K> >>,
    deletions: usize,
  }

  #[derive (Debug)]
  pub struct Snapshot <K: Clone + Eq> {
    buffer: Buffer <K>,
    used_length: usize,
  }
  #[derive (Debug)]
  pub struct SnapshotIter <'a, K: Clone + Eq + Hash + 'a> {
    snapshot: & 'a Snapshot <K>,
    position: usize,
    ignore: HashSet <K>,
  }

  #[derive (Debug)]
  pub struct Set <K: Clone + Eq> {
    live_buffer: Buffer <K>,
    next_buffer: Buffer <K>,
    next_transfer_index: usize,
    potential_next_buffer_usage: usize,
    operating: bool,
  }
  unsafe impl<K: Clone + Eq + Sync> Send for Snapshot <K>{}
  unsafe impl<K: Clone + Eq + Sync> Sync for Snapshot <K>{}
  impl<K: Clone + Eq + Hash> Snapshot <K> {
    pub fn iter <'a> (& 'a self)->SnapshotIter <'a, K> {
      //this Acquire should sync with at least the Release performed just after the
      //last insertion into this buffer.
      //To make sure we only need to do this once, at the beginning of iteration,
      //we deliberately unimplemented Send for the iterator type
      unsafe {(*self.buffer.data.get()).marker.load (Ordering::Acquire)};

      SnapshotIter {
        snapshot: self,
        position: self.used_length,
        ignore: HashSet::with_capacity(self.buffer.deletions),
      }
    }
  }
  impl<'a, K> !marker::Send for SnapshotIter <'a, K> {}
  impl<'a, K: Clone + Eq + Hash> SnapshotIter <'a, K> {
    /// Do a single iteration step, which MAY return an iteration result,
    /// and is guaranteed to finish in O(1) time (worst-case except for bad luck with hashing).
    pub fn step (&mut self)->Option <Option <K>> {
      if self.position == 0 {return Some (None);}
      self.position -= 1;
      let entry = unsafe {& (*self.snapshot.buffer.data.get()).data [self.position]};
      if entry.insertion == false {
        self.ignore.insert (entry.key.clone());
      }
      else if !self.ignore.contains (& entry.key) {
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
  impl<K: Clone + Eq + Debug> Set <K> {
    pub fn new()->Set <K> {
      Set {
        live_buffer: Buffer {data: Arc::new (UnsafeCell::new (BufferInner {data: Vec::new (), marker: AtomicBool::new(false)})), deletions: 0},
        next_buffer: Buffer {data: Arc::new (UnsafeCell::new (BufferInner {data: Vec:: with_capacity(2), marker: AtomicBool::new(false)})), deletions: 0},
        next_transfer_index: 0,
        potential_next_buffer_usage: 0,
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
      let live_data = unsafe {self.live_buffer.data.get().as_mut().unwrap()};
      live_data.data.push (Entry {key: key, insertion: true});
      self.potential_next_buffer_usage += 1;
      live_data.marker.store (false, Ordering::Release);
      self.operating = false;
    }
    /// Removes a value from the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn remove <F: Fn (& K)->bool> (&mut self, key: K, currently_existent: & F) {
      assert!(!self.operating, "Attempt to call PartiallyPersistentNonindexedSet::insert() from inside its own existence checker callback; what ghastly code led to this situation?");
      self.operating = true;
      self.make_room (currently_existent);
      let live_data = unsafe {self.live_buffer.data.get().as_mut().unwrap()};
      let next_data = unsafe {self.next_buffer.data.get().as_mut().unwrap()};
      self.live_buffer.deletions += 1;
      live_data.data.push (Entry {key: key.clone(), insertion: false});
      // the next buffer doesn't necessarily have an insertion
      // corresponding to this deletion, so this could be wasteful. However,
      // it would also be wasteful to look up whether the insertion is there
      // (we currently get away with not even being ABLE to do so)
      self.next_buffer.deletions += 1;
      next_data.data.push (Entry {key: key, insertion: false});
      self.potential_next_buffer_usage += 1;
      live_data.marker.store (false, Ordering::Release);
      self.operating = false;
    }
    /// Takes out a snapshot to the current state of the set.
    ///
    /// These snapshots can safely be passed to other threads
    /// even as you continue to modify the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn snapshot (&self)->Snapshot <K> {
      Snapshot {buffer: self.live_buffer.clone(), used_length: unsafe {(*self.live_buffer.data.get()).data.len()}}
    }

    //After let ops = operations_before_reset_possibly_needed operations, there may be in next buffer: current live insertions - current legit live deletions - ops insertions, and current next deletions + ops deletions. We want deletions/insertions <= 1/6 on reset, so…
    //(Next deletions + ops)/(live insertions - legit deletions - ops) <= 1/6
    //next deletions + ops <= (live insertions - legit deletions - ops)/3
    //6 (next deletions + ops) <= live insertions - legit deletions - ops
    //7 ops <= live insertions - legit deletions - 6 next deletions
    //it's okay to overestimate legit deletions, resulting in the following more strict condition on ops:
    //7 ops <= live insertions - live deletions - 6 next deletions
    //ops <= ((live insertions - live deletions - 6 next deletions)/7)
    //ops <= ((live insertions - live deletions - 6 next deletions)/7).floor()
    //But actually we have to tolerate one extra deletion to deal with
    //the case where a deletion is added immediately after a reset.
    //After reset + deletion, new ops could be forced as low as
    //new ops <= (((live insertions - legit deletions - ops) - ops - 7)/7).floor()
    //we also need to obey the condition
    //(new ops + 1)*X >= ((live insertions - legit deletions) + ops + new ops)
    //(new ops)*(X+1) + X >= ((live insertions - legit deletions) + ops)
    //new ops >= ((live insertions - legit deletions) + ops - X)/(X+1)
    //(((live insertions - legit deletions - ops) - ops - 7)/7 >= ((live insertions - legit deletions) + ops - X)/(X+1)
    //(((live insertions - legit deletions - ops) - ops - 7)*(X+1) >= ((live insertions - legit deletions) + ops - X)*7
    //(live insertions - legit deletions)*(X + 1 - 7) + (7)*(X+1) >= ops*(7 + 2X + 2) - X*7
    //(live insertions - legit deletions)*(X + 1 - 7) + 7 >= ops*(7 + 2X + 2)
    
    //(live insertions - legit deletions - 2*ops)*(X) >= ops*(9) + (live insertions - legit deletions)*6 - 7
    //X >= (ops*(9) + (live insertions - legit deletions)*6 - 7)/(live insertions - legit deletions - 2*ops)
    //and ops can be up to 1/7 of that insertion count, so
    //X >= (Z*(9/7) + Z*6 - 7)/(Z - 2*Z/7)
    //X >= (Z*(51/7) - 7)/(5*Z/7)
    //X >= (Z*(51) - 49)/(5*Z)
    //X >= 51/5 >10
    fn make_room <F: Fn (& K)->bool> (&mut self, currently_existent: F) {
  const MAX_TRANSFER_SPEED: usize = 11;
      unsafe {assert!((*self.next_buffer.data.get()).data.len() <= self.potential_next_buffer_usage)};
      let live_data = unsafe {self.live_buffer.data.get().as_ref().unwrap()};
      let next_data = unsafe {self.next_buffer.data.get().as_mut().unwrap()};
      //printlnerr!("starting {:?} \n LIFE: {:?}\n NEXT: {:?}\n CAPACITIES: {} {} DELETIONS: {}/{} {}/{} ", self, live_data, next_data, live_data.data.capacity(), next_data.data.capacity(), self.live_buffer.deletions, live_data.data.len(), self.next_buffer.deletions, next_data.data.len());
      let buffer_pushes_before_reset_needed = min (
        live_data.data.capacity() - live_data.data.len(),
        next_data.data.capacity()/2 - self.potential_next_buffer_usage,
      );
      let current_insertions = live_data.data.len() - self.live_buffer.deletions;
      //let tolerable_deletions = (current_insertions*5)/12;
      //let deletions_before_reset_needed = tolerable_deletions - self.live_buffer.deletions;
      let deletions_before_reset_needed = (current_insertions + 7 - self.live_buffer.deletions - 6*self.next_buffer.deletions)/7;
      let operations_before_reset_possibly_needed = min (buffer_pushes_before_reset_needed, deletions_before_reset_needed);
      assert!(self.next_transfer_index <= live_data.data.len() );
      let transfer_steps_needed_before_reset_possible = live_data.data.len() + operations_before_reset_possibly_needed - self.next_transfer_index;
      //printlnerr!("NEEDED {:?} \n OPERATIONS: {:?}\n (buffer pushes): {:?}\n (deletions): {} {} ", transfer_steps_needed_before_reset_possible , operations_before_reset_possibly_needed , buffer_pushes_before_reset_needed , deletions_before_reset_needed , "something");
      assert!(transfer_steps_needed_before_reset_possible <= (operations_before_reset_possibly_needed + 1) * MAX_TRANSFER_SPEED);
      if transfer_steps_needed_before_reset_possible >operations_before_reset_possibly_needed * MAX_TRANSFER_SPEED {
        for _ in 0..MAX_TRANSFER_SPEED {
          if let Some (entry) = live_data.data.get (self.next_transfer_index) {
            if entry.insertion && currently_existent (& entry.key) {
              next_data.data.push ((*entry).clone());
            }
            self.next_transfer_index += 1;
          }
          else {break;}
        }
      }
      //printlnerr!("ending {:?} \n LIFE: {:?}\n NEXT: {:?}\n CAPACITIES: {} {} ", self, live_data, next_data, live_data.data.capacity(), next_data.data.capacity());
      if operations_before_reset_possibly_needed == 0 {
        self.next_transfer_index = 0;
        self.potential_next_buffer_usage = next_data.data.len() - self.next_buffer.deletions;
        mem::swap (&mut self.live_buffer, &mut self.next_buffer);
        //danger: after we overwrite self.next_buffer, we may have deallocated
        //the memory for live_data
        self.next_buffer = Buffer {
          data: Arc::new (UnsafeCell::new (BufferInner {data: Vec::with_capacity ((self.potential_next_buffer_usage + 1)*4), marker: AtomicBool::new(false)})), deletions: 0,
        };
      }
      //printlnerr!("ending {:?}", self);
      unsafe {
        assert!((*self.live_buffer.data.get()).data.len() < (*self.live_buffer.data.get()).data.capacity());
        assert!((*self.next_buffer.data.get()).data.len() <= self.potential_next_buffer_usage);
        assert!(self.potential_next_buffer_usage < (*self.next_buffer.data.get()).data.capacity()/2);
      }
    }
  }
#[cfg (test)]
mod tests {
use super::*; use std::collections::HashSet;        use std::thread; use std::sync::mpsc::channel;
  #[quickcheck]
  fn test_operation_sequence (operations: Vec<(usize, bool)>)->bool {
    let mut existences: HashSet <usize> = HashSet::new();
    let mut set: Set <usize> = Set::new();
    
    let (send_snapshot, receive_snapshot) = channel::<Option <(Snapshot <usize>, HashSet <usize>)>>();
    let (send_results, receive_results) = channel();
    let check_thread = thread::spawn ( move | | {
      while let Some ((snapshot, checker)) = receive_snapshot.recv().unwrap() {
        let checker_2 = snapshot.iter().collect();
        send_results.send (checker == checker_2);
      }
    });
    for operation in operations {
      if existences.contains(& operation.0) != operation.1 {
        if operation.1 {
          existences.insert (operation.0);
          set.insert (operation.0, & | value | existences.contains (value));
        } else {
          existences.remove (& operation. 0);
          set.remove (operation.0, & | value | existences.contains (value));
        }
        let checker = existences.clone();
        let snapshot = set.snapshot();

        send_snapshot.send (Some ((snapshot, checker)));
        if ! receive_results.recv().unwrap() {send_snapshot.send (None); check_thread.join(); return false;}
      }
    }
    send_snapshot.send (None); check_thread.join(); 
    true
}
//#[test]
fn hack () { 

//test_operation_sequence ((0..100).map (| number | (number, true)).collect());
}}
}
