use std::hash::{BuildHasherDefault, Hasher};

#[derive(Clone, Default)]
pub struct TrivialU64Hasher {
  state: u64,
}
impl Hasher for TrivialU64Hasher {
  fn finish(&self) -> u64 {
    self.state
  }
  fn write(&mut self, _: &[u8]) {
    panic!("TrivialU64Hasher can only hash u64!");
  }
  fn write_u64(&mut self, value: u64) {
    self.state ^= value;
  }
}

pub type BuildTrivialU64Hasher = BuildHasherDefault<TrivialU64Hasher>;

pub mod partially_persistent_nonindexed_set {
  use std::cell::UnsafeCell;
  use std::cmp::min;
  use std::collections::HashSet;
  use std::fmt::Debug;
  use std::hash::{BuildHasher, Hash};
  use std::mem;
  use std::sync::Arc;

  #[derive(Clone, Debug)]
  struct Entry<K: Clone + Eq> {
    key: K,
    insertion: bool,
  }
  // using Vec< in this buffer type has unnecessary indirection, but I was unable to find
  // a library that does the job safely without it.
  // Compare Arc<[Entry<K>]>, which could behave unsafely if I
  // didn't initialize it, but would waste time if I did.
  #[derive(Debug)]
  struct BufferInner<K: Clone + Eq> {
    data: Vec<Entry<K>>,
  }
  #[derive(Clone, Debug)]
  struct Buffer<K: Clone + Eq> {
    data: Arc<UnsafeCell<BufferInner<K>>>,
    deletions: usize,
  }

  #[derive(Debug)]
  pub struct Snapshot<K: Clone + Eq> {
    buffer: Buffer<K>,
    used_length: usize,
  }
  #[derive(Debug)]
  pub struct SnapshotIter<'a, K: Clone + Eq + Hash + 'a> {
    snapshot: &'a Snapshot<K>,
    position: usize,
    ignore: HashSet<K>,
  }

  #[derive(Debug)]
  pub struct Set<K: Clone + Eq + Hash, S: BuildHasher + Clone> {
    live_buffer: Buffer<K>,
    next_buffer: Buffer<K>,
    live_members: HashSet<K, S>,
    next_members: HashSet<K, S>,
    next_transfer_index: usize,
    potential_next_buffer_usage: usize,
    hash_builder: S,
  }
  unsafe impl<K: Clone + Eq + Sync> Send for Snapshot<K> {}
  unsafe impl<K: Clone + Eq + Sync> Sync for Snapshot<K> {}
  impl<K: Clone + Eq + Hash> Snapshot<K> {
    pub fn iter(&self) -> SnapshotIter<K> {
      SnapshotIter {
        snapshot: self,
        position: self.used_length,
        ignore: HashSet::with_capacity(self.buffer.deletions),
      }
    }
  }
  impl<'a, K: Clone + Eq + Hash> SnapshotIter<'a, K> {
    /// Do a single iteration step, which MAY return an iteration result,
    /// and is guaranteed to finish in O(1) time (worst-case except for bad luck with hashing).
    pub fn step(&mut self) -> Option<Option<K>> {
      if self.position == 0 {
        return Some(None);
      }
      self.position -= 1;
      let entry = unsafe { &(*self.snapshot.buffer.data.get()).data[self.position] };
      if !entry.insertion {
        self.ignore.insert(entry.key.clone());
      } else if !self.ignore.contains(&entry.key) {
        return Some(Some(entry.key.clone()));
      }
      None
    }
  }
  impl<'a, K: Clone + Eq + Hash> Iterator for SnapshotIter<'a, K> {
    type Item = K;
    fn next(&mut self) -> Option<K> {
      loop {
        if let Some(result) = self.step() {
          return result;
        }
      }
    }
  }
  impl<K: Clone + Eq + Hash + Debug, S: BuildHasher + Clone + Default> Default for Set<K, S> {
    fn default() -> Set<K, S> {
      Set::with_hasher(Default::default())
    }
  }
  impl<K: Clone + Eq + Hash + Debug, S: BuildHasher + Clone> Set<K, S> {
    pub fn with_hasher(hash_builder: S) -> Set<K, S> {
      Set {
        live_buffer: Buffer {
          data: Arc::new(UnsafeCell::new(BufferInner { data: Vec::new() })),
          deletions: 0,
        },
        next_buffer: Buffer {
          data: Arc::new(UnsafeCell::new(BufferInner {
            data: Vec::with_capacity(2),
          })),
          deletions: 0,
        },
        live_members: HashSet::with_hasher(hash_builder.clone()),
        next_members: HashSet::with_capacity_and_hasher(2, hash_builder.clone()),

        next_transfer_index: 0,
        potential_next_buffer_usage: 0,
        hash_builder,
      }
    }

    /// Adds a value to the set.
    ///
    /// Complexity: O(1) expected worst-case if K does not implement Drop, O(1) expected amortized otherwise.
    pub fn insert(&mut self, key: K) {
      self.make_room();
      if self.live_members.insert(key.clone()) {
        let live_data = unsafe { self.live_buffer.data.get().as_mut().unwrap() };
        live_data.data.push(Entry {
          key,
          insertion: true,
        });
        self.potential_next_buffer_usage += 1;
      }
    }

    /// Removes a value from the set.
    ///
    /// Complexity: O(1) expected worst-case if K does not implement Drop, O(1) expected amortized otherwise.
    pub fn remove(&mut self, key: K) {
      self.make_room();

      if self.live_members.remove(&key) {
        let live_data = unsafe { self.live_buffer.data.get().as_mut().unwrap() };
        self.live_buffer.deletions += 1;
        live_data.data.push(Entry {
          key: key.clone(),
          insertion: false,
        });
        if self.next_members.remove(&key) {
          let next_data = unsafe { self.next_buffer.data.get().as_mut().unwrap() };
          self.next_buffer.deletions += 1;
          next_data.data.push(Entry {
            key,
            insertion: false,
          });
          self.potential_next_buffer_usage += 1;
        }
      }
    }

    /// Takes out a snapshot to the current state of the set.
    ///
    /// These snapshots can safely be passed to other threads
    /// even as you continue to modify the set.
    ///
    /// Complexity: O(1) worst-case if K does not implement Drop, O(1) amortized otherwise.
    pub fn snapshot(&self) -> Snapshot<K> {
      Snapshot {
        buffer: self.live_buffer.clone(),
        used_length: unsafe { (*self.live_buffer.data.get()).data.len() },
      }
    }

    // After let ops = operations_before_reset_possibly_needed operations, there may be in next buffer: current live insertions - current legit live deletions - ops insertions, and current next deletions + ops deletions. We want deletions/insertions <= 1/6 on reset, so…
    // (Next deletions + ops)/(live insertions - legit deletions - ops) <= 1/6
    // next deletions + ops <= (live insertions - legit deletions - ops)/3
    // 6 (next deletions + ops) <= live insertions - legit deletions - ops
    // 7 ops <= live insertions - legit deletions - 6 next deletions
    // it's okay to overestimate legit deletions, resulting in the following more strict condition on ops:
    // 7 ops <= live insertions - live deletions - 6 next deletions
    // ops <= ((live insertions - live deletions - 6 next deletions)/7)
    // ops <= ((live insertions - live deletions - 6 next deletions)/7).floor()
    // But actually we have to tolerate one extra deletion to deal with
    // the case where a deletion is added immediately after a reset.
    // After reset + deletion, new ops could be forced as low as
    // new ops <= (((live insertions - legit deletions - ops) - ops - 7)/7).floor()
    // we also need to obey the condition
    // (new ops + 1)*X >= ((live insertions - legit deletions) + ops + new ops)
    // (new ops)*(X+1) + X >= ((live insertions - legit deletions) + ops)
    // new ops >= ((live insertions - legit deletions) + ops - X)/(X+1)
    // (((live insertions - legit deletions - ops) - ops - 7)/7 >= ((live insertions - legit deletions) + ops - X)/(X+1)
    // (((live insertions - legit deletions - ops) - ops - 7)*(X+1) >= ((live insertions - legit deletions) + ops - X)*7
    // (live insertions - legit deletions)*(X + 1 - 7) + (7)*(X+1) >= ops*(7 + 2X + 2) - X*7
    // (live insertions - legit deletions)*(X + 1 - 7) + 7 >= ops*(7 + 2X + 2)

    // (live insertions - legit deletions - 2*ops)*(X) >= ops*(9) + (live insertions - legit deletions)*6 - 7
    // X >= (ops*(9) + (live insertions - legit deletions)*6 - 7)/(live insertions - legit deletions - 2*ops)
    // and ops can be up to 1/7 of that insertion count, so
    // X >= (Z*(9/7) + Z*6 - 7)/(Z - 2*Z/7)
    // X >= (Z*(51/7) - 7)/(5*Z/7)
    // X >= (Z*(51) - 49)/(5*Z)
    // X >= 51/5 >10
    fn make_room(&mut self) {
      // MAX_TRANSFER_SPEED could safely be 11 as per the above calculation,
      // but cache efficiency is probably better with a higher value.
      const MAX_TRANSFER_SPEED: usize = 32;

      unsafe {
        assert!((*self.next_buffer.data.get()).data.len() <= self.potential_next_buffer_usage)
      };
      let live_data = unsafe { self.live_buffer.data.get().as_ref().unwrap() };
      let next_data = unsafe { self.next_buffer.data.get().as_mut().unwrap() };
      // println!("starting {:?} \n LIFE: {:?}\n NEXT: {:?}\n CAPACITIES: {} {} DELETIONS: {}/{} \
      // {}/{} ",
      // self,
      // live_data,
      // next_data,
      // live_data.data.capacity(),
      // next_data.data.capacity(),
      // self.live_buffer.deletions,
      // live_data.data.len(),
      // self.next_buffer.deletions,
      // next_data.data.len());
      let buffer_pushes_before_reset_needed = min(
        live_data.data.capacity() - live_data.data.len(),
        next_data.data.capacity() / 2 - self.potential_next_buffer_usage,
      );
      let current_insertions = live_data.data.len() - self.live_buffer.deletions;
      // let tolerable_deletions = (current_insertions*5)/12;
      // let deletions_before_reset_needed = tolerable_deletions - self.live_buffer.deletions;
      let deletions_before_reset_needed =
        (current_insertions + 7 - self.live_buffer.deletions - 6 * self.next_buffer.deletions) / 7;
      let operations_before_reset_possibly_needed = min(
        buffer_pushes_before_reset_needed,
        deletions_before_reset_needed,
      );
      assert!(self.next_transfer_index <= live_data.data.len());
      let transfer_steps_needed_before_reset_possible =
        live_data.data.len() + operations_before_reset_possibly_needed - self.next_transfer_index;
      // println!("NEEDED {:?} \n OPERATIONS: {:?}\n (buffer pushes): {:?}\n (deletions): {} {} ",
      // transfer_steps_needed_before_reset_possible,
      // operations_before_reset_possibly_needed,
      // buffer_pushes_before_reset_needed,
      // deletions_before_reset_needed,
      // "something");
      assert!(
        transfer_steps_needed_before_reset_possible
          <= (operations_before_reset_possibly_needed + 1) * MAX_TRANSFER_SPEED
      );
      if transfer_steps_needed_before_reset_possible
        > operations_before_reset_possibly_needed * MAX_TRANSFER_SPEED
      {
        for _ in 0..MAX_TRANSFER_SPEED {
          if let Some(entry) = live_data.data.get(self.next_transfer_index) {
            if entry.insertion && self.live_members.contains(&entry.key) {
              next_data.data.push((*entry).clone());
              self.next_members.insert(entry.key.clone());
            }
            self.next_transfer_index += 1;
          } else {
            break;
          }
        }
      }
      // println!("ending {:?} \n LIFE: {:?}\n NEXT: {:?}\n CAPACITIES: {} {} ",
      // self,
      // live_data,
      // next_data,
      // live_data.data.capacity(),
      // next_data.data.capacity());
      if operations_before_reset_possibly_needed == 0 {
        self.next_transfer_index = 0;
        self.potential_next_buffer_usage = next_data.data.len() - self.next_buffer.deletions;
        mem::swap(&mut self.live_buffer, &mut self.next_buffer);
        mem::swap(&mut self.live_members, &mut self.next_members);
        let next_capacity = (self.potential_next_buffer_usage + 1) * 4;
        // danger: after we overwrite self.next_buffer, we may have deallocated
        // the memory for live_data
        self.next_buffer = Buffer {
          data: Arc::new(UnsafeCell::new(BufferInner {
            data: Vec::with_capacity(next_capacity),
          })),
          deletions: 0,
        };
        self.next_members =
          HashSet::with_capacity_and_hasher(next_capacity, self.hash_builder.clone());
      }

      unsafe {
        assert!(
          (*self.live_buffer.data.get()).data.len()
            < (*self.live_buffer.data.get()).data.capacity()
        );
        assert!((*self.next_buffer.data.get()).data.len() <= self.potential_next_buffer_usage);
        assert!(
          self.potential_next_buffer_usage < (*self.next_buffer.data.get()).data.capacity() / 2
        );
      }
    }
  }
  #[cfg(test)]
  mod tests {
    use super::*;
    use std::collections::HashSet;
    use std::sync::mpsc::channel;
    use std::thread;

    type Key = u8;
    fn test_operation_sequence(operations: Vec<(Key, bool)>) -> bool {
      let mut existences: HashSet<Key> = HashSet::new();
      let mut set: Set<Key, ::std::collections::hash_map::RandomState> =
        Set::with_hasher(Default::default());

      let (send_snapshot, receive_snapshot) = channel::<Option<(Snapshot<Key>, HashSet<Key>)>>();
      let (send_results, receive_results) = channel();
      let check_thread = thread::spawn(move || {
        while let Some((snapshot, checker)) = receive_snapshot.recv().unwrap() {
          let checker_2 = snapshot.iter().collect();
          send_results.send(checker == checker_2).unwrap();
        }
      });
      for operation in operations {
        if operation.1 {
          existences.insert(operation.0);
          set.insert(operation.0);
        } else {
          existences.remove(&operation.0);
          set.remove(operation.0);
        }
        let checker = existences.clone();
        let snapshot = set.snapshot();

        send_snapshot.send(Some((snapshot, checker))).unwrap();
        if !receive_results.recv().unwrap() {
          send_snapshot.send(None).unwrap();
          check_thread.join().unwrap();
          return false;
        }
      }
      send_snapshot.send(None).unwrap();
      check_thread.join().unwrap();
      true
    }

    quickcheck! {
      fn operation_sequences_work(operations: Vec<(Key, bool)>) -> bool {
        test_operation_sequence(operations)
      }
    }

    #[test]
    fn many_insertions() {
      test_operation_sequence((0..255).map(|number| (number, true)).collect());
    }

    #[test]
    fn many_insertions_and_deletions() {
      test_operation_sequence(
        (0..100)
          .map(|number| (number, true))
          .chain((0..100).map(|number| (number, false)))
          .chain((0..100).map(|number| (number, true)))
          .chain((0..100).map(|number| (99 - number, false)))
          .collect(),
      );
    }
  }
}
