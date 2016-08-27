

mod PartiallyPersistentExistenceTracker {
  use std::sync::Arc;
  struct Entry <K: Eq> {
    key: K,
    deleted_at: usize,
  }
  //this buffer type has unnecessary indirection, but I was unable to find
  //a library that does the job safely without it.
  //Compare Arc<[Entry<K>]>, which could behave unsafely if I
  //didn't initialize it, but would waste time if I did
  type Buffer <K: Eq> = Arc <Vec<Entry <K>>>;
  struct Inner <K: Eq> {
    buffers: [Buffer; 2],
    revision: usize,
    next_transfer_index: usize,
  } 
  pub struct Snapshot <K: Eq> {
    buffer: Buffer,
    used_length: usize,
    revision: usize,
  }
  pub struct Handle <K: Eq> {
    tracker: Arc <Mutex <Inner <K>>,
    key: K,
    possible_indices: [usize; 2],
  }
  pub struct Tracker <K: Eq> {
    inner: Arc <Mutex <Inner <K>>>,
  }
  
  impl<K: Eq> Inner <K> {
    fn transfer_batch (&mut self ) {
      
    }
  }
  
  impl<K: Eq> Tracker <K> {
    pub fn insert (&mut self, key: K)->Handle <K> {
      
    }
  }
  impl<K: Eq> Drop for Handle <K> {
    fn drop (&mut self) {
      if let tracker = Some (self.tracker.get_mut())
    }
  } 
}
