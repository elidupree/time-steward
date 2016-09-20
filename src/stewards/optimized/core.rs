use std::mem;
use std::sync::Arc;
use std::cmp::min;
use {FieldId, ColumnId, RowId, PredictorId, Basics, Column, ExtendedTime};
use crossbeam::mem::epoch::{self, Atomic};

struct AtomicFieldInner <B: Basics, C: Column> {
  data: Option <C::FieldType>,
  when: Arc <EventRecord <B>>, 
}

struct AtomicField <B: Basics, C: Column> {
  data: Atomic <AtomicFieldInner <B, C>>,
  
  #[cfg (debug_assertions)]
  column_id: ColumnId,
}


struct FieldHistory <B: Basics, C: Column> {
  current_data: AtomicField <B, C>,
  old_data: BackBiasedTreeDeque <AtomicField <B, C>>,
  
  // TODO: how often are these empty? Should we try to optimize size_of <FieldHistory>?
  dependent_events: BackBiasedTreeDeque <Arc <EventRecord <B>>>,
  dependent_bounded_predictions: BackBiasedTreeDeque <Arc <Prediction <B>>>,
  dependent_unbounded_predictions: AtomicBag <Arc <Prediction <B>>>,
}





struct EventRecord <B: Basics> {
  time: ExtendedTime <B>,
  fields_changed: Box <[FieldId]>,
  checksum: u64,
  predicted_by: Option <Arc <Prediction <B>>>
}
struct Prediction <B: Basics> {
  made_after: Arc <EventRecord <B>>,
  
}

struct EventTask <B: Basics> (Arc <Prediction <B>>);
struct PredictionTask <B: Basics> (PredictorId, RowId);
enum Task {
  Event (EventTask <B>),
  Prediction (PredictionTask <B>),
  Nothing,
}


struct Core <B: Basics> {
  fields: ConcurrentHashMap <FieldId, FieldHistory <B>, BuildTrivialU64Hasher>,
  
  available_memory: AtomicUsize,
  
  memory_batch_size: usize,
  predictors: Predictors <B>,
}

struct Handle <B: Basics> {
  core: Arc <Core <B>>,
  
  /// Store a reserved quantity of memory locally in the handle,
  /// so that we don't have to make as many contested atomic operations
  /// on core.available_memory.
  local_memory: usize,
}


fn reserve_memory (handle: &mut Handle <B>, bytes: usize)->bool {
  if handle.local_memory >= bytes {
    handle.local_memory -= bytes;
    true
  }
  else {
    let mut available = handle.core.available_memory.load (Ordering::Relaxed);
    while available + handle.local_memory >= bytes {
      let seizure = min (available, bytes + handle.core.memory_batch_size);
      match handle.core.available_memory.compare_exchange (
        available, available - seizure, Ordering::Relaxed) {
        Ok (_) => {
          handle.local_memory = handle.local_memory + seizure - bytes;
          return true;
        }
        Err (previous) => {available = previous;},
      }
    }
    false
  }
}

fn free_memory<B: Basics> (handle: &mut Handle <B>, bytes: usize) {
  handle.local_memory += bytes;
  if handle.local_memory > handle.core.memory_batch_size*2 {
    handle.core.available_memory.fetch_add (handle.local_memory - handle.core.memory_batch_size, Ordering::Relaxed);
    handle.local_memory = handle.core.memory_batch_size;
  }
}

fn work<B: Basics>(handle: &mut Handle <B>, guard: &epoch::Guard) {
  garbage_collect(handle, guard);
  match handle.scheduler.pop() {
    Event (event) => try_event (handle, guard, event),
    Prediction (prediction) => try_prediction (handle, guard, prediction),
    Nothing => (),
  }
}

fn garbage_collect <B: Basics>(handle: &mut Handle <B>, guard: &epoch::Guard) {
  unimplemented!()
}

fn try_event<B: Basics>(handle: &mut Handle <B>, guard: &epoch::Guard, event: EventTask <B>) {
  let max_needed = max_event_memory_usage(event);
  if reserve_memory (handle, max_needed) {
    let memory_used = do_event (handle, guard, event);
    assert!(memory_used <= max_needed);
    free_memory (handle, max_needed - memory_used);
  }
}

fn try_prediction<B: Basics> (handle: &mut Handle <B>, guard: &epoch::Guard, prediction: PredictionTask <B>) {
  let max_needed = max_prediction_memory_usage(prediction);
  if reserve_memory (handle, max_needed) {
    let memory_used = do_prediction (handle, guard, prediction);
    assert!(memory_used <= max_needed);
    free_memory (handle, max_needed - memory_used);
  }
}

//fn max_event_memory_usage


fn do_event<B: Basics>(handle: &mut Handle <B>, guard: &epoch::Guard, event: EventTask <B>)->isize {
  let results = do_event_callback (handle, guard, event);
  let field_guards = Vec::with_capacity(results.fields.len());
  let mut modified_fields = 0;
  for (id, data) in results.fields {
    // Lock all fields for writing.
    // If we run into a field that is locked by another event,
    // just abort. Half the time, that means this event was invalidated anyway.
    // The other half, it's unfortunate, but the scheduler should try to optimize
    // for making that case be rare.
    let modified = data.new_value.last_change == event.time;
    if modified {modified_fields += 1;}
    match handle.core.fields.get (id).try_write() {
      Ok (field_guard) => {
        // Also, the field could have been modified AND UNLOCKED
        // in between when we read it and now.
        // So we need to check that it is consistent with the observed value.
        if modified && !field_options_are_equal (field_guard.value_before (event.time), data.old_value) {
          return 0;
        }
        field_guards.push ((id, data, field_guard));
      },
      Err (_) => return 0,
    }
  }
  
  // If we reach this point, the event is valid, and will remain valid
  // until the end of this function when we drop the guards,
  // so we're A-OK to make changes.
  let mut memory_used: isize = mem::size_of::<EventRecord>() + modified_fields*mem::size_of::<FieldId>();
  let modified_ids = Vec::with_capacity (modified_fields);
  let record: Arc<EventRecord> = ;
  for (id, data, field_guard) in field_guards {
    memory_used += field_guard.add_event_dependency (event.time);
    if data.new_value.last_change == event.time {
      modified_ids.push (id);
      memory_used += discard_field_data_before (handle, guard, field_guard, event.time);
      memory_used += push_field_data (handle, guard, field_guard, record.clone(), event.new_data)
    }
  }
  memory_used
}

fn do_prediction<B: Basics> (handle: &mut Handle <B>, guard: &epoch::Guard, prediction: PredictionTask <B>)->usize {
  unimplemented!()
}



