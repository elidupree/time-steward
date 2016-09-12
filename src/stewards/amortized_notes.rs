
// An ExtendedTime may be in one of several states
// – empty and unused
// – a fiat event scheduled but nothing executed
// – a fiat event scheduled and executed consistently to that, and still valid
// – a fiat event scheduled and executed consistently to that, but its accessed fields have changed
// – a fiat event executed, but not scheduled (we could disallow this by doing it immediately)
// – a fiat event executed but then rescheduled differently (ditto)
// – a predicted event scheduled but nothing executed
// – a predicted event scheduled and executed consistently to that, and still valid
// – a predicted event scheduled and executed consistently to that, but its accessed fields have changed
// – a predicted event executed, but not scheduled (we could disallow this in STABLE states but it is guaranteed to occur at least temporarily during a function)
// – a predicted event executed but then rescheduled differently (ditto)
//
// There are enough parallels between fiat and predicted that we should probably combine them:
//
// 0. Unused
// 1. Scheduled only
// 2. Executed consistently, still valid
// 3. Executed consistently, fields changed
// 4. Executed but not scheduled
// 5. Executed but scheduled differently
//
// possible movements:
// (1, 4)->0
// 0->1
// (1, 3, 5)->2
// 2->3
// (2, 3, 5)->4
// 4->5
//
// The ExtendedTime needs attention in (1, 3, 4, 5) but not (2, 0).
// Thus, the changes that affect "needs attention" are:
// (1, 4)->0
// 0->1
// (1, 3, 5)->2
// 2->3
// 2->4
//
// Which can be split up into the ones CAUSED by giving the time attention:
// 4->0
// (1, 3, 5)->2
// And the ones caused from a distance:
// 0->1 (scheduling)
// 1->0 and 2->4 (un-scheduling)
// 2->3 (invalidating)
//
// that's assuming that you're not allowed to reschedule without un-scheduling first (which, in my current model, is true for both fiat events and predicted events)
//
// The only things distinguishing 3 and 5 are how they are created. Let's combine them:
//
// 0. Unused
// 1. Scheduled only
// 2. Executed consistently, still valid
// 3. Executed consistently, fields OR schedule different than when it was executed
// 4. Executed but not scheduled
//
// possible movements:
// (1, 4)->0
// 0->1
// (1, 3)->2
// (2, 4)->3
// (2, 3)->4
//
// The ExtendedTime needs attention in (1, 3, 4) but not (2, 0).
// Thus, the changes that affect "needs attention" are:
// (1, 4)->0
// 0->1
// (1, 3)->2
// 2->3
// 2->4
//
// Which can be split up into the ones CAUSED by giving the time attention:
// 4->0
// (1, 3)->2
// And the ones caused from a distance:
// 0->1 (scheduling)
// 1->0 and 2->4 (un-scheduling)
// 2->3 (invalidating)
//
// notice that the (3, 4) trap can only be escaped by giving the time attention.
// The only way to REMOVE "needs attention" from a time from a distance is 1->0.
//
//
//
// What about predictions?
// Each (RowId, PredictorId) defines one "prediction history". The prediction history may be incomplete (predictions missing after a specific ExtendedTime) but may not be invalid (containing incorrect predictions). To do that, we clear invalidated predictions whenever a FieldHistory changes (but we don't clear invalidated events/fields when a prediction history changes).
//
// The incompleteness must be simple: things can be missing after a certain time, but there can't be missing patches in the middle of the history.
//
// For each history, keep exactly a record of 0 or 1 times when we need to make a new prediction. That time is always one of the following:
// 0. Nothing
// 1. The end of the validity-interval of the last prediction in the history
// 2. The earliest nonexistent->existent transition of the corresponding field after (1) or (0).
//
// This can change in several ways:
// 3. A tail of the prediction history gets invalidated, but there are still valid predictions immediately before that. (1->1)
// 4. A tail of the prediction history gets invalidated, and there are no valid predictions immediately before that (because the field didn't exist). (1->2, 1->0, or 2->0)
// 5. The field becomes existent. (0->2)
// 6. We make a new prediction at the current prediction-needed-time. ((1 or 2)->(1 or 2))
//
// 3 and 5 are easy. 6 is the trickiest case – 6 needs to find the NEXT creation time. When 4 does 1->2, it can only be referring to a creation time at exactly the invalidation start time, because if it was an earlier one, the history would have been invalid.
//
//
//
//


/*
Wait, we don't actually need to do this, because self.shared isn't dropped as long as the snapshot exist!
impl<B: Basics> Drop for Steward<B> {
  fn drop(&mut self) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut*fields_guard;
    for (id, field) in fields.field_states.iter_mut() {
      field.update_snapshots (*id, & fields.changed_since_snapshots);
    }
  }
}
*/

