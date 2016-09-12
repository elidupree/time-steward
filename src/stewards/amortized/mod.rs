//! A full TimeSteward implementation that has decent (amortized) asymptotic performance for all common operations.
//!
//! This is intended to be the simplest possible implementation that
//! meets those conditions. As such, it's not especially optimized.
//! Here are some of its specific weaknesses:
//!
//! * no support for multithreading
//! * when a field changes in the past, this TimeSteward immediately erases all
//! more-recent versions of that field. This can take time proportional
//! to the amount of times that field has changed since the past change.
//! (It doesn't affect the amortized time because the recording of
//! each thing amortizes its eventual deletion, but it can cause a hiccup.)
//! * This erasing happens even if the field was overwritten at some point
//! without being examined. In that case, we could theoretically optimize
//! by leaving the future of the field untouched.
//! * There can also be hiccups at arbitrary times when the hash table resizes.
//! * We haven't optimized for the "most changes happen in the present" case,
//! which means we pay a bunch of log n factors when we could be paying O(1).
//! * If you keep around old snapshots of times when no fields are
//! actually being modified anymore, they will eventually have
//! all their data copied into them unnecessarily. This could be avoided
//! if we had a good two-dimensional tree type so that the snapshots
//! could be queried by (SnapshotIdx X BaseTime) rectangles.
//! * There might be more small dependency optimizations we could do,
//! like distinguishing between accessing just a field's data
//! and accessing just its last change time, or even the difference
//! between those and just checking whether the field exists or not,
//! or other conditions (although we would need an API change to track
//! some of those things). However, I suspect that the additional runtime
//! cost of recording these different dependencies wouldn't be worth it.
//! (This would only have a small effect at best, because it wouldn't
//! slow down dependency chain propagation.)
//!
//!

mod impls;
mod types;
use self::impls::*;
pub use self::types::Steward;

