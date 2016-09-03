/* Future TimeSteward API goals:
 *
 * – groups
 * I made up API functions for this before, but I'm not sure they were perfect.
 * We can use RowId's for group ids, but what kinds of things can be stored IN a group? Previously I said only RowId's could, which doesn't seem ideal. I think it would work to allow anything hashable. That would basically make a group behave like a HashSet. But then, why not make it behave like a HashMap instead? But accessor itself already behaves like a HashMap over RowId's – the essential thing groups do is to allow iterating particular subsets of that HashMap.
 *
 * – conveniences for serializing snapshots (not sure what)
 *
 * – be able to construct a new TimeSteward from any snapshot
 * Issues: should snapshots expose the predictors? Also, the predictors can't be serialized. So you'd probably actually have to construct a TimeSteward from snapshot + predictors. This requires a way to iterate all fields (and group data if we add groups) in a snapshot, which is similar to the previous 2 items (iterating the whole set, which is a special case of iterating a subset; and serializing requires you to iterate everything as well)
 *
 * – parallelism support for predictors and events
 * When an event or predictor gets invalidated while it is still running, it would be nice for it to save time by exiting early.
 * Moreover, it would probably be more efficient to discard invalidated fields than to preserve them for predictors/events that are in process. The natural way for the accessors to handle this is to have get() return None, which would mean that you can never safely unwrap() the result. We could provide a "unwrap or return" macro.
 *
 * – Optimization features
 * one possibility: user can provide a function FieldId->[(PredictorId, RowId)] that lists predictors you KNOW will be invalidated by a change to that field, then have that predictor run its get() calls with an input called "promise_inferred" or something so that we don't spend time and memory recording the dependency
 * another: a predictor might have a costly computation to find the exact time of a future event, which it won't need to do if it gets invalidated long before that time comes. For that, we can provide a defer_until(time) method
 *
 *
 * */

#![feature(unboxed_closures, fn_traits)]
#![feature (plugin, custom_derive)]
#![plugin (serde_macros)]
// #![plugin (quickcheck_macros)]

extern crate rand;
extern crate nalgebra;
#[macro_use]
extern crate glium;
extern crate serde;
extern crate bincode;
#[cfg (test)]
#[macro_use]
extern crate quickcheck;

#[macro_use]
mod api;
pub use api::*;

mod insert_only;
pub mod data_structures;

#[macro_use]
pub mod stewards {
  #[macro_use]
  pub mod common;
  
  pub mod inefficient_flat;
  pub mod memoized_flat;
  // pub mod amortized;

  pub mod crossverified;
}

#[macro_use]
pub mod support {
  pub mod rounding_error_tolerant_math;
  pub mod time_functions;
  #[macro_use]
  pub mod collision_detection;
}

pub mod examples {
  pub mod handshakes;
  pub mod bouncy_circles;
}
