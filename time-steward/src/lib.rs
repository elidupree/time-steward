// Future TimeSteward API goals:
//
// – groups
// I made up API functions for this before, but I'm not sure they were perfect.
// We can use RowId's for group ids, but what kinds of things can be stored IN a group? Previously I said only RowId's could, which doesn't seem ideal. I think it would work to allow anything hashable. That would basically make a group behave like a HashSet. But then, why not make it behave like a HashMap instead? But accessor itself already behaves like a HashMap over RowId's – the essential thing groups do is to allow iterating particular subsets of that HashMap.
//
// – conveniences for serializing snapshots (not sure what)
//
// – be able to construct a new TimeSteward from any snapshot
// Issues: should snapshots expose the predictors? Also, the predictors can't be serialized. So you'd probably actually have to construct a TimeSteward from snapshot + predictors. This requires a way to iterate all fields (and group data if we add groups) in a snapshot, which is similar to the previous 2 items (iterating the whole set, which is a special case of iterating a subset; and serializing requires you to iterate everything as well)
//
// – parallelism support for predictors and events
// When an event or predictor gets invalidated while it is still running, it would be nice for it to save time by exiting early.
// Moreover, it would probably be more efficient to discard invalidated fields than to preserve them for predictors/events that are in process. The natural way for the accessors to handle this is to have get() return None, which would mean that you can never safely unwrap() the result. We could provide a "unwrap or return" macro.
//
// – Optimization features
// one possibility: user can provide a function FieldId->[(PredictorId, RowId)] that lists predictors you KNOW will be invalidated by a change to that field, then have that predictor run its get() calls with an input called "promise_inferred" or something so that we don't spend time and memory recording the dependency
// another: a predictor might have a costly computation to find the exact time of a future event, which it won't need to do if it gets invalidated long before that time comes. For that, we can provide a defer_until(time) method
//
//
//

#![feature(
  arbitrary_self_types,
  unboxed_closures,
  specialization,
  never_type,
  map_first_last,
  raw,
  try_trait,
  min_type_alias_impl_trait,
  generic_associated_types
)]
#![warn(unsafe_op_in_unsafe_fn)]

#[cfg_attr(test, macro_use)]
extern crate proptest;
#[cfg_attr(test, macro_use)]
extern crate quickcheck;

pub extern crate time_steward_integer_math as integer_math;
pub extern crate time_steward_type_utils as type_utils;

macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

/// Used by TimeSteward implementations.
///
/// These are public in case someone wants to try implementing a TimeSteward
/// outside of this crate, sharing some of the details with my own implementations.
/// However, anything inside this module should be
/// considered very unstable at present.
#[macro_use]
pub mod implementation_support {
  pub mod data_structures;
  pub mod insert_only;
  #[macro_use]
  pub mod common;
  //pub mod serialization;
}

/*#[macro_use]
/// Allows dynamic dispatch to object-unsafe traits, with some caveats.
pub mod dynamic {
  #[macro_use]
  pub mod list_of_types;
  pub mod thin_arc;
}*/

mod entity_id;
pub use crate::entity_id::*;

#[macro_use]
mod api;
mod access;
pub use crate::access::*;
pub use crate::api::*;
pub mod api_impls;

#[macro_use]
pub mod support {
  pub mod rounding_error_tolerant_math;
  pub mod time_functions;
  pub use time_steward_integer_math as integer_math;
  pub mod trajectories;
  //#[macro_use]
  //pub mod simple_timeline;
  //#[macro_use]
  //pub mod bbox_collision_detection;
}

//#[macro_use]
//pub mod serialization;

pub mod stewards {
  pub mod simple_flat;
  //#[macro_use]
  //pub mod simple_full;
  //pub mod auditing;
}
