#![feature(generic_associated_types)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

extern crate time_steward_type_utils as type_utils;

mod entity_id;
pub use crate::entity_id::*;

#[macro_use]
mod api;
pub use crate::api::*;
mod api_impls;
