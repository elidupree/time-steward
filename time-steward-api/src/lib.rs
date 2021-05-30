#![feature(generic_associated_types, const_generics, const_evaluatable_checked)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

extern crate time_steward_type_utils as type_utils;

mod entity_id;
pub use crate::entity_id::*;

#[macro_use]
mod api;
pub use crate::api::*;
mod entity_handles;
pub use crate::entity_handles::*;
mod api_impls;
mod entity_handle_impls;
