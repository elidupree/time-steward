#![feature(generic_associated_types, adt_const_params, generic_const_exprs)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

extern crate time_steward_type_utils as type_utils;

mod entity_id;
pub use crate::entity_id::*;

#[macro_use]
mod api;
pub use crate::api::*;
pub mod entity_handles;
pub use crate::entity_handles::{
  DynHandle, DynHandleRef, EntityHandleKind, EntityHandleKindDeref, EntityKind, Globals,
  ImmutableData, MutableData, TypedHandle, TypedHandleRef,
};
mod entity_handle_impls;
