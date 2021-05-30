use derivative::Derivative;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;

use crate::type_utils::PersistentlyIdentifiedType;
use crate::{EntityId, SimulationSpec, SimulationStateData};

pub trait EntityHandle: Clone + Debug {
  fn id(&self) -> EntityId;
}
pub trait TypedEntityHandle<E: EntityKind, H: EntityHandleKind>: EntityHandle {
  fn get_immutable(&self) -> &ImmutableData<E, H>
  where
    H: EntityHandleKindDeref;
}
pub trait OwnedEntityHandle: EntityHandle + 'static {}
impl<E: EntityHandle + 'static> OwnedEntityHandle for E {}
pub trait BorrowedEntityHandle: EntityHandle + Copy {}
impl<E: EntityHandle + Copy> BorrowedEntityHandle for E {}
pub trait OwnedTypedEntityHandle<E: EntityKind, H: EntityHandleKind>:
  OwnedEntityHandle + TypedEntityHandle<E, H>
{
  fn erase(self) -> DynHandle<H>;
  fn borrow(&self) -> TypedHandleRef<E, H>
  where
    H: EntityHandleKindDeref;
}
pub trait OwnedDynEntityHandle<H: EntityHandleKind>: OwnedEntityHandle {
  // fn downcast<E: EntityKind>(self) -> Result<TypedHandle<E, H>, Self>
  // where
  //   H: EntityHandleKindDeref;
  fn borrow(&self) -> DynHandleRef<H>
  where
    H: EntityHandleKindDeref;
}
pub trait BorrowedTypedEntityHandle<'a, E: EntityKind, H: EntityHandleKindDeref>:
  BorrowedEntityHandle + TypedEntityHandle<E, H>
{
  fn erase(self) -> DynHandleRef<'a, H>;
  fn to_owned(self) -> TypedHandle<E, H>;
}
pub trait BorrowedDynEntityHandle<'a, H: EntityHandleKindDeref>: BorrowedEntityHandle {
  // fn downcast<E: EntityKind>(self) -> Option<TypedHandleRef<'a, E, H>>;
  // fn to_owned(self) -> DynHandle<H>;
}

pub trait EntityKind: PersistentlyIdentifiedType + Sized + 'static {
  type ImmutableData<E: EntityHandleKind>: SimulationStateData;
  type MutableData<E: EntityHandleKind>: SimulationStateData;
}

pub type ImmutableData<E, H> = <E as EntityKind>::ImmutableData<H>;
pub type MutableData<E, H> = <E as EntityKind>::MutableData<H>;
pub type Globals<S, H> = <S as SimulationSpec>::Globals<H>;

pub trait EntityHandleKind: Sized + 'static {
  type TypedHandle<E: EntityKind>: OwnedTypedEntityHandle<E, Self>;
  type DynHandle: OwnedDynEntityHandle<Self>;
}
/// # Safety
///
/// The implementor must guarantee that the associated types are covariant in their lifetime argument,
/// and exactly the size of *const () and *const dyn Any, respectively.
pub unsafe trait EntityHandleKindDeref: EntityHandleKind {
  type TypedHandleRef<'a, E: EntityKind>: BorrowedTypedEntityHandle<'a, E, Self>;
  type DynHandleRef<'a>: BorrowedDynEntityHandle<'a, Self>;
}

#[repr(transparent)]
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct TypedHandle<E: EntityKind, H: EntityHandleKind>(H::TypedHandle<E>);

#[repr(transparent)]
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct DynHandle<H: EntityHandleKind>(H::DynHandle);

// a hacky wrapper to export covariance
// safety: the EntityHandleKindDeref trait guarantees that the wrapped GAT is covariant,
// and (via the BorrowedEntityHandle trait) guarantees that it is Copy.
// the `&'a ()` enforces covariance (as opposed to bivariance)
#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct TypedHandleRef<'a, E: EntityKind, H: EntityHandleKindDeref> {
  wrapped_gat: H::TypedHandleRef<'static, E>,
  _marker: PhantomData<&'a ()>,
}

// a hacky wrapper to export covariance.
// safety: the EntityHandleKindDeref trait guarantees that the wrapped GAT is covariant,
// and (via the BorrowedEntityHandle trait) guarantees that it is Copy.
// the `&'a ()` enforces covariance (as opposed to bivariance)
#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct DynHandleRef<'a, H: EntityHandleKindDeref> {
  wrapped_gat: H::DynHandleRef<'static>,
  _marker: PhantomData<&'a ()>,
}

impl<E: EntityKind, H: EntityHandleKind> TypedHandle<E, H> {
  #[inline(always)]
  pub fn from_wrapped_gat(handle: H::TypedHandle<E>) -> Self {
    Self(handle)
  }
  #[inline(always)]
  pub fn wrapped_gat(&self) -> &H::TypedHandle<E> {
    &self.0
  }
  #[inline(always)]
  pub fn id(&self) -> EntityId {
    self.0.id()
  }
  #[inline(always)]
  pub fn into_wrapped_gat(self) -> H::TypedHandle<E> {
    self.0
  }
  #[inline(always)]
  pub fn erase(self) -> DynHandle<H> {
    self.0.erase()
  }
}

impl<E: EntityKind, H: EntityHandleKindDeref> TypedHandle<E, H> {
  #[inline(always)]
  pub fn borrow(&self) -> TypedHandleRef<E, H> {
    self.0.borrow()
  }
}

impl<H: EntityHandleKind> DynHandle<H> {
  #[inline(always)]
  pub fn from_wrapped_gat(handle: H::DynHandle) -> Self {
    Self(handle)
  }
  #[inline(always)]
  pub fn wrapped_gat(&self) -> &H::DynHandle {
    &self.0
  }
  #[inline(always)]
  pub fn into_wrapped_gat(self) -> H::DynHandle {
    self.0
  }
  #[inline(always)]
  pub fn id(&self) -> EntityId {
    self.0.id()
  }
}

impl<'a, E: EntityKind, H: EntityHandleKindDeref> TypedHandleRef<'a, E, H> {
  #[inline(always)]
  pub fn from_wrapped_gat(handle: H::TypedHandleRef<'a, E>) -> Self {
    Self {
      wrapped_gat: unsafe { mem::transmute_copy(&handle) },
      _marker: PhantomData,
    }
  }
  #[inline(always)]
  pub fn into_wrapped_gat(self) -> H::TypedHandleRef<'a, E> {
    unsafe { mem::transmute_copy(&self.wrapped_gat) }
  }
  #[inline(always)]
  pub fn id(&self) -> EntityId {
    self.into_wrapped_gat().id()
  }
  #[inline(always)]
  pub fn to_owned(self) -> TypedHandle<E, H> {
    self.into_wrapped_gat().to_owned()
  }
}

impl<'a, H: EntityHandleKindDeref> DynHandleRef<'a, H> {
  #[inline(always)]
  pub fn from_wrapped_gat(handle: H::DynHandleRef<'a>) -> Self {
    Self {
      wrapped_gat: unsafe { mem::transmute_copy(&handle) },
      _marker: PhantomData,
    }
  }
  #[inline(always)]
  pub fn id(&self) -> EntityId {
    self.into_wrapped_gat().id()
  }
  #[inline(always)]
  pub fn into_wrapped_gat(self) -> H::DynHandleRef<'a> {
    unsafe { mem::transmute_copy(&self.wrapped_gat) }
  }
}
