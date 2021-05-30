use crate::type_utils::{ChoiceOfObjectContainedIn, GetContained};
use crate::{
  Accessor, EntityKind, EventAccessor, MutableData, SimulationSpec, SimulationStateData,
  TypedHandleRef, Wake,
};
use derivative::Derivative;
use time_steward_type_utils::delegate;

/// types that allow undo-safe access to entity data in some way; this is about undo safety, not memory safety,
/// and the read method needs an accessor to actually be allowed to view the data
pub trait ReadAccess<'a, E: EntityKind, A: Accessor>: Copy {
  type Target;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind>;
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>;
}
impl<'a, E: EntityKind, A: Accessor> ReadAccess<'a, E, A>
  for TypedHandleRef<'a, E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind> {
    self
  }
  // when you just have a TypedHandleRef, read undo-safely by explicitly recording an access
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target> {
    accessor.record_read(self);
    accessor.raw_read(self)
  }
}
pub trait EntityReadAccess<'a, E: EntityKind, A: Accessor>:
  ReadAccess<'a, E, A, Target = MutableData<E, A::EntityHandleKind>>
{
}
impl<
    'a,
    E: EntityKind,
    A: Accessor,
    RA: ReadAccess<'a, E, A, Target = MutableData<E, A::EntityHandleKind>>,
  > EntityReadAccess<'a, E, A> for RA
{
}

/// types that allow undo-safe mutable access to entity data in some way; this is about undo safety, not memory safety,
/// and the write method needs an accessor to actually be allowed to view the data
pub trait WriteAccess<'a, E: EntityKind, A: EventAccessor>: ReadAccess<'a, E, A> {
  fn write(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target>;
}
impl<'a, E: EntityKind, A: EventAccessor> WriteAccess<'a, E, A>
  for TypedHandleRef<'a, E, A::EntityHandleKind>
{
  // when you just have a TypedHandleRef, write undo-safely by recording the full old state before returning mutable reference
  fn write(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target> {
    let old_value = accessor.raw_read(self).clone();
    accessor.record_undo(self, move |m| *m = old_value.clone());
    accessor.raw_write(self)
  }
}
pub trait EntityWriteAccess<'a, E: EntityKind, A: EventAccessor>:
  WriteAccess<'a, E, A, Target = MutableData<E, A::EntityHandleKind>>
{
}
impl<
    'a,
    E: EntityKind,
    A: EventAccessor,
    RA: WriteAccess<'a, E, A, Target = MutableData<E, A::EntityHandleKind>>,
  > EntityWriteAccess<'a, E, A> for RA
{
}

#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct ReadRecordedRef<'a, E: EntityKind, A: Accessor>(
  TypedHandleRef<'a, E, A::EntityHandleKind>,
);
delegate! (
  ['a, E: EntityKind, A: Accessor]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [ReadRecordedRef<'a, E, A>]
  to [this => &this.0]
);
impl<'a, E: EntityKind, A: Accessor> ReadRecordedRef<'a, E, A> {
  pub fn new_by_recording(
    entity: TypedHandleRef<'a, E, A::EntityHandleKind>,
    accessor: &'a A,
  ) -> Self {
    accessor.record_read(entity);
    Self(entity)
  }
}
impl<'a, E: EntityKind, A: Accessor> ReadAccess<'a, E, A> for ReadRecordedRef<'a, E, A> {
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind> {
    self.0
  }
  // read undo-safely because we know an access has already been recorded
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target> {
    accessor.raw_read(self.0)
  }
}

pub trait AccessorExt: Accessor {
  // read undo-safely by explicitly recording an access
  fn read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    self.record_read(entity);
    self.raw_read_schedule(entity)
  }
}
impl<A: Accessor> AccessorExt for A {}

pub trait EventAccessorExt: EventAccessor {
  fn write_contained<
    'a,
    E: EntityKind,
    U: SimulationStateData,
    Choice: ChoiceOfObjectContainedIn<MutableData<E, Self::EntityHandleKind>, Target = U>,
  >(
    &'a mut self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
    choice: Choice,
  ) -> Self::WriteGuard<'a, U> {
    let old_value = (*self.raw_read(entity)).get_contained(choice).clone();
    self.record_undo(entity, move |m| {
      *m.get_contained_mut(choice) = old_value.clone()
    });
    Self::map_write_guard(self.raw_write(entity), |t| t.get_contained_mut(choice))
  }
}
impl<A: EventAccessor> EventAccessorExt for A {}
