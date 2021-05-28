use crate::type_utils::{ChoiceOfObjectContainedIn, GetContained};
use crate::{
  Accessor, EntityHandleKindDeref, EntityKind, EventAccessor, MutableData, SimulationSpec,
  SimulationStateData, TypedHandleRef, Wake,
};

/// types that allow undo-safe access to entity data in some way; this is about undo safety, not memory safety,
/// and the read method needs an accessor to actually be allowed to view the data
pub trait ReadAccess<'b, E: EntityKind, A: Accessor>: Copy {
  type Target;
  fn entity(self) -> TypedHandleRef<'b, E, A::EntityHandleKind>;
  fn read<'a>(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>
  where
    'b: 'a;
}
impl<'b, E: EntityKind, A: Accessor> ReadAccess<'b, E, A>
  for TypedHandleRef<'b, E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'b, E, A::EntityHandleKind> {
    self
  }
  // when you just have a TypedHandleRef, read undo-safely by explicitly recording an access
  fn read<'a>(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>
  where
    'b: 'a,
  {
    accessor.record_read(self);
    accessor.raw_read(self)
  }
}
pub trait EntityReadAccess<'b, E: EntityKind, A: Accessor>:
  ReadAccess<'b, E, A, Target = MutableData<Self::EntityKind, A::EntityHandleKind>>
{
}
impl<
    'b,
    A: Accessor,
    E: EntityKind,
    RA: ReadAccess<'b, E, A, Target = MutableData<Self::EntityKind, A::EntityHandleKind>>,
  > EntityReadAccess<'b, E, A> for RA
{
}

/// types that allow undo-safe mutable access to entity data in some way; this is about undo safety, not memory safety,
/// and the write method needs an accessor to actually be allowed to view the data
pub trait WriteAccess<'b, A: EventAccessor, E: EntityKind>: ReadAccess<'b, E, A> {
  type Target;
  fn write<'a>(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target>
  where
    'b: 'a;
}
impl<'b, A: EventAccessor, E: EntityKind> WriteAccess<'b, E, A>
  for TypedHandleRef<'b, E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  // when you just have a TypedHandleRef, write undo-safely by recording the full old state before returning mutable reference
  fn write<'a>(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target>
  where
    'b: 'a,
  {
    let old_value = accessor.raw_read(self).clone();
    accessor.record_undo(self, move |m| *m = old_value.clone());
    accessor.raw_write(self)
  }
}
pub trait EntityWriteAccess<'b, E: EntityKind, A: Accessor>:
  WriteAccess<'b, E, A, Target = MutableData<Self::EntityKind, A::EntityHandleKind>>
{
}
impl<
    'b,
    A: Accessor,
    E: EntityKind,
    RA: WriteAccess<'b, E, A, Target = MutableData<Self::EntityKind, A::EntityHandleKind>>,
  > EntityWriteAccess<'b, E, A> for RA
{
}

#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct ReadRecordedRef<'b, E: EntityKind, A: Accessor>(
  TypedHandleRef<'b, E, A::EntityHandleKind>,
);
delegate! (
  ['b, E: EntityKind, A: Accessor]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [ReadRecordedRef<'b, E, A>]
  to [this => &this.0]
);
impl<'b, E: EntityKind, A: Accessor> ReadRecordedRef<'b, E, A> {
  fn new_by_recording<'a>(
    entity: TypedHandleRef<'b, E, A::EntityHandleKind>,
    accessor: &'a A,
  ) -> Self
  where
    'b: 'a,
  {
    accessor.record_read(entity);
    Self(entity)
  }
}
impl<'b, E: EntityKind, A: Accessor> ReadAccess<'b, E, A> for ReadRecordedRef<'b, E, A> {
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'b, E, A::EntityHandleKind> {
    self.0
  }
  // read undo-safely because we know an access has already been recorded
  fn read<'a>(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>
  where
    'b: 'a,
  {
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
    'b: 'a,
    E: EntityKind,
    U: SimulationStateData,
    Choice: ChoiceOfObjectContainedIn<MutableData<E, Self::EntityHandleKind>, Target = U>,
  >(
    &'a mut self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
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
