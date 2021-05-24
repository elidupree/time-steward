use crate::type_utils::{ChoiceOfObjectContainedIn, GetContained};
use crate::{
  Accessor, EntityHandleKindDeref, EntityKind, EventAccessor, MutableData, SimulationSpec,
  SimulationStateData, TypedHandleRef, Wake,
};

/// types that allow undo-safe access to entity data in some way; this is about undo safety, not memory safety,
/// and the read method needs an accessor to actually be allowed to view the data
pub trait ReadAccess<'b, A: Accessor, E: EntityKind> {
  type Target;
  fn read<'a>(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>
  where
    'b: 'a;
}
impl<'b, A: Accessor, E: EntityKind> ReadAccess<'b, A, E>
  for TypedHandleRef<'b, E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  // when you just have a TypedHandleRef, read undo-safely by explicitly recording an access
  fn read<'a>(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>
  where
    'b: 'a,
  {
    accessor.record_read(self);
    accessor.raw_read(self)
  }
}

/// types that allow undo-safe mutable access to entity data in some way; this is about undo safety, not memory safety,
/// and the write method needs an accessor to actually be allowed to view the data
pub trait WriteAccess<'b, A: EventAccessor, E: EntityKind> {
  type Target;
  fn write<'a>(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target>
  where
    'b: 'a;
}
impl<'b, A: EventAccessor, E: EntityKind> WriteAccess<'b, A, E>
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
