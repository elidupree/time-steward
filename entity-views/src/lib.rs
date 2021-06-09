#![feature(generic_associated_types)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

//use crate::type_utils::{ChoiceOfObjectContainedIn, GetContained};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use time_steward_api::{
  Accessor, EntityKind, EventAccessor, MutableData, SimulationStateData, TypedHandle,
  TypedHandleRef, UndoData,
};
use time_steward_type_utils::delegate;

pub trait ChoiceOfObjectContainedIn<T>: Copy + 'static {
  type Target;
  fn get(self, object: &T) -> &Self::Target;
  fn get_mut(self, object: &mut T) -> &mut Self::Target;
}

pub trait GetContained<Choice> {
  type Target;
  fn get_contained(&self, choice: Choice) -> &Self::Target;
  fn get_contained_mut(&mut self, choice: Choice) -> &mut Self::Target;
}

impl<T, C: ChoiceOfObjectContainedIn<T>> GetContained<C> for T {
  type Target = C::Target;
  fn get_contained(&self, choice: C) -> &Self::Target {
    choice.get(self)
  }
  fn get_contained_mut(&mut self, choice: C) -> &mut Self::Target {
    choice.get_mut(self)
  }
}

// macro for implementing n-ary tuple functions and operations, adapted from libcore
macro_rules! tuple_impls {
    ($(
        $Tuple:ident {
            $First: ident
            ($($T:ident $Choice:ident $U:ident,)*)
            $Last: ident
        }
    )+) => {
        $(
            #[allow(non_snake_case)]
            impl<$($T: 'static,)* $Last: 'static, $($Choice: ChoiceOfObjectContainedIn<$T, Target=$U>,)* > ChoiceOfObjectContainedIn<$First> for ($($Choice,)*) {
              type Target= $Last;
              fn get(self, object: &T) -> &Self::Target {
                let $First = object;
                let ($($Choice,)*) = self;
                $(let $U = $Choice.get($T);)*
                $Last
              }
              fn get_mut(self, object: &mut T) -> &mut Self::Target {
                let $First = object;
                let ($($Choice,)*) = self;
                $(let $U = $Choice.get_mut($T);)*
                $Last
              }
            }
        )+
    }
}

tuple_impls! {
    Tuple1 {
        T (T TU U,) U
    }
    Tuple2 {
        T (T TU U, U UV V,) V
    }
    Tuple3 {
        T (T TU U, U UV V, V VW W,) W
    }
    Tuple4 {
        T (T TU U, U UV V, V VW W, W WX X,) X
    }
    Tuple5 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z,) Z
    }
    Tuple6 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A,) A
    }
    Tuple7 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A, A AB B,) B
    }
    Tuple8 {
        T (T TU U, U UV V, V VW W, W WX X, X XY Z, Z ZA A, A AB B, B BC C,) C
    }
}

#[derive(
  Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Default,
)]
pub struct RestoreOldValue<T>(pub T);
impl<T: SimulationStateData> UndoData<T> for RestoreOldValue<T> {
  fn undo(&self, target: &mut T) {
    *target = self.0.clone();
  }
}

#[derive(
  Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Default,
)]
pub struct UndoInContained<Choice, Undo> {
  choice: Choice,
  undo_data: Undo,
}
impl<
    T,
    Choice: ChoiceOfObjectContainedIn<T> + SimulationStateData,
    Undo: UndoData<Choice::Target>,
  > UndoData<T> for UndoInContained<Choice, Undo>
{
  fn undo(&self, target: &mut T) {
    self.undo_data.undo(target.get_contained_mut(self.choice));
  }
}

/// types that allow undo-safe access to entity data in some way; this is about undo safety, not memory safety,
/// and the read method needs an accessor to actually be allowed to view the data
pub trait ReadAccess<'a, E: EntityKind, A: Accessor>: Copy {
  type Target;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind>;
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target>;
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
pub trait WriteAccess<'a, 'acc, E: EntityKind, A: EventAccessor<'acc>>:
  ReadAccess<'a, E, A>
{
  fn write(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target>;
}
pub trait EntityWriteAccess<'a, 'acc, E: EntityKind, A: EventAccessor<'acc>>:
  WriteAccess<'a, 'acc, E, A, Target = MutableData<E, A::EntityHandleKind>>
{
}
impl<
    'a,
    'acc,
    E: EntityKind,
    A: EventAccessor<'acc>,
    RA: WriteAccess<'a, 'acc, E, A, Target = MutableData<E, A::EntityHandleKind>>,
  > EntityWriteAccess<'a, 'acc, E, A> for RA
{
}

// when you just have a TypedHandleRef, read undo-safely by explicitly recording an access
impl<'a, E: EntityKind, A: Accessor> ReadAccess<'a, E, A>
  for TypedHandleRef<'a, E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind> {
    self
  }
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target> {
    accessor.record_read(self);
    accessor.raw_read(self)
  }
}

// when you just have a TypedHandleRef, write undo-safely by storing a clone of the entire old state
impl<'a, 'acc, E: EntityKind, A: EventAccessor<'acc>> WriteAccess<'a, 'acc, E, A>
  for TypedHandleRef<'a, E, A::EntityHandleKind>
{
  fn write(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target> {
    let old_value = accessor.raw_read(self).clone();
    accessor.record_undo(self, RestoreOldValue(old_value));
    accessor.raw_write(self)
  }
}

// when you just have a TypedHandle, delegate to self.borrow()
impl<'a, E: EntityKind, A: Accessor> ReadAccess<'a, E, A>
  for &'a TypedHandle<E, A::EntityHandleKind>
{
  type Target = MutableData<E, A::EntityHandleKind>;
  fn entity(self) -> TypedHandleRef<'a, E, A::EntityHandleKind> {
    self.borrow()
  }
  fn read(self, accessor: &'a A) -> A::ReadGuard<'a, Self::Target> {
    self.borrow().read(accessor)
  }
}
impl<'a, 'acc, E: EntityKind, A: EventAccessor<'acc>> WriteAccess<'a, 'acc, E, A>
  for &'a TypedHandle<E, A::EntityHandleKind>
{
  fn write(self, accessor: &'a mut A) -> A::WriteGuard<'a, Self::Target> {
    self.borrow().write(accessor)
  }
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

// pub trait AccessorExt: Accessor {
//   // read undo-safely by explicitly recording an access
//   fn read_schedule<E: Wake<Self::SimulationSpec>>(
//     &self,
//     entity: TypedHandleRef<E, Self::EntityHandleKind>,
//   ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
//     self.record_read(entity);
//     self.raw_read_schedule(entity)
//   }
// }
// impl<A: Accessor> AccessorExt for A {}
//
// pub trait EventAccessorExt: EventAccessor {
//   fn write_contained<
//     'a,
//     E: EntityKind,
//     U: SimulationStateData,
//     Choice: ChoiceOfObjectContainedIn<MutableData<E, Self::EntityHandleKind>, Target = U>,
//   >(
//     &'a mut self,
//     // at the time of this writing, we cannot use the type alias TypedHandleRef due to
//     // https://github.com/rust-lang/rust/issues/85533
//     entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
//     choice: Choice,
//   ) -> Self::WriteGuard<'a, U> {
//     let old_value = (*self.raw_read(entity)).get_contained(choice).clone();
//     self.record_undo(entity, move |m| {
//       *m.get_contained_mut(choice) = old_value.clone()
//     });
//     Self::map_write_guard(self.raw_write(entity), |t| t.get_contained_mut(choice))
//   }
// }
// impl<A: EventAccessor> EventAccessorExt for A {}

pub trait HasDefaultAccessWrapper {
  type Wrapper<A: Access>;
  fn wrap_access<A: Access>(input: A) -> Self::Wrapper<A>;
}
pub trait Access {
  type Target;
  type Mapped<Choice: ChoiceOfObjectContainedIn<Self::Target>>: Access<Target = Choice::Target>;
  fn map<Choice: ChoiceOfObjectContainedIn<Self::Target>>(
    self,
    choice: Choice,
  ) -> Self::Mapped<Choice>;
  fn read(&self) -> &Self::Target;
}

impl<'a, T> Access for &'a T {
  type Target = T;
  type Mapped<Choice: ChoiceOfObjectContainedIn<Self::Target>> = &'a Choice::Target;
  fn map<Choice: ChoiceOfObjectContainedIn<Self::Target>>(
    self,
    choice: Choice,
  ) -> Self::Mapped<Choice> {
    choice.get(self)
  }
  fn read(&self) -> &Self::Target {
    self
  }
}

impl<'a, T> Access for &'a mut T {
  type Target = T;
  type Mapped<Choice: ChoiceOfObjectContainedIn<Self::Target>> = &'a mut Choice::Target;
  fn map<Choice: ChoiceOfObjectContainedIn<Self::Target>>(
    self,
    choice: Choice,
  ) -> Self::Mapped<Choice> {
    choice.get_mut(self)
  }
  fn read(&self) -> &Self::Target {
    self
  }
}

#[derive(Copy, Clone)]
pub struct VecEntryChoice(usize);

impl<T> ChoiceOfObjectContainedIn<Vec<T>> for VecEntryChoice {
  type Target = T;
  fn get(self, object: &Vec<T>) -> &Self::Target {
    object.get(self.0).unwrap()
  }
  fn get_mut(self, object: &mut Vec<T>) -> &mut Self::Target {
    object.get_mut(self.0).unwrap()
  }
}

pub struct VecAccessWrapper<A: Access>(A);
impl<T> HasDefaultAccessWrapper for Vec<T> {
  type Wrapper<A: Access> = VecAccessWrapper<A>;
  fn wrap_access<A: Access>(input: A) -> Self::Wrapper<A> {
    VecAccessWrapper(input)
  }
}

impl<T: HasDefaultAccessWrapper + 'static, A: Access<Target = Vec<T>>> VecAccessWrapper<A> {
  pub fn get(self, index: usize) -> Option<T::Wrapper<A::Mapped<VecEntryChoice>>> {
    (index < self.0.read().len()).then(move || T::wrap_access(self.0.map(VecEntryChoice(index))))
  }
}
