#![feature(generic_associated_types, type_alias_impl_trait)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

//use crate::type_utils::{ChoiceOfObjectContainedIn, GetContained};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;
use std::ops::{Deref, IndexMut};
use time_steward_api::{
  Accessor, EntityKind, EventAccessor, MutableData, PerformUndo, RecordUndo, SimulationStateData,
  TypedHandle, TypedHandleRef,
};
use time_steward_type_utils::delegate;

/// A path to a subobject, which can map either & or &mut access.
///
/// The name "Lens" is inspired by its usage in functional programming, but
/// this type does not provide non-mutating updates.
pub trait Lens<T>: SimulationStateData {
  type Target: SimulationStateData;
  fn get<'a>(&self, object: &'a T) -> &'a Self::Target;
  fn get_mut<'a>(&self, object: &'a mut T) -> &'a mut Self::Target;
}

// /// An object that can do something given an arbitrary lens.
// pub trait LensVisitor<T: SimulationStateData> {
//   fn visit<'a, L: Lens<T>>(&self, lens: L, target: &'a L::Target)->
// }
//
// pub trait Lenses<T>: Clone + Serialize + DeserializeOwned + 'static {
//   type Target: SimulationStateData;
//   fn with_mapped<'a>(&self, object: &'a T, Fn) -> &'a Self::Target;
//   fn with_mapped_mut<'a>(&self, object: &'a mut T) -> &'a mut Self::Target;
// }

pub trait GetContained<L> {
  type Target: SimulationStateData;
  fn get_contained(&self, lens: &L) -> &Self::Target;
  fn get_contained_mut(&mut self, lens: &L) -> &mut Self::Target;
}

impl<T, L: Lens<T>> GetContained<L> for T {
  type Target = L::Target;
  fn get_contained(&self, lens: &L) -> &Self::Target {
    lens.get(self)
  }
  fn get_contained_mut(&mut self, lens: &L) -> &mut Self::Target {
    lens.get_mut(self)
  }
}

// macro for implementing n-ary tuple functions and operations, adapted from libcore
macro_rules! tuple_impls {
    ($(
        $Tuple:ident {
            $First: ident
            ($($T:ident $L:ident $U:ident,)*)
            $Last: ident
        }
    )+) => {
        $(
            #[allow(non_snake_case)]
            impl<$($T: SimulationStateData,)* $Last: SimulationStateData, $($L: Lens<$T, Target=$U>,)* > Lens<$First> for ($($L,)*) {
              type Target= $Last;
              fn get<'a>(&self, object: &'a T) -> &'a Self::Target {
                let $First = object;
                let ($($L,)*) = self;
                $(let $U = $L.get($T);)*
                $Last
              }
              fn get_mut<'a>(&self, object: &'a mut T) -> &'a mut Self::Target {
                let $First = object;
                let ($($L,)*) = self;
                $(let $U = $L.get_mut($T);)*
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

#[derive(Debug)]
pub struct RestoreOldValue<T>(PhantomData<T>);
impl<T: SimulationStateData> PerformUndo<T> for RestoreOldValue<T> {
  type UndoData = T;

  fn perform_undo(data: &mut T, undo_data: Self::UndoData) {
    *data = undo_data;
  }
}

#[derive(
  Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Default,
)]
pub struct UndoInContained<T, L, P>(PhantomData<(T, L, P)>);
impl<T: 'static, L: Lens<T> + SimulationStateData, P: PerformUndo<L::Target>> PerformUndo<T>
  for UndoInContained<T, L, P>
{
  type UndoData = (L, P::UndoData);

  fn perform_undo(data: &mut T, (lens, undo_data): Self::UndoData) {
    P::perform_undo(lens.get_mut(data), undo_data);
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
    let (value, undo_recorder) = accessor.raw_write(self);
    undo_recorder
      .record_undo::<
        MutableData<E, A::EntityHandleKind>,
        RestoreOldValue<MutableData<E, A::EntityHandleKind>>
      >(&*value);
    value
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

pub trait LensMappable<T> {
  type Mapped<L: Lens<T>>: LensMappable<L::Target>;
  fn map<L: Lens<T>>(&self, lens: &L) -> Self::Mapped<L>;
}

pub struct AugmentedRefMut<'a, T, Metadata> {
  data: &'a mut T,
  metadata: Metadata,
}

impl<'a, T: SimulationStateData, Metadata: LensMappable<T>> AugmentedRefMut<'a, T, Metadata> {
  pub fn map<L: Lens<T>>(self, lens: &L) -> AugmentedRefMut<'a, L::Target, Metadata::Mapped<L>> {
    AugmentedRefMut {
      data: lens.get_mut(self.data),
      metadata: self.metadata.map(lens),
    }
  }
}

impl<'a, T: SimulationStateData, Metadata: LensMappable<Vec<T>> + 'a>
  AugmentedRefMut<'a, Vec<T>, Metadata>
{
  pub fn iter_mut(
    self,
  ) -> impl Iterator<Item = AugmentedRefMut<'a, T, Metadata::Mapped<usize>>> + 'a {
    let AugmentedRefMut { data, metadata } = self;
    data
      .iter_mut()
      .enumerate()
      .map(move |(index, data)| AugmentedRefMut {
        data,
        metadata: metadata.map(&index),
      })
  }
}

impl<'a, T: SimulationStateData, Metadata: LensMappable<T> + 'a> Deref
  for AugmentedRefMut<'a, T, Metadata>
{
  type Target = T;

  fn deref(&self) -> &Self::Target {
    self.data
  }
}

// pub trait AccessMut: MappableWrapper {
//   type UndoRecorder: RecordUndo<Self::Target>;
//   fn raw_write(&mut self) -> (&mut Self::Target, &Self::UndoRecorder);
//   fn write(&mut self) -> &mut Self::Target {
//     let (value, undo_recorder) = self.raw_write();
//     undo_recorder.record_undo::<_, Perform>(&*value);
//
//     struct Perform;
//     impl<T: SimulationStateData> PerformUndo<T> for Perform {
//       type UndoData = T;
//       fn perform_undo(data: &mut T, undo_data: Self::UndoData) {
//         *data = undo_data;
//       }
//     }
//     value
//   }
//   //fn record_undo(&mut self, undo: impl UndoData<Self::Target>);
// }

// impl<'a, T: SimulationStateData> LensMappable for &'a T {
//   type LensTarget = T;
//   type Mapped<L: Lens<Self::LensTarget>> = &'a L::Target;
//   fn map<L: Lens<Self::LensTarget>>(self, lens: &L) -> Self::Mapped<L> {
//     lens.get(self)
//   }
// }
//
// impl<'a, T: SimulationStateData> LensMappable for &'a mut T {
//   type LensTarget = T;
//   type Mapped<L: Lens<Self::LensTarget>> = &'a mut L::Target;
//   fn map<L: Lens<Self::LensTarget>>(self, lens: &L) -> Self::Mapped<L> {
//     lens.get_mut(self)
//   }
// }

// #[repr(transparent)]
// pub struct UndoRecordingWrapper<'a,T>(&'a mut T);
//
// impl<T: SimulationStateData> MappableWrapper for UndoRecordingWrapper<T> {
//   type Target = T;
//   type Mapped<L: Lens<Self::Target>> = UndoRecordingWrapper<L::Target>;
//
//   fn map<L: Lens<Self::Target>>(&self, lens: &L) -> &Self::Mapped<L> {
//     UndoRecordingWrapper(lens.get(&self.0);
//     // Safety: Wrapper<U> has the same memory layout as U
//     let wrapped_mapped_inner: &UndoRecordingWrapper<L::Target> = unsafe {std::mem::transmute(mapped_inner)}
//     wrapped_mapped_inner
//   }
// }

pub struct WriteRefUndoRecorder<
  'a,
  'acc: 'a,
  E: EntityKind,
  A: EventAccessor<'acc> + 'a,
  L: Lens<MutableData<E, A::EntityHandleKind>>,
> {
  lens: L,
  undo_recorder: A::UndoRecorder<'a, MutableData<E, A::EntityHandleKind>>,
}
// pub struct WriteRef<
//   'a,
//   'acc: 'a,
//   E: EntityKind,
//   A: EventAccessor<'acc>,
//   L: Lens<MutableData<E, A::EntityHandleKind>>,
// > {
//   value: &'a mut L::Target,
//   undo_recorder: WriteRefUndoRecorder<'a, 'acc, E, A, L>,
// }

impl<
    'a,
    'acc: 'a,
    E: EntityKind,
    A: EventAccessor<'acc>,
    L: Lens<MutableData<E, A::EntityHandleKind>>,
  > LensMappable<L::Target> for WriteRefUndoRecorder<'a, 'acc, E, A, L>
{
  type Mapped<L2: Lens<L::Target>> = WriteRefUndoRecorder<'a, 'acc, E, A, (L, L2)>;
  fn map<L2: Lens<L::Target>>(&self, lens2: &L2) -> Self::Mapped<L2> {
    WriteRefUndoRecorder {
      lens: (self.lens.clone(), lens2.clone()),
      undo_recorder: self.undo_recorder.clone(),
    }
  }
}

pub type UndoRecordingRef<'a, 'acc, E, A, L> = AugmentedRefMut<
  'a,
  <L as Lens<MutableData<E, <A as Accessor>::EntityHandleKind>>>::Target,
  WriteRefUndoRecorder<'a, 'acc, E, A, L>,
>;

impl<
    'a,
    'acc: 'a,
    E: EntityKind,
    A: EventAccessor<'acc>,
    L: Lens<MutableData<E, A::EntityHandleKind>>,
  > UndoRecordingRef<'a, 'acc, E, A, L>
{
  pub fn write(self) -> &'a mut L::Target {
    self
      .metadata
      .undo_recorder
      .record_undo::<
        (&L, &L::Target),
        UndoInContained <MutableData<E, A::EntityHandleKind>, L, RestoreOldValue<L::Target>>
      >(&(&self.metadata.lens,self.data));
    self.data
  }
}

// impl<
//     'a,
//     'acc: 'a,
//     E: EntityKind,
//     A: EventAccessor<'acc> + 'a,
//     L: Lens<MutableData<E, A::EntityHandleKind>>,
//   > AccessMut for WriteRef<'a, 'acc, E, A, L>
// {
//   type UndoRecorder = WriteRefUndoRecorder<'a, 'acc, E, A, L>;
//   fn raw_write(&mut self) -> (&mut Self::Target, &Self::UndoRecorder) {
//     (self.value, &mut self.undo_recorder)
//   }
// }

// impl<
//     'a,
//     'acc: 'a,
//     E: EntityKind,
//     A: EventAccessor<'acc>,
//     L: Lens<MutableData<E, A::EntityHandleKind>>,
//   > Deref for WriteRef<'a, 'acc, E, A, L>
// {
//   type Target = L::Target;
//
//   fn deref(&self) -> &Self::Target {
//     self.value
//   }
// }

impl<
    'a,
    'acc: 'a,
    E: EntityKind,
    A: EventAccessor<'acc>,
    L: Lens<MutableData<E, A::EntityHandleKind>>,
  > RecordUndo<L::Target> for WriteRefUndoRecorder<'a, 'acc, E, A, L>
{
  fn record_undo<S: Serialize, P: PerformUndo<L::Target>>(&self, undo_data: &S) {
    self
      .undo_recorder
      .record_undo::<(&L, &S), Perform<MutableData<E, A::EntityHandleKind>, L, P>>(&(
        &self.lens, undo_data,
      ));

    struct Perform<T, L, P>(PhantomData<*const (T, L, P)>);
    impl<T: 'static, L: Lens<T>, P: PerformUndo<L::Target>> PerformUndo<T> for Perform<T, L, P> {
      type UndoData = (L, P::UndoData);
      fn perform_undo(data: &mut T, (lens, undo_data): Self::UndoData) {
        P::perform_undo(lens.get_mut(data), undo_data);
      }
    }
  }
}

impl<T: SimulationStateData, C: IndexMut<usize, Output = T>> Lens<C> for usize {
  type Target = T;

  fn get<'a>(&self, container: &'a C) -> &'a Self::Target {
    container.index(*self)
  }

  fn get_mut<'a>(&self, container: &'a mut C) -> &'a mut Self::Target {
    container.index_mut(*self)
  }
}

// pub struct VecAccessWrapper<A>(A);
// impl<T> HasDefaultAccessWrapper for Vec<T> {
//   type Wrapper<A: MappableWrapper> = VecAccessWrapper<A>;
//   fn wrap_access<A: MappableWrapper>(input: A) -> Self::Wrapper<A> {
//     VecAccessWrapper(input)
//   }
// }
// impl<A> Deref for VecAccessWrapper<A> {
//   type Target = A;
//
//   fn deref(&self) -> &Self::Target {
//     &self.0
//   }
// }
// impl<A> DerefMut for VecAccessWrapper<A> {
//   fn deref_mut(&mut self) -> &mut Self::Target {
//     &mut self.0
//   }
// }

// pub trait VecWrapperExt<T: SimulationStateData>: LensMappable<LensTarget = Vec<T>> {
//   type Iterator: Iterator<Item = Self::Mapped<usize>>;
//   fn get(self, index: usize) -> Option<Self::Mapped<usize>>;
//   fn iter(self) -> Self::Iterator;
// }
//
// pub type VecWrapperIterator<T: SimulationStateData, W: LensMappable<LensTarget = Vec<T>>> =
//   impl Iterator<Item = W::Mapped<usize>>;
//
// impl<T: SimulationStateData, W: LensMappable<LensTarget = Vec<T>>> VecWrapperExt<T> for W {
//   type Iterator = VecWrapperIterator<T, Self>;
//
//   fn get(self, index: usize) -> Option<Self::Mapped<usize>> {
//     (index < self.read().len()).then(move || self.map(&index))
//   }
//
//   fn iter(self) -> Self::Iterator {
//     (0..self.read().len()).map(move |index| self.map(&index))
//   }
//   // pub fn get(&self, index: usize) -> Option<T::Wrapper<W::Mapped<usize>>> {
//   //   (index < self.0.read().len()).then(move || T::wrap_access(self.0.map(index)))
//   // }
//   // // TODO: this had a problem where the undo recorder had to be an &mut, so an iterator that can be collected would be an aliasing violation. We can address this by making the undo recorder an & (using either a RefCell, which is very cheap, or a safety obligation for client code; and luckily the API doesn't have to commit to which of those it is, individual TimeSteward implementors can decide)
//   // pub fn iter(&self) -> impl Iterator<Item = T::Wrapper<W::Mapped<usize>>> + '_ {
//   //   (0..self.0.read().len()).map(move |index| self.get(index).unwrap())
//   // }
// }
// impl<T: SimulationStateData, A: AccessMut<Target = Vec<T>>> VecAccessWrapper<A> {
//   pub fn push(&mut self, value: T) {
//     let (v, undo_recorder) = self.raw_write();
//     v.push(value);
//     undo_recorder.record_undo::<_, Perform>(());
//
//     struct Perform;
//     impl<T> PerformUndo<Vec<T>> for Perform {
//       type UndoData = ();
//       fn perform_undo(data: &mut Vec<T>, (): Self::UndoData) {
//         data.pop();
//       }
//     }
//   }
// }
