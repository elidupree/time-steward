use crate::SimulationStateData;
use std::ops::IndexMut;

/// A path to a subobject, which can map either & or &mut access.
///
/// The name "Lens" is inspired by its usage in functional programming, but
/// this type does not provide non-mutating updates.
pub trait Lens<T>: SimulationStateData + Send + Sync {
  type Target: SimulationStateData;
  fn get<'a>(&self, object: &'a T) -> &'a Self::Target;
  fn get_mut<'a>(&self, object: &'a mut T) -> &'a mut Self::Target;
}

pub trait LensMappable<T> {
  type Mapped<L: Lens<T>>: LensMappable<L::Target>;
  fn map<L: Lens<T>>(&self, lens: &L) -> Self::Mapped<L>;
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
  type Target;
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

impl<T: SimulationStateData, C: IndexMut<usize, Output = T>> Lens<C> for usize {
  type Target = T;

  fn get<'a>(&self, container: &'a C) -> &'a Self::Target {
    container.index(*self)
  }

  fn get_mut<'a>(&self, container: &'a mut C) -> &'a mut Self::Target {
    container.index_mut(*self)
  }
}
