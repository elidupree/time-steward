use std::ops::Add;

/**
An integer that may either be a runtime T, or a ZST that can produce a constant T.

By taking `impl MaybeConst<T>` rather than `T` as an argument,
functions can be generic as to whether they take the parameter as a runtime or compile time
input; in the case where they are invoked with a constant input, this allows inner functions
to be optimize with the knowledge of the constant input, without being inlined.
*/
pub trait ShiftSize: Into<u32> + Copy {}

/**
A ZST representing a specific u32 value.
*/
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Default)]
pub struct ConstU32<const VALUE: u32>;

impl<const VALUE: u32> From<ConstU32<VALUE>> for u32 {
  fn from(_: ConstU32<VALUE>) -> Self {
    VALUE
  }
}

impl ShiftSize for u32 {}
impl<const VALUE: u32> ShiftSize for ConstU32<VALUE> {}

impl<const V1: u32, const V2: u32> Add<ConstU32<V2>> for ConstU32<V1>
where
  ConstU32<{ V1 + V2 }>: Sized,
{
  type Output = ConstU32<{ V1 + V2 }>;

  fn add(self, _: ConstU32<V2>) -> Self::Output {
    ConstU32
  }
}
