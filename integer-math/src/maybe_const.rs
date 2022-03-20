/**
An integer that may either be a runtime u32, or a ZST that can produce a constant u32.

By taking `impl ShiftSize` rather than `u32` as an argument,
functions can be generic as to whether they take the parameter as a runtime or compile time
input; in the case where they are invoked with a `ConstU32` input, this allows inner functions
to be optimize with the knowledge of the constant input, even without being inlined.

Using ConstU32 may be a pessimization if many different values are used (worse code cache efficiency
due to generating excess copies of the function), but is helpful under the assumption that
a typical TimeSteward simulation will use only one, constant shift size;
in this situation, I was able to measure a 8-10% speedup in the bouncy_circles benchmarks
from using ConstU32 instead of u32.
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

// Note: I didn't actually end up using this Add impl for anything;
// being able to add ShiftSizes might hypothetically be useful, but isn't possible without
// some more cleverness in the trait definition, which I wasn't easily able to achieve.
// impl<const V1: u32, const V2: u32> Add<ConstU32<V2>> for ConstU32<V1>
// where
//   ConstU32<{ V1 + V2 }>: Sized,
// {
//   type Output = ConstU32<{ V1 + V2 }>;
//
//   fn add(self, _: ConstU32<V2>) -> Self::Output {
//     ConstU32
//   }
// }
