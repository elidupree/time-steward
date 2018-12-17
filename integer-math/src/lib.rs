#![feature(nll, try_trait, try_from, specialization)]

#[macro_use]
extern crate failure;
extern crate nalgebra;
extern crate num;
#[cfg(test)]
#[macro_use]
extern crate proptest;
#[cfg(test)]
#[macro_use]
extern crate quickcheck;
#[macro_use]
extern crate smallvec;
extern crate array_ext;
extern crate arrayvec;
#[cfg(test)]
extern crate rand;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use num::traits::{CheckedShl, CheckedShr, Signed, WrappingAdd, WrappingMul, WrappingSub, Zero};
use std::mem;
//use std::cmp::{min};
use std::convert::TryInto;
use std::fmt::{Debug, Display};
use std::ops::{
  Add, AddAssign, Mul, MulAssign, Neg, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

pub trait Integer:
  'static
  + num::PrimInt
  + num::Integer
  + num::FromPrimitive
  + AddAssign<Self>
  + SubAssign<Self>
  + MulAssign<Self>
  + WrappingAdd
  + WrappingSub
  + WrappingMul
  + for<'a> Add<&'a Self, Output = Self>
  + for<'a> Sub<&'a Self, Output = Self>
  + for<'a> Mul<&'a Self, Output = Self>
  + CheckedShl
  + CheckedShr
  + Shl<u32, Output = Self>
  + Shr<u32, Output = Self>
  + ShlAssign<u32>
  + ShrAssign<u32>
  + Debug
  + Display
  + Default
  + Send
  + Sync
{
  fn saturating_mul(self, other: Self) -> Self;
  fn total_bits() -> u32 {
    mem::size_of::<Self>() as u32 * 8
  }
  fn nonsign_bits() -> u32;
}
/*impl <T: 'static + num::PrimInt + num::Integer + num::FromPrimitive + AddAssign <Self> + SubAssign <Self> + MulAssign <Self> + WrappingAdd + WrappingSub + WrappingMul + for<'a> Add <&'a Self, Output = Self> + for<'a> Sub <&'a Self, Output = Self> + for<'a> Mul <&'a Self, Output = Self> + CheckedShl + CheckedShr + Shl <u32, Output = Self> + Shr <u32, Output = Self> + ShlAssign <u32> + ShrAssign <u32> + Debug + Display + Send + Sync> Integer for T {}*/

pub trait HasCoordinates {
  type Coordinate: Integer + Signed + Vector + HasCoordinates<Coordinate = Self::Coordinate>;
}

pub trait Vector:
  'static + Sized + Copy + Clone + Eq + HasCoordinates +
  Add <Self, Output = Self> + Sub <Self, Output = Self> + Mul <<Self as HasCoordinates>::Coordinate, Output = Self> +
  for<'a> Add <&'a Self, Output = Self> + for<'a> Sub <&'a Self, Output = Self> + //for<'a> Mul <&'a <Self as HasCoordinates>::Coordinate, Output = Self> +
  AddAssign <Self> + SubAssign <Self> + MulAssign <<Self as HasCoordinates>::Coordinate> +
  for<'a> AddAssign <&'a Self> + for<'a> SubAssign <&'a Self> + //for<'a> MulAssign <&'a <Self as HasCoordinates>::Coordinate> +
  Zero + Neg <Output = Self> +
  Debug + Send + Sync
  {
  const DIMENSIONS: usize;
  fn coordinate (&self, which: usize)->Self::Coordinate;
  fn set_coordinate (&mut self, which: usize, value: Self::Coordinate);
  fn map_coordinates <F: Fn(Self::Coordinate)->Self::Coordinate> (mut self, transform: F)->Self {
    for index in 0..Self::DIMENSIONS {
      let new_coordinate = (transform) (self.coordinate (index));
      self.set_coordinate (index, new_coordinate);
    }
    self
  }
}

pub trait DoubleSizedSignedInteger: Integer + Signed {
  type DoubleSized: Integer + Signed + From<Self> + TryInto<Self>;
}
pub type DoubleSized<T> = <T as DoubleSizedSignedInteger>::DoubleSized;

pub mod impls {
  use super::*;
  use nalgebra::*;
  macro_rules! impl_vector {
    ($([$coordinates: expr, $Vector: ident],)*) => {
      $(
        impl <T: Integer + Signed + Vector + HasCoordinates <Coordinate = T>> HasCoordinates for $Vector <T> {type Coordinate = T;}
        impl <T: Integer + Signed + Vector + HasCoordinates <Coordinate = T>> Vector for $Vector <T> {
          const DIMENSIONS: usize = $coordinates;
          fn coordinate (&self, which: usize)->Self::Coordinate {self [which]}
          fn set_coordinate (&mut self, which: usize, value: Self::Coordinate) {self [which] = value}
        }
      )*
    }
  }
  macro_rules! impl_integer {
    ($($Integer: ident $sign_bits: expr,)*) => {
      $(
        impl Integer for $Integer {
          fn saturating_mul (self, other: Self)->Self {
            self.saturating_mul(other)
          }
          fn nonsign_bits ()->u32 {
            Self::total_bits() - $sign_bits
          }
        }
      )*
    }
  }
  macro_rules! impl_signed_integer {
    ($($Integer: ident,)*) => {
      $(
        impl HasCoordinates for $Integer {type Coordinate = $Integer;}
        impl Vector for $Integer {
          const DIMENSIONS: usize = 1;
          fn coordinate (&self, _which: usize)->Self::Coordinate {*self}
          fn set_coordinate (&mut self, _which: usize, value: Self::Coordinate) {*self = value}
        }
      )*
    }
  }
  macro_rules! impl_double_sized_integer {
    ($(($Integer: ident, $Double: ident),)*) => {
      $(
        impl DoubleSizedSignedInteger for $Integer {
          type DoubleSized = $Double;
        }
      )*
    }
  }
  impl_vector!(
    [1, Vector1],
    [2, Vector2],
    [3, Vector3],
    [4, Vector4],
    [5, Vector5],
    [6, Vector6],
  );
  impl_integer! (
    i8 1, i16 1, i32 1, i64 1, i128 1, isize 1,
    u8 0, u16 0, u32 0, u64 0, u128 0, usize 0,
  );
  impl_signed_integer!(i8, i16, i32, i64, i128, isize,);
  impl_double_sized_integer!(
    (i8, i16),
    (i16, i32),
    (i32, i64),
    (i64, i128),
    //(u8, u16), (u16, u32), (u32, u64),
  );
}

/// Right-shift an integer, but round to nearest, with ties rounding to even.
///
/// This minimizes error, and avoids a directional bias.
pub fn shr_nicely_rounded<T: Integer>(input: T, shift: impl Into<u32>) -> T {
  let shift: u32 = shift.into();
  let (mask, shifted) = match T::one().checked_shl(shift) {
    Some(divisor) => (divisor.wrapping_sub(&T::one()), input >> shift),
    None => (T::max_value(), T::zero()),
  };
  // there's gotta be a better way to handle all situations...
  // This doesn't work because divisor may have wrapped around to negative
  //let half = divisor >> 1u32;
  let round_up = match shift.checked_sub(1) {
    None => return input,
    Some(half_shift) => match T::one().checked_shl(half_shift) {
      None => false,
      Some(half) => half >= T::zero() && (input & mask) + (shifted & T::one()) > half,
    },
  };
  shifted + if round_up { T::one() } else { T::zero() }
}

/// Right-shift an integer, but round to even.
///
/// This avoids a directional bias.
pub fn shr_round_to_even<T: Integer>(input: T, shift: impl Into<u32>) -> T {
  let shift: u32 = shift.into();
  let divisor = match T::one().checked_shl(shift) {
    Some(value) => value,
    None => return T::zero(),
  };
  let mask = divisor.wrapping_sub(&T::one());
  let shifted = input >> shift;
  shifted
    + if (input & mask) != T::zero() {
      shifted & T::one()
    } else {
      T::zero()
    }
}

/// Right-shift an integer, but round towards positive infinity.
pub fn shr_ceil<T: Integer>(input: T, shift: impl Into<u32>) -> T {
  let shift: u32 = shift.into();
  let divisor = match T::one().checked_shl(shift) {
    Some(value) => value,
    None => {
      return if input > T::zero() {
        T::one()
      } else {
        T::zero()
      }
    }
  };
  let mask = divisor.wrapping_sub(&T::one());
  (input >> shift)
    + if input & mask != T::zero() {
      T::one()
    } else {
      T::zero()
    }
}

/// Right-shift an integer, but allow shifting by more than the size of the type, with the expected numerical behavior.
pub fn shr_floor<T: Integer>(input: T, shift: impl Into<u32>) -> T {
  let shift: u32 = shift.into();
  match input.checked_shr(shift) {
    Some(value) => value,
    None => {
      if input >= T::zero() {
        T::zero()
      } else {
        T::zero() - T::one()
      }
    }
  }
}

/// Right-shift an integer, but round towards 0.
pub fn shr_round_towards_zero<T: Integer>(input: T, shift: impl Into<u32>) -> T {
  let shift: u32 = shift.into();
  (input
    + if input < T::zero() {
      (T::one() << shift).wrapping_sub(&T::one())
    } else {
      T::zero()
    })
    >> shift
}

/// Left-shift an integer, returning Some(input*(2^shift)) if it fits within the type, None otherwise.
pub fn overflow_checked_shl<T: Integer>(input: T, shift: impl Into<u32>) -> Option<T> {
  let shift: u32 = shift.into();
  if input == T::zero() {
    return Some(T::zero());
  }
  let maximum = match T::max_value().checked_shr(shift) {
    None => return None,
    Some(value) => value,
  };
  if input > maximum {
    return None;
  }
  let minimum = T::min_value() >> shift;
  if input < minimum {
    return None;
  }
  Some(input << shift)
}

/// Compute the arithmetic mean of two integers, rounded towards negative infinity. Never overflows.
pub fn mean_floor<T: Integer>(first: T, second: T) -> T {
  (first >> 1u32) + (second >> 1u32) + (first & second & T::one())
}

/// Compute the arithmetic mean of two integers, rounded towards positive infinity. Never overflows.
pub fn mean_ceil<T: Integer>(first: T, second: T) -> T {
  (first >> 1u32) + (second >> 1u32) + ((first | second) & T::one())
}

/// Compute the arithmetic mean of two integers, rounded towards even. Never overflows.
pub fn mean_round_to_even<T: Integer>(first: T, second: T) -> T {
  let floor = mean_floor(first, second);
  floor + ((first ^ second) & floor & T::one())
}

fn mul_shr_round<
  T: Integer + Signed,
  ShiftAmount: Copy + Into<u32>,
  ShiftRoundPos: Fn(T, u32) -> T,
  ShiftRoundNeg: Fn(T, u32) -> T,
  FinalShift: Fn(T, u32) -> T,
>(
  factor0: T,
  factor1: T,
  shift: ShiftAmount,
  shift_round_pos: ShiftRoundPos,
  shift_round_neg: ShiftRoundNeg,
  final_shift: FinalShift,
  overflow_pos: Option<T>,
  overflow_neg: Option<T>,
) -> Option<T> {
  if let Some(result) = factor0.checked_mul(&factor1) {
    return Some((final_shift)(result, shift.into()));
  }
  let mut shift = shift.into();
  let mut factor0 = factor0;
  let mut factor1 = factor1;
  while shift > 0 {
    shift -= 1;
    if factor0.abs() > factor1.abs() {
      factor0 = if factor1 > Zero::zero() {
        (shift_round_pos)(factor0, 1u32)
      } else {
        (shift_round_neg)(factor0, 1u32)
      };
    } else {
      factor1 = if factor0 > Zero::zero() {
        (shift_round_pos)(factor1, 1u32)
      } else {
        (shift_round_neg)(factor1, 1u32)
      };
    }
    if let Some(result) = factor0.checked_mul(&factor1) {
      return Some((final_shift)(result, shift.into()));
    }
  }
  if (factor0 > Zero::zero()) == (factor1 > Zero::zero()) {
    overflow_pos
  } else {
    overflow_neg
  }
}

pub fn mul_shr_round_up<T: Integer + Signed>(
  factor0: T,
  factor1: T,
  shift: impl Copy + Into<u32>,
) -> Option<T> {
  mul_shr_round(
    factor0,
    factor1,
    shift,
    shr_ceil,
    shr_floor,
    shr_ceil,
    None,
    Some(T::min_value()),
  )
}

pub fn mul_shr_round_down<T: Integer + Signed>(
  factor0: T,
  factor1: T,
  shift: impl Copy + Into<u32>,
) -> Option<T> {
  mul_shr_round(
    factor0,
    factor1,
    shift,
    shr_floor,
    shr_ceil,
    shr_floor,
    Some(T::max_value()),
    None,
  )
}

pub fn mul_shr_floor_round_up<T: Integer + Signed>(
  factor0: T,
  factor1: T,
  shift: impl Copy + Into<u32>,
) -> Option<T> {
  mul_shr_round(
    factor0,
    factor1,
    shift,
    shr_ceil,
    shr_floor,
    shr_floor,
    None,
    Some(T::min_value()),
  )
}

pub fn mul_shr_ceil_round_down<T: Integer + Signed>(
  factor0: T,
  factor1: T,
  shift: impl Copy + Into<u32>,
) -> Option<T> {
  mul_shr_round(
    factor0,
    factor1,
    shift,
    shr_floor,
    shr_ceil,
    shr_ceil,
    Some(T::max_value()),
    None,
  )
}

pub fn saturating_downcast<T: Integer + From<U> + TryInto<U>, U: Integer>(a: T) -> U {
  if a > From::from(U::max_value()) {
    U::max_value()
  } else if a < From::from(U::min_value()) {
    U::min_value()
  } else {
    a.try_into().ok().unwrap()
  }
}

pub mod array;
pub mod polynomial;
pub mod polynomial2;

#[cfg(test)]
mod tests {
  use super::*;
  use num::bigint::BigInt;
  use num::rational::Ratio;
  use num::{BigRational, Integer, One, ToPrimitive};
  use proptest::prelude::*;
  use std::cmp::Ordering;

  fn perfect_shr_nicely_rounded<T: Integer>(input: T, shift: u32) -> BigInt
  where
    BigInt: From<T>,
  {
    let perfect_result = Ratio::new(BigInt::from(input), BigInt::one() << shift as usize);
    let rounded_down = perfect_result.floor();
    let fraction = &perfect_result - &rounded_down;
    let rounded_down = rounded_down.to_integer();
    match fraction.cmp(&Ratio::new(BigInt::one(), BigInt::one() << 1)) {
      Ordering::Less => rounded_down,
      Ordering::Greater => rounded_down + BigInt::one(),
      Ordering::Equal => &rounded_down + rounded_down.mod_floor(&(BigInt::one() << 1)),
    }
  }

  fn perfect_shr_round_to_even<T: Integer>(input: T, shift: u32) -> BigInt
  where
    BigInt: From<T>,
  {
    let perfect_result = Ratio::new(BigInt::from(input), BigInt::one() << shift as usize);
    let rounded_down = perfect_result.floor();
    if perfect_result == rounded_down {
      rounded_down.to_integer()
    } else {
      let rounded_down = rounded_down.to_integer();
      if &rounded_down >> 1 << 1 == rounded_down {
        rounded_down
      } else {
        rounded_down + 1
      }
    }
  }

  fn perfect_shr_floor<T: Integer>(input: T, shift: u32) -> BigInt
  where
    BigInt: From<T>,
  {
    let perfect_result = Ratio::new(BigInt::from(input), BigInt::one() << shift as usize);
    perfect_result.floor().to_integer()
  }
  fn perfect_shr_ceil<T: Integer>(input: T, shift: u32) -> BigInt
  where
    BigInt: From<T>,
  {
    let perfect_result = Ratio::new(BigInt::from(input), BigInt::one() << shift as usize);
    perfect_result.ceil().to_integer()
  }

  #[test]
  fn test_shr_nicely_rounded() {
    let inputs: Vec<(i64, u32, i64)> = vec![
      (0, 0, 0),
      (0, 5, 0),
      (5, 0, 5),
      (1, 3, 0),
      (4, 3, 0),
      (5, 3, 1),
      (999, 1, 500),
      (998, 1, 499),
      (997, 1, 498),
    ];
    for (input, shift, result) in inputs {
      println!("{:?}", (input, shift, result));
      assert_eq!(shr_nicely_rounded(input, shift), result);
      assert_eq!(shr_nicely_rounded(-input, shift), -result);
    }
  }

  #[test]
  fn test_shr_round_to_even() {
    let inputs: Vec<(i64, u32, i64)> = vec![
      (0, 0, 0),
      (0, 5, 0),
      (5, 0, 5),
      (1, 3, 0),
      (4, 3, 0),
      (5, 3, 0),
      (999, 1, 500),
      (998, 1, 499),
      (997, 1, 498),
    ];
    for (input, shift, result) in inputs {
      println!("{:?}", (input, shift, result));
      assert_eq!(shr_round_to_even(input, shift), result);
      assert_eq!(shr_round_to_even(-input, shift), -result);
    }
  }

  proptest! {
    #[test]
    fn randomly_test_shr_nicely_rounded_signed (input in any::<i32>(), shift in 0u32..40) {
      let result = shr_nicely_rounded (input, shift);
      let perfect_result = perfect_shr_nicely_rounded (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_nicely_rounded_unsigned (input in any::<u32>(), shift in 0u32..40) {
      let result = shr_nicely_rounded (input, shift);
      let perfect_result = perfect_shr_nicely_rounded (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_round_to_even_signed (input in any::<i32>(), shift in 0u32..40) {
      let result = shr_round_to_even (input, shift);
      let perfect_result = perfect_shr_round_to_even (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_round_to_even_unsigned (input in any::<u32>(), shift in 0u32..40) {
      let result = shr_round_to_even (input, shift);
      let perfect_result = perfect_shr_round_to_even (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_ceil_signed (input in any::<i32>(), shift in 0u32..40) {
      let result = shr_ceil (input, shift);
      let perfect_result = perfect_shr_ceil (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_ceil_unsigned (input in any::<u32>(), shift in 0u32..40) {
      let result = shr_ceil (input, shift);
      let perfect_result = perfect_shr_ceil (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_floor_signed (input in any::<i32>(), shift in 0u32..40) {
      let result = shr_floor (input, shift);
      let perfect_result = perfect_shr_floor (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_shr_floor_unsigned (input in any::<u32>(), shift in 0u32..40) {
      let result = shr_floor (input, shift);
      let perfect_result = perfect_shr_floor (input, shift);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      prop_assert_eq!(perfect_result, BigInt::from (result))
    }

    #[test]
    fn randomly_test_overflow_checked_shl (input in any::<i32>(), shift in 0u32..40) {
      let result = overflow_checked_shl (input, shift);
      let perfect_result = BigInt::from (input) << shift as usize;
      prop_assert_eq!(result, perfect_result.to_i32())
    }

    #[test]
    fn randomly_test_mul_shr_out_of_bounds (factor0 in any::<i32>(), factor1 in any::<i32>(), shift in 0u32..40) {
      let perfect_result = BigRational::new (BigInt::from (factor0)*BigInt::from (factor1), BigInt::from (1i64 << shift));
      let upper_bound = mul_shr_round_up(factor0, factor1, shift);
      if let Some(upper_bound) = upper_bound {
        prop_assert!(BigRational::from (BigInt::from (upper_bound)) >= perfect_result, "{:?} < {:?}", upper_bound, perfect_result);
      }
      let lower_bound = mul_shr_round_down(factor0, factor1, shift);
      if let Some(lower_bound) = lower_bound {
        prop_assert!(BigRational::from (BigInt::from (lower_bound)) <= perfect_result, "{:?} > {:?}", lower_bound, perfect_result);

      }
    }

    #[test]
    fn randomly_test_mul_shr_in_bounds (factor0 in any::<i16>(), factor1 in any::<i16>(), shift in 0u32..40) {
      let factor0: i32 = factor0.into();
      let factor1: i32 = factor1.into();
      let perfect_result = BigRational::new (BigInt::from (factor0)*BigInt::from (factor1), BigInt::from (1i64 << shift));
      let upper_bound = mul_shr_round_up(factor0, factor1, shift);
      if let Some(upper_bound) = upper_bound {
        prop_assert!(BigRational::from (BigInt::from (upper_bound)) >= perfect_result, "{:?} < {:?}", upper_bound, perfect_result);
      }
      let lower_bound = mul_shr_round_down(factor0, factor1, shift);
      if let Some(lower_bound) = lower_bound {
        prop_assert!(BigRational::from (BigInt::from (lower_bound)) <= perfect_result, "{:?} > {:?}", lower_bound, perfect_result);

      }
    }

  }
}
