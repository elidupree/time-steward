use nalgebra::Vector2;
use std::cmp::max;
use super::rounding_error_tolerant_math::*;
use std::ops::{Add, Sub, Mul, Neg, Shl, Shr, BitAnd};
use array_ext::*;
use num::{PrimInt, Signed};


/// Right-shift an integer, but round to nearest, with ties rounding to even.
///
/// This minimizes error, and avoids a directional bias.
pub fn right_shift_nicely_rounded <T: PrimInt> (input: T, shift: usize)->T {
  if shift == 0 {return input}
  let half = T::one() << (shift - 1);
  let mask = (T::one() << shift) - T::one();
  if input & mask == half {
    let shifted = input >> shift;
    shifted + (shifted & T::one())
  } else {
    (input + half) >> shift
  }
}


/// Approximately evaluate an integer polynomial at an input in the range [-0.5, 0.5].
///
/// The input is represented as an integer combined with a right-shift size.
/// The coefficients are required to be small enough that `coefficient.abs() << shift` does not overflow.
/// Given these conditions, the result is guaranteed to be strictly within 1 of the ideal result.
/// For instance, if the ideal result was 2.125, this function could return 2 or 3,
/// but if it was 2, this function can only return exactly 2.
pub fn evaluate_polynomial_fractional <Coefficient: Copy, T: PrimInt + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, shift: usize)->T {
  if coefficients.len () == 0 {return T::zero()}
  let half = T::one() << (shift - 1);
  assert!(-half <= input && input <= half, "inputs to evaluate_polynomial_fractional must be in the range [-0.5, 0.5]");
  let mut result = T::zero();
  for coefficient in coefficients.iter().skip(1).rev() {
    result = right_shift_nicely_rounded ((result + (*coefficient).into())*input, shift);
  }
  result + coefficients [0].into()
}

#[cfg (test)]
mod tests {
  use super::*;
  use num::FromPrimitive;
  use num::bigint::BigInt;
  use num::rational::{Ratio, BigRational};  
  
  #[test]
  fn test_right_shift_nicely_rounded() {
    let inputs: Vec<(i64, usize, i64)> = vec![
      (0, 0, 0), (0, 5, 0), (1, 3, 0), (4, 3, 0), (5, 3, 1),
      (999, 1, 500), (998, 1, 499), (997, 1, 498)
    ];
    for (input, shift, result) in inputs {
      assert_eq!(right_shift_nicely_rounded (input, shift), result);
      assert_eq!(right_shift_nicely_rounded (-input, shift), -result);
    }
  }
  
  quickcheck! {
    fn evaluate_polynomial_fractional_is_accurate (coefficients: Vec<i32>, input: i32, shift: u8)->bool {
      //scale everything to reasonable values
      let shift = (shift >> 5) as usize + 1; //up to 16, so all i32 coefficients are valid
      let half = 1i64 << (shift - 1);
      let input = ((input as i64)*half) >> 31;
      let result = evaluate_polynomial_fractional (& coefficients, input, shift) ;
      let mut perfect_result: BigRational = Ratio::from_integer (FromPrimitive::from_i64 (0).unwrap());
      let perfect_input = Ratio::from_integer (BigInt::from_i64 (input).unwrap())/Ratio::from_integer (BigInt::from_i64 (1i64).unwrap() << shift);
      for coefficient in coefficients.iter().rev() {
        perfect_result = perfect_result*& perfect_input + Ratio::from_integer (FromPrimitive::from_i32 (*coefficient).unwrap ());
      }
      let difference = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap()) - perfect_result;
      difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap())
    }
  }
}
