use nalgebra::Vector2;
use std::cmp::max;
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
/// For a signed integer type with B bits:
/// Each coefficient, except the constant one, must obey coefficient.abs() < 1 << (B - 1 - shift),
/// i.e. be small enough that `coefficient.abs() << shift` does not overflow.
/// The constant coefficient must obey coefficient.abs() <= (1 << (B - 1)) - (1 << (B - 1 - shift)),
/// i.e. be small enough that you can add one of the other coefficients to it without overflowing.
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
  use rand::distributions::Distribution;  
  use rand::distributions::range::{Range};
  use rand::{Rng, SeedableRng};
  
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
  
  #[test]
  fn test_evaluate_polynomial_fractional() {
    let mut generator =::rand::chacha::ChaChaRng::from_seed ([33; 32]) ;
    for shift in 0..5 {
      let shift = 1 << shift;
      let half = 1i64 << (shift - 1);
      let maximum = ((1u64 << (63 - shift)) - 1) as i64;
      let constant_maximum = ((1u64 << 63) - (1u64 << (63 - shift))) as i64;
      let range = Range::new_inclusive (- maximum, maximum) ;
      let constant_range = Range::new_inclusive (- constant_maximum, constant_maximum);
      let mut test_inputs = vec![- half, - half + 1, - 1, 0, 1, half - 1, half];
      if half > 2 {for _ in 0..6 {test_inputs.push (generator.gen_range (- half + 2, half - 1));}}
      for _ in 0..40 {
        let mut coefficients = vec![constant_range.sample (&mut generator)];
        while generator.gen() {coefficients.push (range.sample (&mut generator));}
        for input in test_inputs.iter() {
          let result = evaluate_polynomial_fractional (& coefficients,*input, shift);
          
          let mut perfect_result: BigRational = Ratio::from_integer (FromPrimitive::from_i64 (0).unwrap());
          let perfect_input = Ratio::from_integer (BigInt::from_i64 (*input).unwrap())/Ratio::from_integer (BigInt::from_i64 (1i64).unwrap() << shift);
          for coefficient in coefficients.iter().rev() {
            perfect_result = perfect_result*& perfect_input + Ratio::from_integer (FromPrimitive::from_i64 (*coefficient).unwrap ());
          }
          let difference = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap()) - perfect_result;
          assert!(difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap()));
        }
      }
    }
  }
}
