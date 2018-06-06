use num::{self, Signed};
use num::traits::{WrappingAdd, WrappingSub, WrappingMul, CheckedShl, CheckedShr};
use std::mem;
use std::cmp::{min};
use std::ops::{Shl, Shr};

pub trait Integer: num::PrimInt + num::Integer + WrappingAdd + WrappingSub + WrappingMul + CheckedShl + CheckedShr + Shl <u32, Output = Self> + Shr <u32, Output = Self> {}
impl <T: num::PrimInt + num::Integer + WrappingAdd + WrappingSub + WrappingMul + CheckedShl + CheckedShr + Shl <u32, Output = Self> + Shr <u32, Output = Self> > Integer for T {}

/// Right-shift an integer, but round to nearest, with ties rounding to even.
///
/// This minimizes error, and avoids a directional bias.
pub fn shr_nicely_rounded <T: Integer> (input: T, shift: u32)->T {
  if shift == 0 {return input}
  let divisor = match T::one().checked_shl ( shift ) {Some (value) => value, None => return T::zero()};
  let half = T::one() << (shift - 1);
  let mask = divisor.wrapping_sub (&T::one());
  if (input & mask) == half {
    let shifted = input >> shift;
    shifted + (shifted & T::one())
  } else {
    (input + half) >> shift
  }
}

/// Left-shift an integer, returning Some(input*(2^shift)) if it fits within the type, None otherwise.
pub fn overflow_checked_shl <T: Integer> (input: T, shift: u32)->Option <T> {
  if input == T::zero() {return Some (T::zero())}
  let maximum = match T::max_value().checked_shr (shift) {
    None => return None,
    Some (value) => value,
  };
  if input > maximum {return None}
  let minimum = T::min_value() >> shift;
  if input < minimum {return None}
  Some (input << shift)
}

/// Compute the arithmetic mean of two integers, rounded towards negative infinity. Never overflows.
pub fn mean_floor <T: Integer> (first: T, second: T)->T {
  (first >> 1u32) + (second >> 1u32) + (first & second & T::one())
}

/// Compute the arithmetic mean of two integers, rounded towards positive infinity. Never overflows.
pub fn mean_ceil <T: Integer> (first: T, second: T)->T {
  (first >> 1u32) + (second >> 1u32) + ((first | second) & T::one())
}



/// Approximately evaluate an integer polynomial at an input in the range [-0.5, 0.5].
///
/// The input is represented as an integer combined with a right-shift size.
/// Each coefficient, except the constant one, must obey coefficient.abs() <= T::max_value() >> shift.
/// The constant coefficient must obey coefficient.abs() <= T::max_value() - (T::max_value() >> shift),
/// i.e. be small enough that you can add one of the other coefficients to it without overflowing.
/// Given these conditions, the result is guaranteed to be strictly within 1 of the ideal result.
/// For instance, if the ideal result was 2.125, this function could return 2 or 3,
/// but if it was 2, this function can only return exactly 2.
pub fn evaluate_polynomial_at_small_input <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, shift: u32)->T {
  let half = (T::one() << shift) >> 1u32;
  assert!(-half <= input && input <= half, "inputs to evaluate_polynomial_at_small_input must be in the range [-0.5, 0.5]");
  
  if coefficients.len () == 0 {return T::zero()}
    
  let mut result = T::zero();
  for coefficient in coefficients.iter().skip(1).rev() {
    result = shr_nicely_rounded ((result + (*coefficient).into())*input, shift);
  }
  result + coefficients [0].into()
}

/// Translate a polynomial horizontally, letting it take inputs relative to a given input instead of relative to 0.
///
/// This is equivalent to replacing its coefficients with its Taylor coefficients at the given input.
/// This is equivalent to translating the graph *left* by the given input amount.
///
/// For constant polynomials, this is a no-op.
///
/// To avoid overflow, each term, when evaluating at the given input,
/// must obey term.abs() <= T::max_value() >> (coefficients.len() - 1).
/// If this condition is broken, translate_polynomial() returns Err and makes no changes.
pub fn translate_polynomial <T: Integer + Signed> (coefficients: &mut [T], input: T)->Result <(),()> {
  if !safe_to_translate_polynomial_to (coefficients, input, |_| T::max_value()) {return Err (())}
  translate_polynomial_unchecked (coefficients, input) ;
  Ok (())
}

/// Same as translate_polynomial(), but assume no overflow will occur.
///
/// Useful in performance-critical code when you already know there won't be overflow,
/// such as in the range returned by conservative_safe_polynomial_translation_range.
pub fn translate_polynomial_unchecked <T: Integer + Signed> (coefficients: &mut [T], input: T) {
  coefficients.reverse();
  for index in 0..coefficients.len() {
    let coefficient = mem::replace (&mut coefficients [index], T::zero());
    for derivative in (1..(index + 1)).rev() {
      coefficients [derivative] = coefficients [derivative]*input + coefficients [derivative - 1]
    }
    coefficients [0] = coefficients [0]*input + coefficient
  }
}

pub fn polynomial_is_within_bounds <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: & [T], maximum: MaximumFn)->bool {
  for (power, coefficient) in coefficients.iter().enumerate() {
    if coefficient == &T::min_value() {return false;}
    let magnitude = coefficient.abs();
    if magnitude > maximum (power) {return false;}
  }
  true
}

pub fn safe_to_translate_polynomial_to <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: & [T], input: T, maximum: MaximumFn)->bool {
  if coefficients.len() == 0 {return true;}
  if input == T::zero() {return polynomial_is_within_bounds (coefficients, maximum);}
  let mut factor = T::one();
  let input = input.abs();
  let sum_safety_shift = coefficients.len() as u32 - 1;
  let mut running_maximum = T::max_value();
  for (power, coefficient) in coefficients.iter().enumerate() {
    running_maximum = min (running_maximum, maximum (power).checked_shr (sum_safety_shift).unwrap_or (T::zero()));
    if coefficient == &T::min_value() {return false;}
    let magnitude = coefficient.abs();
    match magnitude.checked_mul (&factor) {
      None => return false,
      Some (term) => if term > running_maximum {return false;},
    }
    if power + 1 < coefficients.len() { match factor.checked_mul (&input) {
      None => return false,
      Some (next_factor) => factor = next_factor,
    }}
  }
  true
}

/// Find the range of inputs to which the polynomial can safely be translated.
///
/// Magnitudes <= result are safe to translate to; magnitudes > result aren't.
/// If even the initial value is out of bounds, this function returns -1.
///
/// The third argument can impose a stricter maximum on the resulting coefficients.
pub fn exact_safe_polynomial_translation_range <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: & [T], maximum: MaximumFn)->T {
  if !polynomial_is_within_bounds (coefficients, & maximum) {return -T::one();}
  // pick a min that's definitely legal and a max where max + 1 is definitely illegal
  let mut min = T::zero();
  let mut max = T::max_value();
  while min < max {
    let mid = mean_ceil (min, max);
    if safe_to_translate_polynomial_to (coefficients, mid, & maximum) {
      min = mid;
    } else {
      max = mid - T::one();
    }
  }
  min
}

/// Find a range of inputs to which the polynomial can safely be translated.
///
/// Magnitudes <= result are safe to translate to.
/// Magnitudes >= result*2 aren't.
/// If even the initial value is out of bounds, this function returns -1.
///
/// The third argument can impose a stricter maximum on the resulting coefficients.
///
/// This function is much faster than exact_safe_polynomial_translation_range, at the cost of being less precise.
pub fn conservative_safe_polynomial_translation_range <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: &[T], maximum: MaximumFn)->T {
  if coefficients.len() == 0 {return T::max_value()}
  if !polynomial_is_within_bounds (coefficients, & maximum) {return -T::one();}
  let sum_safety_shift = coefficients.len() as u32 - 1;
  let mut running_maximum = T::max_value();
  let mut result_shift = mem::size_of::<T>()*8 - 2;
  for (power, coefficient) in coefficients.iter().enumerate() {
    let magnitude = coefficient.abs();
    running_maximum = min (running_maximum, maximum (power).checked_shr (sum_safety_shift).unwrap_or (T::zero()));
    while magnitude > running_maximum.checked_shr((result_shift*power) as u32).unwrap_or (T::zero()) {
      if result_shift == 0 {return T::zero();}
      result_shift -= 1;
    }
  }
  T::one() << result_shift
}

#[cfg (test)]
mod tests {
  use super::*;
  use num::{Zero, One, FromPrimitive, Integer};
  use num::bigint::BigInt;
  use num::rational::{Ratio, BigRational};
  use rand::distributions::Distribution;  
  use rand::distributions::range::{Range};
  use rand::{Rng, SeedableRng};
  use std::cmp::Ordering;
  
  fn evaluate_polynomial_exactly <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, shift: u32)->BigRational
    where BigInt: From <Coefficient> + From <T> {
    let mut result: BigRational = Ratio::zero();
    let input = Ratio::new(BigInt::from (input), BigInt::one() << shift as usize);
    for coefficient in coefficients.iter().rev() {
      result = result*&input + BigInt::from(*coefficient);
    }
    result
  }
  
  fn perfect_shr_nicely_rounded <T: Integer> (input: T, shift: u32)->BigInt where BigInt: From <T> {
    let perfect_result = Ratio::new (BigInt::from (input), BigInt::one() << shift as usize);
    let rounded_down = perfect_result.floor();
    let fraction = & perfect_result - & rounded_down;
    let rounded_down = rounded_down.to_integer();
    match fraction.cmp (& Ratio::new (BigInt::one(), BigInt::one() << 1)) {
      Ordering::Less => rounded_down, Ordering::Greater => rounded_down + BigInt::one() ,
      Ordering::Equal => & rounded_down + rounded_down.mod_floor (& (BigInt::one() << 1)),
    }
  }
  
  #[test]
  fn test_shr_nicely_rounded() {
    let inputs: Vec<(i64, u32, i64)> = vec![
      (0, 0, 0), (0, 5, 0), (1, 3, 0), (4, 3, 0), (5, 3, 1),
      (999, 1, 500), (998, 1, 499), (997, 1, 498)
    ];
    for (input, shift, result) in inputs {
      assert_eq!(shr_nicely_rounded (input, shift), result);
      assert_eq!(shr_nicely_rounded (-input, shift), -result);
    }
  }
  
  #[test]
  fn test_evaluate_polynomial_at_small_input() {
    let mut generator =::rand::chacha::ChaChaRng::from_seed ([33; 32]) ;
    for shift in 0..5 {
      let shift = 1 << shift;
      let half = 1i64 << (shift - 1);
      let maximum = i64::max_value() >> shift;
      let constant_maximum = i64::max_value() - maximum;
      let range = Range::new_inclusive (- maximum, maximum) ;
      let constant_range = Range::new_inclusive (- constant_maximum, constant_maximum);
      let mut test_inputs = vec![- half, - half + 1, - 1, 0, 1, half - 1, half];
      if half > 2 {for _ in 0..6 {test_inputs.push (generator.gen_range (- half + 2, half - 1));}}
      for _ in 0..40 {
        let mut coefficients = vec![constant_range.sample (&mut generator)];
        while generator.gen() {coefficients.push (range.sample (&mut generator));}
        for input in test_inputs.iter() {
          let result = evaluate_polynomial_at_small_input (& coefficients,*input, shift);
          
          let perfect_result = evaluate_polynomial_exactly (& coefficients,*input, shift);
          let difference = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap()) - perfect_result;
          assert!(difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap()));
        }
      }
    }
  }
  
  quickcheck! {
    fn quickcheck_shr_nicely_rounded_signed (input: i32, shift: u8)->bool {
      let result = shr_nicely_rounded (input, shift as u32);
      let perfect_result = perfect_shr_nicely_rounded (input, shift as u32);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      perfect_result == BigInt::from (result)
    }
    
    fn quickcheck_shr_nicely_rounded_unsigned (input: u32, shift: u8)->bool {
      let result = shr_nicely_rounded (input, shift as u32);
      let perfect_result = perfect_shr_nicely_rounded (input, shift as u32);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      perfect_result == BigInt::from (result)
    }
    
    fn quickcheck_safe_translation_is_safe (coefficients_and_maxima: Vec<(i64, i64)>, input: i64)->bool {
      let mut coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let safe = safe_to_translate_polynomial_to(& coefficients, input, maxima);
      if !safe {return true}
      translate_polynomial_unchecked (&mut coefficients, input);
      println!( "{:?}", coefficients);
      polynomial_is_within_bounds (& coefficients, maxima)
    }
    
    fn quickcheck_safe_translation_reverses_correctly (coefficients: Vec<i64>, input: i64)->bool {
      let mut coefficients = coefficients;
      let maxima = |_: usize | i64::max_value();
      if input == i64::min_value () {return true}
      let original_coefficients = coefficients.clone ();
      if !safe_to_translate_polynomial_to(& coefficients, input, maxima) {return true}
      translate_polynomial_unchecked (&mut coefficients, input);
      if !safe_to_translate_polynomial_to(& coefficients, -input, maxima) {return true}
      println!( "{:?}", coefficients);
      translate_polynomial_unchecked (&mut coefficients, -input);
      coefficients == original_coefficients
    }
    
    fn quickcheck_exact_safe_translation_range_is_safe (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let range = exact_safe_polynomial_translation_range (& coefficients, maxima);
      println!( "{:?}", range);
      range < 0 || safe_to_translate_polynomial_to (& coefficients, range, maxima)
    }
    
    fn quickcheck_exact_safe_translation_range_is_maximal (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let range = exact_safe_polynomial_translation_range (& coefficients, maxima);
      println!( "{:?}", range);
      range == i64::max_value() || !safe_to_translate_polynomial_to (& coefficients, range + 1, maxima)
    }
    
    fn quickcheck_conservative_safe_translation_range_is_safe (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let exact_range = exact_safe_polynomial_translation_range (& coefficients, maxima);
      let conservative_range = conservative_safe_polynomial_translation_range (& coefficients, maxima);
      println!( "{:?}", (exact_range, conservative_range));
      conservative_range <= exact_range
    }
    
    fn quickcheck_conservative_safe_translation_range_is_within_half_maximal (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let exact_range = exact_safe_polynomial_translation_range (& coefficients, maxima);
      let conservative_range = conservative_safe_polynomial_translation_range (& coefficients, maxima);
      println!( "{:?}", (exact_range, conservative_range));
      conservative_range == exact_range || conservative_range > exact_range >> 1
    }
  }
}
