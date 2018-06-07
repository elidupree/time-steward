use num::{Signed};

use super::*;


#[derive (Debug, Fail)]
pub enum Error <T: Integer> {
  #[fail (display = "out-of-bounds value in the x^{} term (coefficient is {}, but must have abs <= {})", power, coefficient, maximum_abs)]
  CoefficientOutOfBounds {
    power: usize,
    coefficient: T,
    maximum_abs: T,
  },
  #[fail (display = "out-of-bounds input to polynomial function (for the x^{} term, |{}*{}^{}| was {}, but must have abs <= {})", power, coefficient, input, power, value, maximum_abs)]
  TermOutOfBounds {
    power: usize,
    input: T,
    coefficient: T,
    value: T,
    maximum_abs: T,
  },
  #[fail (display = "out-of-bounds input to polynomial function (for the x^{} term, |{}*{}^{}| overflowed the type)", power, input, coefficient, power)]
  TermOverflowed {
    power: usize,
    input: T,
    coefficient: T,
  },
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
pub fn evaluate_at_small_input <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, shift: u32)->T {
  let half = (T::one() << shift) >> 1u32;
  assert!(-half <= input && input <= half, "inputs to evaluate_at_small_input must be in the range [-0.5, 0.5]");
  
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
/// If this condition is broken, translate() returns Err and makes no changes.
pub fn translate <T: Integer + Signed> (coefficients: &mut [T], input: T)->Result <(), Error <T>> {
  translate_check (coefficients, input, |_| T::max_value())?;
  translate_unchecked (coefficients, input);
  Ok (())
}

/// Same as translate(), but assume no overflow will occur.
///
/// Useful in performance-critical code when you already know there won't be overflow,
/// such as in the range returned by conservative_safe_translation_range.
pub fn translate_unchecked <T: Integer + Signed> (coefficients: &mut [T], input: T) {
  coefficients.reverse();
  for index in 0..coefficients.len() {
    let coefficient = mem::replace (&mut coefficients [index], T::zero());
    for derivative in (1..(index + 1)).rev() {
      coefficients [derivative] = coefficients [derivative]*input + coefficients [derivative - 1]
    }
    coefficients [0] = coefficients [0]*input + coefficient
  }
}

pub fn within_bounds_check <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: & [T], maximum: MaximumFn)->Result <(), Error <T>> {
  for (power, coefficient) in coefficients.iter().enumerate() {
    let max = maximum (power);
    if coefficient == &T::min_value() {return Err (Error::CoefficientOutOfBounds {power, coefficient: *coefficient, maximum_abs: max})}
    let magnitude = coefficient.abs();
    if magnitude > max {return Err (Error::CoefficientOutOfBounds {power, coefficient: *coefficient, maximum_abs: max})}
  }
  Ok (())
}

pub fn translate_check <T: Integer + Signed, MaximumFn: Fn (usize)->T> (mut coefficients: & [T], input: T, maximum: MaximumFn)->Result <(), Error <T>> {
  let original_len = coefficients.len();
  loop {
    if coefficients.len() == 0 {return Ok (());}
    if coefficients [coefficients.len() - 1] == T::zero() {coefficients = & coefficients [..(coefficients.len() - 1)];}
    else {break;}
  }
  let coefficients = coefficients;
  if input == T::zero() {within_bounds_check (coefficients, maximum)?; return Ok (());}
  let mut factor = T::one();
  let input = input.abs();
  // base the safety check on the original length, so that the bounds are consistent
  // for all polynomials with a given len(), rather than being dependent on the values in them
  let sum_safety_shift = original_len as u32 - 1;
  let mut running_maximum = T::max_value();
  for (power, coefficient) in coefficients.iter().enumerate() {
    if power > 0 { match factor.checked_mul (&input) {
      None => return Err (Error::TermOverflowed {power, input, coefficient:*coefficient}),
      Some (next_factor) => factor = next_factor,
    }}
    running_maximum = min (running_maximum, maximum (power).checked_shr (sum_safety_shift).unwrap_or (T::zero()));
    if coefficient == &T::min_value() {return Err (Error::TermOverflowed {power, input, coefficient:*coefficient});}
    let magnitude = coefficient.abs();
    match magnitude.checked_mul (&factor) {
      None => return Err (Error::TermOverflowed {power, input, coefficient:*coefficient}),
      Some (term) => if term > running_maximum {return Err (Error::TermOutOfBounds {power, input, coefficient:*coefficient, value: term, maximum_abs: running_maximum})},
    }
  }
  Ok (())
}

/// Find the range of inputs to which the polynomial can safely be translated.
///
/// Magnitudes <= result are safe to translate to; magnitudes > result aren't.
/// If even the initial value is out of bounds, this function returns -1.
///
/// The third argument can impose a stricter maximum on the resulting coefficients.
pub fn exact_safe_translation_range <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: & [T], maximum: MaximumFn)->T {
  if within_bounds_check (coefficients, & maximum).is_err() {return -T::one();}
  // pick a min that's definitely legal and a max where max + 1 is definitely illegal
  let mut min = T::zero();
  let mut max = T::max_value();
  while min < max {
    let mid = mean_ceil (min, max);
    if translate_check (coefficients, mid, & maximum).is_ok() {
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
/// This function is much faster than exact_safe_translation_range, at the cost of being less precise.
pub fn conservative_safe_translation_range <T: Integer + Signed, MaximumFn: Fn (usize)->T> (coefficients: &[T], maximum: MaximumFn)->T {
  if coefficients.len() == 0 {return T::max_value()}
  if within_bounds_check (coefficients, & maximum).is_err() {return -T::one();}
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
  
  fn evaluate_exactly <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, shift: u32)->BigRational
    where BigInt: From <Coefficient> + From <T> {
    let mut result: BigRational = Ratio::zero();
    let input = Ratio::new(BigInt::from (input), BigInt::one() << shift as usize);
    for coefficient in coefficients.iter().rev() {
      result = result*&input + BigInt::from(*coefficient);
    }
    result
  }
  
  #[test]
  fn test_evaluate_at_small_input() {
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
          let result = evaluate_at_small_input (& coefficients,*input, shift);
          
          let perfect_result = evaluate_exactly (& coefficients,*input, shift);
          let difference = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap()) - perfect_result;
          assert!(difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap()));
        }
      }
    }
  }
  
  quickcheck! {
    fn quickcheck_safe_translation_is_safe (coefficients_and_maxima: Vec<(i64, i64)>, input: i64)->bool {
      let mut coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      if translate_check (& coefficients, input, maxima).is_err() {return true}
      translate_unchecked (&mut coefficients, input);
      println!( "{:?}", coefficients);
      within_bounds_check (& coefficients, maxima).is_ok()
    }
    
    fn quickcheck_safe_translation_reverses_correctly (coefficients: Vec<i64>, input: i64)->bool {
      let mut coefficients = coefficients;
      let maxima = |_: usize | i64::max_value();
      if input == i64::min_value () {return true}
      let original_coefficients = coefficients.clone ();
      if translate_check (& coefficients, input, maxima).is_err() {return true}
      translate_unchecked (&mut coefficients, input);
      if translate_check (& coefficients, -input, maxima).is_err() {return true}
      println!( "{:?}", coefficients);
      translate_unchecked (&mut coefficients, -input);
      coefficients == original_coefficients
    }
    
    fn quickcheck_exact_safe_translation_range_is_safe (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let range = exact_safe_translation_range (& coefficients, maxima);
      println!( "{:?}", range);
      range < 0 || translate_check (& coefficients, range, maxima).is_ok()
    }
    
    fn quickcheck_exact_safe_translation_range_is_maximal (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let range = exact_safe_translation_range (& coefficients, maxima);
      println!( "{:?}", range);
      range == i64::max_value() || translate_check (& coefficients, range + 1, maxima).is_err()
    }
    
    fn quickcheck_conservative_safe_translation_range_is_safe (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let exact_range = exact_safe_translation_range (& coefficients, maxima);
      let conservative_range = conservative_safe_translation_range (& coefficients, maxima);
      println!( "{:?}", (exact_range, conservative_range));
      conservative_range <= exact_range
    }
    
    fn quickcheck_conservative_safe_translation_range_is_within_half_maximal (coefficients_and_maxima: Vec<(i64, i64)>)->bool {
      let coefficients: Vec<_> = coefficients_and_maxima.iter().map (| & (coefficient,_) | coefficient).collect();
      let maxima = | index: usize | coefficients_and_maxima [index].1.checked_abs().unwrap_or (0);
      let exact_range = exact_safe_translation_range (& coefficients, maxima);
      let conservative_range = conservative_safe_translation_range (& coefficients, maxima);
      println!( "{:?}", (exact_range, conservative_range));
      conservative_range == exact_range || conservative_range > exact_range >> 1
    }
  }
}
