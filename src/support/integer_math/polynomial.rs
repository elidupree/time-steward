use num::{Signed};
use smallvec::SmallVec;
use array_ext::*;
use std::cmp::max;

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

/// Approximately evaluate an integer polynomial at a non-integer input.
///
/// The result is guaranteed to be strictly within 1 of the ideal result.
/// For instance, if the ideal result was 2.125, this function could return 2 or 3,
/// but if it was 2, this function can only return exactly 2.
pub fn evaluate_at_fractional_input <Coefficient: Copy + Debug, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, input_shift: u32)->Result <T, ()>  {
  evaluate_at_fractional_input_check (coefficients, input, input_shift)?;
  if coefficients.len () == 0 {return Ok(T::zero())}
  if coefficients.len () == 1 || input == T::zero() {return Ok(coefficients [0].into())}
  let integer_input = shr_nicely_rounded (input, input_shift);
  let small_input = input - (integer_input << input_shift);
  let bits = mem::size_of::<T>() as u32*8;
  let min_precision_shift = 3; //at the end, the error can be as much as 3, and it must be reduced to less than half
  let precision_shift_increment = max (1, (bits + 1).saturating_sub ((input.abs() - T::one()).leading_zeros() + input_shift));
  debug_assert! (precision_shift_increment < bits, "{:?}", (input, input_shift, precision_shift_increment));
  debug_assert! ((T::one() << precision_shift_increment) >= (integer_input.abs() << 1u32), "{:?}", (input, input_shift, precision_shift_increment));
  debug_assert! (precision_shift_increment >0);
  let mut precision_shift = min_precision_shift + precision_shift_increment*(coefficients.len() as u32 - 1);
  println!("ll {:?}", (precision_shift_increment, integer_input, small_input));
  let mut result = T::zero();
  for coefficient in coefficients.iter().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    //note: we don't need to do the multiplying and dividing by precision_shift on the FIRST iteration; TODO fix
    result += coefficient << precision_shift;
    let integer_part = result*integer_input;
    let fractional_part = shr_round_to_even (result*small_input, input_shift);
    result = integer_part + fractional_part;
    precision_shift -= precision_shift_increment;
    result = shr_round_to_even (result, precision_shift_increment);
  }
  debug_assert! (precision_shift == min_precision_shift);
  Ok(shr_nicely_rounded (result, precision_shift) + coefficients [0].into())
}


pub fn evaluate_at_fractional_input_check <Coefficient: Copy + Debug, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input: T, input_shift: u32)->Result <(), ()> {
  if coefficients.len() <= 1 || input == T::zero() {return Ok (());}
  let bits = mem::size_of::<T>() as u32*8;
  let min_precision_shift = 3; //at the end, the error can be as much as 3, and it must be reduced to less than half
  let precision_shift_increment = max (1, (bits + 1).saturating_sub ((input.abs() - T::one()).leading_zeros() + input_shift));
  let mut precision_shift = min_precision_shift + precision_shift_increment*(coefficients.len() as u32 -1) + max (precision_shift_increment, input_shift);
  for coefficient in coefficients.iter().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    if overflow_checked_shl (coefficient, precision_shift).is_none() {return Err (()) ;}
    precision_shift -= precision_shift_increment;
  }

  Ok (())
}

pub fn evaluate_at_fractional_input_range <Coefficient: Copy + Debug, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], input_shift: u32)->T {
  if coefficients.len() <= 1 {return T::max_value();}
  let bits = mem::size_of::<T>() as u32*8;
  let min_precision_shift = 3; //at the end, the error can be as much as 3, and it must be reduced to less than half
  let mut precision_shift_increment = bits - 1 - min_precision_shift;
  for (power, coefficient ) in coefficients.iter().enumerate().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    while overflow_checked_shl (coefficient, precision_shift_increment*(power as u32) + max (precision_shift_increment, input_shift) + min_precision_shift).is_none() {
      if precision_shift_increment == 1 {return T::zero() ;}
      precision_shift_increment -= 1;
    }
  }

  let input = T::one() << (precision_shift_increment + input_shift - 1);
  
  let verified_precision_shift_increment = max (1, (bits + 1).saturating_sub ((input.abs() - T::one()).leading_zeros() + input_shift));
  assert_eq! (precision_shift_increment, verified_precision_shift_increment);
  input
}

pub fn coefficient_bounds_for_evaluate_at_small_input <T: Integer + Signed> (shift: u32)->impl Fn (usize)->T {
  let non_constant_max = T::max_value() >> shift;
  move | power | if power == 0 {T::max_value() - non_constant_max} else {non_constant_max}
}

pub fn within_bounds_check <Coefficient: Copy, T: Integer + Signed + From <Coefficient>, MaximumFn: Fn (usize)->T> (coefficients: & [Coefficient], maximum: MaximumFn)->Result <(), Error <T>> {
  for (power, coefficient) in coefficients.iter().enumerate() {
    let coefficient: T = (*coefficient).into();
    let max = maximum (power);
    if coefficient == T::min_value() {return Err (Error::CoefficientOutOfBounds {power, coefficient, maximum_abs: max})}
    let magnitude = coefficient.abs();
    if magnitude > max {return Err (Error::CoefficientOutOfBounds {power, coefficient, maximum_abs: max})}
  }
  Ok (())
}

pub fn translate_check <Coefficient: Copy, T: Integer + Signed + From <Coefficient>, MaximumFn: Fn (usize)->T> (coefficients: & [Coefficient], input: T, maximum: MaximumFn)->Result <(), Error <T>> {
  if coefficients.len() == 0 {return Ok (());}
  if input == T::zero() {within_bounds_check (coefficients, maximum)?; return Ok (());}
  let mut factor = T::one();
  let input = input.abs();
  let sum_safety_shift = coefficients.len() as u32 - 1;
  let mut running_maximum = T::max_value();
  for (power, coefficient) in coefficients.iter().enumerate() {
    let coefficient: T = (*coefficient).into();
    if power > 0 { match factor.checked_mul (&input) {
      // note: there's a slight weirdness here, where if the factor overflows
      // but the current coefficient is 0, we still advance to the next iteration,
      // and the factor is the "wrong" value on that iteration.
      // However, it just overflows again, leading to the correct result.
      None => if coefficient != T::zero() {return Err (Error::TermOverflowed {power, input, coefficient})},
      Some (next_factor) => factor = next_factor,
    }}
    running_maximum = min (running_maximum, maximum (power).checked_shr (sum_safety_shift).unwrap_or (T::zero()));
    if coefficient == T::min_value() {return Err (Error::TermOverflowed {power, input, coefficient});}
    let magnitude = coefficient.abs();
    match magnitude.checked_mul (&factor) {
      None => return Err (Error::TermOverflowed {power, input, coefficient}),
      Some (term) => if term > running_maximum {return Err (Error::TermOutOfBounds {power, input, coefficient, value: term, maximum_abs: running_maximum})},
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



pub fn compute_derivative <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], results: &mut [T])->Result <(),()> {
  assert_eq!(results.len() + 1, coefficients.len());
  let mut factor = T::one();
  for ((power, coefficient), result) in coefficients.iter().enumerate().skip (1).zip(results.iter_mut()) {
    let coefficient: T = (*coefficient).into();
    factor *= T::from_usize (power).unwrap();
    match coefficient.checked_mul (&factor) {
      Some (value) => *result = value,
      None => return Err (()),
    }
  }
  Ok (())
}

pub enum RootSearchResult <T> {
  Root (T),
  Overflow (T),
  Finished,
}


pub (super) fn root_search <Coefficient: Copy, T: Integer + Signed + From <Coefficient>> (coefficients: & [Coefficient], range: [T; 2], input_shift: u32)->RootSearchResult <T> {
  let mut derivatives: SmallVec<[T; 15]> = SmallVec::with_capacity ((coefficients.len()*(coefficients.len() + 1)) >> 1);
  
  derivatives.extend (coefficients.iter().map (| coefficient | (*coefficient).into()));
  
  let mut derivative_start = 0;
  for derivative_size in (3..coefficients.len() + 1).rev() {
    let (first, second) = derivatives.split_at_mut (derivative_start + derivative_size);
    if compute_derivative (& first [derivative_start..], &mut second [..derivative_size - 1]).is_err() {
      return RootSearchResult::Overflow (range [0]);
    }
    derivative_start += derivative_size;
  }
  
  use self::impls::RootSearchMetadata;
  let mut metadata = RootSearchMetadata {
    input_shift, original_range: range, derivatives: Default::default()
  };
  
  let mut derivative_start = 0;
  for derivative_size in (3..coefficients.len() + 1).rev() {
    metadata.derivatives.push (& derivatives [derivative_start..derivative_start + derivative_size]);
    derivative_start += derivative_size;
  }
  
  self::impls::root_search (& metadata, [
    (range [0] >> input_shift) << input_shift,
    shr_ceil (range [1], input_shift) << input_shift,
  ], coefficients.len() - 1)
}


mod impls {
use super::*;

pub (super) struct RootSearchMetadata <'a, T: 'a> {
  pub (super) input_shift: u32,
  pub (super) original_range: [T; 2],
  pub (super) derivatives: SmallVec<[& 'a [T]; 8]>,
}

/// Search for root, assuming that which_derivative is almost-monotonic
pub (super) fn root_search <T: Integer + Signed> (metadata: & RootSearchMetadata <T>, range: [T; 2], which_derivative: usize)->RootSearchResult <T> {
  let shift = metadata.input_shift;
  let bound_values = range.map (| bound | evaluate_at_fractional_input (metadata.derivatives [which_derivative], bound, shift));
  if bound_values [0].is_err () {return RootSearchResult::Overflow (range [0])}
  
  if let [Ok (first), Ok (last)] = bound_values {
  if (first >= T::zero() && last >= T::zero()) ||
     (first <= T::zero() && last <= T::zero()) {
    // if this is an integer range, then we are exactly monotonic,
    // so we are exactly not-0-crossing and our anti-derivative is exactly monotonic.
    // If this is a fractional range, its length is <= half, and "almost monotonic" means that our maximum movement in the "wrong direction" is less than 1. Combined with the error of 1 in evaluating us, this guarantees that our furthest possible value in the "wrong direction" is less than 2, meaning that our anti-derivative has a maximum slope-in-the-wrong-direction less than 2 over a length <= half, so our anti-derivative is also "almost monotonic".
    // Either way, our anti-derivative is "almost monotonic".
    if which_derivative > 0 {
      return root_search (metadata, range, which_derivative - 1);
    } else {
      // if the polynomial value is "almost not 0 crossing" on in interval,
      // we generally don't have to return any results for that interval.
      // However, if we ignored a "positive then 0" interval followed by a "0 then negative" interval,
      // then we would miss a 0 crossing. So explicitly notice zeros at the bounds of intervals.
      if first == T::zero() && range [0] >= metadata.original_range [0] { return RootSearchResult::Root (range [0]); }
      if last == T::zero() && range [1] <= metadata.original_range [1] { return RootSearchResult::Root (range [1]); }
      return RootSearchResult::Finished;
    }
  }}
  
  // we might be 0-crossing on this interval, so we can't recurse into a lower derivative yet.
  // But maybe we are not-0-crossing on one half of this interval?
  let integer_range = range.map (| bound | bound >> shift);
  let split_point;
  // don't use fractional inputs until necessary
  let distance = range [1].saturating_sub (range [0]);
  if distance > T::one() << shift {
    split_point = mean_floor (range [0] >> shift, range [1] >> shift) << shift;
  } else if distance > T::one() {
    split_point = mean_floor (range [0], range [1]);
  } else {
    if bound_values [1].is_err() { return RootSearchResult::Overflow (range [1]) }
    if which_derivative > 0 {
      //although the ideal polynomial isn't necessarily monotonic on this interval,
      //the "polynomial evaluated only at scaled-integer inputs" is ALWAYS monotonic
      //on an interval with only 2 points in it.
      //So we can skip straight down to the original polynomial.
      return root_search (metadata, range, 0);
    } else {
      // the original polynomial is definitely 0-crossing on this interval: success!
      return RootSearchResult::Root (range [0]);
    }
  }
  
  if split_point > metadata.original_range [0] {
    let first_half_result = root_search (metadata, [range [0], split_point], which_derivative);
    match first_half_result { RootSearchResult::Finished =>(),_=> return first_half_result }
  }
  if split_point < metadata.original_range [1] {
    return root_search (metadata, [split_point, range [1]], which_derivative)
  }
  RootSearchResult::Finished
}

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
  
    
  #[test]
  fn test_evaluate_at_fractional_input() {
    let mut generator =::rand::chacha::ChaChaRng::from_seed ([33; 32]) ;
    for shift in 0..5 {
      let shift = 1 << shift;
      let maximum = i64::max_value() >> shift;
      let constant_maximum = i64::max_value() - maximum;
      let range = Range::new_inclusive (- maximum >> 5, maximum >> 5) ;
      let constant_range = Range::new_inclusive (- constant_maximum, constant_maximum);
      
      for _ in 0..160 {
        let mut coefficients = vec![generator.gen::<i64>() >> shift];
        while coefficients.len() < 8 && generator.gen::<f64>() < 0.7 {
          let coefficient_shift = shift + coefficients.len() as u32*4;
          coefficients.push (generator.gen::<i64>() >> coefficient_shift);
        }
        
  println!("jkh {:?}", (& coefficients, shift));
        let input_maximum: i64 = evaluate_at_fractional_input_range(& coefficients, shift);
  println!("jh {:?}", (& coefficients, input_maximum, shift));
        evaluate_at_fractional_input_check (& coefficients, input_maximum, shift).unwrap();
        if input_maximum >= 0 {
          let input_range = Range::new_inclusive (- input_maximum, input_maximum) ;
          for _ in 0..3 {
            let input = input_range.sample (&mut generator);
  println!("{:?}", (& coefficients, input_maximum, input, shift));
            //if evaluate_at_fractional_input_check (& coefficients, input, shift).is_err() {continue;}
            let result = evaluate_at_fractional_input (& coefficients, input, shift).unwrap();
            let perfect_result = evaluate_exactly (& coefficients, input, shift);
            let difference = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap()) - perfect_result;
            assert!(difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap()));
          }
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
