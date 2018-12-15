use num::Signed;
use smallvec::SmallVec;
use std::cmp::{max, min};

use super::*;

#[derive(Debug, Fail)]
pub enum Error<T: Integer> {
  #[fail(
    display = "out-of-bounds value in the x^{} term (coefficient is {}, but must have abs <= {})",
    power, coefficient, maximum_abs
  )]
  CoefficientOutOfBounds {
    power: usize,
    coefficient: T,
    maximum_abs: T,
  },
  #[fail(
    display = "out-of-bounds input to polynomial function (for the x^{} term, |{}*{}^{}| was {}, but must have abs <= {})",
    power, coefficient, input, power, value, maximum_abs
  )]
  TermOutOfBounds {
    power: usize,
    input: T,
    coefficient: T,
    value: T,
    maximum_abs: T,
  },
  #[fail(
    display = "out-of-bounds input to polynomial function (for the x^{} term, |{}*{}^{}| overflowed the type)",
    power, input, coefficient, power
  )]
  TermOverflowed {
    power: usize,
    input: T,
    coefficient: T,
  },
}

#[derive(Copy, Clone, Debug)]
pub struct OverflowError;

use std::option::NoneError;
impl From<NoneError> for OverflowError {
  fn from(_: NoneError) -> OverflowError {
    OverflowError
  }
}
impl<T: Integer> From<Error<T>> for OverflowError {
  fn from(_: Error<T>) -> OverflowError {
    OverflowError
  }
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
pub fn evaluate_at_small_input<Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  coefficients: &[Coefficient],
  input: T,
  shift: u32,
) -> T {
  let half = (T::one() << shift) >> 1u32;
  assert!(
    -half <= input && input <= half,
    "inputs to evaluate_at_small_input must be in the range [-0.5, 0.5]"
  );

  if coefficients.len() == 0 {
    return T::zero();
  }

  let mut result = T::zero();
  for coefficient in coefficients.iter().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    result = shr_nicely_rounded((result + coefficient) * input, shift);
  }
  let constant_coefficient: T = coefficients[0].into();
  result + constant_coefficient
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
pub fn translate<T: Integer + Signed>(coefficients: &mut [T], input: T) -> Result<(), Error<T>> {
  translate_check(coefficients, input, |_| T::max_value())?;
  translate_unchecked(coefficients, input);
  Ok(())
}

/// Same as translate(), but assume no overflow will occur.
///
/// Useful in performance-critical code when you already know there won't be overflow,
/// such as in the range returned by conservative_safe_translation_range.
pub fn translate_unchecked<T: Integer + Signed>(coefficients: &mut [T], input: T) {
  for first_source in (1..coefficients.len()).rev() {
    for source in first_source..coefficients.len() {
      coefficients[source - 1] += coefficients[source] * input;
    }
  }
}

mod evaluate_at_fractional_input_impls {
  use super::*;

  // at the end of computing for the nonconstant coefficients, the error can be as much as 3, and it must be reduced to less than half, so right-shift it by 3.
  pub(super) const MIN_PRECISION_SHIFT: u32 = 3;

  pub(super) fn precision_shift_increment<T: Integer + Signed>(
    input_numerator: T,
    input_shift: u32,
  ) -> u32 {
    let bits = mem::size_of::<T>() as u32 * 8;
    let leading_zeros = if input_numerator <= T::min_value() + T::one() {
      bits - 1
    } else {
      (input_numerator.abs() - T::one()).leading_zeros()
    };
    max(1, (bits + 1).saturating_sub(leading_zeros + input_shift))
  }
}

/// Approximately evaluate an integer polynomial at a non-integer input.
///
/// The result is guaranteed to be strictly within 1 of the ideal result.
/// For instance, if the ideal result was 2.125, this function could return 2 or 3,
/// but if it was 2, this function can only return exactly 2.
///
/// If the input is an integer, the result is guaranteed to be exactly the ideal result.
pub fn evaluate_at_fractional_input<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &[Coefficient],
  input_numerator: T,
  input_shift: u32,
) -> Result<T, OverflowError> {
  evaluate_at_fractional_input_check(coefficients, input_numerator, input_shift)?;
  if coefficients.len() <= 1 || input_numerator == T::zero() {
    if coefficients.len() == 0 {
      return Ok(T::zero());
    }
    return Ok(coefficients[0].into());
  }
  let integer_input = shr_nicely_rounded(input_numerator, input_shift);
  let small_input = input_numerator.wrapping_sub(&(integer_input << input_shift));
  let precision_shift_increment =
    evaluate_at_fractional_input_impls::precision_shift_increment(input_numerator, input_shift);
  /*debug_assert! (precision_shift_increment < bits, "{:?}", (input, input_shift, precision_shift_increment));
  debug_assert! ((T::one() << precision_shift_increment) >= (integer_input.abs() << 1u32), "{:?}", (input, input_shift, precision_shift_increment));
  debug_assert! (precision_shift_increment >0);*/
  let mut precision_shift = evaluate_at_fractional_input_impls::MIN_PRECISION_SHIFT
    + precision_shift_increment * (coefficients.len() as u32 - 1);
  //println!("ll {:?}", (precision_shift_increment, integer_input, small_input));
  let bits = mem::size_of::<T>() as u32 * 8;
  if precision_shift >= bits {
    //special case that can only occur under certain circumstances
    assert!(coefficients[1..]
      .iter()
      .all(|coefficient| coefficient == &Coefficient::zero()));
    return Ok(coefficients[0].into());
  }
  let mut result = T::zero();
  for coefficient in coefficients.iter().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    //note: we don't need to do the multiplying and dividing by precision_shift on the FIRST iteration; TODO fix
    result += coefficient << precision_shift;
    let integer_part = result * integer_input;
    let fractional_part = shr_round_to_even(result * small_input, input_shift);
    result = integer_part + fractional_part;
    precision_shift -= precision_shift_increment;
    result = shr_round_to_even(result, precision_shift_increment);
  }
  debug_assert!(precision_shift == evaluate_at_fractional_input_impls::MIN_PRECISION_SHIFT);
  let constant_coefficient: T = coefficients[0].into();
  Ok(
    shr_nicely_rounded(
      result,
      evaluate_at_fractional_input_impls::MIN_PRECISION_SHIFT,
    ) + constant_coefficient,
  )
}

pub fn evaluate_at_fractional_input_check<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &[Coefficient],
  input_numerator: T,
  input_shift: u32,
) -> Result<(), OverflowError> {
  if coefficients.len() <= 1 || input_numerator == T::zero() {
    return Ok(());
  }
  let constant_coefficient: T = coefficients[0].into();
  if constant_coefficient > (T::max_value() >> 1u32)
    || constant_coefficient < -(T::max_value() >> 1u32)
  {
    return Err(OverflowError);
  }
  let precision_shift_increment =
    evaluate_at_fractional_input_impls::precision_shift_increment(input_numerator, input_shift);
  let mut precision_shift = evaluate_at_fractional_input_impls::MIN_PRECISION_SHIFT
    + precision_shift_increment * (coefficients.len() as u32 - 1)
    + max(precision_shift_increment, input_shift);
  for coefficient in coefficients.iter().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    overflow_checked_shl(coefficient, precision_shift)?;
    precision_shift -= precision_shift_increment;
  }

  Ok(())
}

pub fn evaluate_at_fractional_input_range<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &[Coefficient],
  input_shift: u32,
) -> T {
  if coefficients.len() <= 1 {
    return T::max_value();
  }
  let constant_coefficient: T = coefficients[0].into();
  if constant_coefficient > (T::max_value() >> 1u32)
    || constant_coefficient < -(T::max_value() >> 1u32)
  {
    return T::zero();
  }
  let bits = mem::size_of::<T>() as u32 * 8;
  let mut precision_shift_increment = bits - input_shift;
  for (power, coefficient) in coefficients.iter().enumerate().skip(1).rev() {
    let coefficient: T = (*coefficient).into();
    while overflow_checked_shl(
      coefficient,
      precision_shift_increment * (power as u32)
        + max(precision_shift_increment, input_shift)
        + evaluate_at_fractional_input_impls::MIN_PRECISION_SHIFT,
    )
    .is_none()
    {
      if precision_shift_increment == 1 {
        return T::zero();
      }
      precision_shift_increment -= 1;
    }
  }

  let input_numerator =
    match overflow_checked_shl(T::one(), precision_shift_increment + input_shift - 1) {
      Some(value) => value,
      None => return T::max_value(),
    };

  let verified_precision_shift_increment =
    evaluate_at_fractional_input_impls::precision_shift_increment(input_numerator, input_shift);
  assert_eq!(
    precision_shift_increment,
    verified_precision_shift_increment
  );
  input_numerator
}

pub fn coefficient_bounds_for_evaluate_at_small_input<T: Integer + Signed>(
  shift: u32,
) -> impl Fn(usize) -> T {
  let non_constant_max = T::max_value() >> shift;
  move |power| {
    if power == 0 {
      T::max_value() - non_constant_max
    } else {
      non_constant_max
    }
  }
}

pub fn within_bounds_check<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
  MaximumFn: Fn(usize) -> T,
>(
  coefficients: &[Coefficient],
  maximum: MaximumFn,
) -> Result<(), Error<T>> {
  for (power, coefficient) in coefficients.iter().enumerate() {
    let coefficient: T = (*coefficient).into();
    let max = maximum(power);
    if coefficient == T::min_value() {
      return Err(Error::CoefficientOutOfBounds {
        power,
        coefficient,
        maximum_abs: max,
      });
    }
    let magnitude = coefficient.abs();
    if magnitude > max {
      return Err(Error::CoefficientOutOfBounds {
        power,
        coefficient,
        maximum_abs: max,
      });
    }
  }
  Ok(())
}

pub fn translate_check<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
  MaximumFn: Fn(usize) -> T,
>(
  coefficients: &[Coefficient],
  input: T,
  maximum: MaximumFn,
) -> Result<(), Error<T>> {
  if coefficients.len() == 0 {
    return Ok(());
  }
  if input == T::zero() {
    within_bounds_check(coefficients, maximum)?;
    return Ok(());
  }
  let mut factor = T::one();
  let input = input.abs();
  let sum_safety_shift = coefficients.len() as u32 - 1;
  let mut running_maximum = T::max_value();
  for (power, coefficient) in coefficients.iter().enumerate() {
    let coefficient: T = (*coefficient).into();
    if power > 0 {
      match factor.checked_mul(&input) {
        // note: there's a slight weirdness here, where if the factor overflows
        // but the current coefficient is 0, we still advance to the next iteration,
        // and the factor is the "wrong" value on that iteration.
        // However, it just overflows again, leading to the correct result.
        None => {
          if coefficient != T::zero() {
            return Err(Error::TermOverflowed {
              power,
              input,
              coefficient,
            });
          }
        }
        Some(next_factor) => factor = next_factor,
      }
    }
    running_maximum = min(
      running_maximum,
      maximum(power)
        .checked_shr(sum_safety_shift)
        .unwrap_or(T::zero()),
    );
    if coefficient == T::min_value() {
      return Err(Error::TermOverflowed {
        power,
        input,
        coefficient,
      });
    }
    let magnitude = coefficient.abs();
    match magnitude.checked_mul(&factor) {
      None => {
        return Err(Error::TermOverflowed {
          power,
          input,
          coefficient,
        })
      }
      Some(term) => {
        if term > running_maximum {
          return Err(Error::TermOutOfBounds {
            power,
            input,
            coefficient,
            value: term,
            maximum_abs: running_maximum,
          });
        }
      }
    }
  }
  Ok(())
}

/// Find the range of inputs to which the polynomial can safely be translated.
///
/// Magnitudes <= result are safe to translate to; magnitudes > result aren't.
/// If even the initial value is out of bounds, this function returns -1.
///
/// The third argument can impose a stricter maximum on the resulting coefficients.
pub fn exact_safe_translation_range<T: Integer + Signed, MaximumFn: Fn(usize) -> T>(
  coefficients: &[T],
  maximum: MaximumFn,
) -> T {
  if within_bounds_check(coefficients, &maximum).is_err() {
    return -T::one();
  }
  // pick a min that's definitely legal and a max where max + 1 is definitely illegal
  let mut min = T::zero();
  let mut max = T::max_value();
  while min < max {
    let mid = mean_ceil(min, max);
    if translate_check(coefficients, mid, &maximum).is_ok() {
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
pub fn conservative_safe_translation_range<T: Integer + Signed, MaximumFn: Fn(usize) -> T>(
  coefficients: &[T],
  maximum: MaximumFn,
) -> T {
  if coefficients.len() == 0 {
    return T::max_value();
  }
  if within_bounds_check(coefficients, &maximum).is_err() {
    return -T::one();
  }
  let sum_safety_shift = coefficients.len() as u32 - 1;
  let mut running_maximum = T::max_value();
  let mut result_shift = mem::size_of::<T>() * 8 - 2;
  for (power, coefficient) in coefficients.iter().enumerate() {
    let magnitude = coefficient.abs();
    running_maximum = min(
      running_maximum,
      maximum(power)
        .checked_shr(sum_safety_shift)
        .unwrap_or(T::zero()),
    );
    while magnitude
      > running_maximum
        .checked_shr((result_shift * power) as u32)
        .unwrap_or(T::zero())
    {
      if result_shift == 0 {
        return T::zero();
      }
      result_shift -= 1;
    }
  }
  T::one() << result_shift
}

pub fn compute_derivative<Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  coefficients: &[Coefficient],
  results: &mut [T],
) -> Result<(), OverflowError> {
  assert_eq!(results.len() + 1, coefficients.len());
  for ((power, coefficient), result) in coefficients
    .iter()
    .enumerate()
    .skip(1)
    .zip(results.iter_mut())
  {
    let coefficient: T = (*coefficient).into();
    *result = coefficient.checked_mul(&T::from_usize(power).unwrap())?;
  }
  Ok(())
}

pub fn compute_nth_derivative<Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  coefficients: &[Coefficient],
  results: &mut [T],
  which_derivative: usize,
) -> Result<(), OverflowError> {
  assert_eq!(results.len() + which_derivative, coefficients.len());
  for ((power, coefficient), result) in coefficients
    .iter()
    .enumerate()
    .skip(which_derivative)
    .zip(results.iter_mut())
  {
    let coefficient: T = (*coefficient).into();
    let factor = (power + 1 - which_derivative..power + 1).product::<usize>();
    *result = coefficient.checked_mul(&T::from_usize(factor).unwrap())?;
  }
  Ok(())
}

pub fn compute_nth_taylor_coefficient_function<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &[Coefficient],
  results: &mut [T],
  which_derivative: usize,
) -> Result<(), OverflowError> {
  assert_eq!(results.len() + which_derivative, coefficients.len());
  let factorial = (1..which_derivative + 1).product::<usize>();
  for ((power, coefficient), result) in coefficients
    .iter()
    .enumerate()
    .skip(which_derivative)
    .zip(results.iter_mut())
  {
    let coefficient: T = (*coefficient).into();
    let factor = (power + 1 - which_derivative..power + 1).product::<usize>() / factorial;
    *result = coefficient.checked_mul(&T::from_usize(factor).unwrap())?;
  }
  Ok(())
}

pub fn derivative_unchecked<'a, Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  coefficients: &'a [Coefficient],
) -> impl Iterator<Item = T> + 'a {
  coefficients
    .iter()
    .enumerate()
    .skip(1)
    .map(|(power, coefficient)| {
      let coefficient: T = (*coefficient).into();
      coefficient * T::from_usize(power).unwrap()
    })
}

pub fn add_product_into<Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  first: &[Coefficient],
  second: &[Coefficient],
  destination: &mut [T],
) -> Result<(), OverflowError> {
  assert!(destination.len() + 1 >= first.len() + second.len());
  for (first_power, first_coefficient) in first.iter().enumerate() {
    let first_coefficient: T = (*first_coefficient).into();
    for (second_power, second_coefficient) in second.iter().enumerate() {
      let second_coefficient: T = (*second_coefficient).into();
      let mut destination = &mut destination[first_power + second_power];
      *destination =
        destination.checked_add(&first_coefficient.checked_mul(&second_coefficient)?)?;
    }
  }
  Ok(())
}

pub fn evaluate_nth_taylor_coefficient_at_fractional_input<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &[Coefficient],
  which_derivative: usize,
  input_numerator: T,
  input_shift: u32,
) -> Result<T, OverflowError> {
  let mut taylor_coefficient_function: SmallVec<[T; 8]> =
    smallvec![T::zero(); coefficients.len() - which_derivative];
  compute_nth_taylor_coefficient_function(
    coefficients,
    &mut taylor_coefficient_function,
    which_derivative,
  )?;
  let current_value =
    evaluate_at_fractional_input(&taylor_coefficient_function, input_numerator, input_shift)?;
  Ok(current_value)
}

pub fn set_nth_taylor_coefficient_at_fractional_input<
  Coefficient: Integer,
  T: Integer + Signed + From<Coefficient>,
>(
  coefficients: &mut [Coefficient],
  which_derivative: usize,
  input_numerator: T,
  input_shift: u32,
  target_value: T,
) -> Result<(), OverflowError> {
  let mut target_values: SmallVec<[T; 8]> = SmallVec::with_capacity(which_derivative + 1);
  for index in 0..which_derivative {
    target_values.push(evaluate_nth_taylor_coefficient_at_fractional_input(
      coefficients,
      index,
      input_numerator,
      input_shift,
    )?);
  }
  target_values.push(target_value);
  for (index, target_value) in target_values.iter().enumerate().rev() {
    let current_value = evaluate_nth_taylor_coefficient_at_fractional_input(
      coefficients,
      index,
      input_numerator,
      input_shift,
    )?;
    let change_size = target_value.checked_sub(&current_value)?;
    coefficients[index] = coefficients[index].checked_add(&Coefficient::from(change_size)?)?;
  }
  Ok(())
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum RootSearchResult<T> {
  Root(T),
  Overflow(T),
  Finished,
}

use std::cmp::Ordering;
impl<T: Ord> Ord for RootSearchResult<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&RootSearchResult::Finished, &RootSearchResult::Finished) => Ordering::Equal,
      (&RootSearchResult::Finished, _) => Ordering::Greater,
      (_, &RootSearchResult::Finished) => Ordering::Less,
      _ => {
        let first = match self {
          &RootSearchResult::Root(ref value) => (value, false),
          &RootSearchResult::Overflow(ref value) => (value, true),
          _ => unreachable!(),
        };
        let second = match other {
          &RootSearchResult::Root(ref value) => (value, false),
          &RootSearchResult::Overflow(ref value) => (value, true),
          _ => unreachable!(),
        };
        first.cmp(&second)
      }
    }
  }
}

impl<T: Ord> PartialOrd for RootSearchResult<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

pub fn root_search<Coefficient: Integer, T: Integer + Signed + From<Coefficient>>(
  coefficients: &[Coefficient],
  range: [T; 2],
  input_shift: u32,
) -> RootSearchResult<T> {
  assert!(range[1] > range[0]);
  if coefficients.len() <= 1 {
    return RootSearchResult::Finished;
  }
  let all_derivatives_terms = (coefficients.len() * (coefficients.len() + 1)) >> 1;
  let mut derivatives: SmallVec<[T; 15]> = SmallVec::with_capacity(all_derivatives_terms);

  derivatives.extend(coefficients.iter().map(|coefficient| (*coefficient).into()));
  derivatives.resize(all_derivatives_terms, T::zero());

  let mut derivative_start = 0;
  for derivative_size in (3..coefficients.len() + 1).rev() {
    let (first, second) = derivatives.split_at_mut(derivative_start + derivative_size);
    if compute_derivative(
      &first[derivative_start..],
      &mut second[..derivative_size - 1],
    )
    .is_err()
    {
      return RootSearchResult::Overflow(range[0]);
    }
    derivative_start += derivative_size;
  }

  use self::impls::RootSearchMetadata;
  let mut metadata = RootSearchMetadata {
    input_shift,
    half: T::one() << input_shift.saturating_sub(1),
    one: T::one() << input_shift,
    original_range: range,
    derivatives: Default::default(),
  };

  let mut derivative_start = 0;
  for derivative_size in (2..coefficients.len() + 1).rev() {
    metadata
      .derivatives
      .push(&derivatives[derivative_start..derivative_start + derivative_size]);
    derivative_start += derivative_size;
  }
  //println!( "{:?}", metadata.derivatives) ;

  self::impls::root_search_unknown_bound_values(
    &metadata,
    [
      (range[0] >> input_shift) << input_shift,
      overflow_checked_shl(shr_ceil(range[1], input_shift), input_shift).unwrap_or(T::max_value()),
    ],
    coefficients.len() - 2,
  )
}

mod impls {
  use super::*;
  use array_ext::*;

  pub(super) struct RootSearchMetadata<'a, T: 'a> {
    pub(super) input_shift: u32,
    pub(super) half: T,
    pub(super) one: T,
    pub(super) original_range: [T; 2],
    pub(super) derivatives: SmallVec<[&'a [T]; 8]>,
  }

  pub(super) fn root_search_unknown_bound_values<T: Integer + Signed>(
    metadata: &RootSearchMetadata<T>,
    range: [T; 2],
    which_derivative: usize,
  ) -> RootSearchResult<T> {
    root_search(
      metadata,
      range,
      range.map(|bound| {
        evaluate_at_fractional_input(
          metadata.derivatives[which_derivative],
          bound,
          metadata.input_shift,
        )
      }),
      which_derivative,
    )
  }

  pub(super) fn root_search_split<T: Integer + Signed>(
    metadata: &RootSearchMetadata<T>,
    range: [T; 2],
    bound_values: [Result<T, OverflowError>; 2],
    which_derivative: usize,
    split_point: T,
  ) -> RootSearchResult<T> {
    assert!(split_point > range[0]);
    assert!(split_point < range[1]);
    let split_point_value = evaluate_at_fractional_input(
      metadata.derivatives[which_derivative],
      split_point,
      metadata.input_shift,
    );
    if split_point > metadata.original_range[0] {
      let first_half_result = root_search(
        metadata,
        [range[0], split_point],
        [bound_values[0], split_point_value],
        which_derivative,
      );
      match first_half_result {
        RootSearchResult::Finished => (),
        _ => return first_half_result,
      }
    }
    if split_point < metadata.original_range[1] {
      return root_search(
        metadata,
        [split_point, range[1]],
        [split_point_value, bound_values[1]],
        which_derivative,
      );
    }
    RootSearchResult::Finished
  }

  /// Search for root, assuming the following:
  /// range is a valid range (min < max)
  /// which_derivative is almost-monotonic on this range
  /// bound_values are the correct outputs for which_derivative at the range endpoints
  pub(super) fn root_search<T: Integer + Signed>(
    metadata: &RootSearchMetadata<T>,
    range: [T; 2],
    bound_values: [Result<T, OverflowError>; 2],
    which_derivative: usize,
  ) -> RootSearchResult<T> {
    debug_assert!(range[1] > range[0]);
    //println!( "something {:?}", (range, bound_values, which_derivative));
    let shift = metadata.input_shift;

    let distance = range[1].saturating_sub(range[0]);
    if distance > metadata.half {
      if distance < metadata.one {
        return root_search_split(
          metadata,
          range,
          bound_values,
          which_derivative,
          mean_floor(range[0], range[1]),
        );
      }
      let min_floor = range[0] >> shift << shift;
      if min_floor != range[0] {
        debug_assert!(min_floor + metadata.one < range[1]);
        return root_search_split(
          metadata,
          range,
          bound_values,
          which_derivative,
          min_floor + metadata.one,
        );
      }
      let max_floor = range[1] >> shift << shift;
      if max_floor != range[1] {
        debug_assert!(max_floor > range[0]);
        return root_search_split(metadata, range, bound_values, which_derivative, max_floor);
      }
    }

    if range[0] >= metadata.original_range[0] && bound_values[0].is_err() {
      return RootSearchResult::Overflow(range[0]);
    }

    if let [Ok(first), Ok(last)] = bound_values {
      if (first >= T::zero() && last >= T::zero()) || (first <= T::zero() && last <= T::zero()) {
        // if this is an integer range, then we are exactly monotonic,
        // so we are exactly not-0-crossing and our anti-derivative is exactly monotonic.
        // If this is a fractional range, its length is <= half, and "almost monotonic" means that our maximum movement in the "wrong direction" is less than 1. Combined with the error of 1 in evaluating us, this guarantees that our furthest possible value in the "wrong direction" is less than 2, meaning that our anti-derivative has a maximum slope-in-the-wrong-direction less than 2 over a length <= half, so our anti-derivative is also "almost monotonic".
        // Either way, our anti-derivative is "almost monotonic".
        if which_derivative > 0 {
          return root_search_unknown_bound_values(metadata, range, which_derivative - 1);
        } else {
          // if the polynomial value is "almost not 0 crossing" on in interval,
          // we generally don't have to return any results for that interval.
          // However, if we ignored a "positive then 0" interval followed by a "0 then negative" interval,
          // then we would miss a 0 crossing. So explicitly notice zeros at the bounds of intervals.
          if first == T::zero() && range[0] >= metadata.original_range[0] {
            return RootSearchResult::Root(range[0]);
          }
          if last == T::zero() && range[1] <= metadata.original_range[1] {
            return RootSearchResult::Root(range[1] - T::one());
          }
          return RootSearchResult::Finished;
        }
      }
    }

    // we might be 0-crossing on this interval, so we can't recurse into a lower derivative yet.
    // But maybe we are not-0-crossing on one half of this interval?
    let split_point;
    // don't use fractional inputs until necessary

    if distance > metadata.one {
      split_point = mean_floor(range[0] >> shift, range[1] >> shift) << shift;
    } else if distance > T::one() {
      split_point = mean_floor(range[0], range[1]);
    } else {
      assert!(range[0] >= metadata.original_range[0]);
      assert!(range[1] <= metadata.original_range[1]);
      if bound_values[1].is_err() {
        return RootSearchResult::Overflow(range[1]);
      }
      if which_derivative > 0 {
        //although the ideal polynomial isn't necessarily monotonic on this interval,
        //the "polynomial evaluated only at scaled-integer inputs" is ALWAYS monotonic
        //on an interval with only 2 points in it.
        //So we can skip straight down to the original polynomial.
        return root_search_unknown_bound_values(metadata, range, 0);
      } else {
        // the original polynomial is definitely 0-crossing on this interval: success!
        return RootSearchResult::Root(range[0]);
      }
    }
    root_search_split(metadata, range, bound_values, which_derivative, split_point)
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use num::bigint::BigInt;
  use num::rational::{BigRational, Ratio};
  use num::{FromPrimitive, Integer, One, ToPrimitive, Zero};
  use proptest::prelude::*;
  use rand::distributions::range::Range;
  use rand::distributions::Distribution;
  use rand::{Rng, SeedableRng};

  fn polynomial(input_shift: u32) -> BoxedStrategy<Vec<i64>> {
    let bits_left = 63 - input_shift as usize;
    prop::collection::vec(any::<i64>(), 0..min(16, bits_left - 1))
      .prop_flat_map(move |source_values| {
        let len = source_values.len() as u32;
        let divisor = max(2, len) - 1;
        (Just(source_values), 0..(63 - input_shift) / divisor)
      })
      .prop_flat_map(move |(source_values, time_scale_shift)| {
        (Just(
          source_values
            .iter()
            .enumerate()
            .map(move |(power, coefficient)| {
              coefficient >> (input_shift + power as u32 * time_scale_shift)
            })
            .collect(),
        ))
      })
      .boxed()
  }

  fn polynomial_and_shift() -> BoxedStrategy<(Vec<i64>, u32)> {
    (0..62u32)
      .prop_flat_map(|input_shift| (polynomial(input_shift), Just(input_shift)))
      .boxed()
  }

  fn polynomial_and_valid_fractional_input_and_shift() -> BoxedStrategy<(Vec<i64>, i64, u32)> {
    polynomial_and_shift()
      .prop_flat_map(|(coefficients, shift)| {
        let input_maximum: i64 = evaluate_at_fractional_input_range(&coefficients, shift);
        (
          Just(coefficients),
          -input_maximum..input_maximum.saturating_add(1),
          Just(shift),
        )
      })
      .boxed()
  }

  fn evaluate_exactly<Coefficient: Copy + Integer, T: Integer + Signed + From<Coefficient>>(
    coefficients: &[Coefficient],
    input: T,
    shift: u32,
  ) -> BigRational
  where
    BigInt: From<Coefficient> + From<T>,
  {
    let mut result: BigRational = Ratio::zero();
    let input = Ratio::new(BigInt::from(input), BigInt::one() << shift as usize);
    for coefficient in coefficients.iter().rev() {
      result = result * &input + BigInt::from(*coefficient);
    }
    result
  }

  proptest! {
    #[test]
    fn randomly_test_evaluate_at_fractional_input ((ref coefficients, input_numerator, input_shift) in polynomial_and_valid_fractional_input_and_shift()) {
      let result = evaluate_at_fractional_input (& coefficients, input_numerator, input_shift).unwrap();
      let result = Ratio::from_integer (FromPrimitive::from_i64 (result).unwrap());
      let perfect_result = evaluate_exactly (& coefficients, input_numerator, input_shift);
      let difference = &result - &perfect_result;
      prop_assert!(difference < Ratio::from_integer (FromPrimitive::from_i64 (1).unwrap()));
      if (input_numerator >> input_shift << input_shift) == input_numerator {
        prop_assert_eq!(result, perfect_result);
      }
    }

    #[test]
    fn randomly_test_evaluation_range_is_strict ((ref coefficients, input_shift) in polynomial_and_shift(), input_numerator in any::<i64>()) {
      let input_maximum: i64 = evaluate_at_fractional_input_range(& coefficients, input_shift);
      let valid = evaluate_at_fractional_input_check (& coefficients, input_numerator, input_shift).is_ok();
      println!( "{:?}", (input_maximum, input_numerator));
      prop_assert_eq! (valid, input_numerator >= - input_maximum && input_numerator <= input_maximum) ;
    }

    #[test]
    fn randomly_test_root_search ((ref coefficients, input_shift, (first, last)) in polynomial_and_shift().prop_flat_map (| (coefficients, input_shift) | {
      let input_maximum: i64 = evaluate_at_fractional_input_range(& coefficients, input_shift);
      (Just (coefficients), Just (input_shift),
        (- input_maximum.saturating_add (1)..input_maximum).prop_flat_map (move | first | (Just (first), first+1..input_maximum.saturating_add (10)))
      )
    }).no_shrink()) {
      let check_before;
      let result = root_search (coefficients, [first, last], input_shift);
      match result {
        RootSearchResult::Root (root) => {
          let root_value = evaluate_at_fractional_input (& coefficients, root, input_shift).unwrap();
          let next_value = evaluate_at_fractional_input (& coefficients, root + 1, input_shift).unwrap();
          prop_assert_ne! (root_value.signum()*next_value.signum(), 1);
          check_before = root+1;
        },
        RootSearchResult::Overflow (overflow) => check_before = overflow,
        RootSearchResult::Finished => check_before = last+1,
      }
      if check_before >first {
        let sample_points = min (check_before.saturating_sub (first), 100);
        let values: Vec<(i64, i64)> = (0..sample_points)
          .map (| index | if index == 0 {first} else {
            let bfirst = BigInt::from(first);
            let offset: BigInt = (BigInt::from(check_before) - first - 1)*index/(sample_points - 1);
            (bfirst + offset).to_i64().unwrap()
          })
          .map (| input | (input, evaluate_at_fractional_input (& coefficients, input, input_shift).unwrap()))
          .collect() ;
        assert!(values.iter().all (| value | value.1 >= -2) || values.iter().all (| value | value.1 <= 2), "There was a significant 0 crossing before the returned result: {:?}", (result, values));
      }
    }

    #[test]
    fn randomly_test_nth_derivative ((ref coefficients, which_derivative) in polynomial(0).prop_flat_map (| coefficients | {
      let len = coefficients.len();
      (Just (coefficients), 0..len + 1)
    })) {
      let mut direct = vec![0; coefficients.len() - which_derivative];
      let result = compute_nth_derivative (coefficients, &mut direct, which_derivative);
      let mut indirect = coefficients.clone();
      for _ in 0..which_derivative {
        let mut next = vec![0; indirect.len() - 1];
        if compute_derivative (& indirect, &mut next).is_err() {
          return Ok (());
        }
        indirect = next;
      }
      prop_assert! (result.is_ok());
      prop_assert_eq! (&direct, &indirect);

      let factorial = (1..which_derivative + 1).product::<usize>() as i64;
      let mut taylor = vec![0; coefficients.len() - which_derivative];
      let _result = compute_nth_taylor_coefficient_function (coefficients, &mut taylor, which_derivative);
      for coefficient in taylor.iter_mut() {*coefficient *= factorial}
      prop_assert_eq! (&direct, &taylor);
    }

    #[test]
    fn randomly_test_set_nth_taylor_coefficient_at_fractional_input ((ref coefficients, input_numerator, input_shift, which_derivative, target_value) in polynomial_and_valid_fractional_input_and_shift().prop_flat_map (| (mut coefficients, input_numerator, input_shift) | {
      if coefficients.len() == 0 {coefficients.push (0);}
      let len = coefficients.len();
      let range = i64::max_value() >> input_shift >> 4;
      (Just (coefficients), Just (input_numerator), Just (input_shift), 0..len, - range..range+1)
    })) {
      let mut target_values: SmallVec<[i64; 8]> = SmallVec::with_capacity (coefficients.len());
      for index in 0..coefficients.len() {
        target_values.push (match evaluate_nth_taylor_coefficient_at_fractional_input (coefficients, index, input_numerator, input_shift) {Ok(a)=>a, Err(_)=>return Ok (())});
      }
      target_values [which_derivative] = target_value;
      let mut coefficients = coefficients.clone();
      if set_nth_taylor_coefficient_at_fractional_input (&mut coefficients, which_derivative, input_numerator, input_shift, target_value).is_err() {return Ok (());}
      for (index, value) in target_values.into_iter().enumerate() {
        let observed = match evaluate_nth_taylor_coefficient_at_fractional_input (& coefficients, index, input_numerator, input_shift) {Ok(a)=>a, Err(_)=>return Ok (())};
        prop_assert_eq! (observed, value);
      }
    }
  }

  #[test]
  fn test_evaluate_at_small_input() {
    let mut generator = ::rand::chacha::ChaChaRng::from_seed([33; 32]);
    for shift in 0..5 {
      let shift = 1 << shift;
      let half = 1i64 << (shift - 1);
      let maximum = i64::max_value() >> shift;
      let constant_maximum = i64::max_value() - maximum;
      let range = Range::new_inclusive(-maximum, maximum);
      let constant_range = Range::new_inclusive(-constant_maximum, constant_maximum);
      let mut test_inputs = vec![-half, -half + 1, -1, 0, 1, half - 1, half];
      if half > 2 {
        for _ in 0..6 {
          test_inputs.push(generator.gen_range(-half + 2, half - 1));
        }
      }
      for _ in 0..40 {
        let mut coefficients = vec![constant_range.sample(&mut generator)];
        while generator.gen() {
          coefficients.push(range.sample(&mut generator));
        }
        for input in test_inputs.iter() {
          let result = evaluate_at_small_input(&coefficients, *input, shift);

          let perfect_result = evaluate_exactly(&coefficients, *input, shift);
          let difference =
            Ratio::from_integer(FromPrimitive::from_i64(result).unwrap()) - perfect_result;
          assert!(difference < Ratio::from_integer(FromPrimitive::from_i64(1).unwrap()));
        }
      }
    }
  }

  #[test]
  fn test_evaluate_at_fractional_input() {
    let mut generator = ::rand::chacha::ChaChaRng::from_seed([33; 32]);
    for shift in 0..5 {
      let shift = 1 << shift;

      for _ in 0..160 {
        let mut coefficients = vec![generator.gen::<i64>() >> shift];
        while coefficients.len() < 8 && generator.gen::<f64>() < 0.7 {
          let coefficient_shift = shift + coefficients.len() as u32 * 4;
          coefficients.push(generator.gen::<i64>() >> coefficient_shift);
        }

        println!("jkh {:?}", (&coefficients, shift));
        let input_maximum: i64 = evaluate_at_fractional_input_range(&coefficients, shift);
        println!("jh {:?}", (&coefficients, input_maximum, shift));
        evaluate_at_fractional_input_check(&coefficients, input_maximum, shift).unwrap();
        if input_maximum < i64::max_value() {
          assert!(
            evaluate_at_fractional_input_check(&coefficients, input_maximum + 1, shift).is_err()
          );
        }
        if input_maximum >= 0 {
          let input_range = Range::new_inclusive(-input_maximum, input_maximum);
          for _ in 0..3 {
            let input = input_range.sample(&mut generator);
            println!("{:?}", (&coefficients, input_maximum, input, shift));
            //if evaluate_at_fractional_input_check (& coefficients, input, shift).is_err() {continue;}
            let result = evaluate_at_fractional_input(&coefficients, input, shift).unwrap();
            let result = Ratio::from_integer(FromPrimitive::from_i64(result).unwrap());
            let perfect_result = evaluate_exactly(&coefficients, input, shift);
            let difference = &result - &perfect_result;
            assert!(difference < Ratio::from_integer(FromPrimitive::from_i64(1).unwrap()));
            if (input >> shift << shift) == input {
              assert_eq!(result, perfect_result);
            }
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
