/*!

Math with polynomials, as represented by arrays.

The array `[a, b, c, ...]` represents the polynomial `a + bx + cx^2 + ...`. Thus, the index of the array is the exponent, which puts it in the opposite of the order polynomials are normally written in (but the same order as Taylor polynomials are normally written in).

*/

use super::*;
use crate::range_search::{RangeSearch, RangeSearchRunner, STANDARD_PRECISION_SHIFT};
use derivative::Derivative;
use derive_more::{Deref, DerefMut};
use num::{CheckedAdd, CheckedMul, Signed};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::cmp::{max, min, Ordering};
use std::marker::PhantomData;
use thiserror::Error;

#[derive(Error, Debug)]
#[error("Overflow")]
pub struct OverflowError;

#[repr(transparent)]
#[serde_as]
#[derive(
  Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize, Deref, DerefMut, Derivative,
)]
#[derivative(Default(bound = "[Coefficient; COEFFICIENTS]: Default"))]
#[serde(bound(serialize = "Coefficient: Serialize"))]
#[serde(bound(deserialize = "Coefficient: Deserialize<'de>"))]
pub struct Polynomial<Coefficient, const COEFFICIENTS: usize>(
  #[serde_as(as = "[_; COEFFICIENTS]")] pub [Coefficient; COEFFICIENTS],
);

/**

Add `first * second` into `destination`.

This can fail due to overflow of intermediate results; we don't currently have a rigorous definition of when that can happen. If this returns Err, there is no guarantee about what values are left in `destination`.

*/
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
      let destination = &mut destination[first_power + second_power];
      *destination = destination
        .checked_add(
          &first_coefficient
            .checked_mul(&second_coefficient)
            .ok_or(OverflowError)?,
        )
        .ok_or(OverflowError)?;
    }
  }
  Ok(())
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> Polynomial<Coefficient, COEFFICIENTS> {
  pub fn zero() -> Self {
    Polynomial([Zero::zero(); COEFFICIENTS])
  }
  pub fn constant(value: Coefficient) -> Self {
    let mut coefficients = [Zero::zero(); COEFFICIENTS];
    coefficients[0] = value;
    Polynomial(coefficients)
  }
  pub fn is_constant(&self) -> bool {
    self.0.iter().skip(1).all(|&a| a == Coefficient::zero())
  }
}

impl<Coefficient: DoubleSizedSignedInteger, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  /// Calculate the Taylor coefficients of the polynomial `self` at the input `input`.
  ///
  /// Returns None if any of the coefficients do not fit in the type.
  /// (We can't just return [WorkingType; COEFFICIENTS], because it could overflow WorkingType too.
  /// By restricting it to the original type, we can at least guarantee that we always return Some
  /// if the results are in-bounds; if we returned WorkingType, there would also be failures due
  /// to internal overflow.)
  pub fn all_taylor_coefficients(
    &self,
    input: impl Copy + TryInto<DoubleSized<Coefficient>>,
  ) -> Option<Self> {
    if self.is_constant() {
      return Some(*self);
    }
    let input = input.try_into().ok()?;
    let mut intermediates: [DoubleSized<Coefficient>; COEFFICIENTS] =
      self.0.map(|coefficient| coefficient.into());
    for first_source in (1..intermediates.len()).rev() {
      for source in first_source..intermediates.len() {
        intermediates[source - 1] =
          intermediates[source - 1].checked_add(&intermediates[source].checked_mul(&input)?)?;
      }
    }
    let mut output = [Coefficient::zero(); COEFFICIENTS];
    for (index, value) in intermediates.iter().enumerate() {
      output[index] = (*value).try_into().ok()?;
    }
    Some(Polynomial(output))
  }
}

/**
Get the minimal S such that the error size of every term in the intermediates for all_taylor_coefficients_bounds_within_half is < (1 << S).
*/
#[inline(always)]
const fn intermediate_error_shift<const COEFFICIENTS: usize>() -> u32 {
  /*
  Hardcoded values calculated by this Python code:

  ```python
  shifts = []
  for coefficients in range(33):
    intermediate_errors = [0 for _ in range(coefficients)]

    for first_source in reversed(range(1, coefficients)):
      print(intermediate_errors)
      for source in range(first_source, coefficients):
        # produces a *strict* upper bound on the error because round-to-integer
        # cannot produce an error of *exactly* 1.0, only slightly less
        intermediate_errors[source - 1] += intermediate_errors[source] * 0.5 + 1.0

    print(intermediate_errors)
    worst = max(intermediate_errors + [0])
    # <= vs < because intermediate_errors are *strict* upper bounds on the error
    shift = next(s for s in range(100) if worst <= 2**s)
    print(f"#{coefficients}: {worst} < 1<<{shift}")
    shifts.append(shift)

  print(shifts)
  ```

   */
  const HARDCODED: [u32; 33] = [
    0, 0, 0, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 12, 12, 13, 13, 14, 14, 15, 15,
    16, 17, 17, 18, 18,
  ];
  if COEFFICIENTS < HARDCODED.len() {
    HARDCODED[COEFFICIENTS]
  } else {
    COEFFICIENTS as u32
  }
}

/**
Get the minimal S such that the accumulated magnitude of every term in the intermediates for
all_taylor_coefficients_bounds_within_half is < (1 << S) times the maximum magnitude
of the original coefficients.
*/
#[inline(always)]
const fn intermediate_magnitude_factor_shift<const COEFFICIENTS: usize>() -> u32 {
  /*
  Hardcoded values calculated by this Python code:

  ```python
  shifts = []
  for coefficients in range(33):
    # Magnitudes proportional to the maximum magnitude of the original coefficient type
    max_intermediates = [1.0 for _ in range(coefficients)]

    for first_source in reversed(range(1, coefficients)):
      print(max_intermediates)
      for source in range(first_source, coefficients):
        # Assume the type is always at least i8, so the maximum increase due to rounding
        # error is <= 1/127 of the maximum value of the coefficient type.
        # Make it a little bigger to guarantee that it's a strict upper bound
        # (these tiny adjustments do not change the computed answers)
        max_intermediates[source - 1] += max_intermediates[source] * 0.5 + (1/126)

    print(max_intermediates)
    worst = max(max_intermediates + [0])
    # <= vs < because max_intermediates are *strict* upper bounds on the magnitude
    shift = next(s for s in range(100) if worst <= 2**s)
    print(f"#{coefficients}: {worst} < 1<<{shift}")
    shifts.append(shift)

  print(shifts)
  ```

  Note that, for the purpose of max_total_shift below,
  we might be able to squeeze out an extra 1 for certain values of COEFFICIENTS
  if we only consider the intermediates *that will actually be multiplied by `input`*.
  However, in the special case where input_shift == 1, the max input is 1, so multiplying
  by input is irrelevant (if you used the rule that works for input_shift>1, and applied
  the maximum precision_shift, then the final intermediates will overflow even though
  they aren't multiplied by `input`.)

  Thus, squeezing out the extra 1 would require us to complicate the max_total_shift rule;
  that's not worth it.

   */
  const HARDCODED: [u32; 33] = [
    0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 11, 12, 12, 13, 13, 14, 15,
    15, 16, 16, 17, 17,
  ];
  if COEFFICIENTS < HARDCODED.len() {
    HARDCODED[COEFFICIENTS]
  } else {
    COEFFICIENTS as u32
  }
}

pub(crate) fn all_taylor_coefficients_bounds_within_half_unchecked<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient>,
  const COEFFICIENTS: usize,
>(
  polynomial: &[Coefficient; COEFFICIENTS],
  input: WorkingType,
  input_shift: impl ShiftSize,
  precision_shift: impl ShiftSize,
) -> [[WorkingType; 2]; COEFFICIENTS] {
  // In the loop, error accumulates each term.
  // Fortunately, the error size is < intermediate_error_shift.
  // We want to scale down the error as far as possible, so we first left-shift by intermediate_error_shift+2,
  // then right-shift by the same amount at the end of the calculation.
  // This reduces the accumulated error range (which is twice the max error magnitude) to < 0.5.
  // It unavoidably adds an error of up to <1.0 due to the final rounding, so the final error is <1.5.
  // This means that the final upper and lower bound can be no more than 2 away from each other,
  // which is the best we can hope for.
  // TODO: would making this a ZST optimize anything, or is it already inlined?
  let intermediate_error_shift = intermediate_error_shift::<COEFFICIENTS>();

  // in total, in the formula, we left-shift by precision_shift,
  // then multiply by a number that is up to half of 1<<input_shift -
  // i.e. we need space for precision_shift+inputshift-1 more bits in the type.

  let total_initial_shift = precision_shift.into() + intermediate_error_shift + 2;
  let mut intermediates: [WorkingType; COEFFICIENTS] = polynomial.map(|raw| {
    let mut raw: WorkingType = raw.into();
    raw <<= total_initial_shift as u32;
    raw
  });
  for first_source in (1..intermediates.len()).rev() {
    for source in first_source..intermediates.len() {
      intermediates[source - 1] += shr_round_to_even(intermediates[source] * input, input_shift);
    }
  }
  intermediates.map(|a| {
    [
      Shr::<u32>::shr(
        // Subtract 0.25, shifting the error range from (answer - 0.25, answer + 0.25)
        // to (answer - 0.5, answer), then floor, leaving (answer - 1.5, answer).
        a - (WorkingType::one() << (intermediate_error_shift)),
        intermediate_error_shift + 2,
      ),
      Shr::<u32>::shr(
        // Add 0.25, shifting the error range from (answer - 0.25, answer + 0.25)
        // to (answer, answer + 0.5), then ceil, leaving (answer, answer + 1.5).
        // The ceil is rolled into one "add+floor". If you remove the last `- WorkingType::one()`,
        // it would still yield a correct answer, but would
        // create a directional bias compared to the min.
        a + ((WorkingType::one() << (intermediate_error_shift))
          + (WorkingType::one() << (intermediate_error_shift + 2))
          - WorkingType::one()),
        intermediate_error_shift + 2,
      ),
    ]
  })
}

pub fn assert_input_within_half<T: Integer + Signed, S: ShiftSize>(
  input: T,
  input_shift: S,
  fn_name: &'static str,
) {
  match input_shift.into().checked_sub(1) {
    Some(half_shift) => {
      let half = T::one() << half_shift;
      assert!(
        input.abs() <= half,
        "{} called with an input({}) that is not within half (shift: {}, half: {})",
        fn_name,
        input,
        input_shift.into(),
        half
      );
    }
    None => {
      panic!(
        "{} called with an input({}) that is not within half (shift: {})",
        fn_name,
        input,
        input_shift.into()
      );
    }
  }
}

impl<Coefficient: Integer + Signed, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  /// The maximum total of `input_shift + precision_shift`.
  /// The bigger WorkingType is, the bigger this can be.
  pub fn all_taylor_coefficients_bounds_within_half_max_total_shift<
    WorkingType: Integer + Signed,
  >() -> u32 {
    let spare = WorkingType::nonsign_bits() - Coefficient::nonsign_bits();
    // we need to take out enough for the initial left-shift to do higher calculations at higher magnitude to remove the error,
    // and then also take out enough to make sure the calculations don't overflow due to accumulated value.
    // That produces the limit on the total *shift* induced by input_shift and precision_shift -
    // but the max input is `1 << (input_shift - 1)` rather than `1 << input_shift`, so we have to add 1 back in.
    spare
      - (intermediate_error_shift::<COEFFICIENTS>() + 2)
      - intermediate_magnitude_factor_shift::<COEFFICIENTS>()
      + 1
  }

  /**
  Evaluate all Taylor coefficients of `self` at a given input in the range [-0.5, 0.5].

  Calculates the Taylor coefficients of the polynomial (`self` * 2^precision_shift) at the input (`input` / 2^`input_shift`).

  The true outputs won't necessarily be integers, so we can't return them exactly.
  Instead, for each coefficient, we return bounds of the form [WorkingType; 2],
  representing an inclusive range that is guaranteed to include the true answer.
  The min and max of that range are also guaranteed to be < 1.5 units away from the true answer.
  Since they're integers, this also implies all of the following:
  * They will be <= 2 units away from each other.
  * Their mean will be <= 0.5 units away from the true answer.
  * Their mean rounded to the nearest integer will be <= 1 units away from the true answer.

  The idea of precision_shift is to let you calculate the answer with a smaller amount of error,
  by scaling up the numbers while guaranteeing the same absolute range of <=2 in the answers.

  # Panics

  The input must be in the range [-0.5, 0.5]. Otherwise, this function will panic.

  `input_shift + precision_shift` must not exceed `all_taylor_coefficients_bounds_within_half_max_total_shift()`. If they are, this function will panic.

  However, this function never fails due to overflow.
  */
  pub fn all_taylor_coefficients_bounds_within_half<
    WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient>,
  >(
    &self,
    input: WorkingType,
    input_shift: impl ShiftSize,
    precision_shift: impl ShiftSize,
  ) -> [[WorkingType; 2]; COEFFICIENTS] {
    assert!(
      input_shift.into() + precision_shift.into()
        <= Self::all_taylor_coefficients_bounds_within_half_max_total_shift::<WorkingType>()
    );
    if input == Zero::zero() || COEFFICIENTS as u32 <= 1 {
      return self.0.map(|raw| {
        let mut raw = raw.into();
        raw <<= precision_shift.into();
        [raw, raw]
      });
    }
    assert_input_within_half(
      input,
      input_shift,
      "all_taylor_coefficients_bounds_within_half",
    );
    all_taylor_coefficients_bounds_within_half_unchecked(self, input, input_shift, precision_shift)
  }
}

impl<Coefficient: DoubleSizedSignedInteger, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  /**
  Evaluate all Taylor coefficients of `self` at a given *fractional* input.

  Calculates the Taylor coefficients of the polynomial (`self` * 2^precision_shift) at the input (`input` / 2^`input_shift`).

  This function is simply the composition of all_taylor_coefficients() (to find the closest integer polynomial) and all_taylor_coefficients_bounds_within_half(). Thus, it may return None if the closest integer polynomial has overflowing exponents, even if the true answer would not overflow. See those two functions' documentation for more details.
  */
  pub fn all_taylor_coefficients_bounds<
    WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
  >(
    &self,
    input: WorkingType,
    input_shift: impl ShiftSize,
    precision_shift: impl ShiftSize,
  ) -> Option<[[WorkingType; 2]; COEFFICIENTS]> {
    let integer_input = shr_nicely_rounded(input, input_shift);
    let small_input = input.wrapping_sub(&(Shl::<u32>::shl(integer_input, input_shift.into())));
    let integer_coefficients = self.all_taylor_coefficients(integer_input)?;
    Some(
      integer_coefficients.all_taylor_coefficients_bounds_within_half(
        small_input,
        input_shift,
        precision_shift,
      ),
    )
  }
}

impl<Coefficient: DoubleSizedSignedInteger, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  /**
  Modify a specific Taylor coefficient at a fractional input.

  Modifies `self` so that its `which_coefficient`th Taylor coefficient will be approximately equal to `target_value` at the input (`input` / 2^`input_shift`), while its other Taylor coefficients at that input will be approximately the same as they were before.

  This can fail due to overflow; we don't currently have a rigorous definition of when that can happen. We also don't have a definition of how bad the rounding error can be.
  */
  // TODO: define the overflow and error size rules, make tests, refactor this function, etc.
  // Possible approach: split off a SetNthTaylorCoefficientWithinHalf, and from that, also split off a AddNthTaylorCoefficientWithinHalf. AddNthTaylorCoefficientWithinHalf can be the one with the rigorous guarantees; the first two can be documented as compositions of the others. Also, these functions should return copies rather than modify `self`.
  pub fn set_nth_taylor_coefficient_at_fractional_input(
    &mut self,
    which_coefficient: usize,
    input: DoubleSized<Coefficient>,
    input_shift: impl ShiftSize,
    target_value: Coefficient,
  ) -> Result<(), OverflowError> {
    let integer_input = shr_nicely_rounded(input, input_shift);
    let small_input = input.wrapping_sub(&(Shl::<u32>::shl(integer_input, input_shift.into())));
    let integer_coefficients = self
      .all_taylor_coefficients(integer_input)
      .ok_or(OverflowError)?;
    let bounds = integer_coefficients.all_taylor_coefficients_bounds_within_half(
      small_input,
      input_shift,
      0u32,
    );

    let mut change_at_input = Polynomial::<_, COEFFICIENTS>::zero();
    change_at_input[which_coefficient] = (DoubleSized::<Coefficient>::from(target_value)
      - mean_round_to_even(bounds[which_coefficient][0], bounds[which_coefficient][1]))
    .try_into()
    .map_err(|_| OverflowError)?;
    let change_at_integer =
      change_at_input.all_taylor_coefficients_bounds_within_half(-small_input, input_shift, 0u32);
    let mut new_at_integer = integer_coefficients;
    for (change, new) in change_at_integer.iter().zip(new_at_integer.iter_mut()) {
      *new = (DoubleSized::<Coefficient>::from(*new) + mean_round_to_even(change[0], change[1]))
        .try_into()
        .map_err(|_| OverflowError)?;
    }
    *self = new_at_integer
      .all_taylor_coefficients(-integer_input)
      .ok_or(OverflowError)?;
    // let bounds = self.all_taylor_coefficients_bounds(input, input_shift, 0u32)?;
    //
    // let mut target_values: ::smallvec::SmallVec<[DoubleSized<Coefficient>; 8]> =
    //   ::smallvec::SmallVec::with_capacity(which_coefficient + 1);
    // let bounds = self.all_taylor_coefficients_bounds(input, input_shift, 0u32)?;
    // for index in 0..which_coefficient {
    //   target_values.push(mean_round_to_even(
    //     bounds.as_slice()[index][0],
    //     bounds.as_slice()[index][1],
    //   ));
    // }
    // target_values.push(target_value.into());
    // for (index, target_value) in target_values.iter().enumerate().rev() {
    //   let current_bounds = self
    //     .all_taylor_coefficients_bounds(input, input_shift, 0u32)?
    //     .as_slice()[index];
    //   let current_value = mean_round_to_even(current_bounds[0], current_bounds[1]);
    //   let change_size = target_value.checked_sub(&current_value)?;
    //   self.as_mut_slice()[index] =
    //     self.as_slice()[index].checked_add(&change_size.try_into().ok()?)?;
    // }
    Ok(())
  }
}

/*pub trait PolynomialBoundsGenerator<Input, Output> {
fn generate_bounds(&self, input: FractionalInput<Input>) -> Option<Output>;
}*/
pub trait PolynomialBoundsFilter<Coefficient> {
  fn interval_filter(&self, bounds: [Coefficient; 2]) -> bool;
  fn result_filter(&self, bounds: [Coefficient; 2]) -> bool;
}

macro_rules! impl_filter {
($Filter: ident, $operation: tt, $yesdir: expr, $nodir: expr, $validcond: tt, $validval: expr) => {
pub struct $Filter<Coefficient> {
permit_threshold: Coefficient,
require_threshold: Coefficient,
}

impl<Coefficient: Integer+Signed> $Filter<Coefficient> {
pub fn new(permit_threshold: Coefficient, require_threshold: Coefficient)->Self {
  assert!(require_threshold.saturating_sub(permit_threshold) $validcond ($validval), "thresholds need to be separated by at least 2 to allow for error");
  $Filter {permit_threshold, require_threshold}
}
}

impl<Coefficient: Integer+Signed, Filtered: Integer+Signed+From<Coefficient>> PolynomialBoundsFilter<Filtered> for $Filter<Coefficient> {
fn interval_filter(&self, bounds: [Filtered; 2]) -> bool {
  bounds [$yesdir] $operation <Filtered as From<Coefficient>>::from(self.require_threshold)
}
fn result_filter(&self, bounds: [Filtered; 2]) -> bool {
  bounds [$nodir] $operation <Filtered as From<Coefficient>>::from(self.permit_threshold)
}
}

}
}
impl_filter!(LessThanFilter, <, 0, 1, <, -Coefficient::one());
impl_filter!(LessThanEqualToFilter, <=, 0, 1, <, -Coefficient::one());
impl_filter!(GreaterThanFilter, >, 1, 0, >, Coefficient::one());
impl_filter!(GreaterThanEqualToFilter, >=, 1, 0, >, Coefficient::one());

// pub trait PolynomialRangeSearch<Coefficient, WorkingType>: PolynomialBase {
//   fn next_time_value_passes<Filter: PolynomialBoundsFilter<Coefficient>>(
//     &self,
//     start_input: WorkingType,
//     input_shift: impl ShiftSize,
//     filter: Filter,
//   ) -> Option<WorkingType>;
// }
// pub trait PolynomialMagnitudeSquaredRangeSearch<WorkingType>: PolynomialBase + Sized {
//   fn next_time_magnitude_squared_passes<Filter: PolynomialBoundsFilter<WorkingType>>(
//     coordinates: &[Self],
//     start_input: WorkingType,
//     input_shift: impl ShiftSize,
//     filter: Filter,
//   ) -> Option<WorkingType>;
// }

// pub trait Polynomial<Coefficient: DoubleSizedSignedInteger>:
//   PolynomialBase<Coefficient = Coefficient>
//   + AllTaylorCoefficients<DoubleSized<Coefficient>>
//   + AllTaylorCoefficientsBounds<DoubleSized<Coefficient>>
//   + PolynomialRangeSearch<Coefficient, DoubleSized<Coefficient>>
// //+ PolynomialMagnitudeSquaredRangeSearch<Coefficient, DoubleSized<Coefficient>>
// {
// }
// impl<
//     Coefficient: DoubleSizedSignedInteger,
//     P: PolynomialBase<Coefficient = Coefficient>
//       + AllTaylorCoefficients<DoubleSized<Coefficient>>
//       + AllTaylorCoefficientsBounds<DoubleSized<Coefficient>>
//       + PolynomialRangeSearch<Coefficient, DoubleSized<Coefficient>>, //+ PolynomialMagnitudeSquaredRangeSearch<Coefficient, DoubleSized<Coefficient>>
//   > Polynomial<Coefficient> for P
// {
// }

macro_rules! impl_polynomials {
($($coefficients: expr),*) => {
$(




impl <Coefficient: DoubleSizedSignedInteger> Polynomial <Coefficient, $coefficients> {
pub fn next_time_value_passes<
  Filter: PolynomialBoundsFilter<Coefficient>,
>(
  &self,
  start_input: DoubleSized<Coefficient>,
  input_shift: impl ShiftSize,
  filter: Filter,
) -> Option<DoubleSized<Coefficient>> {
  struct Search <S,Coefficient, Filter> {
    polynomial: Polynomial <Coefficient, $coefficients>,
    filter: Filter,
    input_shift: S,
    _marker: PhantomData <Coefficient>,
  }
  impl<S: ShiftSize,Coefficient: DoubleSizedSignedInteger, Filter: PolynomialBoundsFilter <Coefficient>> RangeSearch for Search<S,Coefficient, Filter> {
    type Input = DoubleSized<Coefficient>;
    type IntegerValue = Polynomial <Coefficient, $coefficients>;
    type FractionalValue = [[DoubleSized <Coefficient>; 2]; $coefficients];
    fn value_at_integer (&self, input: Self::Input)->Option<Self::IntegerValue> {
      self.polynomial.all_taylor_coefficients (input)
    }
    fn value_at_fractional (&self, nearest_integer_value: &Self::IntegerValue, relative_input: Self::Input)->Self::FractionalValue {
      nearest_integer_value.all_taylor_coefficients_bounds_within_half (relative_input, self.input_shift, STANDARD_PRECISION_SHIFT)
    }
    fn integer_interval_filter (&self, endpoints: [&Self::IntegerValue; 2], duration: Self::Input)->bool {
      self.filter.interval_filter (range_search::coefficient_bounds_on_integer_interval (endpoints, duration) [0])
    }
    fn fractional_interval_filter (&self, endpoints: [&Self::FractionalValue; 2], duration_shift: impl ShiftSize)->bool {
      self.filter.interval_filter (range_search::value_bounds_on_negative_power_of_2_interval::<_, Coefficient, $coefficients> (endpoints, duration_shift))
    }
    fn tail_filter (&self, endpoint: &Self::IntegerValue)->bool {
      self.filter.interval_filter (range_search::coefficient_bounds_on_tail (endpoint) [0])
    }
    fn fractional_result_filter (&self, value: &Self::FractionalValue)->bool {
      self.filter.result_filter ([
        saturating_downcast(shr_floor(value[0][0], STANDARD_PRECISION_SHIFT)),
        saturating_downcast(shr_ceil(value[0][1], STANDARD_PRECISION_SHIFT)),
      ])
    }
  }
  RangeSearchRunner::run (Search {
    polynomial: *self,
    filter, input_shift,
    _marker: PhantomData
  }, start_input, input_shift)
}
}

)*

}
}

macro_rules! impl_squarable_polynomials {
($($coefficients: expr),*) => {
$(
impl <Coefficient: DoubleSizedSignedInteger> Polynomial <Coefficient, $coefficients> where DoubleSized<Coefficient>: DoubleSizedSignedInteger
{
pub fn next_time_magnitude_squared_passes<
  Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
>(
  coordinates: &[Self],
  start_input: DoubleSized<Coefficient>,
  input_shift: impl ShiftSize,
  filter: Filter,
) -> Option<DoubleSized<Coefficient>> {
  struct Search <'a, S, Coefficient, Filter> {
    coordinates: &'a [Polynomial <Coefficient, $coefficients>],
    filter: Filter,
    input_shift: S,
    _marker: PhantomData <Coefficient>,
  }
  impl<'a, S: ShiftSize, Coefficient: DoubleSizedSignedInteger, Filter: PolynomialBoundsFilter <DoubleSized<Coefficient>>> RangeSearch for Search<'a, S, Coefficient, Filter> where DoubleSized<Coefficient>: DoubleSizedSignedInteger {
    type Input = DoubleSized<Coefficient>;
    type IntegerValue = Polynomial <DoubleSized<Coefficient>, {$coefficients*2-1}>;
    type FractionalValue = [[DoubleSized<DoubleSized <Coefficient>>; 2]; $coefficients*2-1];
    fn value_at_integer (&self, input: Self::Input)->Option<Self::IntegerValue> {
      let mut magsq: [DoubleSized<Coefficient>; $coefficients*2 -1] = [Zero::zero(); $coefficients*2 -1];
      for coordinate in self.coordinates {
        let integer_coefficients = coordinate.all_taylor_coefficients(input)?;

        add_product_into(
          &integer_coefficients.0.as_slice(),
          integer_coefficients.0.as_slice(),
          magsq.as_mut_slice(),
        ).ok()?;
      }
      Some(Polynomial (magsq))
    }
    fn value_at_fractional (&self, nearest_integer_value: &Self::IntegerValue, relative_input: Self::Input)->Self::FractionalValue {
      nearest_integer_value.all_taylor_coefficients_bounds_within_half(From::from(relative_input), self.input_shift, STANDARD_PRECISION_SHIFT)
    }
   fn integer_interval_filter (&self, endpoints: [&Self::IntegerValue; 2], duration: Self::Input)->bool {
      self.filter.interval_filter (range_search::coefficient_bounds_on_integer_interval (endpoints, duration.into()) [0])
    }
    fn fractional_interval_filter (&self, endpoints: [&Self::FractionalValue; 2], duration_shift: impl ShiftSize)->bool {
      self.filter.interval_filter (range_search::value_bounds_on_negative_power_of_2_interval::<_, DoubleSized<Coefficient>, {$coefficients* 2 -1}> (endpoints, duration_shift))
    }
    fn tail_filter (&self, endpoint: &Self::IntegerValue)->bool {
      self.filter.interval_filter (range_search::coefficient_bounds_on_tail (endpoint) [0])
    }
    fn fractional_result_filter (&self, value: &Self::FractionalValue)->bool {
      self.filter.result_filter ([
        saturating_downcast(shr_floor(value[0][0], STANDARD_PRECISION_SHIFT)),
        saturating_downcast(shr_ceil(value[0][1], STANDARD_PRECISION_SHIFT)),
      ])
    }
  }
  RangeSearchRunner::run (Search {
    coordinates,
    filter, input_shift,
    _marker: PhantomData
  }, start_input, input_shift)
}
}
)*

}
}

impl_polynomials!(1, 2, 3, 4, 5);
impl_squarable_polynomials!(1, 2, 3);

const SQUARED_PRECISION_SHIFT: u32 = 3;
fn square_bounds<T: Integer>(scale_down: bool, bounds: impl IntoIterator<Item = [T; 2]>) -> [T; 2] {
  let mut result: [T; 2] = [Zero::zero(); 2];
  for [lower, upper] in bounds {
    let lower_square = lower.saturating_mul(lower);
    let upper_square = upper.saturating_mul(upper);
    if (lower > Zero::zero()) == (upper > Zero::zero()) {
      result[0] = result[0].saturating_add(min(lower_square, upper_square));
    }
    result[1] = result[1].saturating_add(max(lower_square, upper_square));
  }
  if scale_down {
    [
      shr_floor(result[0], SQUARED_PRECISION_SHIFT * 2),
      shr_ceil(result[1], SQUARED_PRECISION_SHIFT * 2),
    ]
  } else {
    result
  }
}

pub fn next_time_magnitude_squared_passes<
  Coefficient: DoubleSizedSignedInteger,
  Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
  const DIMENSIONS: usize,
  const COEFFICIENTS: usize,
>(
  coordinates: &[&Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  start_input: DoubleSized<Coefficient>,
  input_shift: impl ShiftSize,
  filter: Filter,
) -> Option<DoubleSized<Coefficient>>
where
  DoubleSized<Coefficient>: DoubleSizedSignedInteger,
  [[[DoubleSized<Coefficient>; 2]; COEFFICIENTS]; DIMENSIONS]: Default,
{
  struct Search<'a, S, Coefficient, Filter, const COEFFICIENTS: usize, const DIMENSIONS: usize> {
    coordinates: &'a [&'a Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
    filter: Filter,
    input_shift: S,
    _marker: PhantomData<Coefficient>,
  }
  impl<
      'a,
      S: ShiftSize,
      Coefficient: DoubleSizedSignedInteger,
      Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
      const COEFFICIENTS: usize,
      const DIMENSIONS: usize,
    > RangeSearch for Search<'a, S, Coefficient, Filter, COEFFICIENTS, DIMENSIONS>
  where
    DoubleSized<Coefficient>: DoubleSizedSignedInteger,
    [[[DoubleSized<Coefficient>; 2]; COEFFICIENTS]; DIMENSIONS]: Default,
  {
    type Input = DoubleSized<Coefficient>;
    type IntegerValue = [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS];
    type FractionalValue = [[[DoubleSized<Coefficient>; 2]; COEFFICIENTS]; DIMENSIONS];
    fn value_at_integer(&self, input: Self::Input) -> Option<Self::IntegerValue> {
      let mut result = [Polynomial::zero(); DIMENSIONS];
      for (coordinate, result) in self.coordinates.iter().zip(&mut result) {
        *result = coordinate.all_taylor_coefficients(input)?;
      }
      Some(result)
    }
    fn value_at_fractional(
      &self,
      nearest_integer_value: &Self::IntegerValue,
      relative_input: Self::Input,
    ) -> Self::FractionalValue {
      let mut result = [[[Zero::zero(); 2]; COEFFICIENTS]; DIMENSIONS];
      for (coordinate, result) in nearest_integer_value.iter().zip(&mut result) {
        *result = coordinate.all_taylor_coefficients_bounds_within_half(
          relative_input,
          self.input_shift,
          SQUARED_PRECISION_SHIFT,
        );
      }
      result
    }
    fn integer_interval_filter(
      &self,
      endpoints: [&Self::IntegerValue; 2],
      duration: Self::Input,
    ) -> bool {
      self.filter.interval_filter(square_bounds(
        false,
        endpoints[0].iter().zip(endpoints[1]).map(|(e1, e2)| {
          range_search::coefficient_bounds_on_integer_interval([e1, e2], duration)[0]
            .map(|a| a.into())
        }),
      ))
    }
    fn fractional_interval_filter(
      &self,
      endpoints: [&Self::FractionalValue; 2],
      duration_shift: impl ShiftSize,
    ) -> bool {
      self.filter.interval_filter(square_bounds(
        true,
        endpoints[0].iter().zip(endpoints[1]).map(|(e1, e2)| {
          let b: [Coefficient; 2] =
            range_search::value_bounds_on_negative_power_of_2_interval([e1, e2], duration_shift);
          b.map(|a| a.into())
        }),
      ))
    }
    fn tail_filter(&self, endpoint: &Self::IntegerValue) -> bool {
      self.filter.interval_filter(square_bounds(
        false,
        endpoint
          .iter()
          .map(|e1| range_search::coefficient_bounds_on_tail(e1)[0].map(|a| a.into())),
      ))
    }
    fn fractional_result_filter(&self, value: &Self::FractionalValue) -> bool {
      self
        .filter
        .result_filter(square_bounds(true, value.iter().map(|c| c[0])))
    }
  }
  RangeSearchRunner::run(
    Search {
      coordinates,
      filter,
      input_shift,
      _marker: PhantomData,
    },
    start_input,
    input_shift,
  )
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct ValueWithPrecision<P> {
  pub value: P,
  pub precision: u32,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct PolynomialBasedAtInput<P, I> {
  pub coefficients: P,
  pub origin: I,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct FractionalInput<T> {
  pub numerator: T,
  pub shift: u32,
}

impl<P, I> PolynomialBasedAtInput<P, I> {
  pub fn new(coefficients: P, origin: I) -> Self {
    PolynomialBasedAtInput {
      coefficients,
      origin,
    }
  }
}
impl<T: Integer> ValueWithPrecision<T> {
  pub fn bounds_without_precision<WorkingType: Integer + From<T>>(&self) -> [WorkingType; 2] {
    [
      shr_floor(self.value.into(), self.precision as u32),
      shr_ceil(self.value.into(), self.precision as u32),
    ]
  }
}

impl<T: Integer> ValueWithPrecision<[T; 2]> {
  pub fn without_precision<WorkingType: Integer + From<T>>(&self) -> [WorkingType; 2] {
    [
      shr_floor(self.value[0].into(), self.precision as u32),
      shr_ceil(self.value[1].into(), self.precision as u32),
    ]
  }
}

impl<T: Integer> PartialEq<FractionalInput<T>> for FractionalInput<T> {
  fn eq(&self, other: &Self) -> bool {
    #[allow(clippy::collapsible_else_if)]
    if self.shift < other.shift {
      if let Some(scaled) = self.raised_to_precision(other.shift) {
        scaled.numerator == other.numerator
      } else {
        false
      }
    } else {
      if let Some(scaled) = other.raised_to_precision(self.shift) {
        self.numerator == scaled.numerator
      } else {
        false
      }
    }
  }
}

impl<T: Integer> Eq for FractionalInput<T> {}

impl<T: Integer> Ord for FractionalInput<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    #[allow(clippy::collapsible_else_if)]
    if self.shift < other.shift {
      if let Some(scaled) = self.raised_to_precision(other.shift) {
        scaled.numerator.cmp(&other.numerator)
      } else {
        if self.numerator < T::zero() {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    } else {
      if let Some(scaled) = other.raised_to_precision(self.shift) {
        self.numerator.cmp(&scaled.numerator)
      } else {
        if other.numerator > T::zero() {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T: Integer> PartialOrd<FractionalInput<T>> for FractionalInput<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: Integer> FractionalInput<T> {
  pub fn new(numerator: T, shift: u32) -> FractionalInput<T> {
    FractionalInput { numerator, shift }
  }

  pub fn raised_to_precision(self, new_shift: u32) -> Option<Self> {
    new_shift.checked_sub(self.shift).and_then(|difference| {
      Some(FractionalInput {
        numerator: overflow_checked_shl(self.numerator, difference)?,
        shift: new_shift,
      })
    })
  }

  pub fn simplest_split(interval: [Self; 2]) -> FractionalInput<T> {
    match interval[0].shift.cmp(&interval[1].shift) {
      Ordering::Equal => {
        if interval[0].numerator.saturating_add(T::one()) == interval[1].numerator {
          FractionalInput::new(
            (interval[0].numerator << 1u32) + T::one(),
            interval[0].shift + 1,
          )
        } else {
          FractionalInput::new(
            mean_floor(interval[0].numerator, interval[1].numerator),
            interval[0].shift,
          )
        }
      }
      Ordering::Less => {
        for shift in interval[0].shift + 1.. {
          let mut attempt = interval[0].raised_to_precision(shift).unwrap();
          attempt.numerator += T::one();
          if attempt < interval[1] {
            return attempt;
          }
        }
        unreachable!()
      }
      Ordering::Greater => {
        for shift in interval[1].shift + 1.. {
          let mut attempt = interval[1].raised_to_precision(shift).unwrap();
          attempt.numerator -= T::one();
          if attempt > interval[0] {
            return attempt;
          }
        }
        unreachable!()
      }
    }
  }
}

//Note: currently, this function is strict (always find the exact time the max goes below the threshold). With a certain amount of error when the value is very close to the threshold, this could force searching every time unit. TODO: fix this by rigorously limiting the error and allowing that much leeway
// Returns a time where the polynomial output is definitely less than permit_threshold, such that there is no EARLIER output less than require_threshold. (Or returns None if it encounters overflow before any output less than require_threshold.) With only approximate polynomial evaluation, for these conditions to be theoretically meetable, we must have permit_threshold >= require_threshold + 2. (Imagine that we have permit_threshold = 5, require_threshold = 4. The polynomial may output the range [3, 5]. We wouldn't be permitted to return that time because the true value may be 5, which is not less than permit_threshold and therefore not permitted. But we wouldn't be able to pass by that time because the true value could be 3, which is less than require_threshold.) For EFFICIENCY, we need permit_threshold >= require_threshold + 3, because there's an extra 1 of error in computing bounds on an interval. (Imagine that we have permit_threshold = 5, require_threshold = 3. The polynomial may output the range [3, 5] for a long interval. But the interval might report a lower bound of 2, meaning the algorithm doesn't know it can skip that interval. Theoretically, this might lead the algorithm to explore every individual time within a long interval.)
