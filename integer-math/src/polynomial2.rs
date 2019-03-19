use array_ext::*;
use num::{
  CheckedAdd, CheckedMul, CheckedSub,
  Signed,
};
use std::cmp::{Ordering};
#[allow(unused_imports)]
use serde::Serialize;

use super::*;
use crate::range_search::{RangeSearch, RangeSearchRunner, STANDARD_PRECISION_SHIFT};
use std::marker::PhantomData;

/// Evaluate all Taylor coefficients of a polynomial.
///
/// Returns None if any of the coefficients do not fit in the type.
pub trait AllTaylorCoefficients<WorkingType>: Sized {
  fn all_taylor_coefficients(&self, input: impl Copy + Into<WorkingType>) -> Option<Self>;
}

pub trait AllTaylorCoefficientsBoundsWithinHalf<WorkingType>
{
  type Output;
  fn accumulated_error_shift() -> u32;
  fn max_total_shift() -> u32;
  fn all_taylor_coefficients_bounds_within_half(
    &self,
    input: WorkingType,
    input_shift: u32,
    precision_shift: u32,
  ) -> Self::Output;
}
pub trait AllTaylorCoefficientsBounds<WorkingType>: AllTaylorCoefficientsBoundsWithinHalf<WorkingType>
{
  fn all_taylor_coefficients_bounds(
    &self,
    input: WorkingType,
    input_shift: u32,
    precision_shift: u32,
  ) -> Option<Self::Output>;
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


pub trait PolynomialBase {
  type Coefficient: Integer + Signed;
}
pub type Coefficient<T> = <T as PolynomialBase>::Coefficient;
pub trait PolynomialRangeSearch<Coefficient, WorkingType>: PolynomialBase {
  fn next_time_value_passes<
    Filter: PolynomialBoundsFilter<Coefficient>,
  >(
    &self,
    start_input: WorkingType,
    input_shift: u32,
    filter: Filter,
  ) -> Option<WorkingType>;
}
pub trait PolynomialMagnitudeSquaredRangeSearch<WorkingType>: PolynomialBase + Sized {
  fn next_time_magnitude_squared_passes<
    Filter: PolynomialBoundsFilter<WorkingType>,
  >(
    coordinates: &[Self],
    start_input: WorkingType,
    input_shift: u32,
    filter: Filter,
  ) -> Option<WorkingType>;
}

pub trait SetNthTaylorCoefficientAtFractionalInput<WorkingType>: PolynomialBase {
  fn set_nth_taylor_coefficient_at_fractional_input(
  &mut self,
  which_derivative: usize,
  input: WorkingType,
  input_shift: u32,
  target_value: Self::Coefficient,
) -> Result<(), ::std::option::NoneError>;
}

pub trait Polynomial<Coefficient: DoubleSizedSignedInteger>:
  PolynomialBase <Coefficient=Coefficient>
  + AllTaylorCoefficients<DoubleSized<Coefficient>>
  + AllTaylorCoefficientsBounds<DoubleSized<Coefficient>>
  + PolynomialRangeSearch<Coefficient, DoubleSized<Coefficient>>
  //+ PolynomialMagnitudeSquaredRangeSearch<Coefficient, DoubleSized<Coefficient>>
{
}
impl<Coefficient: DoubleSizedSignedInteger,
    P: PolynomialBase<Coefficient=Coefficient>
      + AllTaylorCoefficients<DoubleSized<Coefficient>>
      + AllTaylorCoefficientsBounds<DoubleSized<Coefficient>>
      + PolynomialRangeSearch<Coefficient, DoubleSized<Coefficient>>
      //+ PolynomialMagnitudeSquaredRangeSearch<Coefficient, DoubleSized<Coefficient>>,
  > Polynomial<Coefficient> for P
{
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(

impl <Coefficient: Integer + Signed> PolynomialBase for [Coefficient; $coefficients] {
  type Coefficient = Coefficient;
}

impl <Coefficient: DoubleSizedSignedInteger> AllTaylorCoefficients<DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients(&self, input: impl Copy+Into<DoubleSized<Coefficient>>)->Option <Self> {
    let input = input.into();
    let mut intermediates: [DoubleSized<Coefficient>; $coefficients] = self.map (| coefficient | coefficient.into());
    for first_source in (1..intermediates.len()).rev() {
      for source in first_source..intermediates.len() {
        intermediates[source - 1] = intermediates [source - 1].checked_add (&intermediates[source].checked_mul (&input)?)?;
      }
    }
    let mut output = [Coefficient::zero(); $coefficients];
    for (index, value) in intermediates.iter().enumerate() {
      output [index] = (*value).try_into().ok()?;
    }
    Some (output)
  }

}



impl <Coefficient: Integer + Signed, WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient>> AllTaylorCoefficientsBoundsWithinHalf<WorkingType> for [Coefficient; $coefficients] {
  type Output = [[WorkingType; 2]; $coefficients];
  fn accumulated_error_shift()->u32 {
    $coefficients
  }
  fn max_total_shift()->u32 {
    let spare = WorkingType::nonsign_bits() - Coefficient::nonsign_bits();
    // we need to take out coefficients - 1 twice: once to do higher calculations at higher magnitude to remove the error, and once to make sure the calculations don't overflow due to accumulated value.
    spare + 1 - <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::accumulated_error_shift() - ($coefficients-1)
  }
  fn all_taylor_coefficients_bounds_within_half(&self, input: WorkingType, input_shift: u32, precision_shift: u32)->Self::Output {
    assert!(input_shift + precision_shift <= <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::max_total_shift());
    if input == Zero::zero() || $coefficients <= 1 {
      return self.map(|raw| {
        let mut raw = raw.into();
        raw <<= precision_shift as u32;
        [raw,raw]
      })
    }
    match input_shift.checked_sub(1) {
      Some(half_shift) => {
        let half = WorkingType::one() << half_shift;
        assert!(input.abs() <= half, "all_taylor_coefficients_bounds_within_half called with an input({}) that is not within half (shift: {}, half: {})", input, input_shift, half);
      },
      None => {
        panic!("all_taylor_coefficients_bounds_within_half called with an input({}) that is not within half (shift: {})", input, input_shift);
      },
    }
    
    // In the loop, error accumulates each term.
    // Fortunately, the error is strictly bounded above by 2^(degree-1).
    // We want to scale down the error as far as possible, so we first left-shift by degree+1,
    // then right-shift by the same amount at the end of the calculation.
    // This reduces the accumulated error range (which is twice the max error magnitude) to less than half.
    // It unavoidably adds an error of up to 1 due to the final rounding, so the final error is up to (not including) 1.5.
    // This means that the final upper and lower bound can be no more than 2 away from each other,
    // which is the best we can hope for.
    // TODO: would making this a ZST optimize anything, or is it already inlined?
    let accumulated_error_shift = <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::accumulated_error_shift();

    // in total, in the formula, we left-shift by precision_shift,
    // then multiply by a number that is up to half of 1<<input_shift -
    // i.e. we need space for precision_shift+inputshift-1 more bits in the type.

    let total_initial_shift = precision_shift + accumulated_error_shift;
    let mut intermediates: [WorkingType; $coefficients] = self.map(|raw| {
      let mut raw: WorkingType = raw.into();
      raw <<= total_initial_shift as u32;
      raw
    });
    for first_source in (1..intermediates.len()).rev() {
      for source in first_source..intermediates.len() {
        intermediates[source - 1] +=
          shr_round_to_even(intermediates[source] * input, input_shift);
      }
    }
    intermediates.map(|a| {
      [
        Shr::<u32>::shr(a - (WorkingType::one() << (accumulated_error_shift-2)),
          accumulated_error_shift),
        Shr::<u32>::shr(a + (WorkingType::one() << (accumulated_error_shift-2))
          + (WorkingType::one() << (accumulated_error_shift)) - WorkingType::one(),
          accumulated_error_shift),
      ]
    })
  }

}

impl <Coefficient: DoubleSizedSignedInteger> AllTaylorCoefficientsBounds<DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients_bounds(
    &self,
    input: DoubleSized<Coefficient>,
    input_shift: u32,
    precision_shift: u32,
  ) -> Option<<Self as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::Output> {
    let integer_input = shr_nicely_rounded(input, input_shift);
    let small_input = input.wrapping_sub(&(Shl::<u32>::shl(integer_input, input_shift)));
    let integer_coefficients = self.all_taylor_coefficients(integer_input)?;
    Some(<Self as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::all_taylor_coefficients_bounds_within_half(
      &integer_coefficients,
      small_input,
      input_shift,
      precision_shift,
    ))
  }
}

impl <Coefficient: DoubleSizedSignedInteger> PolynomialRangeSearch<Coefficient, DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn next_time_value_passes<
    Filter: PolynomialBoundsFilter<Coefficient>,
  >(
    &self,
    start_input: DoubleSized<Coefficient>,
    input_shift: u32,
    filter: Filter,
  ) -> Option<DoubleSized<Coefficient>> {
    struct Search <Coefficient, Filter> {
      polynomial: [Coefficient; $coefficients],
      filter: Filter,
      input_shift: u32,
      _marker: PhantomData <Coefficient>,
    }
    impl<Coefficient: DoubleSizedSignedInteger, Filter: PolynomialBoundsFilter <Coefficient>> RangeSearch for Search<Coefficient, Filter> {
      type Input = DoubleSized<Coefficient>;
      type IntegerValue = [Coefficient; $coefficients];
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
      fn fractional_interval_filter (&self, endpoints: [&Self::FractionalValue; 2], duration_shift: u32)->bool {
        self.filter.interval_filter (range_search::value_bounds_on_negative_power_of_2_interval::<_, Coefficient> (endpoints, duration_shift))
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
impl <Coefficient: DoubleSizedSignedInteger> SetNthTaylorCoefficientAtFractionalInput<DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn set_nth_taylor_coefficient_at_fractional_input(
  &mut self,
  which_derivative: usize,
  input: DoubleSized<Coefficient>,
  input_shift: u32,
  target_value: Coefficient,
) -> Result<(), ::std::option::NoneError> {
  let mut target_values: ::smallvec::SmallVec<[DoubleSized<Coefficient>; 8]> =
    ::smallvec::SmallVec::with_capacity(which_derivative + 1);
  let bounds = self.all_taylor_coefficients_bounds(input, input_shift, 0u32)?;
  for index in 0..which_derivative {
    target_values.push(mean_round_to_even(
      bounds.as_slice()[index][0],
      bounds.as_slice()[index][1],
    ));
  }
  target_values.push(target_value.into());
  for (index, target_value) in target_values.iter().enumerate().rev() {
    let current_bounds = self
      .all_taylor_coefficients_bounds(input, input_shift, 0u32)?
      .as_slice()[index];
    let current_value = mean_round_to_even(current_bounds[0], current_bounds[1]);
    let change_size = target_value.checked_sub(&current_value)?;
    self.as_mut_slice()[index] =
      self.as_slice()[index].checked_add(&change_size.try_into().ok()?)?;
  }
  Ok(())
}
}


)*

  }
}



macro_rules! impl_squarable_polynomials {
  ($($coefficients: expr),*) => {
$(
impl <Coefficient: DoubleSizedSignedInteger> PolynomialMagnitudeSquaredRangeSearch<DoubleSized<Coefficient>> for [Coefficient; $coefficients] where DoubleSized<Coefficient>: DoubleSizedSignedInteger
 {
  fn next_time_magnitude_squared_passes<
    Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
  >(
    coordinates: &[Self],
    start_input: DoubleSized<Coefficient>,
    input_shift: u32,
    filter: Filter,
  ) -> Option<DoubleSized<Coefficient>> {
    struct Search <'a, Coefficient, Filter> {
      coordinates: &'a [[Coefficient; $coefficients]],
      filter: Filter,
      input_shift: u32,
      _marker: PhantomData <Coefficient>,
    }
    impl<'a, Coefficient: DoubleSizedSignedInteger, Filter: PolynomialBoundsFilter <DoubleSized<Coefficient>>> RangeSearch for Search<'a, Coefficient, Filter> where DoubleSized<Coefficient>: DoubleSizedSignedInteger {
      type Input = DoubleSized<Coefficient>;
      type IntegerValue = [DoubleSized<Coefficient>; $coefficients*2-1];
      type FractionalValue = [[DoubleSized<DoubleSized <Coefficient>>; 2]; $coefficients*2-1];
      fn value_at_integer (&self, input: Self::Input)->Option<Self::IntegerValue> {
        let mut magsq: [DoubleSized<Coefficient>; $coefficients*2 -1] = [Zero::zero(); $coefficients*2 -1];
        for coordinate in self.coordinates {
          let integer_coefficients = coordinate.all_taylor_coefficients(input)?;

          super::polynomial::add_product_into(
            &integer_coefficients.as_slice(),
            integer_coefficients.as_slice(),
            magsq.as_mut_slice(),
          )
          .ok()?;
        }
        Some(magsq)
      }
      fn value_at_fractional (&self, nearest_integer_value: &Self::IntegerValue, relative_input: Self::Input)->Self::FractionalValue {
        <[DoubleSized<Coefficient>; $coefficients*2 -1] as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<DoubleSized<Coefficient>>>>::all_taylor_coefficients_bounds_within_half(nearest_integer_value, From::from(relative_input), self.input_shift, STANDARD_PRECISION_SHIFT)
      }
     fn integer_interval_filter (&self, endpoints: [&Self::IntegerValue; 2], duration: Self::Input)->bool {
        self.filter.interval_filter (range_search::coefficient_bounds_on_integer_interval (endpoints, duration.into()) [0])
      }
      fn fractional_interval_filter (&self, endpoints: [&Self::FractionalValue; 2], duration_shift: u32)->bool {
        self.filter.interval_filter (range_search::value_bounds_on_negative_power_of_2_interval::<_, DoubleSized<Coefficient>> (endpoints, duration_shift))
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
impl_squarable_polynomials!(1,2,3);




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
    PolynomialBasedAtInput { coefficients, origin }
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


