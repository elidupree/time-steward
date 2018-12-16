use array::{Array, ReplaceItemType};
use array_ext::{Array as ArrayExtArray, *};
use arrayvec::{self, ArrayVec};
use num::{
  Bounded, CheckedAdd, CheckedMul, CheckedSub, FromPrimitive, Integer as NumInteger, One,
  Saturating, Signed,
};
use std::cmp::{max, min, Ordering};
use serde::Serialize;

use super::*;

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
  fn max_total_shift() -> i32;
  fn all_taylor_coefficients_bounds_within_half(
    &self,
    input: WorkingType,
    input_shift: u32,
    precision_shift: i32,
  ) -> Option<Self::Output>;
}
pub trait AllTaylorCoefficientsBounds<WorkingType>: AllTaylorCoefficientsBoundsWithinHalf<WorkingType>
{
  fn all_taylor_coefficients_bounds(
    &self,
    input: WorkingType,
    input_shift: u32,
    precision_shift: i32,
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
pub trait PolynomialRangeSearch<WorkingType>: PolynomialBase {
  fn next_time_value_passes<
    Filter: PolynomialBoundsFilter<WorkingType>,
  >(
    &self,
    start_time: FractionalInput<WorkingType>,
    input_shift: u32,
    filter: Filter,
  ) -> Option<WorkingType>;
}
pub trait PolynomialMagnitudeSquaredRangeSearch<WorkingType>: PolynomialBase + Sized {
  fn next_time_magnitude_squared_passes<
    Filter: PolynomialBoundsFilter<WorkingType>,
  >(
    coordinates: &[Self],
    start_time: FractionalInput<WorkingType>,
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
  + PolynomialRangeSearch<DoubleSized<Coefficient>>
  //+ PolynomialMagnitudeSquaredRangeSearch<DoubleSized<Coefficient>>
{
}
impl<Coefficient: DoubleSizedSignedInteger,
    P: PolynomialBase<Coefficient=Coefficient>
      + AllTaylorCoefficients<DoubleSized<Coefficient>>
      + AllTaylorCoefficientsBounds<DoubleSized<Coefficient>>
      + PolynomialRangeSearch<DoubleSized<Coefficient>>
      //+ PolynomialMagnitudeSquaredRangeSearch<DoubleSized<Coefficient>>,
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
    $coefficients - 1
  }
  fn max_total_shift()->i32 {
    let spare = WorkingType::nonsign_bits() - Coefficient::nonsign_bits();
    // we need to take out coefficients - 1 twice: once to do higher calculations at higher magnitude to remove the error, and once to make sure the calculations don't overflow due to accumulated value.
    spare as i32 + 1 - <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::accumulated_error_shift() as i32 - ($coefficients-1)
  }
  fn all_taylor_coefficients_bounds_within_half(&self, input: WorkingType, input_shift: u32, precision_shift: i32)->Option<<Self as ReplaceItemType<[WorkingType; 2]>>::Type> {
    assert!(input_shift as i32 + precision_shift <= <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::max_total_shift());
    if input_shift == 0 {
      return Some(self.map(|raw| {
        let mut raw = raw.into();
        if precision_shift > 0 {
          raw <<= precision_shift as u32;
        }
        let mut raw = [raw,raw];
        if precision_shift < 0 {
          raw[0] = shr_floor(raw[0], (-precision_shift) as u32);
          raw[1] = shr_ceil(raw[1], (-precision_shift) as u32);
        }
        raw
      }))
    }
    assert!(input.abs() <= WorkingType::one() << (input_shift - 1));
    // In the loop, error accumulates each term.
    // Fortunately, the error is strictly bounded above by 2^(degree-1).
    // We want to scale down the error as far as possible, so we first left-shift by degree,
    // then right-shift by degree at the end of the calculation.
    // This reduces the accumulated error to less than half.
    // It unavoidably adds an error of up to 1 due to the final rounding, so the final error is up to (not including) 1.5.
    // This means that the final upper and lower bound can be no more than 2 away from each other,
    // which is the best we can hope for.
    // TODO: would making this a ZST optimize anything, or is it already inlined?
    let accumulated_error_shift = <Self as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::accumulated_error_shift();

    // in total, in the formula, we left-shift by precision_shift,
    // then multiply by a number that is up to half of 1<<input_shift -
    // i.e. we need space for precision_shift+inputshift-1 more bits in the type.

    // In the loop, we use floor/ceil to make sure we keep getting a lower/upper bound.
    // But we also multiply by input each time, and if the input was negative, we would keep switching the direction.
    // Fortunately, negative input is equivalent to having all of the odd terms be negated.
    let flip_odd = input < Zero::zero();
    let input = if flip_odd {-input} else {input};

    let total_initial_shift = precision_shift + accumulated_error_shift as i32;
    let mut intermediates: [[WorkingType; 2]; $coefficients] = array_ext::Array::from_fn(|index| {
      let mut raw: WorkingType = self[index].into();
      if total_initial_shift > 0 {
        raw <<= total_initial_shift as u32;
      }
      if flip_odd && index.is_odd() { raw = -raw; }
      let mut raw = [raw,raw];
      if total_initial_shift < 0 {
        raw[0] = shr_floor(raw[0], (-total_initial_shift) as u32);
        raw[1] = shr_ceil(raw[1], (-total_initial_shift) as u32);
      }
      raw
    });
    for first_source in (1..intermediates.len()).rev() {
      for source in first_source..intermediates.len() {
        intermediates[source - 1][0] +=
          shr_floor(intermediates[source][0] * input, input_shift);
        intermediates[source - 1][1] +=
          shr_ceil(intermediates[source][1] * input, input_shift);
      }
    }
    for (index, value) in intermediates.iter_mut().enumerate() {
      value[0] = shr_floor(value[0], accumulated_error_shift);
      value[1] = shr_ceil(value[1], accumulated_error_shift);

      if flip_odd && index.is_odd() {
        *value = [
          -value[1],
          -value[0],
        ];
      }
    }
    Some (intermediates)
  }

}

impl <Coefficient: DoubleSizedSignedInteger> AllTaylorCoefficientsBounds<DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients_bounds(
    &self,
    input: DoubleSized<Coefficient>,
    input_shift: u32,
    precision_shift: i32,
  ) -> Option<<Self as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::Output> {
    let integer_input = shr_nicely_rounded(input, input_shift);
    let small_input = input.wrapping_sub(&(Shl::<u32>::shl(integer_input, input_shift)));
    let integer_coefficients = self.all_taylor_coefficients(integer_input)?;
    <Self as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::all_taylor_coefficients_bounds_within_half(
      &integer_coefficients,
      small_input,
      input_shift,
      precision_shift,
    )
  }
}

impl <Coefficient: DoubleSizedSignedInteger> PolynomialRangeSearch<DoubleSized<Coefficient>> for [Coefficient; $coefficients] {
  fn next_time_value_passes<
    Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
  >(
    &self,
    start_time: FractionalInput<DoubleSized<Coefficient>>,
    input_shift: u32,
    filter: Filter,
  ) -> Option<DoubleSized<Coefficient>> {
  
  range_search(
    start_time,
    input_shift,
    |time| {
      let precision = <Self as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::max_total_shift() - time.shift as i32;
      self
        .all_taylor_coefficients_bounds(time.numerator, time.shift, precision)
        .map(|foo| ValueWithPrecision {
          value: foo,
          precision,
        })
    },
    |interval| filter.interval_filter(coefficient_bounds_on_interval(interval).as_slice()[0]),
    |tail_endpoint| {
      filter.interval_filter(coefficient_bounds_on_tail(&tail_endpoint.coefficients).as_slice()[0])
    },
    |result| {
      filter.result_filter(
        ValueWithPrecision {
          value: result.coefficients.value.as_slice()[0],
          precision: result.coefficients.precision,
        }
        .without_precision(),
      )
    },
  )
  
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
  let bounds = self.all_taylor_coefficients_bounds(input, input_shift, 0i32)?;
  for index in 0..which_derivative {
    target_values.push(mean_round_to_even(
      bounds.as_slice()[index][0],
      bounds.as_slice()[index][1],
    ));
  }
  target_values.push(target_value.into());
  for (index, target_value) in target_values.iter().enumerate().rev() {
    let current_bounds = self
      .all_taylor_coefficients_bounds(input, input_shift, 0i32)?
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
impl <Coefficient: DoubleSizedSignedInteger> PolynomialMagnitudeSquaredRangeSearch<DoubleSized<Coefficient>> for [Coefficient; $coefficients] //where DoubleSized<Coefficient>: DoubleSizedSignedInteger
 {
  fn next_time_magnitude_squared_passes<
    Filter: PolynomialBoundsFilter<DoubleSized<Coefficient>>,
  >(
    coordinates: &[Self],
    start_time: FractionalInput<DoubleSized<Coefficient>>,
    input_shift: u32,
    filter: Filter,
  ) -> Option<DoubleSized<Coefficient>> {
  
  range_search(
    start_time,
    input_shift,
    |time| {
      let precision = <[DoubleSized<Coefficient>; $coefficients*2 -1] as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::max_total_shift() - time.shift as i32;
      let mut magsq: [DoubleSized<Coefficient>; $coefficients*2 -1] = [Zero::zero(); $coefficients*2 -1];
      let integer_input = shr_nicely_rounded(time.numerator, time.shift);
      let small_input = time
        .numerator
        .wrapping_sub(&(Shl::<u32>::shl(integer_input, time.shift)));

      for coordinate in coordinates {
        let integer_coefficients = coordinate.all_taylor_coefficients(integer_input)?;

        super::polynomial::add_product_into(
          &integer_coefficients.as_slice(),
          integer_coefficients.as_slice(),
          magsq.as_mut_slice(),
        )
        .ok()?;
      }

      <[DoubleSized<Coefficient>; $coefficients*2 -1] as AllTaylorCoefficientsBoundsWithinHalf<DoubleSized<Coefficient>>>::all_taylor_coefficients_bounds_within_half(&magsq,
        small_input, time.shift, precision)
        .map(|foo| ValueWithPrecision {
          value: foo,
          precision,
        })
    },
    |interval| filter.interval_filter(coefficient_bounds_on_interval(interval).as_slice()[0]),
    |tail_endpoint| {
      filter.interval_filter(coefficient_bounds_on_tail(&tail_endpoint.coefficients).as_slice()[0])
    },
    |result| {
      filter.result_filter(
        ValueWithPrecision {
          value: result.coefficients.value.as_slice()[0],
          precision: result.coefficients.precision,
        }
        .without_precision(),
      )
    },
  )

  }
}
)*

  }
}

impl_polynomials!(1, 2, 3, 4, 5);
impl_squarable_polynomials!(1,2,3);




#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct ValueWithPrecision<P> {
  value: P,
  precision: i32,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct PolynomialBasedAtInput<P, I> {
  coefficients: P,
  origin: I,
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug, Default)]
pub struct FractionalInput<T> {
  numerator: T,
  shift: u32,
}

impl<T: Integer> ValueWithPrecision<T> {
  pub fn bounds_without_precision<WorkingType: Integer + From<T>>(&self) -> [WorkingType; 2] {
    if self.precision > 0 {
      [
        shr_floor(self.value.into(), self.precision as u32),
        shr_ceil(self.value.into(), self.precision as u32),
      ]
    } else {
      let t: WorkingType = self.value.into();
      [t << (-self.precision) as u32; 2]
    }
  }
}

impl<T: Integer> ValueWithPrecision<[T; 2]> {
  pub fn without_precision<WorkingType: Integer + From<T>>(&self) -> [WorkingType; 2] {
    if self.precision > 0 {
      [
        shr_floor(self.value[0].into(), self.precision as u32),
        shr_ceil(self.value[1].into(), self.precision as u32),
      ]
    } else {
      self.value.map(|a| {
        let a: WorkingType = a.into();
        a << (-self.precision) as u32
      })
    }
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

pub fn coefficient_bounds_on_interval<
  P: Array + arrayvec::Array<Item = [Coefficient; 2]>,
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient>,
>(
  endpoints: [&PolynomialBasedAtInput<ValueWithPrecision<P>, FractionalInput<WorkingType>>; 2],
) -> P {
  let mut result: P = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  let max_input_shift = max(endpoints[0].origin.shift, endpoints[1].origin.shift);
  let adjusted_times = endpoints.map(|a| a.origin.numerator << (max_input_shift - a.origin.shift));
  let duration = match adjusted_times[1].checked_sub(&adjusted_times[0]) {
    Some(a) => a,
    None => {
      // if the duration overflows, give up on the missing the range at all
      result = array_ext::Array::from_fn(|_| [Coefficient::min_value(), Coefficient::max_value()]);
      return result;
    }
  };
  let mut previous_derivative_range: [WorkingType; 2] = [Zero::zero(), Zero::zero()];
  let worse_precision = min(
    endpoints[0].coefficients.precision,
    endpoints[1].coefficients.precision,
  );
  for exponent in (0..result.len()).rev() {
    let end_bounds = endpoints.map(|endpoint| {
      let mut b = endpoint.coefficients.value.as_slice()[exponent]
        .map(<WorkingType as From<Coefficient>>::from);
      let excess_precision = endpoint.coefficients.precision - worse_precision;
      b[0] = shr_floor(b[0], excess_precision as u32);
      b[1] = shr_ceil(b[1], excess_precision as u32);
      b
    });

    let bounds: [WorkingType; 2] = if previous_derivative_range[0] >= Zero::zero() {
      [end_bounds[0][0], end_bounds[1][1]]
    } else if previous_derivative_range[1] <= Zero::zero() {
      [end_bounds[1][0], end_bounds[0][1]]
    } else {
      // TODO: these bounds can be tightened by analyzing the parallelogram
      // but it might overflow
      // did the algebra as (v1*s1 + v0*-s0 + (t1-t0)*s1*-s0)/(s1+ -s0) = max_value
      // let slope_product = previous_derivative_range[0]*previous_derivative_range[1];

      let (previous_min_movement, previous_max_movement) = (
        mul_shr_round_down(previous_derivative_range[0], duration, max_input_shift),
        mul_shr_round_up(previous_derivative_range[1], duration, max_input_shift),
      );

      // If an earlier derivative overflowed, assume it's arbitrarily high
      let unknown = (Bounded::min_value(), Bounded::max_value());
      let (left_min, right_max) = if previous_derivative_range[0] == Bounded::min_value() {
        unknown
      } else if let Some(previous_min_movement) = previous_min_movement {
        (
          end_bounds[0][0].saturating_add(previous_min_movement),
          end_bounds[1][1].saturating_sub(previous_min_movement),
        )
      } else {
        unknown
      };
      let (right_min, left_max) = if previous_derivative_range[1] == Bounded::max_value() {
        unknown
      } else if let Some(previous_max_movement) = previous_max_movement {
        (
          end_bounds[1][0].saturating_sub(previous_max_movement),
          end_bounds[0][1].saturating_add(previous_max_movement),
        )
      } else {
        unknown
      };

      [max(left_min, right_min), min(left_max, right_max)]
    };

    let bounds_without_precision: [WorkingType; 2] = ValueWithPrecision {
      value: bounds,
      precision: worse_precision,
    }
    .without_precision();
    result.as_mut_slice()[exponent] = bounds_without_precision.map(saturating_downcast);
    previous_derivative_range =
      bounds.map(|a| a.saturating_mul(FromPrimitive::from_usize(exponent).unwrap()));
  }
  result
}

pub fn coefficient_bounds_on_tail<
  P: Array + arrayvec::Array<Item = [Coefficient; 2]>,
  Coefficient: Integer + Signed,
>(
  endpoint: &ValueWithPrecision<P>,
) -> P {
  let mut result: P = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  let mut previous_derivative_range: [Coefficient; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len()).rev() {
    let mut bounds = endpoint.value.as_slice()[exponent];
    let mut bounds_without_precision: [Coefficient; 2] = ValueWithPrecision {
      value: bounds,
      precision: endpoint.precision,
    }
    .without_precision();
    if previous_derivative_range[0] < Zero::zero() {
      bounds[0] = Coefficient::min_value();
      bounds_without_precision[0] = Coefficient::min_value();
    }
    if previous_derivative_range[1] > Zero::zero() {
      bounds[1] = Coefficient::max_value();
      bounds_without_precision[1] = Coefficient::max_value();
    }
    result.as_mut_slice()[exponent] = bounds_without_precision;
    previous_derivative_range = bounds;
  }
  result
}



//pub struct

type HackP = ValueWithPrecision<[[i64; 2]; 5]>;

#[derive(Copy, Clone, Serialize, Deserialize, Debug)]
pub enum RangeSearchRecordHack {
  Interval { endpoints: [PolynomialBasedAtInput<HackP, FractionalInput<i64>>; 2], bounds: [[i64;2];5] },
  ToEnd { endpoint: PolynomialBasedAtInput<HackP, FractionalInput<i64>>, bounds: [[i64;2];5] },
}

fn hackhackhack(_:FractionalInput<i64>)->Option<ValueWithPrecision<HackP>> {None}
trait RangeSearchRecorderHack<I,P> {
  type Func: Fn(FractionalInput<i64>) -> Option<HackP>;
  fn range_search_record_hack<G: FnOnce(&RangeSearch<Self::Func, i64, HackP>)> (&self, g:G);
}

impl<F: Fn(FractionalInput<I>) -> Option<P>, I, P> RangeSearchRecorderHack<I,P> for RangeSearch<F,I,P> {
  default type Func = F;
  default fn range_search_record_hack<G: FnOnce(&RangeSearch<Self::Func, i64, HackP>)> (&self, _g:G) {}
}

impl<F: Fn(FractionalInput<i64>) -> Option<HackP>> RangeSearchRecorderHack<i64, HackP> for RangeSearch<F, i64, HackP> {
  type Func = F;
  fn range_search_record_hack<G: FnOnce(&RangeSearch<Self::Func, i64, HackP>)> (&self, g:G) {
    //(g)(self);
  }
}


pub struct RangeSearch<F, I, P> {
  func: F,
  max_input_shift: u32,
  stack: ArrayVec<[PolynomialBasedAtInput<P, FractionalInput<I>>; 64]>,
  next_jump: I,
}

impl<F: Fn(FractionalInput<I>) -> Option<P>, I: Integer, P> RangeSearch<F, I, P> {
  pub fn new(func: F, start_input: FractionalInput<I>, max_input_shift: u32) -> Option<Self> {
    let mut result = RangeSearch {
      func,
      max_input_shift,
      stack: ArrayVec::new(),
      next_jump: One::one(),
    };
    if let Some(coefficients) = (result.func)(start_input) {
      result.stack.push(PolynomialBasedAtInput {
        origin: start_input,
        coefficients,
      });
      result.add_next_endpoint();
      Some(result)
    } else {
      None
    }
  }
  pub fn latest_interval(&self) -> [&PolynomialBasedAtInput<P, FractionalInput<I>>; 2] {
    [
      &self.stack[self.stack.len() - 1],
      &self.stack[self.stack.len() - 2],
    ]
  }
  pub fn split_latest(&mut self) {
    let split_input = {
      let interval = self.latest_interval();
      FractionalInput::simplest_split([interval[0].origin, interval[1].origin])
    };

    if !self.add_endpoint(split_input) {
      let earliest = self.stack.pop().unwrap();
      self.stack.clear();
      self.stack.push(earliest);
      self.add_next_endpoint();
    }
  }
  pub fn skip_latest(&mut self) {
    self.stack.pop();
    if self.stack.len() < 2 {
      self.add_next_endpoint();
    }
  }
  pub fn reached_overflow(&self) -> bool {
    self.stack.len() == 2 && self.stack[0].origin == self.stack[1].origin
  }
  fn add_next_endpoint(&mut self) {
    assert_eq!(self.stack.len(), 1);
    let last_endpoint_input = self.stack.last().unwrap().origin;
    let mut attempt = FractionalInput::new(
      shr_floor(last_endpoint_input.numerator, last_endpoint_input.shift)
        .saturating_add(self.next_jump),
      0u32,
    );
    loop {
      if self.add_endpoint(attempt) {
        break;
      }
      attempt = FractionalInput::simplest_split([last_endpoint_input, attempt]);
      if attempt.shift > self.max_input_shift {
        attempt = last_endpoint_input;
      }
      self.next_jump = self.next_jump >> 1u32;
    }
    self.next_jump = max(One::one(), self.next_jump << 1u32);
  }
  fn add_endpoint(&mut self, input: FractionalInput<I>) -> bool {
    if let Some(coefficients) = (self.func)(input) {
      self.stack.insert(
        self.stack.len() - 1,
        PolynomialBasedAtInput {
          origin: input,
          coefficients,
        },
      );
      true
    } else {
      false
    }
  }
}


pub fn range_search<
  F: Fn(FractionalInput<I>) -> Option<P>,
  I: Integer,
  P,
  G: FnMut([&PolynomialBasedAtInput<P, FractionalInput<I>>; 2]) -> bool,
  H: FnMut(&PolynomialBasedAtInput<P, FractionalInput<I>>) -> bool,
  J: FnMut(&PolynomialBasedAtInput<P, FractionalInput<I>>) -> bool,
>(
  start_time: FractionalInput<I>,
  max_input_shift: u32,
  func: F,
  mut interval_filter: G,
  mut tail_filter: J,
  mut result_filter: H,
) -> Option<I> {
  let mut search = RangeSearch::new(func, start_time, max_input_shift)?;
  let mut records = Vec::new();
  
  fn print_records<T: RangeSearchRecorderHack<I, P>, I, P> (search: &T, records: &Vec<RangeSearchRecordHack>) {
    search.range_search_record_hack(|search| {
      serde_json::to_writer_pretty(::std::io::stderr(), records);
      use std::io::Write;
      write!(::std::io::stderr(), ",");
    });
  }

  let result_endpoint = 'a: loop {
    search.range_search_record_hack(|search| {
      records.push(RangeSearchRecordHack::Interval { endpoints: [search.latest_interval()[0].clone(), search.latest_interval()[1].clone()], bounds: coefficient_bounds_on_interval(search.latest_interval()) });

    });
    if (interval_filter)(search.latest_interval()) {
      if search.latest_interval()[0]
        .origin
        .raised_to_precision(max_input_shift)
        .unwrap()
        .numerator
        .saturating_add(One::one())
        == search.latest_interval()[1]
          .origin
          .raised_to_precision(max_input_shift)
          .unwrap()
          .numerator
      {
        if (result_filter)(search.latest_interval()[0]) {
          break 'a Some(search.latest_interval()[0]);
        }
        search.skip_latest();
      } else {
        search.split_latest();
      }
    } else {
      if (result_filter)(search.latest_interval()[0]) {
        break 'a Some(search.latest_interval()[0]);
      }
      if search.stack.len() == 2 {
        search.range_search_record_hack(|search| {
          records.push(RangeSearchRecordHack::ToEnd { endpoint: search.latest_interval()[1].clone(), bounds: coefficient_bounds_on_tail(&search.latest_interval()[1].coefficients) });
        });
        if !(tail_filter)(&search.latest_interval()[1]) {
          break 'a None;
        }
      }
      search.skip_latest();
    }
    if search.reached_overflow() {
      break 'a None;
    }
  };
  
  print_records(&search, &records);
  return result_endpoint.map(|endpoint| endpoint
              .origin
              .raised_to_precision(max_input_shift)
              .unwrap()
              .numerator
          );
}


//Note: currently, this function is strict (always find the exact time the max goes below the threshold). With a certain amount of error when the value is very close to the threshold, this could force searching every time unit. TODO: fix this by rigorously limiting the error and allowing that much leeway
/// Returns a time where the polynomial output is definitely less than permit_threshold, such that there is no EARLIER output less than require_threshold. (Or returns None if it encounters overflow before any output less than require_threshold.) With only approximate polynomial evaluation, for these conditions to be theoretically meetable, we must have permit_threshold >= require_threshold + 2. (Imagine that we have permit_threshold = 5, require_threshold = 4. The polynomial may output the range [3, 5]. We wouldn't be permitted to return that time because the true value may be 5, which is not less than permit_threshold and therefore not permitted. But we wouldn't be able to pass by that time because the true value could be 3, which is less than require_threshold.) For EFFICIENCY, we need permit_threshold >= require_threshold + 3, because there's an extra 1 of error in computing bounds on an interval. (Imagine that we have permit_threshold = 5, require_threshold = 3. The polynomial may output the range [3, 5] for a long interval. But the interval might report a lower bound of 2, meaning the algorithm doesn't know it can skip that interval. Theoretically, this might lead the algorithm to explore every individual time within a long interval.)


/*
fn next_time_in_bounds_2d <T: Polynomial, Input> (polynomials: [T;2], start_time: Input, min: [Coefficient;2], max: Coefficient)->Option <Time> {
  let search: ThresholdSearch = ([min, max+1]);
  while a time when the answer is not yet computed comes before the first time when the answer is known to be within bounds {
    (search with the earliest time not yet computed when the answer is also not yet computed).refine_first_bracketed_root(range of times where it matters);
  }
  search.
}*/



#[cfg(test)]
mod tests {

  use super::*;
  use num::{BigInt, BigRational, One};
  use proptest::prelude::*;

  fn naive_perfect_evaluate<Coefficient: Integer>(
    coefficients: &[Coefficient],
    input: BigRational,
  ) -> BigRational
  where
    BigInt: From<Coefficient>,
  {
    let mut result = BigRational::zero();
    for (exponent, coefficient) in coefficients.iter().enumerate() {
      let mut term = BigRational::from(BigInt::from(*coefficient));
      for _ in 0..exponent {
        term = term * &input;
      }
      result = result + term;
    }
    result
  }

  fn naive_factorial(value: usize) -> BigInt {
    let mut result = BigInt::one();
    for factor in 2..=value {
      result = result * BigInt::from(factor);
    }
    result
  }

  fn naive_binomial_coefficient(n: usize, k: usize) -> BigInt {
    naive_factorial(n) / (naive_factorial(k) * naive_factorial(n - k))
  }

  fn naive_perfect_nth_taylor_coefficient<Coefficient: Integer>(
    coefficients: &[Coefficient],
    input: BigRational,
    n: usize,
  ) -> BigRational
  where
    BigInt: From<Coefficient>,
  {
    let mut result = BigRational::zero();
    for (exponent, coefficient) in coefficients.iter().enumerate().skip(n) {
      let mut term = BigRational::from(BigInt::from(*coefficient));
      for _ in n..exponent {
        term = term * &input;
      }
      result = result + term * naive_binomial_coefficient(exponent, n);
    }
    result
  }

  fn precision_scale(precision_shift: i32) -> BigRational {
    if precision_shift > 0 {
      BigRational::new(
        BigInt::from_i64(1i64 << precision_shift).unwrap(),
        BigInt::one(),
      )
    } else {
      BigRational::new(
        BigInt::one(),
        BigInt::from_i64(1i64 << (-precision_shift)).unwrap(),
      )
    }
  }

  fn rational_input<T>(input: FractionalInput<T>) -> BigRational
  where
    BigInt: From<T>,
  {
    BigRational::new(
      BigInt::from(input.numerator),
      BigInt::from_i64(1i64 << input.shift).unwrap(),
    )
  }

  fn arbitrary_fractional_input() -> BoxedStrategy<FractionalInput<i64>> {
    (0u32..16)
      .prop_flat_map(|shift| ((-16i64 << shift..16i64 << shift), Just(shift)))
      .prop_map(|(numerator, shift)| FractionalInput { numerator, shift })
      .boxed()
  }

  macro_rules! test_polynomials {
  ($($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident,)*) => {
$(
  mod $name {
    use super::*;
    //use super::super::*;
    //use proptest::prelude::*;

    fn test_max_total_shift_generic<WorkingType: Integer+Signed+From<$integer>+TryInto<$integer>>() {
      let accumulated_error_shift = <[$integer; $coefficients] as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::accumulated_error_shift();
      let max_total_shift = <[$integer; $coefficients] as AllTaylorCoefficientsBoundsWithinHalf<WorkingType>>::max_total_shift();
      for input_shift in 1..$integer::total_bits() {
        for precision_shift in -16..max_total_shift -input_shift as i32 {
          let initial_shift = precision_shift + accumulated_error_shift as i32;
          let max_initial_intermediate = if initial_shift >= 0 {
            <WorkingType as From<$integer>>::from($integer::max_value()) << initial_shift as u32
          } else {
            shr_ceil (<WorkingType as From<$integer>>::from($integer::max_value()), (- initial_shift) as u32)
          };
          let max_input = WorkingType::one() << (input_shift - 1) as u32;
          assert!((max_initial_intermediate * <WorkingType as From<$integer>>::from($coefficients - 1)).checked_mul (&max_input).is_some());
        }
      }


    }

    #[test]
    fn test_max_total_shift() {
      test_max_total_shift_generic::<$integer>();
      test_max_total_shift_generic::<$double>();
    }

    proptest! {
      #[test]
      fn randomly_test_polynomial_translation_inverts (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16) {
        let translated = coefficients.all_taylor_coefficients (input);
        prop_assume! (translated.is_some());
        let translated = translated.unwrap();
        let translated_back = translated.all_taylor_coefficients (-input);
        prop_assert! (translated_back.is_some(), "we know that the original value was in bounds, so translating back should return some");
        prop_assert_eq! (coefficients, translated_back.unwrap());

      }

      #[test]
      fn randomly_test_taylor_coefficients_evaluates (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16) {
        let translated = coefficients.all_taylor_coefficients (input);
        prop_assume! (translated.is_some());
        let translated = translated.unwrap();
        let evaluated = naive_perfect_evaluate (&coefficients, BigRational::from(BigInt::from(input)));
        prop_assert_eq! (BigRational::from(BigInt::from(translated[0])), evaluated);
        for which in 0..$coefficients {
          prop_assert_eq! (BigRational::from(BigInt::from(translated[which])), naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::from(BigInt::from(input)), which), "Incorrect {}th taylor coefficient ", which);
        }
      }

      #[test]
      fn randomly_test_taylor_coefficients_bounds_correct (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), precision_shift in -10i32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input.numerator, input.shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), which) * precision_scale(precision_shift);

          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) <= exact, "Incorrect {}th taylor coefficient lower bound: {} > {:?}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) >= exact, "Incorrect {}th taylor coefficient upper bound: {} < {:?}", which, bounds[which][1], exact);
        }
      }

      #[test]
      fn randomly_test_taylor_coefficients_bounds_close(coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), precision_shift in -10i32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input.numerator, input.shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        let leeway = BigRational::new(BigInt::from(3i32), BigInt::from(2i32));
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), which) * precision_scale(precision_shift);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) > &exact - &leeway, "Too loose {}th taylor coefficient lower bound: {} + 1.5 <= {:?}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) < &exact + &leeway, "Too loose {}th taylor coefficient upper bound: {} - 1.5 >= {:?}", which, bounds[which][1], exact);
          prop_assert! (bounds[which][1] <= bounds[which][0].saturating_add(2), "{}th taylor coefficient bounds are too far from each other (note: this should be impossible if the other conditions are met): {} > {} + 2", which, bounds[which][1], bounds[which][0]);
        }
      }


      #[test]
      fn randomly_test_next_time_definitely_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16) {
        let require_threshold = permit_threshold - threshold_difference;
        let time = coefficients.next_time_value_passes (input, input.shift, LessThanFilter::new(permit_threshold, require_threshold));
        prop_assume! (time .is_some());
        let time = time.unwrap();

        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(time, input.shift)), 0);
        //if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input.shift, 0u32) {
        prop_assert!(exact < BigRational::from(BigInt::from(permit_threshold)));
      }

      #[test]
      fn randomly_test_next_time_definitely_lt_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16, test_frac in 0f64..1f64) {
        let require_threshold = permit_threshold - threshold_difference;
        let time = coefficients.next_time_value_passes (input, input.shift, LessThanFilter::new(permit_threshold, require_threshold));
        let last_not_lt = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input.numerator);
            k-1
          },
        };
        prop_assume!(last_not_lt >= input.numerator);
        let test_time = input.numerator + ((last_not_lt.saturating_sub(input.numerator)) as f64 * test_frac).floor() as $double;

        let exact_require_threshold = BigRational::from(BigInt::from(require_threshold));
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), 0);
        prop_assert!(exact >= exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(last_not_lt, input.shift)), 0);
        prop_assert!(exact >= exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(test_time, input.shift)), 0);
        prop_assert!(exact >= exact_require_threshold);
      }

      #[test]
      fn randomly_test_next_time_definitely_ge_is_ge(coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16) {
        let require_threshold = permit_threshold + threshold_difference;
        let time = coefficients.next_time_value_passes (input, input.shift, GreaterThanEqualToFilter::new(permit_threshold, require_threshold));
        prop_assume! (time .is_some());
        let time = time.unwrap();

        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(time, input.shift)), 0);
        //if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input.shift, 0u32) {
        prop_assert!(exact >= BigRational::from(BigInt::from(permit_threshold)));
      }

      #[test]
      fn randomly_test_next_time_definitely_ge_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16, test_frac in 0f64..1f64) {
        let require_threshold = permit_threshold + threshold_difference;
        let time = coefficients.next_time_value_passes (input, input.shift, GreaterThanEqualToFilter::new(permit_threshold, require_threshold));
        let last_not_ge = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input.numerator);
            k-1
          },
        };
        prop_assume!(last_not_ge >= input.numerator);
        let test_time = input.numerator + ((last_not_ge.saturating_sub(input.numerator)) as f64 * test_frac).floor() as $double;

        let exact_require_threshold = BigRational::from(BigInt::from(require_threshold));
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), 0);
        prop_assert!(exact < exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(last_not_ge, input.shift)), 0);
        prop_assert!(exact < exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(test_time, input.shift)), 0);
        prop_assert!(exact < exact_require_threshold);
      }
    }
  }
)*

  }
}

  test_polynomials!(
    1,
    i32,
    i64,
    uniform1,
    polynomial_tests_1,
    2,
    i32,
    i64,
    uniform2,
    polynomial_tests_2,
    3,
    i32,
    i64,
    uniform3,
    polynomial_tests_3,
    4,
    i32,
    i64,
    uniform4,
    polynomial_tests_4,
    5,
    i32,
    i64,
    uniform5,
    polynomial_tests_5,
  );

  /*#[test]
  fn print_record_data() {
     use std::io::Write;
     let mut data: Vec<Vec<RangeSearchRecordHack>> = serde_json::from_reader (::std::fs::File::open("/n/pfft/range_search_data.json").unwrap()).unwrap();
     data.sort_by_key(|datum|datum.len());
     let percentiles:Vec<usize> = (0..=100).map(|percent| data[percent*(data.len()-1)/100].len()).collect();
     write!(::std::io::stderr(), "Number of searches: {:?}", data.len());
     write!(::std::io::stderr(), "Sizes: {:?}", percentiles);
     write!(::std::io::stderr(), "Worst: {}", serde_json::to_string_pretty(data.last().unwrap()).unwrap());
  }*/
}
