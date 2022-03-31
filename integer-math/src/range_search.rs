use arrayvec::{self, ArrayVec};
use guard::guard;
use num::{CheckedAdd, FromPrimitive, One};
#[allow(unused_imports)]
use serde::Serialize;
use std::cmp::{max, min};

use super::*;
use crate::polynomial2::{assert_input_within_half, Polynomial, PolynomialBasedAtInput};

pub fn coefficient_bounds_on_integer_interval<
  Coefficient: DoubleSizedSignedInteger,
  const COEFFICIENTS: usize,
>(
  endpoints: [&Polynomial<Coefficient, COEFFICIENTS>; 2],
  duration: DoubleSized<Coefficient>,
) -> [[Coefficient; 2]; COEFFICIENTS] {
  let mut result = [[Zero::zero(); 2]; COEFFICIENTS];
  assert!(duration >= Zero::zero());
  let double_sized_max_bounds = [
    <DoubleSized<Coefficient>>::from(Coefficient::min_value()),
    <DoubleSized<Coefficient>>::from(Coefficient::max_value()),
  ];
  let mut movement_range_from_previous: [DoubleSized<Coefficient>; 2] =
    [Zero::zero(), Zero::zero()];
  for exponent in (0..COEFFICIENTS as u32).rev() {
    let end_values: [DoubleSized<Coefficient>; 2] =
      endpoints.map(|endpoint| endpoint[exponent as usize].into());

    let bounds: [DoubleSized<Coefficient>; 2] = if movement_range_from_previous[0] >= Zero::zero() {
      [end_values[0], end_values[1]]
    } else if movement_range_from_previous[1] <= Zero::zero() {
      [end_values[1], end_values[0]]
    } else {
      // TODO: these bounds can be tightened by analyzing the parallelogram
      // but it might overflow
      // did the algebra as (v1*s1 + v0*-s0 + (t1-t0)*s1*-s0)/(s1+ -s0) = max_value
      // let slope_product = previous_derivative_range[0]*previous_derivative_range[1];

      // If an earlier derivative overflowed, assume it's arbitrarily high
      let [left_min, right_max] = if movement_range_from_previous[0] <= double_sized_max_bounds[0] {
        double_sized_max_bounds
      } else {
        [
          end_values[0] + movement_range_from_previous[0],
          end_values[1] - movement_range_from_previous[0],
        ]
      };
      let [right_min, left_max] = if movement_range_from_previous[1] >= double_sized_max_bounds[1] {
        double_sized_max_bounds
      } else {
        [
          end_values[1] - movement_range_from_previous[1],
          end_values[0] + movement_range_from_previous[1],
        ]
      };

      [
        max(double_sized_max_bounds[0], max(left_min, right_min)),
        min(double_sized_max_bounds[1], min(left_max, right_max)),
      ]
    };

    result[exponent as usize] = bounds.map(|a| a.try_into().ok().unwrap());
    let factor = duration * <DoubleSized<Coefficient>>::from_u32(exponent).unwrap();
    movement_range_from_previous = bounds.map(|a| a * factor);
  }
  result
}

pub const STANDARD_PRECISION_SHIFT: u32 = 2;

// TODO: define guarantees of the function
pub fn value_bounds_on_negative_power_of_2_interval<
  S: ShiftSize,
  Coefficient: DoubleSizedSignedInteger,
  const COEFFICIENTS: usize,
>(
  endpoints: [&[[DoubleSized<Coefficient>; 2]; COEFFICIENTS]; 2],
  duration_shift: S,
) -> [Coefficient; 2] {
  let duration_shift = duration_shift.into();
  let max_magnitude = Shl::<u32>::shl(
    <DoubleSized<Coefficient>>::one(),
    Coefficient::nonsign_bits() + STANDARD_PRECISION_SHIFT,
  );
  let double_sized_max_bounds = [-max_magnitude, max_magnitude];
  let mut movement_range_from_previous: [DoubleSized<Coefficient>; 2] =
    [Zero::zero(), Zero::zero()];
  let duration_round_up = Shl::<u32>::shl(<DoubleSized<Coefficient>>::one(), duration_shift)
    - <DoubleSized<Coefficient>>::one();
  for exponent in (0..COEFFICIENTS as u32).rev() {
    let end_bounds = endpoints.map(|endpoint| endpoint[exponent as usize]);

    let bounds: [DoubleSized<Coefficient>; 2] = if movement_range_from_previous[0] >= Zero::zero() {
      [end_bounds[0][0], end_bounds[1][1]]
    } else if movement_range_from_previous[1] <= Zero::zero() {
      [end_bounds[1][0], end_bounds[0][1]]
    } else {
      // TODO: try tightening these bounds by analyzing the parallelogram
      // but it might overflow
      // and it might be a pessimization anyway
      // did the algebra as (v1*s1 + v0*-s0 + (t1-t0)*s1*-s0)/(s1+ -s0) = max_value
      // let slope_product = previous_derivative_range[0]*previous_derivative_range[1];

      // If an earlier derivative overflowed, assume it's arbitrarily high
      let [left_min, right_max] = if movement_range_from_previous[0] <= double_sized_max_bounds[0] {
        double_sized_max_bounds
      } else {
        [
          end_bounds[0][0] + movement_range_from_previous[0],
          end_bounds[1][1] - movement_range_from_previous[0],
        ]
      };
      let [right_min, left_max] = if movement_range_from_previous[1] >= double_sized_max_bounds[1] {
        double_sized_max_bounds
      } else {
        [
          end_bounds[1][0] - movement_range_from_previous[1],
          end_bounds[0][1] + movement_range_from_previous[1],
        ]
      };

      [
        max(double_sized_max_bounds[0], max(left_min, right_min)),
        min(double_sized_max_bounds[1], min(left_max, right_max)),
      ]
    };

    if exponent == 0 {
      return bounds.map(saturating_downcast);
    }
    let factor = <DoubleSized<Coefficient>>::from_u32(exponent).unwrap();
    movement_range_from_previous = bounds.map(|a| a * factor);
    movement_range_from_previous[0] = if bounds[0] <= double_sized_max_bounds[0] {
      double_sized_max_bounds[0]
    } else {
      Shr::<u32>::shr(bounds[0] * factor, duration_shift)
    };
    movement_range_from_previous[1] = if bounds[1] >= double_sized_max_bounds[1] {
      double_sized_max_bounds[1]
    } else {
      Shr::<u32>::shr(bounds[1] * factor + duration_round_up, duration_shift)
    };
  }
  unreachable!()
}

/**

Computes a lower/upper bound of the polynomial (`coefficients` * 2^precision_shift) within a fractional input range.

Unfortunately, this implementation turns out to be a lot looser than the one above (because it never computes the exact coefficients at the endpoints), despite being faster. As a result, it caused a ~26000% slowdown in the bouncy_circles benchmarks.

Finding the *least* upper bound is complex: you have to find:

max_{x3 \in [x1, x2]} (((a*x3) + b)*x3 + c)*x3 +...

which is as difficult as polynomial solving.

But you can find a *decent* upper bound by relaxing the above formula to:

max_{x3,x4,x5,... \in [x1, x2]} (((a*x3) + b)*x4 + c)*x5 +...

which is much simpler to compute. If 0 <= x1 <= x2, you just check if each intermediate term is positive; if it's positive, you choose xn = x2, and if it's negative, you choose xn = x1. (With x1 <= x2 <= 0, you alternate the senses.)

In addition to this looseness, there is an extra looseness up to <2 due to rounding error.

# Panics

The inputs must either be in the range [-0.5, 0] or the range [0, 0.5]. Otherwise, this function will panic.

`input_shift + precision_shift` must not exceed `WorkingType::nonsign_bits() - Coefficient::nonsign_bits() + 1`. If they are, this function will panic.

However, this function never fails due to overflow.
*/
pub fn bound_on_interval_within_half<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient>,
  S1: ShiftSize,
  S2: ShiftSize,
  const COEFFICIENTS: usize,
  const UPPER: bool,
>(
  coefficients: &[Coefficient; COEFFICIENTS],
  inputs: [WorkingType; 2],
  input_shift: S1,
  precision_shift: S2,
) -> WorkingType {
  assert!(
    input_shift.into() + precision_shift.into() <= WorkingType::nonsign_bits() - Coefficient::nonsign_bits() + 1,
    "bound_on_interval_within_half `input_shift ({}) + precision_shift ({})` must not exceed `WorkingType::nonsign_bits()({}) - Coefficient::nonsign_bits() ({}) + 1`",
    input_shift.into(), precision_shift.into(), WorkingType::nonsign_bits() , Coefficient::nonsign_bits(),
  );
  for &input in &inputs {
    assert_input_within_half(input, input_shift, "bound_on_interval_within_half");
  }
  assert!(
    inputs[1] >= inputs[0],
    "bound_on_interval_within_half inputs must be in-order"
  );
  assert!(
    inputs[1] <= Zero::zero() || inputs[0] >= Zero::zero(),
    "bound_on_interval_within_half inputs must not be opposite signs"
  );

  let mut inputs = inputs;
  let inputs_negative = inputs[0] < Zero::zero();
  if inputs_negative {
    inputs = [-inputs[1], -inputs[0]];
  }

  guard!(let Some((&highest, remaining)) = coefficients.split_last() else { return Zero::zero(); });
  let mut intermediate: WorkingType = highest.into();
  // if getting upper bound instead of lower, flip everything;
  // also, if inputs negative, flip odd-numbered coefficients
  if UPPER != (inputs_negative && ((COEFFICIENTS - 1) & 1) != 0) {
    intermediate = -intermediate;
  }
  intermediate <<= precision_shift.into() + 1;
  for (exponent, &coefficient) in remaining.iter().enumerate().rev() {
    // minimize result (assuming lower bound, inputs >= 0);
    // negative numbers should be multiplied as much as possible,
    // positive numbers as little as possible
    let best_input: WorkingType = if intermediate <= Zero::zero() {
      inputs[1]
    } else {
      inputs[0]
    };
    let mut coefficient: WorkingType = coefficient.into();
    // if getting upper bound instead of lower, flip everything;
    // also, if inputs negative, flip odd-numbered coefficients
    if UPPER != (inputs_negative && (exponent & 1) != 0) {
      coefficient = -coefficient;
    }
    coefficient <<= precision_shift.into() + 1;
    intermediate = Shr::<u32>::shr(intermediate * best_input, input_shift.into()) + coefficient;
  }
  intermediate >>= 1;
  // if getting upper bound instead of lower, we now have "lower bound of the negated
  // polynomial", so flip it
  if UPPER {
    intermediate = -intermediate;
  }
  intermediate
}
pub fn upper_bound_on_interval_within_half<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient>,
  S1: ShiftSize,
  S2: ShiftSize,
  const COEFFICIENTS: usize,
>(
  coefficients: &[Coefficient; COEFFICIENTS],
  inputs: [WorkingType; 2],
  input_shift: S1,
  precision_shift: S2,
) -> WorkingType {
  bound_on_interval_within_half::<_, _, _, _, COEFFICIENTS, true>(
    coefficients,
    inputs,
    input_shift,
    precision_shift,
  )
}
pub fn lower_bound_on_interval_within_half<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient>,
  S1: ShiftSize,
  S2: ShiftSize,
  const COEFFICIENTS: usize,
>(
  coefficients: &[Coefficient; COEFFICIENTS],
  inputs: [WorkingType; 2],
  input_shift: S1,
  precision_shift: S2,
) -> WorkingType {
  bound_on_interval_within_half::<_, _, _, _, COEFFICIENTS, false>(
    coefficients,
    inputs,
    input_shift,
    precision_shift,
  )
}
pub fn bounds_on_interval_within_half<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient>,
  S1: ShiftSize,
  S2: ShiftSize,
  const COEFFICIENTS: usize,
>(
  coefficients: &[Coefficient; COEFFICIENTS],
  inputs: [WorkingType; 2],
  input_shift: S1,
  precision_shift: S2,
) -> [WorkingType; 2] {
  [
    lower_bound_on_interval_within_half(coefficients, inputs, input_shift, precision_shift),
    upper_bound_on_interval_within_half(coefficients, inputs, input_shift, precision_shift),
  ]
}

pub fn coefficient_bounds_on_tail<Coefficient: Integer + Signed, const COEFFICIENTS: usize>(
  endpoint: &Polynomial<Coefficient, COEFFICIENTS>,
) -> [[Coefficient; 2]; COEFFICIENTS] {
  let mut result = [[Zero::zero(); 2]; COEFFICIENTS];
  let mut previous_derivative_range: [Coefficient; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..COEFFICIENTS).rev() {
    // Note: I made a more sophisticated version of this once,
    // (commit bdb48cbbf7821b8006b665732b0b98d932cbc738)
    // which did some multiplication and division to give tighter bounds in certain cases,
    // but turned out to be a slight pessimization in the bouncy_circles benchmark
    let mut bounds = [endpoint[exponent]; 2];
    if previous_derivative_range[0] < Zero::zero() {
      bounds[0] = Coefficient::min_value();
    }
    if previous_derivative_range[1] > Zero::zero() {
      bounds[1] = Coefficient::max_value();
    }
    result[exponent] = bounds;
    previous_derivative_range = bounds;
  }
  result
}

//pub struct
/*
type HackP = ValueWithPrecision<[[i64; 2]; 5]>;

#[derive(Copy, Clone, Serialize, Deserialize, Debug)]
pub enum RangeSearchRecordHack {
  Interval { endpoints: [PolynomialBasedAtInput<HackP, FractionalInput<i64>>; 2], bounds: [[i64;2];5] },
  ToEnd { endpoint: PolynomialBasedAtInput<HackP, FractionalInput<i64>>, bounds: [[i64;2];5] },
}

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
  #[allow (unused_variables)]
  fn range_search_record_hack<G: FnOnce(&RangeSearch<Self::Func, i64, HackP>)> (&self, g:G) {
    //(g)(self);
  }
}*/

pub trait RangeSearch {
  type Input: Integer;
  type IntegerValue: Debug;
  type FractionalValue: Debug + Default;
  fn value_at_integer(&self, input: Self::Input) -> Option<Self::IntegerValue>;
  fn value_at_fractional(
    &self,
    nearest_integer_value: &Self::IntegerValue,
    relative_input: Self::Input,
  ) -> Self::FractionalValue;
  fn integer_to_fractional(&self, value: &Self::IntegerValue) -> Self::FractionalValue {
    self.value_at_fractional(value, Zero::zero())
  }
  fn integer_interval_filter(
    &self,
    endpoints: [&Self::IntegerValue; 2],
    duration: Self::Input,
  ) -> bool;
  fn fractional_interval_filter(
    &self,
    endpoints: [&Self::FractionalValue; 2],
    duration_shift: impl ShiftSize,
  ) -> bool;
  fn tail_filter(&self, endpoint: &Self::IntegerValue) -> bool;
  fn fractional_result_filter(&self, value: &Self::FractionalValue) -> bool;
  fn integer_result_filter(&self, value: &Self::IntegerValue) -> bool {
    self.fractional_result_filter(&self.integer_to_fractional(value))
  }
}

/*

*/

pub enum IncrementalRangeSearchState<S: RangeSearch> {
  Completed(Option<S::Input>),
  SearchingFractional,
}

pub struct RangeSearchRunner<S: RangeSearch> {
  search: S,
  max_input_shift: u32,
  start_input: S::Input,
  integer_stack: ArrayVec<PolynomialBasedAtInput<S::IntegerValue, S::Input>, 64>,
  next_jump: S::Input,
}

impl<S: RangeSearch> RangeSearchRunner<S> {
  pub fn run(search: S, start_input: S::Input, input_shift: impl ShiftSize) -> Option<S::Input> {
    let start_integer = shr_floor(start_input, input_shift);
    let start_value =
      PolynomialBasedAtInput::new(search.value_at_integer(start_integer)?, start_integer);
    if !search.tail_filter(&start_value.coefficients) {
      return None;
    }
    let next_integer = start_integer + S::Input::one();
    let next_value = PolynomialBasedAtInput::new(
      search.value_at_integer(next_integer)?,
      start_integer + S::Input::one(),
    );

    /*let start_diff = start_input - (start_integer<<input_shift);
    let next_diff = start_input - (next_integer<<input_shift);
    let closer = if -next_diff < start_diff {
      search.value_at_fractional (next_value, next_diff)
    } else {
      search.value_at_fractional (start_value, start_diff)
    };
    if search.result_filter (&value_at_fractional (&closer)) { return start_input; }*/

    let mut runner = RangeSearchRunner {
      search,
      max_input_shift: input_shift.into(),
      start_input,
      integer_stack: ArrayVec::new(),
      next_jump: One::one(),
    };
    runner.integer_stack.push(next_value);
    runner.integer_stack.push(start_value);

    runner.search_integers()
  }
  #[inline]
  pub fn latest_integer_interval(&self) -> [&PolynomialBasedAtInput<S::IntegerValue, S::Input>; 2] {
    [
      &self.integer_stack[self.integer_stack.len() - 1],
      &self.integer_stack[self.integer_stack.len() - 2],
    ]
  }
  pub fn search_integers(&mut self) -> Option<S::Input> {
    loop {
      // note: can use plain subtraction because the duration is never overflowing
      'searching_subintervals: while self.search.integer_interval_filter(
        self.latest_integer_interval().map(|a| &a.coefficients),
        self.latest_integer_interval()[1].origin - self.latest_integer_interval()[0].origin,
      ) {
        let scaled_start_origin = Shl::<u32>::shl(
          self.latest_integer_interval()[0].origin,
          self.max_input_shift,
        );
        if scaled_start_origin >= self.start_input
          && self
            .search
            .integer_result_filter(&self.latest_integer_interval()[0].coefficients)
        {
          //eprintln!("int {:?}", (&self.latest_integer_interval()[0]));
          return Some(scaled_start_origin);
        }
        let mut end_inputs = self.latest_integer_interval().map(|a| a.origin);

        'finding_midpoint: loop {
          if end_inputs[0] + S::Input::one() == end_inputs[1] {
            if let Some(result) = self.search_fractional() {
              return Some(result);
            }
            break 'searching_subintervals;
          }
          let new_endpoint = mean_floor(end_inputs[0], end_inputs[1]);
          if let Some(new_value) = self.search.value_at_integer(new_endpoint) {
            self.integer_stack.insert(
              self.integer_stack.len() - 1,
              PolynomialBasedAtInput {
                origin: new_endpoint,
                coefficients: new_value,
              },
            );
            break 'finding_midpoint;
          }
          end_inputs[1] = new_endpoint;
        }
      }
      self.integer_stack.pop();
      if self.integer_stack.len() < 2 {
        if !self
          .search
          .tail_filter(&self.integer_stack.last().unwrap().coefficients)
        {
          return None;
        }
        let last_endpoint_input = self.integer_stack.last().unwrap().origin;
        'finding_next: loop {
          if let Some(new_endpoint) = last_endpoint_input.checked_add(&self.next_jump) {
            if let Some(new_value) = self.search.value_at_integer(new_endpoint) {
              self.integer_stack.insert(
                self.integer_stack.len() - 1,
                PolynomialBasedAtInput {
                  origin: new_endpoint,
                  coefficients: new_value,
                },
              );
              self.next_jump = overflow_checked_shl(self.next_jump, 1u32).unwrap_or(self.next_jump);
              break 'finding_next;
            }
          }
          self.next_jump >>= 1u32;
          if self.next_jump == Zero::zero() {
            return None;
          }
        }
      }
    }
  }

  pub fn search_fractional(&mut self) -> Option<S::Input> {
    if self.max_input_shift < 1 {
      return None;
    }
    let one = Shl::<u32>::shl(S::Input::one(), self.max_input_shift);
    let beginning_of_this_unit_interval = Shl::<u32>::shl(
      self.latest_integer_interval()[0].origin,
      self.max_input_shift,
    );
    let relative_start_input = self.start_input - beginning_of_this_unit_interval;
    let half = Shr::<u32>::shr(one, 1);

    let mut current_input_shift = 1;
    let mut data: [PolynomialBasedAtInput<S::FractionalValue, S::Input>; 3] = Default::default();

    data[0] = PolynomialBasedAtInput {
      origin: S::Input::zero(),
      coefficients: self
        .search
        .integer_to_fractional(&self.latest_integer_interval()[0].coefficients),
    };
    data[1] = PolynomialBasedAtInput {
      origin: half,
      coefficients: self
        .search
        .value_at_fractional(&self.latest_integer_interval()[0].coefficients, half),
    };

    let mut left_end = 0;
    let mut right_end = 1;
    let mut next_right_end = 2;
    let mut which_half_bits: u64 = 0;

    loop {
      //eprintln!("top {:?}", (current_input_shift));
      'searching_subintervals: while data[right_end].origin > relative_start_input
        && self.search.fractional_interval_filter(
          [&data[left_end].coefficients, &data[right_end].coefficients],
          current_input_shift,
        )
      {
        //eprintln!("sub {:?}", (current_input_shift, &data[left_end].origin, &data[right_end].origin, &data[next_right_end].origin));
        if data[left_end].origin >= relative_start_input
          && self
            .search
            .fractional_result_filter(&data[left_end].coefficients)
        {
          //eprintln!("frac {:?}", (&data[left_end].origin));
          return Some(beginning_of_this_unit_interval + data[left_end].origin);
        }
        if data[left_end].origin + S::Input::one() == data[right_end].origin {
          break 'searching_subintervals;
        }
        current_input_shift += 1;
        let new_endpoint = data[left_end].origin + Shr::<u32>::shr(one, current_input_shift);
        debug_assert_eq!(
          new_endpoint - data[left_end].origin,
          data[right_end].origin - new_endpoint
        );
        //eprintln!("sub {:?}", (end_inputs, new_endpoint, current_input_shift, self.fractional_stack.len()));
        let new_value = if new_endpoint > half {
          self.search.value_at_fractional(
            &self.latest_integer_interval()[1].coefficients,
            new_endpoint - one,
          )
        } else {
          self.search.value_at_fractional(
            &self.latest_integer_interval()[0].coefficients,
            new_endpoint,
          )
        };
        mem::swap(&mut right_end, &mut next_right_end);
        data[right_end] = PolynomialBasedAtInput {
          origin: new_endpoint,
          coefficients: new_value,
        };
      }

      let mut next_valid = current_input_shift > 1;
      while which_half_bits & (1 << current_input_shift) != 0 {
        which_half_bits &= !(1 << current_input_shift);
        current_input_shift -= 1;
        next_valid = false;
      }
      if current_input_shift < 1 {
        return None;
      }
      which_half_bits |= 1 << current_input_shift;
      mem::swap(&mut left_end, &mut right_end);
      mem::swap(&mut right_end, &mut next_right_end);
      if !next_valid {
        let new_endpoint = data[left_end].origin + Shr::<u32>::shr(one, current_input_shift);
        let new_value = if new_endpoint > half {
          self.search.value_at_fractional(
            &self.latest_integer_interval()[1].coefficients,
            new_endpoint - one,
          )
        } else {
          self.search.value_at_fractional(
            &self.latest_integer_interval()[0].coefficients,
            new_endpoint,
          )
        };
        data[right_end] = PolynomialBasedAtInput {
          origin: new_endpoint,
          coefficients: new_value,
        };
      }
    }
  }
}
