use array::{Array, ReplaceItemType};
use array_ext::{Array as ArrayExtArray, *};
use arrayvec::{self, ArrayVec};
use num::{
  Bounded, CheckedAdd, CheckedSub, FromPrimitive, One,
  Signed,
};
use std::cmp::{max, min};
#[allow(unused_imports)]
use serde::Serialize;

use super::*;
use polynomial2::{PolynomialBasedAtInput,ValueWithPrecision,FractionalInput,};


pub fn coefficient_bounds_on_integer_interval<
  P: Array + arrayvec::Array<Item = Coefficient> + ReplaceItemType<[Coefficient; 2]>,
  Coefficient: DoubleSizedSignedInteger,
  Input: Integer + Signed + TryInto<Coefficient> + Into<DoubleSized<Coefficient>>,
>(
  endpoints: [&PolynomialBasedAtInput<P, Input>; 2],
) -> <P as ReplaceItemType<[Coefficient; 2]>>::Type {
  let mut result: <P as ReplaceItemType<[Coefficient; 2]>>::Type = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  let duration: DoubleSized<Coefficient> = match endpoints[1].origin.checked_sub(&endpoints[0].origin).map(Into::into).filter(|a| *a * <DoubleSized<Coefficient>>::from_u32(result.len()as u32-1).unwrap() < <DoubleSized<Coefficient>>::from(Coefficient::max_value())) {
    Some(a) => a,
    None => {
      // if the duration overflows, give up on the missing the range at all
      result = array_ext::Array::from_fn(|_| [Coefficient::min_value(), Coefficient::max_value()]);
      return result;
    }
  };
  assert!(duration >= Zero::zero());
  let double_sized_max_bounds = [<DoubleSized<Coefficient>>::from(Coefficient::min_value()), <DoubleSized<Coefficient>>::from(Coefficient::max_value())];
  let mut movement_range_from_previous: [DoubleSized<Coefficient>; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len() as u32).rev() {
    let end_values = endpoints.map(|endpoint|
      <DoubleSized<Coefficient> as From<Coefficient>>::from(endpoint.coefficients.as_slice()[exponent as usize])
    );

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

      [max(double_sized_max_bounds[0], max(left_min, right_min)), min(double_sized_max_bounds[1], min(left_max, right_max))]
    };

    result.as_mut_slice()[exponent as usize] = bounds.map(|a|a.try_into().ok().unwrap());
    let factor = duration * <DoubleSized<Coefficient>>::from_u32(exponent).unwrap();
    movement_range_from_previous = bounds.map(|a| a * factor);
  }
  result
}

const STANDARD_PRECISION_SHIFT: u32 = 2;

pub fn coefficient_bounds_on_negative_power_of_2_interval<
  P: Array + arrayvec::Array<Item = [DoubleSized<Coefficient>; 2]> + ReplaceItemType<[Coefficient; 2]>,
  Coefficient: DoubleSizedSignedInteger,
  Input: Integer + Signed + TryInto<Coefficient>,
>(
  endpoints: [&P; 2], duration_shift: u32
) -> <P as ReplaceItemType<[Coefficient; 2]>>::Type {
  let mut result: <P as ReplaceItemType<[Coefficient; 2]>>::Type = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  let max_magnitude = Shl::<u32>::shl(<DoubleSized<Coefficient>>::one(), Coefficient::nonsign_bits() + STANDARD_PRECISION_SHIFT);
  let double_sized_max_bounds = [-max_magnitude, max_magnitude];
  let mut movement_range_from_previous: [DoubleSized<Coefficient>; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len() as u32).rev() {
    let end_bounds = endpoints.map(|endpoint|
      endpoint.as_slice()[exponent as usize]
    );

    let bounds: [DoubleSized<Coefficient>; 2] = if movement_range_from_previous[0] >= Zero::zero() {
      [end_bounds[0][0], end_bounds[1][1]]
    } else if movement_range_from_previous[1] <= Zero::zero() {
      [end_bounds[1][0], end_bounds[0][1]]
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

      [max(double_sized_max_bounds[0], max(left_min, right_min)), min(double_sized_max_bounds[1], min(left_max, right_max))]
    };

    result.as_mut_slice()[exponent as usize][0] = saturating_downcast(shr_floor(bounds[0], STANDARD_PRECISION_SHIFT));
    result.as_mut_slice()[exponent as usize][1] = saturating_downcast(shr_floor(bounds[1], STANDARD_PRECISION_SHIFT));
    let factor = <DoubleSized<Coefficient>>::from_u32(exponent).unwrap();
    movement_range_from_previous = bounds.map(|a| a * factor);
    movement_range_from_previous[0] = if bounds[0] <= double_sized_max_bounds[0] { double_sized_max_bounds[0] } else {shr_floor(bounds[0] * factor, duration_shift)};
    movement_range_from_previous[1] = if bounds[1] >= double_sized_max_bounds[1] { double_sized_max_bounds[1] } else {shr_ceil(bounds[1] * factor, duration_shift)};
  }
  result
}

pub fn coefficient_bounds_on_tail<
  P: Array + arrayvec::Array<Item = Coefficient> + ReplaceItemType<[Coefficient; 2]>,
  Coefficient: Integer + Signed,
>(
  endpoint: &P,
) -> <P as ReplaceItemType<[Coefficient; 2]>>::Type {
  let mut result: <P as ReplaceItemType<[Coefficient; 2]>>::Type = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  let mut previous_derivative_range: [Coefficient; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len()).rev() {
    let mut bounds = [endpoint.as_slice()[exponent]; 2];
    if previous_derivative_range[0] < Zero::zero() {
      bounds[0] = Coefficient::min_value();
    }
    if previous_derivative_range[1] > Zero::zero() {
      bounds[1] = Coefficient::max_value();
    }
    result.as_mut_slice()[exponent] = bounds;
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
  type IntegerValue;
  type FractionalValue;
  fn value_at_integer (&self, input: Self::Input)->Option<Self::IntegerValue>;
  fn value_at_fractional (&self, nearest_integer_value: &Self::IntegerValue, relative_input: Self::Input)->Self::FractionalValue;
  fn integer_to_fractional (&self, value: &Self::IntegerValue)->Self::FractionalValue {
    self.value_at_fractional (value, Zero::zero())
  }
  fn integer_interval_filter (&self, endpoints: [&Self::IntegerValue; 2], duration: Self::Input)->bool;
  fn fractional_interval_filter (&self, endpoints: [&Self::FractionalValue; 2], duration_shift: u32)->bool;
  fn tail_filter (&self, endpoint: &Self::IntegerValue)->bool;
  fn fractional_result_filter (&self, value: &Self::FractionalValue)->bool;
  fn integer_result_filter (&self, value: &Self::IntegerValue)->bool {
    self.fractional_result_filter (&self.integer_to_fractional(value))
  }
}


pub struct RangeSearchRunner<S: RangeSearch> {
  search: S,
  max_input_shift: u32,
  integer_stack: ArrayVec<[PolynomialBasedAtInput<S::IntegerValue, S::Input>; 64]>,
  fractional_stack: ArrayVec<[PolynomialBasedAtInput<S::FractionalValue, S::Input>; 64]>,
  next_jump: S::Input,
}

impl<S: RangeSearch> RangeSearchRunner<S> {
  pub fn run(search: S, start_input: S::Input, input_shift: u32) -> Option<S::Input> {
    let start_integer = shr_floor(start_input, input_shift);
    let start_value = PolynomialBasedAtInput::new(search.value_at_integer(start_integer)?, start_integer);
    if !search.tail_filter (start_value) { return None; }
    let next_integer = start_integer + S::Input::one();
    let next_value = PolynomialBasedAtInput::new(search.value_at_integer(start_integer)?, start_integer + One::one())?;
    
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
      max_input_shift: input_shift,
      integer_stack: ArrayVec::new(),
      fractional_stack: ArrayVec::new(),
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
  #[inline]
  pub fn latest_fractional_interval(&self) -> [&PolynomialBasedAtInput<S::FractionalValue, S::Input>; 2] {
    [
      &self.fractional_stack[self.fractional_stack.len() - 1],
      &self.fractional_stack[self.fractional_stack.len() - 2],
    ]
  }
  pub fn search_integers(&mut self) -> Option<S::Input> {
    loop {
      'searching_subintervals: while self.search.integer_interval_filter (self.latest_integer_interval().map(|a| &a.coefficients), duration) {
        if self.search.integer_result_filter (&self.latest_integer_interval()[0].coefficients) {
          return Some(self.latest_integer_interval()[0].origin);
        }
        let mut end_inputs = self.latest_integer_interval().map(|a|a.origin); 
        
        'finding_midpoint: loop {
          if end_inputs[0] + One::one() == end_inputs[1] {
            if let Some(result) = self.search_fractional() { return Some(result); }
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
      if self.integer_stack.len() == 2 && !self.search.tail_filter(&self.latest_integer_interval()[1].coefficients) {
        return None;
      }
      self.integer_stack.pop();
      if self.integer_stack.len() < 2 {
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
              self.next_jump <<= 1u32;
              break 'finding_next;
            }
          }
          self.next_jump >>= 1u32;
          if self.next_jump == Zero::zero() { return None; }
        }
      }
    }
  }
  
  
  pub fn search_fractional(&mut self) -> Option<S::Input> {
    let one = Shl::<u32>::shl(S::Input::one(), self.max_input_shift);
    let half = Shr::<u32>::shr(one, 1);
    self.fractional_stack.push(
      PolynomialBasedAtInput {
        origin: half,
        coefficients: self.search.value_at_fractional(&self.latest_integer_interval()[0].coefficients, half),
      },
    );
    self.fractional_stack.push(
      PolynomialBasedAtInput {
        origin: S::Input::zero(),
        coefficients: self.search.integer_to_fractional (&self.latest_integer_interval()[0].coefficients),
      },
    );
    let mut closer = 0;
    let current_input_shift = 1;
    loop {
      'searching_subintervals: while self.search.fractional_interval_filter (self.latest_fractional_interval().map(|a|&a.coefficients), current_input_shift) {
        if self.search.fractional_result_filter (&self.latest_fractional_interval()[0].coefficients) {
          return Some(self.latest_fractional_interval()[0].origin);
        }
        let end_inputs = self.latest_fractional_interval().map(|a|a.origin); 
        if end_inputs[0] + One::one() == end_inputs[1] {
          break 'searching_subintervals;
        }
        current_input_shift += 1;
        let new_endpoint = end_inputs[0] + Shr::<u32>::shr(one, current_input_shift);
        let new_value = self.search.value_at_fractional(&self.latest_integer_interval()[closer].coefficients, new_endpoint);
        self.fractional_stack.insert(
          self.fractional_stack.len() - 1,
          PolynomialBasedAtInput {
            origin: new_endpoint,
            coefficients: new_value,
          },
        );
      }

      self.fractional_stack.pop();
      current_input_shift -= 1;
      if self.fractional_stack.len() < 2 {
        if self.fractional_stack.last().unwrap().origin < one {
          self.fractional_stack.insert(1,
            PolynomialBasedAtInput {
              origin: one,
              coefficients: self.search.integer_to_fractional (&self.latest_integer_interval()[1].coefficients),
            },
          );
          closer = 1;
        }
        else {
          return None;
        }
      }
    }
  }
}
