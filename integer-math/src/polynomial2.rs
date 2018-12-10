use num::{Integer as NumInteger, Signed, CheckedAdd, CheckedMul, Saturating, One, FromPrimitive, Bounded};
use array_ext::{Array as ArrayExtArray, *};
use std::cmp::{min, max};
use array::{Array, ReplaceItemType};
use arrayvec::{self, ArrayVec};

use super::*;

pub trait PolynomialBase1 {
  type Coefficient: DoubleSizedInteger + Signed;
}
pub trait PolynomialBase2: PolynomialBase1 + Array + arrayvec::Array<Item = <Self as PolynomialBase1>::Coefficient> + ReplaceItemType<[<Self as PolynomialBase1>::Coefficient; 2]> {}



/// Evaluate all Taylor coefficients of a polynomial.
///
/// Returns None if any of the coefficients do not fit in the type.
pub trait AllTaylorCoefficients<Input>: Sized {
  fn all_taylor_coefficients(&self, input: impl Copy+Into<Input>)->Option<Self>;
}

pub trait AllTaylorCoefficientsBounds<Input>: PolynomialBase1 + ReplaceItemType<[<Self as PolynomialBase1>::Coefficient; 2]> {
  fn all_taylor_coefficients_bounds(&self, input: impl Copy+Into<Input>, input_shift: impl Copy+Into<u32>)->Option<<Self as ReplaceItemType<[Self::Coefficient; 2]>>::Type>;
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(

impl <Coefficient: DoubleSizedInteger + Signed> PolynomialBase1 for [Coefficient; $coefficients] {type Coefficient = Coefficient; }
impl <Coefficient: DoubleSizedInteger + Signed> PolynomialBase2 for [Coefficient; $coefficients] {}

impl <Coefficient: DoubleSizedInteger> AllTaylorCoefficients<<Coefficient as DoubleSizedInteger>::Type> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients(&self, input: impl Copy+Into<<Coefficient as DoubleSizedInteger>::Type>)->Option <Self> {
    let input = input.into();
    let mut intermediates: [<Coefficient as DoubleSizedInteger>::Type; $coefficients] = self.map (| coefficient | coefficient.into());
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



impl <Coefficient: DoubleSizedInteger + Signed> AllTaylorCoefficientsBounds<<Coefficient as DoubleSizedInteger>::Type> for [Coefficient; $coefficients] where <Coefficient as DoubleSizedInteger>::Type: Signed {
  fn all_taylor_coefficients_bounds(&self, input: impl Into<<Coefficient as DoubleSizedInteger>::Type>, input_shift: impl Copy+Into<u32>)->Option<<Self as ReplaceItemType<[Self::Coefficient; 2]>>::Type> {
    let input = input.into();
    let input_shift_dynamic: u32 = input_shift.into();
    let integer_input = shr_nicely_rounded (input, input_shift);
    let small_input = input.wrapping_sub (& (Shl::<u32>::shl(integer_input, input_shift_dynamic)));
    let integer_coefficients = self.all_taylor_coefficients (integer_input)?;
    let flip_odd = small_input < Zero::zero();
    let small_input = if flip_odd {-small_input} else {small_input};
    let mut intermediates: [[<Coefficient as DoubleSizedInteger>::Type; 2]; $coefficients] = array_ext::Array::from_fn(|index| {
      let raw: <Coefficient as DoubleSizedInteger>::Type = integer_coefficients[index].into();
      if flip_odd && index.is_odd() { [-raw,-raw] } else { [raw,raw] }
    });
    for first_source in (1..intermediates.len()).rev() {
      for source in first_source..intermediates.len() {
        intermediates[source - 1][0] += 
          Shr::<u32>::shr(intermediates[source][0] * small_input, input_shift_dynamic);
        intermediates[source - 1][1] += 
          shr_ceil(intermediates[source][1] * small_input, input_shift);
      }
    }
    let mut output = [[Coefficient::zero(); 2]; $coefficients];
    for (index, value) in intermediates.iter().enumerate() {
      output [index] = if flip_odd && index.is_odd() {[
        (-value[1]).try_into().ok()?,
        (-value[0]).try_into().ok()?,
      ]}
      else {[
        value[0].try_into().ok()?,
        value[1].try_into().ok()?,
      ]};
    }
    Some (output)
  }
  
}
    
)*  
    
  }
}


impl_polynomials!(1,2,3,4,5);

pub trait Polynomial: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type> + AllTaylorCoefficientsBounds<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type> {}
impl<P: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type> + AllTaylorCoefficientsBounds<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type>> Polynomial for P {}



pub struct RangeSearchEndpoint<P: PolynomialBase2> {
  time: <P::Coefficient as DoubleSizedInteger>::Type,
  coefficients: <P as ReplaceItemType<[P::Coefficient; 2]>>::Type,
}

pub fn coefficient_ranges<P: PolynomialBase2, InputShift: Copy+Into<u32>> (endpoints: [& RangeSearchEndpoint<P>; 2], input_shift: InputShift)-><P as ReplaceItemType<[P::Coefficient; 2]>>::Type {
  let mut result: <P as ReplaceItemType<[P::Coefficient; 2]>>::Type = array_ext::Array::from_fn(|_| [P::Coefficient::zero(), P::Coefficient::zero()]);
  let input_shift_dynamic = input_shift.into();
  // TODO what if overflow
  let duration = endpoints [1].time - endpoints [0].time;
  let mut previous_derivative_range: [<P::Coefficient as DoubleSizedInteger>::Type; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len()).rev() {
    let endpoint_0 = endpoints [0].coefficients.as_slice() [exponent].map(|a| <P::Coefficient as DoubleSizedInteger>::Type::from(a));
    let endpoint_1 = endpoints [1].coefficients.as_slice() [exponent].map(|a| <P::Coefficient as DoubleSizedInteger>::Type::from(a));
    let bounds = if previous_derivative_range[0] >= Zero::zero() {
      [endpoint_0[0], endpoint_1[1]]
    } else if previous_derivative_range[1] <= Zero::zero() {
      [endpoint_1[0], endpoint_0[1]]
    } else {
    
      // TODO: these bounds can be tightened by analyzing the parallelogram
      // but it might overflow
      // did the algebra as (v1*s1 + v0*-s0 + (t1-t0)*s1*-s0)/(s1+ -s0) = max_value
      // let slope_product = previous_derivative_range[0]*previous_derivative_range[1];
      
      let previous_max_movement = [
        Shr::<u32>::shr(previous_derivative_range[0].saturating_mul(duration), input_shift_dynamic),
        shr_ceil(previous_derivative_range[1].saturating_mul(duration), input_shift),
      ];
      
      let left_max = endpoint_0[1].saturating_add(previous_max_movement[1]);
      let left_min = endpoint_0[0].saturating_add(previous_max_movement[0]);
      
      let right_max = endpoint_1[1].saturating_sub(previous_max_movement[0]);
      let right_min = endpoint_1[0].saturating_sub(previous_max_movement[1]);
      
      [
        max(left_min, right_min),
        min(left_max, right_max)
      ]
    };

    result.as_mut_slice() [exponent] = bounds.map(|a| {
      if a > <P::Coefficient as DoubleSizedInteger>::Type::from(P::Coefficient::max_value()) { P::Coefficient::max_value() }
      else if a < <P::Coefficient as DoubleSizedInteger>::Type::from(P::Coefficient::min_value()) { P::Coefficient::max_value() }
      else {
        a.try_into().ok().unwrap()
      }
    });
    previous_derivative_range = bounds.map(|a|a.saturating_mul(FromPrimitive::from_usize(exponent).unwrap()));
  }
  result
}

pub struct RangeSearch<P: PolynomialBase2, InputShift> {
  polynomial: P,
  input_shift: InputShift,
  stack: ArrayVec<[RangeSearchEndpoint<P>; 64]>,
  next_jump: <P::Coefficient as DoubleSizedInteger>::Type,
}

impl<P: Polynomial, InputShift: Copy+Into<u32>> RangeSearch<P, InputShift> {
  pub fn new(polynomial: P, start_time: <P::Coefficient as DoubleSizedInteger>::Type, input_shift: InputShift)->Option<Self> {
    let mut result = RangeSearch {polynomial, input_shift, stack: ArrayVec::new(), next_jump: One::one()};
    if let Some(coefficients) = result.polynomial.all_taylor_coefficients_bounds (start_time, input_shift) {
      result.stack.push(RangeSearchEndpoint {time: start_time, coefficients});
      result.add_next_endpoint();
      Some(result)
    }
    else {
      None
    }
  }
  pub fn latest_interval(&self)->[&RangeSearchEndpoint<P>; 2] {
    [
      &self.stack[self.stack.len() - 1],
      &self.stack[self.stack.len() - 2],
    ]
  }
  pub fn split_latest(&mut self) {
    let split_time = mean_floor(self.latest_interval()[0].time, self.latest_interval()[1].time); 
    if !self.add_endpoint (split_time) {
      let earliest = self.stack.pop().unwrap();
      self.stack.clear();
      self.stack.push(earliest);
      self.add_next_endpoint();
    }
  }
  pub fn skip_latest(&mut self) {
    self.stack.pop() ;
    if self.stack.len() < 2 {
      self.add_next_endpoint();
    }
  }
  pub fn reached_overflow(&self)->bool {
    self.stack.len() == 2 && self.stack [0].time == self.stack [1].time
  }
  fn add_next_endpoint(&mut self) {
    let last_endpoint_time = self.stack.last().unwrap().time;
    loop {
      let endpoint_time = last_endpoint_time.saturating_add (self.next_jump);
      if self.add_endpoint(endpoint_time) { break; }
      self.next_jump = self.next_jump >> 1;
    }
    self.next_jump = self.next_jump << 1;
  }
  fn add_endpoint(&mut self, time: <P::Coefficient as DoubleSizedInteger>::Type)->bool {
    if let Some(coefficients) = self.polynomial.all_taylor_coefficients_bounds (time, self.input_shift) {
      self.stack.insert (self.stack.len() - 1, RangeSearchEndpoint {time, coefficients});
      true
    }
    else {
      false
    }
  }
}


//Note: currently, this function is strict (always find the exact time the max goes below the threshold). With a certain amount of error when the value is very close to the threshold, this could force searching every time unit. TODO: fix this by rigorously limiting the error and allowing that much leeway
pub fn next_time_definitely_lt <P: Polynomial, InputShift: Copy+Into<u32>> (polynomial: P, start_time: <P::Coefficient as DoubleSizedInteger>::Type, input_shift: InputShift, threshold: P::Coefficient)->Option <<P::Coefficient as DoubleSizedInteger>::Type> {
  let mut search = RangeSearch::new(polynomial, start_time, input_shift)?;
  
  loop {
    if coefficient_ranges(search.latest_interval(), input_shift).as_slice()[0][0] < threshold {
      if search.latest_interval()[0].time + One::one() == search.latest_interval()[1].time {
        if search.latest_interval()[0].coefficients.as_slice()[0][1] < threshold {
          return Some(search.latest_interval()[0].time);
        }
        search.skip_latest();
      }
      else {
        search.split_latest();
      }
    }
    else {
      search.skip_latest();
    }
    if search.reached_overflow() { return None; }
  }
}

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
use proptest::prelude::*;
use num::{BigInt, BigRational, One};


fn naive_perfect_evaluate <Coefficient: Integer> (coefficients: & [Coefficient], input: BigRational)->BigRational where BigInt: From<Coefficient> {
  let mut result = BigRational::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate() {
    let mut term = BigRational::from(BigInt::from(*coefficient));
    for _ in 0..exponent { term = term * &input; }
    result = result + term;
  }
  result
}

fn naive_factorial (value: usize)->BigInt {
  let mut result = BigInt::one();
  for factor in 2..=value {
    result = result * BigInt::from(factor);
  }
  result
}

fn naive_binomial_coefficient(n:usize,k:usize)->BigInt {
  naive_factorial (n)/(naive_factorial (k)*naive_factorial (n-k))
}

fn naive_perfect_nth_taylor_coefficient <Coefficient: Integer> (coefficients: & [Coefficient], input: BigRational, n: usize)->BigRational where BigInt: From<Coefficient> {
  let mut result = BigRational::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate().skip(n) {
    let mut term = BigRational::from(BigInt::from(*coefficient));
    for _ in n..exponent { term = term * &input; }
    result = result + term * naive_binomial_coefficient (exponent, n);
  }
  result
}

fn arbitrary_fractional_input()->BoxedStrategy <(i64, u32)> {
  (0u32..16).prop_flat_map (| shift | {
    ((-16i64 << shift..16i64 << shift), Just (shift))
  }).boxed()
}

macro_rules! test_polynomials {
  ($($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident,)*) => {
$(
  mod $name {
    use super::*;
    //use super::super::*;
    //use proptest::prelude::*;
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
      fn randomly_test_taylor_coefficients_bounds_correct (coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input()) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input, input_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::new(BigInt::from(input), BigInt::from(1i64 << input_shift)), which);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) <= exact, "Incorrect {}th taylor coefficient lower bound: {} > {:?}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) >= exact, "Incorrect {}th taylor coefficient upper bound: {} < {:?}", which, bounds[which][1], exact);
        }
      }
      
      
      #[test]
      fn randomly_test_next_time_definitely_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input(), threshold in -16 as $integer..16) {
        let time = next_time_definitely_lt (coefficients, input, input_shift, threshold);
        prop_assume! (time .is_some());
        let time = time.unwrap();
        if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input_shift) { prop_assert!(coefficients[0][1] < threshold); }
      }
      
      #[test]
      fn randomly_test_next_time_definitely_lt_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input(), threshold in -16 as $integer..16, test_frac in 0f64..1f64) {
        let time = next_time_definitely_lt (coefficients, input, input_shift, threshold);
        let last_not_lt = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input);
            k-1
          },
        };
        prop_assume!(last_not_lt >= input);
        if let Some(first_coefficients) = coefficients.all_taylor_coefficients_bounds (input, input_shift) { prop_assert!(first_coefficients[0][1] >= threshold); }
        if let Some(last_coefficients) = coefficients.all_taylor_coefficients_bounds (last_not_lt, input_shift) {prop_assert!(last_coefficients[0][1] >= threshold); }
        let test_time = input + ((last_not_lt.saturating_sub(input)) as f64 * test_frac).floor() as $double;
        if let Some(test_coefficients) = coefficients.all_taylor_coefficients_bounds (test_time, input_shift) {prop_assert!(test_coefficients[0][1] >= threshold); }
      }
    }
  }
)*  
    
  }
}

test_polynomials!(
  1, i32, i64, uniform1, polynomial_tests_1,
  2, i32, i64, uniform2, polynomial_tests_2,
  3, i32, i64, uniform3, polynomial_tests_3,
  4, i32, i64, uniform4, polynomial_tests_4,
  5, i32, i64, uniform5, polynomial_tests_5,
);

}
