use num::{Signed, CheckedAdd, CheckedMul, Saturating, One, FromPrimitive};
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
  fn all_taylor_coefficients(&self, input: impl Into<Input>)->Option<Self>;
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(

impl <Coefficient: DoubleSizedInteger + Signed> PolynomialBase1 for [Coefficient; $coefficients] {type Coefficient = Coefficient; }
impl <Coefficient: DoubleSizedInteger + Signed> PolynomialBase2 for [Coefficient; $coefficients] {}

impl <Coefficient: DoubleSizedInteger> AllTaylorCoefficients<<Coefficient as DoubleSizedInteger>::Type> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients(&self, input: impl Into<<Coefficient as DoubleSizedInteger>::Type>)->Option <Self> {
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
    
)*  
    
  }
}


impl_polynomials!(1,2,3,4,5);

pub trait Polynomial: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type> {}
impl<P: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedInteger>::Type>> Polynomial for P {}



pub struct RangeSearchEndpoint<P: PolynomialBase2> {
  time: P::Coefficient,
  coefficients: P,
}

pub fn coefficient_ranges<P: PolynomialBase2> (endpoints: [& RangeSearchEndpoint<P>; 2])-><P as ReplaceItemType<[P::Coefficient; 2]>>::Type {
  let mut result: <P as ReplaceItemType<[P::Coefficient; 2]>>::Type = array_ext::Array::from_fn(|_| [P::Coefficient::zero(), P::Coefficient::zero()]);
  // TODO what if overflow
  let duration = endpoints [1].time - endpoints [0].time;
  let mut previous = [P::Coefficient::zero(), P::Coefficient::zero()];
  for exponent in (0..result.len()).rev() {
    let left_max = endpoints [0].coefficients.as_slice() [exponent].saturating_add(max(P::Coefficient::zero(), previous[1]).saturating_mul(duration));
    let left_min = endpoints [0].coefficients.as_slice() [exponent].saturating_add(min(P::Coefficient::zero(), previous[0]).saturating_mul(duration));
    let right_max = endpoints [1].coefficients.as_slice() [exponent].saturating_add(max(P::Coefficient::zero(), -previous[0]).saturating_mul(duration));
    let right_min = endpoints [1].coefficients.as_slice() [exponent].saturating_add(min(P::Coefficient::zero(), -previous[1]).saturating_mul(duration));
    let bounds = [max(left_min, right_min), min(left_max, right_max)];
    previous = bounds.map(|a|a.saturating_mul(P::Coefficient::from_usize(exponent).unwrap()));
    result.as_mut_slice() [exponent] = bounds;
  }
  result
}

pub struct RangeSearch<P: PolynomialBase2> {
  polynomial: P,
  stack: ArrayVec<[RangeSearchEndpoint<P>; 64]>,
  next_jump: P::Coefficient,
}

impl<P: Polynomial> RangeSearch<P> {
  pub fn new(polynomial: P, start_time: P::Coefficient)->Option<Self> {
    let mut result = RangeSearch {polynomial, stack: ArrayVec::new(), next_jump: P::Coefficient::one()};
    if let Some(coefficients) = result.polynomial.all_taylor_coefficients (start_time) {
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
  fn add_endpoint(&mut self, time: P::Coefficient)->bool {
    if let Some(coefficients) = self.polynomial.all_taylor_coefficients (time) {
      self.stack.insert (self.stack.len() - 1, RangeSearchEndpoint {time, coefficients});
      true
    }
    else {
      false
    }
  }
}



pub fn next_time_lt <P: Polynomial> (polynomial: P, start_time: P::Coefficient, threshold: P::Coefficient)->Option <P::Coefficient> {
  let mut search = RangeSearch::new(polynomial, start_time)?;
  
  loop {
    if coefficient_ranges(search.latest_interval()).as_slice()[0][0] < threshold {
      if search.latest_interval()[0].time + P::Coefficient::one() == search.latest_interval()[1].time {
        if search.latest_interval()[0].coefficients.as_slice()[0] < threshold {
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
//use proptest::prelude::*;
use num::{BigInt, One};


fn naive_perfect_evaluate <Coefficient: Integer> (coefficients: & [Coefficient], input: Coefficient)->BigInt where BigInt: From<Coefficient> {
  let input = BigInt::from (input) ;
  let mut result = BigInt::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate() {
    let mut term = BigInt::from(*coefficient);
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

fn naive_perfect_nth_taylor_coefficient <Coefficient: Integer> (coefficients: & [Coefficient], input: Coefficient, n: usize)->BigInt where BigInt: From<Coefficient> {
  let input = BigInt::from (input) ;
  let mut result = BigInt::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate().skip(n) {
    let mut term = BigInt::from(*coefficient);
    for _ in n..exponent { term = term * &input; }
    result = result + term * naive_binomial_coefficient (exponent, n);
  }
  result
}



macro_rules! test_polynomials {
  ($($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident,)*) => {
$(
  mod $name {
    use super::*;
    //use super::super::*;
    use proptest::prelude::*;
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
        let evaluated = naive_perfect_evaluate (&coefficients, input);
        prop_assert_eq! (BigInt::from(translated[0]), evaluated);
        for which in 0..$coefficients {
          prop_assert_eq! (BigInt::from(translated[which]), naive_perfect_nth_taylor_coefficient(&coefficients, input, which), "Incorrect {}th taylor coefficient ", which);
        }
      }
      
      
      #[test]
      fn randomly_test_next_time_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16, threshold in -16 as $integer..16) {
        let time = next_time_lt (coefficients, input, threshold);
        prop_assume! (time .is_some());
        let time = time.unwrap();
        if let Some(coefficients) = coefficients.all_taylor_coefficients (time) { prop_assert!(coefficients[0] < threshold); }
      }
      
      #[test]
      fn randomly_test_next_time_lt_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16, threshold in -16 as $integer..16, test_frac in 0f64..1f64) {
        let time = next_time_lt (coefficients, input, threshold);
        let last_not_lt = match time {
          None => $integer::max_value(),
          Some(k) => {
            prop_assert!(k >= input);
            k-1
          },
        };
        prop_assume!(last_not_lt >= input);
        if let Some(first_coefficients) = coefficients.all_taylor_coefficients (input) { prop_assert!(first_coefficients[0] >= threshold); }
        if let Some(last_coefficients) = coefficients.all_taylor_coefficients (last_not_lt) {prop_assert!(last_coefficients[0] >= threshold); }
        let test_time = input + ((last_not_lt.saturating_sub(input)) as f64 * test_frac).floor() as $integer;
        if let Some(test_coefficients) = coefficients.all_taylor_coefficients (test_time) {prop_assert!(test_coefficients[0] >= threshold); }
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
