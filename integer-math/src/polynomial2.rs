use num::{CheckedAdd, CheckedMul};
use array_ext::*;
//use std::cmp::{min, max};
use array::{Array, ReplaceItemType};
use arrayvec;

use super::*;

pub trait PolynomialBase: Array + ReplaceItemType<[<Self as arrayvec::Array>::Item; 2]> {}

/// Evaluate all Taylor coefficients of a polynomial.
///
/// Returns None if any of the coefficients do not fit in the type.
pub trait AllTaylorCoefficients<Input>: Sized {
  fn all_taylor_coefficients(&self, input: impl Into<Input>)->Option<Self>;
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(
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


/*
struct RangeSearchEndpoint {

}

pub fn coefficient_ranges (endpoints: [& RangeSearchEndpoint; 2])-> {
  let mut result = [[0,0]; $coefficients];
  // TODO what if overflow
  let duration = endpoints [1].time - endpoints [2].time;
  let mut previous = [0, 0];
  for exponent in (0..$coefficients).rev() {
    let left_max = endpoints [0] [exponent] + max(0, previous[1]*duration);
    let left_min = endpoints [0] [exponent] + min(0, previous[0]*duration);
    let right_max = endpoints [1] [exponent] + max(0, -previous[0]*duration);
    let right_min = endpoints [1] [exponent] + min(0, -previous[1]*duration);
    let bounds = [max(left_min, right_min), min(left_max, right_max)];
    previous = bounds;
    result [exponent] = bounds;
  }
  result
}

struct RangeSearch {
  polynomial: 
  stack: ArrayVec<[RangeSearchEndpoint; 64]>;
  next_jump: Time,
}

impl RangeSearch {
  pub fn latest_interval(&self)->[&RangeSearchEndpoint; 2] {
    [
      &self.stack[self.stack.len() - 1],
      &self.stack[self.stack.len() - 2],
    ]
  }
  pub fn split_latest(&mut self) {
    let split_time = mean_floor(self.latest_interval().endpoints [0].time, self.latest_interval().endpoints [1].time); 
    if !self.add_endpoint (split_time) {
      let earliest = self.stack.pop();
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
  fn add_endpoint(&mut self, time)->bool {
    if let Some(coefficients) = self.polynomial.all_taylor_coefficients (time) {
      self.stack.insert (self.stack.len() - 1, RangeSearchEndpoint {time, coefficients});
      true
    }
    else {
      false
    }
  }
}

*/

/*fn next_time_lt <T: Polynomial, Input> (polynomial: T, start_time: Input, threshold: Coefficient)->Option <Time> {
  let search = RangeSearch::new(polynomial,);
  
  loop {
    if search.latest_interval().coefficient_ranges[0][0] < threshold {
      if search.latest_interval().endpoints[0].time + 1 == search.latest_interval().endpoints[1].time {
        if search.latest_interval().endpoints[0].coefficients[0] < threshold {
          return Some(search.latest_interval().endpoints[0].time);
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
    if search.reached_overflow() { break; }
  }
}*/

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
      
      
      /*#[test]
      fn randomly_test_next_time_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16, threshold in -16 as $integer..16) {
        let time = next_time_lt (coefficients, input);
        prop_assume! (time .is_some());
        let time = time.unwrap();
        prop_assert!(coefficients.all_taylor_coefficients (time)[0] < threshold);
      }
      
      #[test]
      fn randomly_test_next_time_lt_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16, threshold in -16 as $integer..16, test_frac in 0f64..1f64) {
        let time = next_time_lt (coefficients, input);
        let last_not_lt = match time {
          None => $integer::max_value(),
          Some(k) => k-1,
        };
        prop_assume!(last_not_lt >= input);
        prop_assert!(coefficients.all_taylor_coefficients (input)[0] >= threshold);
        prop_assert!(coefficients.all_taylor_coefficients (last_not_lt)[0] >= threshold);
        let test_time = input + ((input - last_not_lt) as f64 * test_frac).floor() as $integer;
        prop_assert!(coefficients.all_taylor_coefficients (test_time)[0] >= threshold);
      }*/
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
