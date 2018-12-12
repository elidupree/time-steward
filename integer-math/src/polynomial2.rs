use num::{Integer as NumInteger, Signed, CheckedAdd, CheckedMul, One, FromPrimitive, Bounded};
use array_ext::{Array as ArrayExtArray, *};
use std::cmp::{min, max};
use array::{Array, ReplaceItemType};
use arrayvec::{self, ArrayVec};

use super::*;

pub trait PolynomialBase1 {
  type Coefficient: DoubleSizedSignedInteger;
}
pub trait PolynomialBase2: PolynomialBase1 + Array + arrayvec::Array<Item = <Self as PolynomialBase1>::Coefficient> + ReplaceItemType<[<Self as PolynomialBase1>::Coefficient; 2]> {}



/// Evaluate all Taylor coefficients of a polynomial.
///
/// Returns None if any of the coefficients do not fit in the type.
pub trait AllTaylorCoefficients<Input>: Sized {
  fn all_taylor_coefficients(&self, input: impl Copy+Into<Input>)->Option<Self>;
}

pub trait AllTaylorCoefficientsBounds<Input>: PolynomialBase1 + ReplaceItemType<[<Self as PolynomialBase1>::Coefficient; 2]> {
  fn all_taylor_coefficients_bounds(&self, input: impl Copy+Into<Input>, input_shift: impl Copy+Into<u32>, precision_shift: impl Copy+Into<u32>)->Option<<Self as ReplaceItemType<[Self::Coefficient; 2]>>::Type>;
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(

impl <Coefficient: DoubleSizedSignedInteger> PolynomialBase1 for [Coefficient; $coefficients] {type Coefficient = Coefficient; }
impl <Coefficient: DoubleSizedSignedInteger> PolynomialBase2 for [Coefficient; $coefficients] {}

impl <Coefficient: DoubleSizedSignedInteger> AllTaylorCoefficients<<Coefficient as DoubleSizedSignedInteger>::Type> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients(&self, input: impl Copy+Into<<Coefficient as DoubleSizedSignedInteger>::Type>)->Option <Self> {
    let input = input.into();
    let mut intermediates: [<Coefficient as DoubleSizedSignedInteger>::Type; $coefficients] = self.map (| coefficient | coefficient.into());
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



impl <Coefficient: DoubleSizedSignedInteger + Signed> AllTaylorCoefficientsBounds<<Coefficient as DoubleSizedSignedInteger>::Type> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients_bounds(&self, input: impl Into<<Coefficient as DoubleSizedSignedInteger>::Type>, input_shift: impl Copy+Into<u32>, precision_shift: impl Copy+Into<u32>)->Option<<Self as ReplaceItemType<[Self::Coefficient; 2]>>::Type> {
    let input = input.into();
    let input_shift_dynamic: u32 = input_shift.into();
    
    // In the loop, error accumulates each term.
    // Fortunately, the error is strictly bounded above by 2^(degree-1).
    // We want to scale down the error as far as possible, so we first left-shift by degree,
    // then right-shift by degree at the end of the calculation.
    // This reduces the accumulated error to less than half.
    // It unavoidably adds an error of up to 1 due to the final rounding, so the final error is up to (not including) 1.5.
    // This means that the final upper and lower bound can be no more than 2 away from each other,
    // which is the best we can hope for.
    // TODO: would making this a ZST optimize anything, or is it already inlined?
    let accumulated_error_shift = $coefficients - 1;
    
    // in total, in the formula, we left-shift by precision_shift,
    // then multiply by a number that is up to half of 1<<input_shift - 
    // i.e. we need space for precision_shift+inputshift-1 more bits in the type.
    assert!(overflow_checked_shl(<Coefficient as DoubleSizedSignedInteger>::Type::from(Coefficient::max_value()), precision_shift.into() + accumulated_error_shift + input_shift_dynamic.saturating_sub(1)).is_some());
    let integer_input = shr_nicely_rounded (input, input_shift);
    let small_input = input.wrapping_sub (& (Shl::<u32>::shl(integer_input, input_shift_dynamic)));
    let integer_coefficients = self.all_taylor_coefficients (integer_input)?;
    
    // In the loop, we use floor/ceil to make sure we keep getting a lower/upper bound.
    // But we also multiply by input each time, and if the input was negative, we would keep switching the direction.
    // Fortunately, negative input is equivalent to having all of the odd terms be negated.
    let flip_odd = small_input < Zero::zero();
    let small_input = if flip_odd {-small_input} else {small_input};
    
    let mut intermediates: [[<Coefficient as DoubleSizedSignedInteger>::Type; 2]; $coefficients] = array_ext::Array::from_fn(|index| {
      let mut raw: <Coefficient as DoubleSizedSignedInteger>::Type = integer_coefficients[index].into();
      raw <<= precision_shift.into() + accumulated_error_shift;
      if flip_odd && index.is_odd() { raw = -raw; }
      [raw,raw]
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
      output [index] = [
        Shr::<u32>::shr(value[0].try_into().ok()?, accumulated_error_shift),
        shr_ceil(value[1].try_into().ok()?, accumulated_error_shift),
      ];
      if flip_odd && index.is_odd() {
        output [index] = [
          -output [index][1],
          -output [index][0],
        ];
      }
    }
    Some (output)
  }
  
}
    
)*  
    
  }
}


impl_polynomials!(1,2,3,4,5);

pub trait Polynomial: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedSignedInteger>::Type> + AllTaylorCoefficientsBounds<<<Self as PolynomialBase1>::Coefficient as DoubleSizedSignedInteger>::Type> {}
impl<P: PolynomialBase1 + PolynomialBase2 + AllTaylorCoefficients<<<Self as PolynomialBase1>::Coefficient as DoubleSizedSignedInteger>::Type> + AllTaylorCoefficientsBounds<<<Self as PolynomialBase1>::Coefficient as DoubleSizedSignedInteger>::Type>> Polynomial for P {}



pub struct PolynomialBasedAtInput <P, I> {
  coefficients: P,
  origin: I,
}


pub fn coefficient_bounds_on_interval<P: Array + arrayvec::Array<Item=[Coefficient; 2]>, Coefficient: Integer+Signed, WorkingType: Integer+Signed + From<Coefficient> + TryInto<Coefficient>, InputShift: Copy+Into<u32>> (endpoints: [& PolynomialBasedAtInput <P, WorkingType>; 2], input_shift: InputShift)-> P{
  let mut result: P = array_ext::Array::from_fn(|_| [Zero::zero(); 2]);
  // TODO what if overflow
  let duration = match endpoints [1].origin.checked_sub(&endpoints [0].origin) {
    Some(a)=>a,
    None => {
      // if the duration overflows, give up on the missing the range at all
      result = array_ext::Array::from_fn(|_| [Coefficient::min_value(), Coefficient::max_value()]);
      return result
    }
  };
  let mut previous_derivative_range: [WorkingType; 2] = [Zero::zero(), Zero::zero()];
  for exponent in (0..result.len()).rev() {
    let endpoint_0 = endpoints [0].coefficients.as_slice() [exponent].map(|a| <WorkingType as From<Coefficient>>::from(a));
    let endpoint_1 = endpoints [1].coefficients.as_slice() [exponent].map(|a| <WorkingType as From<Coefficient>>::from(a));
    let bounds = if previous_derivative_range[0] >= Zero::zero() {
      [endpoint_0[0], endpoint_1[1]]
    } else if previous_derivative_range[1] <= Zero::zero() {
      [endpoint_1[0], endpoint_0[1]]
    } else {
    
      // TODO: these bounds can be tightened by analyzing the parallelogram
      // but it might overflow
      // did the algebra as (v1*s1 + v0*-s0 + (t1-t0)*s1*-s0)/(s1+ -s0) = max_value
      // let slope_product = previous_derivative_range[0]*previous_derivative_range[1];
      
      // If we're looking at the final value, round inwards because fractional movement can't reach the next integer.
      // But for the derivatives, fractional movement could stack up.
      let (previous_min_movement, previous_max_movement) = if exponent == 0 {(
        mul_shr_ceil_round_down(previous_derivative_range[0], duration, input_shift),
        mul_shr_floor_round_up(previous_derivative_range[1], duration, input_shift),
      )}
      else {(
        mul_shr_round_down(previous_derivative_range[0], duration, input_shift),
        mul_shr_round_up(previous_derivative_range[1], duration, input_shift),
      )};
      
      // If an earlier derivative overflowed, assume it's arbitrarily high
      let unknown = (Bounded::min_value(), Bounded::max_value());
      let (left_min, right_max) = if previous_derivative_range[0] == Bounded::min_value() {
        unknown
      } else if let Some(previous_min_movement) = previous_min_movement {
        (
          endpoint_0[0].saturating_add(previous_min_movement),
          endpoint_1[1].saturating_sub(previous_min_movement),
        )
      } else { unknown };
      let (right_min, left_max) = if previous_derivative_range[1] == Bounded::max_value() {
        unknown
      } else if let Some(previous_max_movement) = previous_max_movement {
        (
          endpoint_1[0].saturating_sub(previous_max_movement),
          endpoint_0[1].saturating_add(previous_max_movement),
        )
      } else { unknown };
      
      [
        max(left_min, right_min),
        min(left_max, right_max)
      ]
    };

    result.as_mut_slice() [exponent] = bounds.map(saturating_downcast);
    previous_derivative_range = bounds.map(|a|a.saturating_mul(FromPrimitive::from_usize(exponent).unwrap()));
  }
  result
}

pub struct RangeSearch<F, I, P> {
  func: F,
  stack: ArrayVec<[PolynomialBasedAtInput<P, I>; 64]>,
  next_jump: I,
}

impl<F: Fn(I) -> Option<P>, I: Integer, P> RangeSearch<F, I, P> {
  pub fn new(func: F, start_input: I)->Option<Self> {
    let mut result = RangeSearch {func, stack: ArrayVec::new(), next_jump: One::one()};
    if let Some(coefficients) = (result.func)(start_input) {
      result.stack.push(PolynomialBasedAtInput {origin: start_input, coefficients});
      result.add_next_endpoint();
      Some(result)
    }
    else {
      None
    }
  }
  pub fn latest_interval(&self)->[&PolynomialBasedAtInput<P, I>; 2] {
    [
      &self.stack[self.stack.len() - 1],
      &self.stack[self.stack.len() - 2],
    ]
  }
  pub fn split_latest(&mut self) {
    let split_input = mean_floor(self.latest_interval()[0].origin, self.latest_interval()[1].origin); 
    if !self.add_endpoint (split_input) {
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
    self.stack.len() == 2 && self.stack [0].origin == self.stack [1].origin
  }
  fn add_next_endpoint(&mut self) {
    let last_endpoint_input = self.stack.last().unwrap().origin;
    loop {
      let endpoint_input = last_endpoint_input.saturating_add (self.next_jump);
      if self.add_endpoint(endpoint_input) { break; }
      self.next_jump = self.next_jump >> 1u32;
    }
    self.next_jump = self.next_jump << 1u32;
  }
  fn add_endpoint(&mut self, input: I)->bool {
    if let Some(coefficients) = (self.func)(input) {
      self.stack.insert (self.stack.len() - 1, PolynomialBasedAtInput{origin: input, coefficients});
      true
    }
    else {
      false
    }
  }
}

pub fn range_search<F: Fn(I) -> Option<P>, I: Integer, P, G: FnMut([&PolynomialBasedAtInput<P, I>;2])-> bool, H: FnMut(&PolynomialBasedAtInput<P, I>)->bool>(start_time: I, func: F, mut interval_filter: G, mut result_filter: H)->Option<I> {
  let mut search = RangeSearch::new(func, start_time)?;
  
  loop {
    if (interval_filter)(search.latest_interval()) {
      if search.latest_interval()[0].origin.saturating_add(One::one()) == search.latest_interval()[1].origin {
        if (result_filter)(search.latest_interval()[0]) {
          return Some(search.latest_interval()[0].origin);
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


pub fn polynomial_value_range_search <P: Polynomial, InputShift: Copy+Into<u32>, G: FnMut([P::Coefficient;2])-> bool, H: FnMut([P::Coefficient;2])->bool> (polynomial: P, start_time: <P::Coefficient as DoubleSizedSignedInteger>::Type, input_shift: InputShift, mut interval_filter: G, mut result_filter: H)->Option <<P::Coefficient as DoubleSizedSignedInteger>::Type> {
  range_search(
    start_time,
    |time| polynomial.all_taylor_coefficients_bounds (time, input_shift, 0u32),
    |interval| interval_filter(coefficient_bounds_on_interval(interval, input_shift).as_slice()[0]),
    |result| result_filter(result.coefficients.as_slice()[0]),
  )
}


//Note: currently, this function is strict (always find the exact time the max goes below the threshold). With a certain amount of error when the value is very close to the threshold, this could force searching every time unit. TODO: fix this by rigorously limiting the error and allowing that much leeway
/// Returns a time where the polynomial output is definitely less than permit_threshold, such that there is no EARLIER output less than require_threshold. (Or returns None if it encounters overflow before any output less than require_threshold.) With only approximate polynomial evaluation, for these conditions to be theoretically meetable, we must have permit_threshold >= require_threshold + 2. (Imagine that we have permit_threshold = 5, require_threshold = 4. The polynomial may output the range [3, 5]. We wouldn't be permitted to return that time because the true value may be 5, which is not less than permit_threshold and therefore not permitted. But we wouldn't be able to pass by that time because the true value could be 3, which is less than require_threshold.)
pub fn next_time_definitely_lt <P: Polynomial, InputShift: Copy+Into<u32>> (polynomial: P, start_time: <P::Coefficient as DoubleSizedSignedInteger>::Type, input_shift: InputShift, threshold: P::Coefficient)->Option <<P::Coefficient as DoubleSizedSignedInteger>::Type> {
  polynomial_value_range_search(
    polynomial, start_time, input_shift,
    |interval| interval[0] < threshold,
    |result| result[1] < threshold,
  )
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
      fn randomly_test_taylor_coefficients_bounds_correct (coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input(), precision_shift in 0u32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input, input_shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::new(BigInt::from(input), BigInt::from(1i64 << input_shift)), which) * BigInt::from(1u32 << precision_shift);
          
          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) <= exact, "Incorrect {}th taylor coefficient lower bound: {} > {:?}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) >= exact, "Incorrect {}th taylor coefficient upper bound: {} < {:?}", which, bounds[which][1], exact);
        }
      }
      
      #[test]
      fn randomly_test_taylor_coefficients_bounds_close(coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input(), precision_shift in 0u32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input, input_shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        let leeway = BigRational::new(BigInt::from(3i32), BigInt::from(2i32));
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::new(BigInt::from(input), BigInt::from(1i64 << input_shift)), which) * BigInt::from(1u32 << precision_shift);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) > &exact - &leeway, "Too loose {}th taylor coefficient lower bound: {} + 1.5 <= {:?}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) < &exact + &leeway, "Too loose {}th taylor coefficient upper bound: {} - 1.5 >= {:?}", which, bounds[which][1], exact);
          prop_assert! (bounds[which][1] <= bounds[which][0].saturating_add(2), "{}th taylor coefficient bounds are too far from each other (note: this should be impossible if the other conditions are met): {} > {} + 2", which, bounds[which][1], bounds[which][0]);
        }
      }
      
      
      #[test]
      fn randomly_test_next_time_definitely_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), (input, input_shift) in arbitrary_fractional_input(), threshold in -16 as $integer..16) {
        let time = next_time_definitely_lt (coefficients, input, input_shift, threshold);
        prop_assume! (time .is_some());
        let time = time.unwrap();
        if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input_shift, 0u32) { prop_assert!(coefficients[0][1] < threshold); }
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
        if let Some(first_coefficients) = coefficients.all_taylor_coefficients_bounds (input, input_shift, 0u32) { prop_assert!(first_coefficients[0][1] >= threshold); }
        if let Some(last_coefficients) = coefficients.all_taylor_coefficients_bounds (last_not_lt, input_shift, 0u32) {prop_assert!(last_coefficients[0][1] >= threshold); }
        let test_time = input + ((last_not_lt.saturating_sub(input)) as f64 * test_frac).floor() as $double;
        if let Some(test_coefficients) = coefficients.all_taylor_coefficients_bounds (test_time, input_shift, 0u32) {prop_assert!(test_coefficients[0][1] >= threshold); }
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
