use std::ops::{Add, Sub, Mul, Div, Neg, Shr, Shl};
use std::cmp::{max, min};
use std::iter::Sum;

use std::fmt;

use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


/**

A numeric-ish type for dealing with integer math that has possible rounding error.

A Range represents an inclusive range of real-number values.

(A: Range `operator` B: Range) yields a Range that is a (non-strict) superset of (for all a in A and all b in B, a `operator` b). Specifically, it chooses the smallest such Range that has integer endpoints. For addition, subtraction, and multiplication, this is a generalization of i64 math. For division, it is slightly different (effectively, it rounds in BOTH directions).

Range is also a partially floating-point type: it handles overflow by increasing an exponent value and rounding off. This rounding never removes elements from the Range, but sometimes adds new ones.

*/
#[derive (Copy, Clone, PartialEq, Eq, Hash)]
pub struct Range {
  min: i64,
  max: i64,
  exponent: u32,
}

impl fmt::Display for Range {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.exponent > 0 {
      write!(f, "Range:({},{})<<{}", self.min, self.max, self.exponent)
    } else {
      write!(f, "Range:({},{})", self.min, self.max)
    }
  }
}

impl fmt::Debug for Range {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.exponent > 0 {
      write!(f, "Range:({},{})<<{}", self.min, self.max, self.exponent)
    } else {
      write!(f, "Range:({},{})", self.min, self.max)
    }
  }
}



fn right_shift_round_up(value: i64, shift: u32) -> i64 {
  (value >> shift) +
  if value & ((1i64 << shift) - 1) != 0 {
    1
  } else {
    0
  }
}



impl Range {
  pub fn new(min: i64, max: i64) -> Range {
    assert!(max >= min, "invalid Range");
    let mut result = Range {
      min: min,
      max: max,
      exponent: 0,
    };
    if min == i64::min_value() {
      result.increase_exponent_by(1);
    }
    result
  }
  pub fn exactly(value: i64) -> Range {
    Range::new(value, value)
  }
  fn error_sized (value: i64) -> Range {
    if value <0 {Range::new(value, - value)} else { Range::new(- value, value)}
  }

  pub fn everywhere() -> Range {
    Range {
      min: -i64::max_value(),
      max: i64::max_value(),
      exponent: u32::max_value(),
    }
  }
  fn increase_exponent_to(&mut self, new: u32) {
    assert!(new >= self.exponent);
    let increase = new - self.exponent;
    self.increase_exponent_by(increase);
  }
  fn increase_exponent_by(&mut self, increase: u32) {
    let confirm = self.clone();
    if increase >= 63 {
      self.min = self.min.signum();
      self.max = self.max.signum();
    } else {
      self.min >>= increase;
      self.max = right_shift_round_up(self.max, increase); 
let mut confirm_2 = self.clone();
confirm_2.min <<= increase; confirm_2.max <<= increase; 
//printlnerr!("{}, {}", confirm, confirm_2);
assert! (confirm_2.includes (& confirm) || confirm_2.max <0);
    }
    self.exponent += increase;
  }
  fn minimize_exponent(&mut self) {
  let confirm = self.clone();
    let leeway = min (self.min.abs().leading_zeros(), self.max.abs().leading_zeros()) - 1;
    let change = min (leeway, self.exponent);
      self.min <<= change;
      self.max <<= change;
      self.exponent -= change;
    let mut confirm_2 = self.clone(); confirm_2.increase_exponent_to (confirm.exponent);
    assert! (confirm == confirm_2);
  }
  pub fn includes_0(&self) -> bool {
    self.min <= 0 && self.max >= 0
  }
fn includes(&self, other: & Range) -> bool {
    assert!(self.exponent == other.exponent);
      self.min <= other.min && self.max >= other.max}
  pub fn rounded_towards_0(&self) -> i64 {
    assert!(self.exponent == 0);
    if self.includes_0() {
      0
    } else if self.max < 0 {
      self.max
    } else {
      self.min
    }
  }
  pub fn min(&self) -> i64 {
    assert!(self.exponent == 0);
    self.min
  }
  pub fn max(&self) -> i64 {
    assert!(self.exponent == 0);
    self.max
  }
  pub fn clamp_to_0_exponent (&self)->Option <Range> {
    let mut result = self.clone();
    if self.exponent >= 63 {
      result.min = self.min.signum()*i64::max_value();
      result.max = self.max.signum()*i64::max_value();
    } else {
      result.min = self.min.saturating_mul(1i64 << self.exponent);
      result.max = self.max.saturating_mul(1i64 << self.exponent);
    }
    result.exponent = 0;
    if result.min == i64::max_value() || result.max <= - i64::max_value() {None} else {Some (result )}
  }
}

macro_rules! binary_operation_fill {
($implementor: ident, $joiner: ident, $operation: ident, $method:ident) => {


impl<'a> $operation <& 'a $joiner> for $implementor{
type Output = $implementor;
fn $method (self, other: & 'a $joiner)->$implementor{
(& self).$method (other)
}
}

impl<'a> $operation <$joiner> for & 'a $implementor{
type Output = $implementor;
fn $method (self, other: $joiner)->$implementor{
self.$method (& other)
}
}


impl $operation <$joiner> for $implementor{
type Output = $implementor;
fn $method (self, other: $joiner)->$implementor{
(& self).$method (& other)
}
}


}

}

impl<'a> Neg for &'a Range {
  type Output = Range;
  fn neg(self) -> Range {
    Range {
      min: -self.max,
      max: -self.min,
      exponent: self.exponent,
    }
  }
}
impl Neg for Range {
  type Output = Range;
  fn neg(self) -> Range {
    Range {
      min: -self.max,
      max: -self.min,
      exponent: self.exponent,
    }
  }
}



impl<'a> Add for &'a Range {
  type Output = Range;
  fn add(self, other: Self) -> Range {
    let possibly_needed = max(self.exponent, other.exponent);
    let mut result = self.clone();
    let mut other = other.clone();
    result.increase_exponent_to(possibly_needed);
    other.increase_exponent_to(possibly_needed);
    if result.min.checked_add(other.min).map_or(true, |result| result == i64::min_value()) ||
       result.max.checked_add(other.max).is_none() {
      result.increase_exponent_by(1);
      other.increase_exponent_by(1);
    }
    result.min += other.min;
    result.max += other.max;
    result.minimize_exponent();
    result
  }
}

binary_operation_fill! (Range, Range, Add, add);

impl<'a> Sub for &'a Range {
  type Output = Range;
  fn sub(self, other: Self) -> Range {
    self + (-other)
  }
}

binary_operation_fill! (Range, Range, Sub, sub);


impl<'a> Mul for &'a Range {
  type Output = Range;
  fn mul(self, other: Self) -> Range {
    let mut result = self.clone();
    let mut other = other.clone();
let result_high_bit = 63 - min (result.min.abs().leading_zeros(), result.max.abs().leading_zeros()) as i32;
let other_high_bit = 63 - min (other.min.abs().leading_zeros(), other.max.abs().leading_zeros()) as i32;
//for each value, it could be anything strictly less than (1 << high_bit+1). So when you multiply them together, it can be just below (1 << high_bit + high_bit + 2). Because increase_exponent_by can round towards a higher magnitude, we have to increase the leeway by one, eliminating "just below". The result must not exceed the 62nd bit.
 let overflow = result_high_bit + other_high_bit + 2  - 62;
if overflow >0 {
let bigger; let smaller; let difference; let shared;
 if result_high_bit >other_high_bit {
 bigger = &mut result; smaller = &mut other; difference = result_high_bit - other_high_bit; shared = other_high_bit;
 } else {bigger = &mut other; smaller = &mut result; difference = other_high_bit - result_high_bit; shared = result_high_bit;}
if overflow <=difference {bigger.increase_exponent_by (overflow as u32);} else {bigger.increase_exponent_by ((difference + (overflow - difference)/2) as u32); smaller.increase_exponent_by (((overflow + 1 - difference)/2) as u32);}
}
    let mut extremes = [result.min * other.min,
                        result.max * other.max,
                        result.min * other.max,
                        result.max * other.min];
    result = Range {
      min: extremes.iter().min ().unwrap().clone(),
      max: max (extremes [1], extremes [0]),
      exponent: result.exponent + other.exponent,
    };
    result.minimize_exponent();
    result
  }
}

impl<'a> Mul<&'a i64> for &'a Range {
  type Output = Range;
  fn mul(self, other: &'a i64) -> Range {
    self * Range::exactly(other.clone())
  }
}


binary_operation_fill! (Range, Range, Mul, mul);
binary_operation_fill! (Range, i64, Mul, mul);




impl<'a> Div for &'a Range {
  type Output = Range;
  fn div(self, other: Self) -> Range {
    let mut result = self.clone();
    let mut other = other.clone();
    
    if other.min < 0 {
      other = -other;
      result = -result;
    }

    if other.includes_0() {
      //TODO: what if if other.min == 0
      return Range::everywhere();
    }
    
    //intuitively, to minimize rounding error, denominator should have about half as many bits as numerator does when we do the actual division operation.
    //TODO: what if denominator is a VERY wide range (like -1, i64::max_value())? Then we will have big rounding problems either way
    if result.exponent >other.exponent {
    let leeway = min (other.min.abs().leading_zeros(), other.max.abs().leading_zeros()) ;
    if leeway <32 {
let shift =min (32 - leeway, result.exponent - other.exponent);
    other.increase_exponent_by (shift);}}
    
    if other.exponent > result.exponent {
      result.increase_exponent_to(other.exponent);
    }
    result.exponent -= other.exponent;
    
    if result.min.checked_sub (other.min).is_none() ||
       result.max.checked_add(other.min).is_none() {
      result.increase_exponent_by(1);
      other.increase_exponent_by(1);
    }


    if result.min < 0 {
      result.min = (result.min + 1 - other.min) / other.min;
    } else {
      result.min = (result.min) / other.max;
    }
    if result.max < 0 {
      result.max = (result.max) / other.max;
    } else {
      result.max = (result.max - 1 + other.min) / other.min;
    }
    result.minimize_exponent();
    result
  }
}

binary_operation_fill! (Range, Range, Div, div);


impl<'a> Shr<&'a u32> for &'a Range {
  type Output = Range;
  fn shr(self, other: &u32) -> Range {
    let mut result = self.clone();
    if result.exponent >= other.clone() {
      result.exponent -= other.clone();
      return result;
    }
    result.increase_exponent_to (other.clone());
    result.exponent = 0;
    result
  }
}
binary_operation_fill! (Range, u32, Shr, shr);

impl<'a> Shl<&'a u32> for &'a Range {
  type Output = Range;
  fn shl(self, other: &u32) -> Range {
    let mut result = self.clone();
    result.exponent += other.clone();
    result.minimize_exponent();
    result
  }
}
binary_operation_fill! (Range, u32, Shl, shl);




impl Sum for Range {
  fn sum<I>(iter: I) -> Self
    where I: Iterator<Item = Range>
  {
    let mut result = Range::exactly(0);
    for value in iter {
      result = result + value;
    }
    result
  }
}



impl Range {
  ///Squaring is a slightly narrower operation than self*self, because it never invokes (for instance) self.min*self.max.
  pub fn squared(&self) -> Range {
    let mut result = self.clone();
    let leeway = min (self.min.abs().leading_zeros(), self.max.abs().leading_zeros()) ;
    if leeway <33 {
      result.increase_exponent_by(33 - leeway);
    }
    result.exponent <<= 1;
    if result.includes_0 () {
      result.max = max(result.min * result.min, result.max * result.max);
      result.min = 0;
    } else {
      let mut extrema = [result.min * result.min, result.max * result.max];
      if extrema [0] <extrema [1] {
      result.max = extrema[1];
      result.min = extrema[0];} else {result.max = extrema [0]; result.min = extrema [1];}
    }
    result.minimize_exponent();
    result
  }

  pub fn sqrt(&self) -> Option<Range> {
    let mut result = self.clone();
    if result.exponent % 2 == 1 {
      result.increase_exponent_by(1)
    }
    result.exponent >>= 1;
    if result.max < 0 {
      return None;
    }
    if result.min < 0 {
      result.min = 0;
    }
    let mut lower_bound = 0;
    let mut upper_bound = 3037000500i64;
    let mut move_size = 1i64 << 31;
    while move_size > 0 {
      if lower_bound + move_size <= upper_bound &&
         (lower_bound + move_size) * (lower_bound + move_size) <= result.min {
        lower_bound += move_size;
      }
      if upper_bound - move_size >= lower_bound &&
         (upper_bound - move_size) * (upper_bound - move_size) >= result.max {
        upper_bound -= move_size;
      }
      move_size >>= 1;
    }
    result.min = lower_bound;
    result.max = upper_bound;
    result.minimize_exponent();
    let confirm = result.squared();
    let mut confirmation = self.clone(); if confirmation.min <0 {confirmation.min = 0;} confirmation.minimize_exponent();
    assert!(confirm.exponent >confirmation.exponent || confirm.includes (& confirmation));
    Some(result)
  }
}




/**

A polynomial pseudo-solver, using Range.

Returns a collection of ranges that include the exact roots. False-positives are possible.

TODO: instead of Vec<Range>, these should return a stack-allocated type.

*/

pub fn roots_linear(coefficients: [Range; 2]) -> Vec<Range> {
  if coefficients[1].min == 0 && coefficients[1].max == 0 &&
     (coefficients[0].min > 0 || coefficients[0].max < 0) {
    return Vec::new();
  }
  if let Some (result) = ((-coefficients[0]) / coefficients[1]).clamp_to_0_exponent () {vec![result]} else {Vec:: new()}
}
pub fn roots_quadratic(terms: [Range; 3]) -> Vec<Range> {
  let a = terms[2];
  let b = terms[1];
  let c = terms[0];
  let discriminant = b.squared() - a * c * Range::exactly(4);
  // printlnerr!(" discriminant {:?}", discriminant);
  //printlnerr!("confirm results: {:?}", roots_derivative_based (& terms));
  
  if discriminant.max < 0 {
    return Vec::new();
  }
  let sqrt = discriminant.sqrt()
                         .expect("I thought we just ruled out the case where the square root \
                                  would be nonexistent");
  // printlnerr!(" sqrt {:?}", sqrt);
  let result_0 = (-b - sqrt) / (a * 2);
  let result_1 = (-b + sqrt) / (a * 2);
  // printlnerr!(" result 0 {:?}", result_0);
  // printlnerr!(" result 1 {:?}", result_1);
let mut results = Vec:: new();  
if let Some (result) = result_0.clamp_to_0_exponent () {results.push (result)}
if let Some (result) = result_1.clamp_to_0_exponent () {results.push (result)}

//printlnerr!("My results: {:?}", results);
return results;
 /*if result_0.max >= result_1.min {
    vec![Range {
           min: result_0.min,
           max: result_1.max,
           exponent: 0,
         }]
  } else {
    vec![result_0, result_1]
  }*/
}

fn find_root(terms: &[Range], min: i64, max: i64) -> Option<Range> {
  if min == max {return None;}
  let min_value = evaluate(terms, min);
  let max_value = evaluate(terms, max);
  // printlnerr!(" Values {:?}:{:?}, {:?}:{:?}", min, min_value, max, max_value);

  let direction;
  if min_value.includes_0() {
    if max_value.includes_0() {
      return Some(Range::new(min, max));
    }
    direction = max_value.min.signum();
  } else if max_value.includes_0() {
    direction = -min_value.min.signum();
  } else if max_value.min.signum() == min_value.min.signum() {
    return None;
  } else {
    direction = max_value.min.signum();
  }

  let mut lower_bound = min;
  let mut upper_bound = max;
  // hack: use a negative number for move_size so that it can store a slightly larger value
  let mut move_size = -1i64 << 63;
  while min.checked_sub(max).is_some() && move_size < min - max {
    move_size /= 2;
  }
  while move_size < 0 {
    // printlnerr!(" Next values {:?}:{:?}, {:?}:{:?}", lower_bound, evaluate (terms, lower_bound ), upper_bound, evaluate (terms, upper_bound ));

    if lower_bound - move_size <= max &&
       (evaluate(terms, lower_bound - move_size) * direction).max <= 0 {
      lower_bound -= move_size;
    }
    if upper_bound + move_size >= min &&
       (evaluate(terms, upper_bound + move_size) * direction).min >= 0 {
      upper_bound += move_size;
    }
    move_size /= 2;
  }
  Some(Range::new(lower_bound, upper_bound))

}
fn collect_root(terms: &[Range], min: i64, max: i64, bucket: &mut Vec<Option<Range>>) {
  bucket.push(find_root(terms, min, max));
}
fn roots_derivative_based (terms: &[Range]) -> Vec<Range> {
  
      let derivative: Vec<Range> = terms[1..]
                                     .iter()
                                     .enumerate()
                                     .map(|(which, term)| term * (which as i64 + 1))
                                     .collect();
      //printlnerr!(" Derivative {:?}", derivative);
      let extrema = roots(derivative.as_slice());
      //printlnerr!("extrema {:?}", extrema);
      if extrema.iter().any(|range| range.exponent > 0) {
        panic!();
      }
      let mut bucket = Vec::new();
      let mut results = Vec::new();
      if extrema.is_empty() {
        collect_root(terms, -i64::max_value(), i64::max_value(), &mut bucket);
      } else {
        collect_root(terms, -i64::max_value(), extrema[0].min, &mut bucket);
        for which in 0..(extrema.len() - 1) {
          if extrema[which].max < extrema[which + 1].min {
            collect_root(terms,
                         extrema[which].max,
                         extrema[which + 1].min,
                         &mut bucket);
          } else {
            bucket.push(None);
          }
        }
        collect_root(terms,
                     extrema.last().unwrap().max,
                     i64::max_value(),
                     &mut bucket);
      }
      // if we found a root on both sides of a derivative-root, we know that the derivative-root is bounded away from 0
      for which in 0..extrema.len() {
        let me = extrema[which];
        if let Some(lower) = bucket[which] {
          results.push(lower);
          if let Some(higher) = bucket[which + 1] {
            if me.min > lower.max && me.max < higher.min {
              continue;
            }
          }
        }
        results.push(me);
      }
      if let Some(lower) = bucket[extrema.len()] {
        results.push(lower);
      }

      results

  }
pub fn roots(terms: &[Range]) -> Vec<Range> {
  let mut terms = terms;
  while terms.last().map_or(false, |term| term == &Range::exactly(0)) {
    terms = &terms[..terms.len() - 1]
  }
  match terms.len() {
    0 => vec![Range::everywhere()],
    1 => {
      if terms[0].min <= 0 && terms[0].max >= 0 {
        vec![Range::everywhere()]
      } else {
        Vec::new()
      }
    }
    2 => roots_linear([terms[0], terms[1]]),
    3 => roots_quadratic([terms[0], terms[1], terms[2]]),
_=> { roots_derivative_based (terms)   }
  }
}

pub fn evaluate(terms: &[Range], input: i64) -> Range {
  let mut factor = Range::exactly(1);
  let mut result = Range::exactly(0);
  for term in terms.iter() {
    result = result + (term * factor);
    factor = factor * input;
  }
  result
}

pub fn multiply_polynomials(terms_0: &[Range], terms_1: &[Range]) -> Vec<Range> {
  (0..terms_0.len() + terms_1.len() - 1)
    .map(|new_index| {
      (max(terms_1.len(), new_index + 1) - terms_1.len()..min(terms_0.len(), new_index + 1))
        .map(|view| terms_0[view] * terms_1[new_index - view])
        .sum()
    })
    .collect()
}
use ::rand::Rng; use rand;
//when coercing the update to land on an integer value, we have a possible rounding error of up to 2 units (one from dividing the velocity, one from dividing the acceleration). But that's not all – the multiplications also have rounding error if they have to prevent overflows, which can be up to a factor of about 1+2^{-29} (Off by 2^{-31} since we are only guaranteed to keep the top 31 bits of each factor, double because there are 2 factors, and double again because we do 2 multiplications in a row in one place – and there's even a little more because they combine multiplicatively). That's usually pretty small, but if the result is large, it's large too.
//TODO: find way to compute the error automatically, so I don't have to rely on having not made any mistakes
//because having a quadratic error term caused too much trouble, we force it to become a constant error term by limiting the total time you can skip with one update
pub fn quadratic_move_origin_rounding_change_towards_0(terms: &mut [i64],
                                                       origin: i64,
                                                       input_scale_shift: u32)->bool {

if ((Range::exactly(terms[2].abs())  * origin * origin) >> (input_scale_shift*2 + 28)).max >2 { printlnerr!("overflow-ish in quadratic_move_origin_rounding_change_towards_0; trying to use rounding to prevent this overflow would increase the error size beyond our arbitrary limit"); return false;}
                                                       let between_time = rand::thread_rng().gen_range (0, origin + 1)                      ;                                
                                                       let confirm =quadratic_future_proxy_minimizing_error (terms, between_time, input_scale_shift);
  terms[0] += (((Range::exactly(terms[1]) * origin) >> input_scale_shift) +
               ((Range::exactly(terms[2]) * origin * origin) >> (input_scale_shift * 2)))
                .rounded_towards_0();
  terms[1] += ((Range::exactly(terms[2]) * origin) >> (input_scale_shift - 1)).rounded_towards_0();
  let experimented =(evaluate (& confirm, origin - between_time) >> (input_scale_shift*2 + MAX_ERROR_SHIFT));
  //printlnerr!("experimented {}, actually {}", experimented, terms [0]);
  assert! (experimented.includes (& Range::exactly(terms [0])));
  true
}

const MAX_ERROR_SHIFT: u32 = 30;

pub fn quadratic_future_proxy_minimizing_error(terms: &[i64],
                                               origin: i64,
                                               input_scale_shift: u32)
                                               -> [Range; 3] {
  //in the constant term, preserve the error of 2 units noted above.
  //Multiplication error term is about (term 1*time since original origin) >> 30+shift + (term 2*time since original origin squared) >> 29+shift*2
  //but time since original origin is actually "origin" + the input of the quadratic we're creating,
  //this error is actually quadratic.
  [(Range::new(terms[0] - 2 - 2, terms[0] + 2 + 2) << (input_scale_shift * 2 +MAX_ERROR_SHIFT)) +
   ((Range::exactly(terms[1]) * origin) << (input_scale_shift +MAX_ERROR_SHIFT)) +
   ((Range::exactly(terms[2]) * origin * origin) <<MAX_ERROR_SHIFT) +
   
   //error term
   //((Range::error_sized(terms[1]) * origin) << (input_scale_shift +MAX_ERROR_SHIFT - 30)) +
   //((Range::error_sized(terms[2]) * origin * origin) << (MAX_ERROR_SHIFT - 28)) +
   Range::exactly(0),

   (Range::exactly(terms[1]) << (input_scale_shift +MAX_ERROR_SHIFT)) +
   ((Range::exactly(terms[2]) * origin) << (MAX_ERROR_SHIFT + 1)) +
   
   //error term
   //(Range::error_sized(terms[1]) << (input_scale_shift + MAX_ERROR_SHIFT - 30)) + 
   //((Range::error_sized(terms[2])*origin) << (MAX_ERROR_SHIFT + 1 - 28)) +
   Range::exactly(0),

   (Range::exactly(terms[2]) <<MAX_ERROR_SHIFT) +
   
   //error term
   //(Range::error_sized (terms[2]) << (MAX_ERROR_SHIFT - 28)) +
   Range::exactly(0)]
}



pub fn quadratic_trajectories_possible_distance_crossing_intervals(distance: i64,
                                                                   first: (i64, &[[i64; 3]]),
                                                                   second: (i64, &[[i64; 3]]),
                                                                   input_scale_shift: u32)
                                                                   -> Vec<Range> {
  assert!(first.1.len() == second.1.len());
  assert!(first.1.len() > 0);
  let base = max(first.0, second.0);
  let mut proxy = [Range::exactly(0),
                   Range::exactly(0),
                   Range::exactly(0),
                   Range::exactly(0),
                   Range::exactly(0)];
  for (third, more) in first.1.iter().zip(second.1.iter()) {
    let mut rubble = quadratic_future_proxy_minimizing_error(third.as_ref(),
                                                             base - first.0,
                                                             input_scale_shift);
    let bravo = quadratic_future_proxy_minimizing_error(more.as_ref(), base - second.0, input_scale_shift);
    for index in 0..3 {
      rubble[index] = rubble[index] - bravo[index];
    }
    for (which, value) in multiply_polynomials(&rubble, &rubble).into_iter().enumerate() {
      proxy[which] = proxy[which] + value
    }
  }
  proxy[0] = proxy[0] - (Range::exactly(distance).squared() << (input_scale_shift * 4 +MAX_ERROR_SHIFT*2));
let real_distance_squared = | input | {
  let mut result = 0;
  for (third, more) in first.1.iter().zip(second.1.iter()) {
    let mut rubble = third.clone();
    if!quadratic_move_origin_rounding_change_towards_0(&mut rubble,
input - first.0,
                                                             input_scale_shift) {return None;}
    let mut bravo = more.clone(); if!quadratic_move_origin_rounding_change_towards_0(&mut bravo, input - second.0, input_scale_shift) {return None;}
    for index in 0..3 {
      rubble[index] = rubble[index] - bravo[index];
    }
    result += rubble [0]*rubble [0];
  }
Some (result)

};
let test = | input | {
let evaluated =evaluate (& proxy, input) >> (input_scale_shift*4 + MAX_ERROR_SHIFT*2);
//printlnerr!("input: {}, base: {}, evaluated: {}", input, base, evaluated);
if input <0 || input >1i64 << 32 {return evaluated;}
if let Some (distance_squared) =real_distance_squared (input + base) {
let real = distance_squared - distance*distance;
//printlnerr!("real: {}", real);
assert! ((evaluated).includes (& Range::exactly (real)));
}
evaluated
};
let test_empty_interval = | start, stop | {
if start >= stop {return;}
let sample_points: Vec<i64> = vec![start, stop, rand::thread_rng().gen_range (start, stop), rand::thread_rng().gen_range (start, stop), rand::thread_rng().gen_range (start, stop)];
let sample_values: Vec<Range> = sample_points.iter().map (| input | test (input.clone())).collect();
let signum = sample_values [0].min.signum();
for value in sample_values.iter() {
assert!(!value.includes_0());
assert!(value.min.signum() == signum);
}

};

  //printlnerr!(" Proxy: {:?}", proxy);
  let mut result = roots(proxy.as_ref());
  //printlnerr!(" Proxy: {:?}\n Roots: {:?}", proxy, result);
  test (0);
  test (1000);
  test (base);
  for (which, root) in result.iter().enumerate() {
  test ((root.max - root.min)/2);
  if which == 0 {test_empty_interval (- i64::max_value(), root.min - 1);}
  if which <result.len() - 1 {test_empty_interval (root.max + 1, result [which + 1].min - 1);}
  else {test_empty_interval (root.max + 1, i64::max_value());}
//printlnerr!("root check: {}: {} and then {} and then {}", root, evaluate (& proxy, root.max - 1),  evaluate (& proxy, root.max), evaluate (& proxy, root.max + 1));
}
  for (which, root) in result.iter_mut().enumerate() {
    root.min = root.min.saturating_add (base);
    root.max = root.max.saturating_add (base);
  }
  result
}


fn test_roots(given_roots: Vec<Range>) {
  let mut polynomial = vec![Range::exactly(1)];
  for root in given_roots.iter() {
    polynomial = multiply_polynomials(polynomial.as_slice(), &[-root, Range::exactly(1)])
  }
  let computed = roots(polynomial.as_slice());
  printlnerr!("\nFor roots {:?}\n  Computed polynomial {:?}\n  And roots {:?}\n  Evaluated root \
               minima: {:?}",
              given_roots,
              polynomial,
              computed,
              given_roots.iter()
                         .map(|root| evaluate(polynomial.as_slice(), root.min))
                         .collect::<Vec<Range>>());
}


//#[test]
fn tests() {
  printlnerr!(" {:?}", Range::exactly(-47).squared());
  assert!(Range::exactly(-47).squared() == Range::exactly(2209));
  assert!(Range::exactly(-440) * Range::exactly(1) == Range::exactly(-440));
  assert!(Range::exactly(-440) * Range::exactly(1) * 4 == Range::exactly(-1760));


  assert!(Range::exactly(99) / Range::exactly(2) == Range::new(49, 50));
  assert!(Range::exactly(3) / Range::exactly(2) == Range::new(1, 2));
  assert!(Range::exactly(100) / Range::exactly(2) == Range::exactly(50));
  assert!(Range::exactly(4) / Range::exactly(2) == Range::exactly(2));
  assert!(Range::exactly(0) / Range::exactly(5) == Range::exactly(0));
  test_roots(vec![Range::exactly(0)]);
  test_roots(vec![Range::exactly(55)]);
  test_roots(vec![Range::exactly(0), Range::exactly(55)]);
  test_roots(vec![Range::exactly(-8), Range::exactly(55)]);
  test_roots(vec![Range::exactly(-8), Range::exactly(55), Range::exactly(999)]);
  test_roots(vec![Range::exactly(-8),
                  Range::exactly(55),
                  Range::exactly(999),
                  Range::exactly(-84)]);
  test_roots(vec![Range::exactly(-8),
                  Range::exactly(55),
                  Range::exactly(999),
                  Range::exactly(-84),
                  Range::exactly(-1967)]);

  test_roots(vec![Range::new(-1, 1), Range::new(54, 56)]);
  test_roots(vec![Range::new(-9, -7), Range::new(50, 60)]);
  test_roots(vec![Range::new(-9, -7), Range::new(54, 56), Range::exactly(999)]);
  test_roots(vec![Range::new(-9, -7),
                  Range::new(54, 56),
                  Range::new(950, 1050),
                  Range::new(-90, -80)]);
  test_roots(vec![Range::new(-9, -7),
                  Range::new(54, 56),
                  Range::new(950, 1050),
                  Range::new(-90, -80),
                  Range::new(-1967, -1940)]);





  printlnerr!(" {:?}",
              roots(&[Range::new(-900, -800), Range::new(500, 501), Range::exactly(50)]));
  printlnerr!(" {:?}",
              roots(&[Range::new(-900, -800),
                      Range::new(500, 501),
                      Range::exactly(50),
                      Range::exactly(1)]));
}
