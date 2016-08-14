use std::ops::{Add, Sub, Mul, Div, Neg, Shr, Shl};
use std::cmp::{max, min};


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


#[derive (Copy, Clone, Debug)]
pub struct Range {
min: i64,
max: i64,
exponent: u32,
}

fn right_shift_round_up (value: i64, shift: u32)->i64 {
(value >> shift) + if value & ((1i64 << shift) - 1) != 0 {1} else {0}
}



impl Range {
pub fn new (min: i64, max: i64)->Range {assert! (max >= min, "invalid Range"); Range {min: min, max: max, exponent: 0}}
pub fn exactly (value: i64)->Range {Range {min: value, max: value, exponent: 0}}
pub fn everywhere ()->Range {Range {min: - i64::max_value(), max: i64::max_value(), exponent: u32::max_value()}}
fn increase_exponent_to (&mut self, new: u32) {
assert! (new >= self.exponent);
let increase =new - self.exponent;
self.increase_exponent_by (increase);
}
fn increase_exponent_by (&mut self, increase: u32) {
self.min >>= increase;
self.max =right_shift_round_up (self.max, increase);
self.exponent += increase;
}
fn minimize_exponent(&mut self) {
while self.exponent >0 && self.min.abs() <(i64::max_value() >> 1) && self.max.abs() <(i64::max_value() >> 1) {
self.min <<= 1;
self.max <<= 1;
self.exponent -= 1;
}
}
pub fn includes_0 (&self)->bool {self.min <= 0 && self.max >= 0}
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

impl<'a> Neg for & 'a Range {
type Output = Range;
fn neg (self)->Range {
Range {min: - self.max, max: - self.min, exponent: self.exponent}
}
}
impl Neg for Range {
type Output = Range;
fn neg (self)->Range {
Range {min: - self.max, max: - self.min, exponent: self.exponent}
}
}



impl<'a> Add for & 'a Range {
type Output = Range;
fn add (self, other: Self)->Range {
let possibly_needed = max (self.exponent, other.exponent) + 1;
let mut result = self.clone();
let mut other = other.clone();
result.increase_exponent_to (possibly_needed);
other.increase_exponent_to (possibly_needed);
result.min += other.min;
result.max += other.max;
result.minimize_exponent();
result
}
}

binary_operation_fill! (Range, Range, Add, add);

impl<'a> Sub for &'a Range {
type Output = Range;
fn sub (self, other: Self)->Range {
self + (- other)
}
}

binary_operation_fill! (Range, Range, Sub, sub);


impl<'a> Mul for &'a Range {
type Output = Range;
fn mul(self, other: Self)->Range {
let mut result = self.clone();
let mut other = other.clone();
while result.min.abs() >= (1i64 << 32) || result.max.abs() >= (1i64 << 32) {result.increase_exponent_by (1)}
while other.min.abs() >= (1i64 << 32) || other.max.abs() >= (1i64 << 32) {other.increase_exponent_by (1)}
let mut extremes = [result.min*other.min, result.max*other.max, result.min*other.max, result.max*other.min];
extremes.sort();
result = Range {min: extremes [0], max: extremes [3], exponent: result.exponent + other.exponent};
result.minimize_exponent();
result
}
} 

impl<'a> Mul<& 'a i64>  for &'a Range {
type Output = Range;
fn mul(self, other: & 'a i64)->Range {
self*Range::exactly (other.clone())
}
} 


binary_operation_fill! (Range, Range, Mul, mul);
binary_operation_fill! (Range, i64, Mul, mul);




impl<'a> Div for &'a Range {
type Output = Range;
fn div (self, other: Self)->Range {
let mut result = self.clone();
let mut other = other.clone();
if other.min <= 0 && other.max >= 0 {return Range::everywhere()}
if other.max <0 {other = - other; result = - result;}

if other.exponent >result.exponent {
result.increase_exponent_to (other.exponent);
}
result.exponent -= other.exponent;

if result.min <0 {result.min = (result.min + 1 - other.min)/other.min;}
 else {result.min = (result.min)/other.max;}
 if result.max <0 {result.max = (result.max)/other.max;}
  else {result.min = (result.max - 1 + other.min)/other.min;}
result.minimize_exponent();
result
}
}

binary_operation_fill! (Range, Range, Div, div);


impl <'a> Shr <& 'a u32> for &'a Range {
type Output = Range;
fn shr (self, other: & u32)->Range {
let mut result = self.clone();
if result.exponent >= other.clone() {
  result.exponent -= other.clone();
  return result;
}
let mut shift = other.clone();
shift -= result.exponent;
result.exponent = 0;
result.min >>= shift;
result.max =right_shift_round_up (self.max, shift);
result
} 
}
binary_operation_fill! (Range, u32, Shr, shr);

impl <'a> Shl <& 'a u32> for &'a Range {
type Output = Range;
fn shl (self, other: & u32)->Range {
let mut result = self.clone();
result.exponent += other.clone();
result.minimize_exponent();
result
} 
}
binary_operation_fill! (Range, u32, Shl, shl);



impl Range {
///Squaring is a slightly narrower operation than self*self, because it never invokes (for instance) self.min*self.max.
fn squared (&self)->Range {
let mut result = self.clone();
while result.min.abs() >= (1i64 << 32) || result.max.abs() >= (1i64 << 32) {result.increase_exponent_by (1)}
if result.min <= 0 && result.max >= 0 {
  result.max = max (result.min*result.min, result.max*result.max);
  result.min = 0;
}
else {
  let mut extrema = [result.min*result.min, result.max*result.max];
  extrema.sort();
  result.max = extrema [1];
  result.min = extrema [0];
}
result.minimize_exponent();
result
}

fn sqrt (&self)->Option <Range> {
let mut result = self.clone();
if result.exponent % 2 == 1 {result.increase_exponent_by (1)}
result.exponent >>= 1;
if result.max <0 {return None;}
if result.min <0 {result.min = 0;}
let mut lower_bound = 0;
let mut upper_bound = 1i64 << 32;
let mut move_size = 1i64 << 31;
while move_size >0 {
if (lower_bound + move_size)*(lower_bound + move_size) <= result.min {lower_bound += move_size;}
if (upper_bound - move_size)*(upper_bound - move_size) >= result.max {upper_bound -= move_size;}
move_size >>= 1;
}
result.min = lower_bound;
result.max = upper_bound;
Some (result)
}
}




/**

A polynomial pseudo-solver, using Range.

Returns a collection of ranges that include the exact roots. False-positives are possible.

TODO: instead of Vec<Range>, these should return a stack-allocated type.

*/

pub fn roots_linear (coefficients: [Range; 2])->Vec<Range> {
  if coefficients [1].min == 0 && coefficients [1].max == 0 && (coefficients [0].min >0 ||coefficients [0].max <0) {return Vec:: new()}
  //TODO: there's also the case where exactly one of the linear term's extrema is 0, which makes the result be a half open interval... But we are, after all, allowed to return strict supersets of the actual result
  vec![(- coefficients [0])/coefficients [1]]
}
pub fn roots_quadratic (terms: [Range; 3])->Vec<Range> {
let a = terms [2];
let b = terms [1];
let c = terms [0];
let discriminant = b.squared() + a*c*Range::exactly (4);
//printlnerr!(" discriminant {:?}", discriminant);
if discriminant.max <0 {return Vec:: new();}
let sqrt = discriminant.sqrt().expect ("I thought we just ruled out the case where the square root would be nonexistent");
//printlnerr!(" sqrt {:?}", sqrt);
let result_0 = (- b - sqrt)/(a*2);
let result_1 = (- b + sqrt)/(a*2);
//printlnerr!(" result 0 {:?}", result_0);
//printlnerr!(" result 1 {:?}", result_1);
//if one of the results is this huge, something bad probably happened
if result_0.exponent >0 || result_1.exponent >0 {return vec![Range::everywhere()];}
if result_0.max >= result_1.min {
  vec![Range {min: result_0.min, max: result_1.max, exponent: 0}]
}
else {
vec![result_0, result_1]
}
}

fn find_root (terms: & [Range], min: i64, max: i64)->Option <Range> {
let min_value = evaluate (terms, min);
let max_value = evaluate (terms, max);
let direction;
if min_value.includes_0() {
if max_value.includes_0() {return Some (Range::new (min, max));}
direction = max_value.min.signum();
}
else if max_value.includes_0() {direction = - min_value.min.signum();}
else if max_value.min.signum() == min_value.min.signum(){return None;} else {
direction = max_value.min.signum();
}

let mut lower_bound = min;
let mut upper_bound = max;
let mut move_size = 1i64 << 63;
while move_size >max - min {move_size >>= 1}
while move_size >0 {
if (evaluate (terms, lower_bound + move_size)*direction).max < 0 {lower_bound += move_size;}
if (evaluate (terms, upper_bound - move_size)*direction).min > 0 {upper_bound -= move_size;}
move_size >>= 1;
}
Some (Range::new (lower_bound, upper_bound))

}
fn collect_root (terms: & [Range], min: i64, max: i64, bucket: &mut Vec<Range>) {find_root (terms, min, max).map (| root | bucket.push (root));}

pub fn roots (terms: & [Range])->Vec<Range> {
  match terms.len() {
    0 => vec![Range::everywhere()],
    1 => if terms [0].min <= 0 && terms [0].max >= 0 {vec![Range::everywhere()]} else {Vec:: new()},
    2 => roots_linear ([terms [0], terms [1]]),
    3 => roots_quadratic ([terms [0], terms [1], terms [2]]),
    size => {
      let derivative: Vec<Range> = terms [1..]. iter().enumerate().map (| (which, term) | term*(which as i64 + 1)).collect();
      let extrema = roots (derivative.as_slice());
      if extrema.iter().any (| range | range.exponent >0) {return vec![Range::everywhere()];}
      let mut results = Vec:: new();
      if extrema.is_empty() {collect_root (terms, - i64::max_value(),i64::max_value(), &mut results);}
      else {
        collect_root (terms, - i64::max_value(), extrema [0].min, &mut results);
        for which in 0..(extrema.len() - 1) {
          results.push (extrema [which]);
          if extrema [which].max <extrema [which +1].min {
            collect_root (terms, extrema [which].max, extrema [which +1].min , &mut results);
          }
        }
        collect_root (terms, extrema.last().unwrap().max, i64::max_value(), &mut results);
      }
      results
    }
  }
}

pub fn evaluate(terms: & [Range], input: i64)->Range {
let mut factor = Range::exactly (1);
let mut result = Range::exactly (0);
for term in terms.iter() {
result = result + (term*factor);
factor = factor*input;
}
result
}
  

#[test]
fn tests() {
printlnerr!(" {:?}", roots(& [Range::new (- 900, - 800), Range::new (500, 501), Range::exactly (50)]));
printlnerr!(" {:?}", roots(& [Range::new (- 900, - 800), Range::new (500, 501), Range::exactly (50), Range::exactly (1)]));
}
