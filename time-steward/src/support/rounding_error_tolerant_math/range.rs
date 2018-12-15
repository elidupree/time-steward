use quickcheck::{Arbitrary, Gen};
use std::cmp::{max, min, Ordering};
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, Div, Mul, Neg, Shl, Shr, Sub};

/**

A numeric-ish type for dealing with integer math that has possible rounding error.

A Range represents an inclusive range of real-number values.

(A: Range `operator` B: Range) yields a Range that is a (non-strict) superset of (for all a in A and all b in B, a `operator` b). Specifically, it chooses the smallest such Range that has integer endpoints. For addition, subtraction, and multiplication, this is a generalization of i64 math. For division, it is slightly different (effectively, it rounds in BOTH directions).

Range is also a partially floating-point type: it handles overflow by increasing an exponent value and rounding off. This rounding never removes elements from the Range, but sometimes adds new ones.

*/
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
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

pub fn right_shift_round_up(value: i64, shift: u32) -> i64 {
  (value >> shift)
    + if value & ((1i64 << shift) - 1) != 0 {
      1
    } else {
      0
    }
}
pub fn average_round_towards_neginf(input_1: i64, input_2: i64) -> i64 {
  (input_1 >> 1) + (input_2 >> 1) + (input_1 & input_2 & 1)
}

pub fn overflow_checked_shift_left(value: i64, shift: u32) -> Option<i64> {
  if value == 0 {
    return Some(0);
  }
  if shift > 63 {
    return None;
  }
  if (value.abs() & !((1i64 << (63 - shift)).wrapping_sub(1))) != 0 {
    return None;
  }
  Some(value << shift)
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
  pub fn new_either_order(min: i64, max: i64) -> Range {
    if min > max {
      Range::new(max, min)
    } else {
      Range::new(min, max)
    }
  }
  pub fn exactly(value: i64) -> Range {
    Range::new(value, value)
  }
  pub fn zero() -> Range {
    Range {
      min: 0,
      max: 0,
      exponent: 0,
    }
  }
  pub fn error_sized(value: i64) -> Range {
    if value < 0 {
      Range::new(value, -value)
    } else {
      Range::new(-value, value)
    }
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
    if increase == 0 {
      return;
    }
    self.increase_exponent_by(increase);
  }
  fn increase_exponent_by(&mut self, increase: u32) {
    // let confirm = self.clone();
    if increase >= 63 {
      self.min = self.min.signum();
      self.max = self.max.signum();
    } else {
      self.min >>= increase;
      self.max = right_shift_round_up(self.max, increase);
      /*let mut confirm_2 = self.clone();
      confirm_2.min <<= increase;
      confirm_2.max <<= increase;
      // printlnerr!("{}, {}", confirm, confirm_2);
      assert!(confirm_2.includes(&confirm) || confirm_2.max < 0);*/
    }
    self.exponent += increase;
  }
  fn minimize_exponent(&mut self) {
    let mut confirm = Range::zero();
    if cfg!(debug_assertions) {
      confirm = self.clone();
    }
    if self.exponent == 0 {
      return;
    }
    if self.min == 0 && self.max == 0 {
      self.exponent = 0;
      return;
    }
    let leeway = min(
      self.min.abs().leading_zeros(),
      self.max.abs().leading_zeros(),
    ) - 1;
    let change = min(leeway, self.exponent);
    self.min <<= change;
    self.max <<= change;
    self.exponent -= change;
    if cfg!(debug_assertions) {
      if self.min <= self.max {
        assert!(self.exponent_is_minimized());
      }
      assert!(
        self.exponent == 0
          || self
            .min
            .checked_mul(2)
            .map_or(true, |result| result == i64::min_value())
          || self
            .max
            .checked_mul(2)
            .map_or(true, |result| result == i64::min_value())
      );
      let mut confirm_2 = self.clone();
      confirm_2.increase_exponent_to(confirm.exponent);
      assert!(confirm == confirm_2);
    }
  }
  fn exponent_is_minimized(&self) -> bool {
    debug_assert!(self.min <= self.max);
    self.exponent == 0 || self.min <= (-1i64 << 62) || self.max >= (1i64 << 62)
  }
  pub fn includes_0(&self) -> bool {
    self.min <= 0 && self.max >= 0
  }
  pub fn includes_0_strictly(&self) -> bool {
    self.min < 0 && self.max > 0
  }
  pub fn includes(&self, other: &Range) -> bool {
    if self.exponent < other.exponent {
      return false;
    }
    if let Some(bound) = overflow_checked_shift_left(self.min, self.exponent - other.exponent) {
      if bound > other.min {
        return false;
      }
    }
    if let Some(bound) = overflow_checked_shift_left(self.max, self.exponent - other.exponent) {
      if bound < other.max {
        return false;
      }
    }
    true
  }
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
  pub fn min_signum(&self) -> i64 {
    self.min.signum()
  }
  pub fn max_signum(&self) -> i64 {
    self.max.signum()
  }
  pub fn internal_min(&self) -> i64 {
    self.min
  }
  pub fn internal_max(&self) -> i64 {
    self.max
  }
  pub fn exponent(&self) -> u32 {
    self.exponent
  }

  pub fn clamp_to_0_exponent(&self) -> Option<Range> {
    let mut result = self.clone();
    if self.exponent >= 63 {
      result.min = self.min.signum() * i64::max_value();
      result.max = self.max.signum() * i64::max_value();
    } else {
      result.min = self.min.saturating_mul(1i64 << self.exponent);
      result.max = self.max.saturating_mul(1i64 << self.exponent);
    }
    result.exponent = 0;
    if result.min == i64::max_value() || result.max <= -i64::max_value() {
      None
    } else {
      Some(result)
    }
  }
  pub fn rounded_to_middle_towards_neginf(&self) -> Range {
    // this could sometimes be slightly more accurate when the exponent is not 0,
    // but it probably isn't worth the complication
    let middle = average_round_towards_neginf(self.min, self.max);
    let mut result = Range {
      min: middle,
      max: middle,
      exponent: self.exponent,
    };
    result.minimize_exponent();
    result
  }
}

macro_rules! binary_operation_fill {
  ($implementor: ident, $joiner: ident, $operation: ident, $method:ident) => {
    impl<'a> $operation<&'a $joiner> for $implementor {
      type Output = $implementor;
      fn $method(self, other: &'a $joiner) -> $implementor {
        (&self).$method(other)
      }
    }

    impl<'a> $operation<$joiner> for &'a $implementor {
      type Output = $implementor;
      fn $method(self, other: $joiner) -> $implementor {
        self.$method(&other)
      }
    }

    impl $operation<$joiner> for $implementor {
      type Output = $implementor;
      fn $method(self, other: $joiner) -> $implementor {
        (&self).$method(&other)
      }
    }
  };
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
    if result
      .min
      .checked_add(other.min)
      .map_or(true, |result| result == i64::min_value())
      || result.max.checked_add(other.max).is_none()
    {
      result.increase_exponent_by(1);
      other.increase_exponent_by(1);
    }
    result.min += other.min;
    result.max += other.max;
    result.minimize_exponent();
    result
  }
}

binary_operation_fill!(Range, Range, Add, add);

impl<'a> Sub for &'a Range {
  type Output = Range;
  fn sub(self, other: Self) -> Range {
    self + (-other)
  }
}

binary_operation_fill!(Range, Range, Sub, sub);

impl<'a> Mul for &'a Range {
  type Output = Range;
  fn mul(self, other: Self) -> Range {
    let mut result = self.clone();
    let mut other = other.clone();
    let result_high_bit = 63
      - min(
        result.min.abs().leading_zeros(),
        result.max.abs().leading_zeros(),
      ) as i32;
    let other_high_bit = 63
      - min(
        other.min.abs().leading_zeros(),
        other.max.abs().leading_zeros(),
      ) as i32;
    // for each value, it could be anything strictly less than (1 << high_bit+1). So when you multiply them together, it can be just below (1 << high_bit + high_bit + 2). Because increase_exponent_by can round towards a higher magnitude, we have to increase the leeway by one, eliminating "just below". The result must not exceed the 62nd bit.
    let overflow = result_high_bit + other_high_bit + 2 - 62;
    if overflow > 0 {
      let bigger;
      let smaller;
      let difference;
      if result_high_bit > other_high_bit {
        bigger = &mut result;
        smaller = &mut other;
        difference = result_high_bit - other_high_bit;
      } else {
        bigger = &mut other;
        smaller = &mut result;
        difference = other_high_bit - result_high_bit;
      }
      if overflow <= difference {
        bigger.increase_exponent_by(overflow as u32);
      } else {
        bigger.increase_exponent_by((difference + (overflow - difference) / 2) as u32);
        smaller.increase_exponent_by(((overflow + 1 - difference) / 2) as u32);
      }
    }
    let extremes = [
      result.min * other.min,
      result.max * other.max,
      result.min * other.max,
      result.max * other.min,
    ];
    result = Range {
      min: extremes[0],
      max: extremes[0],
      exponent: result.exponent + other.exponent,
    };
    for extreme in extremes[1..].iter() {
      if *extreme < result.min {
        result.min = *extreme;
      }
      if *extreme > result.max {
        result.max = *extreme;
      }
    }
    result.minimize_exponent();
    //debug_assert! (result.exponent_is_minimized());
    result
  }
}

impl<'a> Mul<&'a i64> for &'a Range {
  type Output = Range;
  fn mul(self, other: &'a i64) -> Range {
    let mut result = self.clone();
    let result_high_bit = 63
      - min(
        result.min.abs().leading_zeros(),
        result.max.abs().leading_zeros(),
      ) as i32;
    let other_high_bit = 63 - other.abs().leading_zeros() as i32;
    // for each value, it could be anything strictly less than (1 << high_bit+1). So when you multiply them together, it can be just below (1 << high_bit + high_bit + 2). Because increase_exponent_by can round towards a higher magnitude, we have to increase the leeway by one, eliminating "just below". The result must not exceed the 62nd bit.
    let overflow = result_high_bit + other_high_bit + 2 - 62;
    if overflow > 0 {
      if result_high_bit - overflow < 31 {
        return self * Range::exactly(other.clone());
      }
      result.increase_exponent_by(overflow as u32);
    }
    if *other >= 0 {
      result.min *= *other;
      result.max *= *other;
    } else {
      result = Range {
        min: result.max * other,
        max: result.min * other,
        exponent: result.exponent,
      };
    }
    result.minimize_exponent();
    //debug_assert! (result.exponent_is_minimized());
    result
  }
}

binary_operation_fill!(Range, Range, Mul, mul);
binary_operation_fill!(Range, i64, Mul, mul);

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
      // TODO: what if if other.min == 0
      return Range::everywhere();
    }

    // intuitively, to minimize rounding error, denominator should have about half as many bits as numerator does when we do the actual division operation.
    // But what if denominator is a VERY wide range (like 1, i64::max_value())?
    // Handle dividing by denominator.min first. Sometimes, denominator.max isn't even used.
    if result.exponent > other.exponent {
      let leeway = other.min.abs().leading_zeros();
      if leeway < 32 {
        let shift = min(32 - leeway, result.exponent - other.exponent);
        other.increase_exponent_by(shift);
      }
    }

    if other.exponent > result.exponent {
      result.increase_exponent_to(other.exponent);
    }
    result.exponent -= other.exponent;

    if result.min < 0 {
      result.min = (result.min + 1) / other.min - 1;
    }
    if result.max > 0 {
      result.max = (result.max - 1) / other.min + 1;
    }
    result.minimize_exponent();

    // TODO: we might still be able to reduce the rounding error further in these cases:
    if result.min > 0 {
      result.min = result.min / other.max;
      result.minimize_exponent();
    }
    if result.max < 0 {
      result.max = result.max / other.max;
      result.minimize_exponent();
    }
    result
  }
}

binary_operation_fill!(Range, Range, Div, div);

impl<'a> Shr<&'a u32> for &'a Range {
  type Output = Range;
  fn shr(self, other: &u32) -> Range {
    let mut result = self.clone();
    if result.exponent >= other.clone() {
      result.exponent -= other.clone();
      return result;
    }
    result.increase_exponent_to(other.clone());
    result.exponent = 0;
    result
  }
}
binary_operation_fill!(Range, u32, Shr, shr);

impl<'a> Shl<&'a u32> for &'a Range {
  type Output = Range;
  fn shl(self, other: &u32) -> Range {
    let mut result = self.clone();
    result.exponent += other.clone();
    result.minimize_exponent();
    result
  }
}
binary_operation_fill!(Range, u32, Shl, shl);

impl Sum for Range {
  fn sum<I>(iter: I) -> Self
  where
    I: Iterator<Item = Range>,
  {
    let mut result = Range::exactly(0);
    for value in iter {
      result = result + value;
    }
    result
  }
}

impl Range {
  pub fn abs(&self) -> Range {
    // this could be made more efficient
    Range {
      min: if self.includes_0() {
        0
      } else {
        min(self.min.abs(), self.max.abs())
      },
      max: max(self.min.abs(), self.max.abs()),
      exponent: self.exponent,
    }
  }

  /// Squaring is a slightly narrower operation than self*self, because it never invokes (for instance) self.min*self.max.
  pub fn squared(&self) -> Range {
    let mut result = self.clone();
    let leeway = min(
      self.min.abs().leading_zeros(),
      self.max.abs().leading_zeros(),
    );
    if leeway < 33 {
      result.increase_exponent_by(33 - leeway);
    }
    result.exponent <<= 1;
    if result.includes_0() {
      result.max = max(result.min * result.min, result.max * result.max);
      result.min = 0;
    } else {
      let extrema = [result.min * result.min, result.max * result.max];
      if extrema[0] < extrema[1] {
        result.max = extrema[1];
        result.min = extrema[0];
      } else {
        result.max = extrema[0];
        result.min = extrema[1];
      }
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
      if lower_bound + move_size <= upper_bound
        && (lower_bound + move_size) * (lower_bound + move_size) <= result.min
      {
        lower_bound += move_size;
      }
      if upper_bound - move_size >= lower_bound
        && (upper_bound - move_size) * (upper_bound - move_size) >= result.max
      {
        upper_bound -= move_size;
      }
      move_size >>= 1;
    }
    result.min = lower_bound;
    result.max = upper_bound;
    result.minimize_exponent();
    if cfg!(debug_assertions) {
      let confirm = result.squared();
      let mut confirmation = self.clone();
      if confirmation.min < 0 {
        confirmation.min = 0;
      }
      confirmation.minimize_exponent();
      assert!(confirm.exponent > confirmation.exponent || confirm.includes(&confirmation));
    }
    Some(result)
  }
}

impl PartialOrd for Range {
  fn partial_cmp(&self, other: &Range) -> Option<Ordering> {
    if self.exponent < other.exponent {
      return other.partial_cmp(self).map(|order| order.reverse());
    }
    if let Some(bound) = overflow_checked_shift_left(self.min, self.exponent - other.exponent) {
      if bound > other.max {
        return Some(Ordering::Greater);
      }
    } else if self.min > 0 {
      return Some(Ordering::Greater);
    }
    if let Some(bound) = overflow_checked_shift_left(self.max, self.exponent - other.exponent) {
      if bound < other.min {
        return Some(Ordering::Less);
      }
    } else if self.max < 0 {
      return Some(Ordering::Less);
    }
    None
  }
  // TODO: implement the others for efficiency
}

impl PartialOrd<i64> for Range {
  fn partial_cmp(&self, other: &i64) -> Option<Ordering> {
    if let Some(bound) = overflow_checked_shift_left(self.min, self.exponent) {
      if bound > *other {
        return Some(Ordering::Greater);
      }
    } else if self.min > 0 {
      return Some(Ordering::Greater);
    }
    if let Some(bound) = overflow_checked_shift_left(self.max, self.exponent) {
      if bound < *other {
        return Some(Ordering::Less);
      }
    } else if self.max < 0 {
      return Some(Ordering::Less);
    }
    None
  }
  // TODO: implement the others for efficiency
}

impl PartialOrd<Range> for i64 {
  fn partial_cmp(&self, other: &Range) -> Option<Ordering> {
    other.partial_cmp(self).map(|order| order.reverse())
  }
  // TODO: implement the others for efficiency
}

impl PartialEq<i64> for Range {
  fn eq(&self, other: &i64) -> bool {
    self.exponent == 0 && self.min == *other && self.max == *other
  }
}

impl PartialEq<Range> for i64 {
  fn eq(&self, other: &Range) -> bool {
    other.exponent == 0 && other.min == *self && other.max == *self
  }
}

impl Arbitrary for Range {
  fn arbitrary<G: Gen>(generator: &mut G) -> Range {
    let mut result = Range::new_either_order(generator.gen(), generator.gen());
    result.exponent = generator.gen();
    result.minimize_exponent();
    result
  }
  // TODO: implement shrink
  fn shrink(&self) -> Box<Iterator<Item = Range>> {
    struct Shrinker {
      value: Range,
    }
    impl Iterator for Shrinker {
      type Item = Range;
      fn next(&mut self) -> Option<Range> {
        if self.value.exponent > 0 {
          self.value.exponent >>= 1;
        } else {
          self.value.min /= 2;
          self.value.max /= 2;
        }
        if self.value == Range::exactly(0) {
          None
        } else {
          Some(self.value)
        }
      }
    }
    Box::new(Shrinker {
      value: self.clone(),
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use quickcheck::TestResult;
  use std::cmp::max;

  quickcheck! {
    fn shift_left (range: Range, shift: u32)->TestResult {
      if range.exponent.checked_add (shift).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool (((range << shift) >> shift).includes (&range))
    }
    fn shift_right (range: Range, shift: u32)->TestResult {
      if range.exponent.checked_sub (shift).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool (((range >> shift) << shift).includes (&range))
    }
    fn multiply (first: Range, second: Range)->TestResult {
      if first.exponent.checked_add (second.exponent).and_then (| total | total.checked_add (64)).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool (((first*second)/second).includes (&first))
    }
    fn divide (first: Range, second: Range)->TestResult {
      if second.includes_0() {
        return TestResult::discard()
      }
      TestResult::from_bool (((first/second)*second).includes (&first))
    }
    fn add (first: Range, second: Range)->TestResult {
      if max (first.exponent, second.exponent).checked_add (1).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool (((first + second) - second).includes (&first))
    }
    fn square (range: Range)->TestResult {
      if range.exponent.checked_add (range.exponent).and_then (| total | total.checked_add (64)).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool (((range.squared()).sqrt().unwrap()).includes (&range.abs()))
    }
    fn sqrt (range: Range)->TestResult {
      TestResult::from_bool (((range.abs().sqrt().unwrap()).squared()).includes (&range.abs()))
    }
    fn square_more_restrictive (range: Range)->TestResult {
      if range.exponent.checked_add (range.exponent).and_then (| total | total.checked_add (64)).is_none() {
        return TestResult::discard()
      }
      TestResult::from_bool ((range*range).includes (&range.squared()))
    }

    fn compare_transitive (first: Range, second: Range, third: Range)->TestResult {
      if !(first < second && second < third) {
        return TestResult::discard()
      }
      TestResult::from_bool (first < third)
    }
    fn compare_antisymmetric (first: Range, second: Range)->TestResult {
      if !(first < second || second < first) {
        return TestResult::discard()
      }
      TestResult::from_bool (!(first < second && second < first))
    }
    fn divide_generalizes_i64 (first: i64, second: i64)->TestResult {
      if second == 0 {
        return TestResult::discard()
      }
      TestResult::from_bool ((Range::exactly (first)/Range::exactly (second)).includes (& Range::exactly (first/second)))
    }

  }

  #[test]
  fn tests() {
    assert_eq!(Range::exactly(-47).squared(), Range::exactly(2209));
    assert_eq!(
      Range::exactly(-440) * Range::exactly(1),
      Range::exactly(-440)
    );
    assert_eq!(
      Range::exactly(-440) * Range::exactly(1) * 4,
      Range::exactly(-1760)
    );

    assert_eq!(Range::exactly(99) / Range::exactly(2), Range::new(49, 50));
    assert_eq!(Range::exactly(3) / Range::exactly(2), Range::new(1, 2));
    assert_eq!(Range::exactly(100) / Range::exactly(2), Range::exactly(50));
    assert_eq!(Range::exactly(4) / Range::exactly(2), Range::exactly(2));
    assert_eq!(Range::exactly(0) / Range::exactly(5), Range::exactly(0));
  }

}
