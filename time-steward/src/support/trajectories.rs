//use nalgebra::Vector2;
use super::integer_math::{Vector as GenericVector, *};
use num::traits::{Signed};
use num::{One, Zero};
use std::cmp::min;
//use self::polynomial::RootSearchResult;
use self::polynomial2::{
  AllTaylorCoefficients, AllTaylorCoefficientsBounds, FractionalInput, Polynomial,
  GreaterThanEqualToFilter, GreaterThanFilter, LessThanFilter, PolynomialRangeSearch, PolynomialMagnitudeSquaredRangeSearch, SetNthTaylorCoefficientAtFractionalInput,
};
use array_ext::*;
use smallvec::SmallVec;
use std::convert::TryInto;
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

pub type Time = i64;
pub type Coordinate = i32;
pub trait Vector: GenericVector<Coordinate = Coordinate> {}
impl<T: GenericVector<Coordinate = Coordinate>> Vector for T {}

fn combine_options<T, F>(a: Option<T>, b: Option<T>, f: F) -> Option<T>
where
  F: FnOnce(T, T) -> T,
{
  match (a, b) {
    (Some(x), None) | (None, Some(x)) => Some(x),
    (Some(x), Some(y)) => Some(f(x, y)),
    (None, None) => None,
  }
}

pub trait Trajectory: Sized {
  type Coefficient: Vector<Coordinate = Coordinate>;
}

pub trait ScalarTrajectory: Trajectory
where
  Self::Coefficient: Integer,
  for<'a> &'a Self: Neg<Output = Self>,
{
  /// Find the first time within a range when the trajectory value is "significantly" >= target.
  ///
  /// To be "significant", the output time will obey this condition: The value at the output time is >= target, and it will not dip back to < target due to rounding error (it will only dip back if the ideal slope actually becomes negative).
  ///
  /// In order to guarantee this, we have to be slightly permissive, skipping over some inputs where the value is observed to be >= target. However, it still obeys this guarantee: The output time will always be <= the first time when the value is >= output + 4.
  fn next_time_significantly_ge(
    &self,
    range: [Time; 2],
    input_shift: u32,
    target: Self::Coefficient,
  ) -> Option<Time>;
  fn next_time_significantly_gt(
    &self,
    range: [Time; 2],
    input_shift: u32,
    target: Self::Coefficient,
  ) -> Option<Time> {
    self.next_time_significantly_ge(range, input_shift, target + Self::Coefficient::one())
  }
  fn next_time_significantly_le(
    &self,
    range: [Time; 2],
    input_shift: u32,
    target: Self::Coefficient,
  ) -> Option<Time> {
    (-self).next_time_significantly_ge(range, input_shift, -target)
  }
  fn next_time_significantly_lt(
    &self,
    range: [Time; 2],
    input_shift: u32,
    target: Self::Coefficient,
  ) -> Option<Time> {
    self.next_time_significantly_le(range, input_shift, target - Self::Coefficient::one())
  }
}

macro_rules! impl_binop {
  ([$($generic_parameters: tt)*], $Left:ty, $Right:ty, $Output:ty, $Trait: ident, $method: ident, $self: ident, $other: ident, $owned_owned: expr, $owned_ref: expr, $ref_owned: expr, $ref_ref: expr,) => {

impl <$($generic_parameters)*> $Trait <$Right> for $Left {
  type Output = $Output;
  fn $method (mut $self, $other: $Right)->Self {
    $owned_owned
  }
}

impl <'a, $($generic_parameters)*> $Trait <& 'a $Right> for $Left {
  type Output = $Output;
  fn $method (mut $self, $other: & 'a $Right)->Self {
    $owned_ref
  }
}

impl <'a, $($generic_parameters)*> $Trait <$Right> for & 'a $Left {
  type Output = $Output;
  fn $method ($self, $other: $Right)->$Left {
    $ref_owned
  }
}

impl <'a, 'b, $($generic_parameters)*> $Trait <& 'b $Right> for & 'a $Left {
  type Output = $Output;
  fn $method ($self, $other: & 'b $Right)->$Left {
    $ref_ref
  }
}

  }
}

macro_rules! impl_binop_and_assign {
  ([$($generic_parameters: tt)*], $Left:ty, $Right:ty, $Output:ty, $Trait: ident, $method: ident, $TraitAssign: ident, $method_assign: ident, $self: ident, $other: ident, $owned_owned: expr, $owned_ref: expr, $ref_owned: expr, $ref_ref: expr, $assign_owned: expr, $assign_ref: expr,) => {

impl_binop! ([$($generic_parameters)*], $Left, $Right, $Output, $Trait, $method, $self, $other, $owned_owned, $owned_ref, $ref_owned, $ref_ref,);

impl <$($generic_parameters)*> $TraitAssign <$Right> for $Left {
  fn $method_assign (&mut $self, $other: $Right) {
    $assign_owned
  }
}

impl <'a, $($generic_parameters)*> $TraitAssign <& 'a $Right> for $Left {
  fn $method_assign (&mut $self, $other: & 'a $Right) {
    $assign_ref
  }
}

  }
}

macro_rules! impl_trajectory_add_sub {
  ($Trajectory:ident, $Trait: ident, $method: ident, $TraitAssign: ident, $method_assign: ident) => {

impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, $Trajectory <T>, $Trajectory <T>, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.$method_assign (other);
  self
},
{
  self.$method_assign (other);
  self
},
{
  self.clone().$method (other)
},
{
  if self.origin < other.origin {self.clone().$method (other)}
  else {self.$method (other.clone())}
},
{
  let mut other = other;
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {other.set_origin (self.origin).unwrap();}
  for (mine, others) in self.coefficients.iter_mut().zip (other.coefficients.into_iter()) {
    mine.$method_assign (others);
  }
},
{
  let mut other_clone;
  let mut other = other;
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {
    other_clone = other.clone();
    other_clone.set_origin (self.origin).unwrap();
    other = & other_clone;
  }
  for (mine, others) in self.coefficients.iter_mut().zip (other.coefficients.iter()) {
    mine.$method_assign (others);
  }
},
}


impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, T, $Trajectory <T>, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.clone().$method (other)
},
{
  self.clone().$method (other)
},
{
  self.coefficients [0].$method_assign (other);
},
{
  self.coefficients [0].$method_assign (other);
},
}


  }
}

macro_rules! impl_trajectory {
  ($Trajectory: ident, $degree: expr, $multiplication: tt, $ProductTrajectory: ident) => {

#[derive (Clone, Serialize, Deserialize, Debug, Default)]
pub struct $Trajectory <T> {
  origin: Time,
  coefficients: [T; $degree + 1],
}

impl <T: Vector> Trajectory for $Trajectory <T> {
  type Coefficient = T;
}

impl <T: Vector> $Trajectory <T> where Time: From <T::Coordinate>,  [T::Coordinate; $degree+1]: Polynomial<T::Coordinate> {
  pub fn constant (value: T)->Self {
    let mut coefficients = [T::zero(); $degree + 1];
    coefficients [0] = value;
    $Trajectory {origin: 0, coefficients}
  }
  pub fn set_origin (&mut self, new_origin: Time)->Option<()> {
    let mut result = self.clone();
    for dimension in 0..T::DIMENSIONS {
      let coefficients = self.coordinate_coefficients (dimension).all_taylor_coefficients(new_origin - self.origin)?;
      for (coefficient, destination) in coefficients.iter().zip (result.coefficients.iter_mut()) {
        destination.set_coordinate (dimension, *coefficient);
      }
    }
    *self = result;
    self.origin = new_origin;
    Some(())
  }

  pub fn coordinate_coefficients (&self, dimension: usize)->[T::Coordinate; $degree + 1] {
    Array::from_fn (| which | self.coefficients [which].coordinate (dimension))
  }
  pub fn coordinate_trajectory (&self, dimension: usize)->$Trajectory <T::Coordinate> {
    $Trajectory {
      origin: self.origin,
      coefficients: self.coordinate_coefficients (dimension),
    }
  }


  pub fn nth_coefficient (&self, which: usize, time_numerator: Time, time_shift: u32)->Option<T> {
    let mut result = T::zero();
    for dimension in 0..T::DIMENSIONS {
      let bounds = self.coordinate_coefficients (dimension)
        .all_taylor_coefficients_bounds(time_numerator - (self.origin << time_shift), time_shift, 0i32)?[which];
      result.set_coordinate(dimension, mean_round_to_even(bounds[0], bounds[1]).try_into().ok()?);
    }
    Some(result)
  }
  pub fn set_nth_coefficient (&mut self, which: usize, time_numerator: Time, time_shift: u32, target_value: T)->Option<()> {
    self.set_origin (time_numerator >> time_shift)?;
    let mut transformed_coefficients: SmallVec<[[T::Coordinate; $degree + 1]; 4]> = SmallVec::with_capacity (T::DIMENSIONS);
    for dimension in 0..T::DIMENSIONS {
      let mut coefficients = self.coordinate_coefficients (dimension);
      coefficients.set_nth_taylor_coefficient_at_fractional_input (which, time_numerator - (self.origin << time_shift), time_shift, target_value.coordinate (dimension).into())?;
      transformed_coefficients.push (coefficients);
    }
    for (dimension, coefficients) in transformed_coefficients.into_iter().enumerate() {
      for (coefficient, destination) in coefficients.iter().zip (self.coefficients.iter_mut()).take (which + 1) {
        destination.set_coordinate (dimension, *coefficient);
      }
    }
    Some(())
  }
  pub fn add_nth_coefficient (&mut self, which: usize, time_numerator: Time, time_shift: u32, added_value: T)->Option<()> {
    let current_value = self.nth_coefficient (which, time_numerator, time_shift)?;
    self.set_nth_coefficient (which, time_numerator, time_shift, current_value + added_value)
  }
  pub fn value (&self, time_numerator: Time, time_shift: u32)->Option<T> {self.nth_coefficient (0, time_numerator, time_shift)}
  pub fn velocity (&self, time_numerator: Time, time_shift: u32)->Option<T> {self.nth_coefficient (1, time_numerator, time_shift)}
  //pub fn acceleration (&self, time_numerator: Time, time_shift: u32)->Option<T> {Ok (self.nth_coefficient (1, time_numerator, time_shift)?<<1)}
  pub fn set_value (&mut self, time_numerator: Time, time_shift: u32, value: T)->Option<()> {self.set_nth_coefficient (0, time_numerator, time_shift, value)}
  pub fn set_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Option<()> {self.set_nth_coefficient (1, time_numerator, time_shift, value)}
  pub fn set_acceleration (&mut self, time_numerator: Time, time_shift: u32, value: T)->Option<()> {self.set_nth_coefficient (2, time_numerator, time_shift, value.map_coordinates (| coordinate | shr_round_to_even (coordinate, 1u32)))}
  pub fn add_value (&mut self, value: T) {*self += value}
  pub fn add_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Option<()> {self.add_nth_coefficient (1, time_numerator, time_shift, value)}
  pub fn add_acceleration (&mut self, time_numerator: Time, time_shift: u32, value: T)->Option<()> {self.add_nth_coefficient (2, time_numerator, time_shift, value.map_coordinates (| coordinate | shr_round_to_even (coordinate, 1u32)))}

  pub fn next_time_possibly_outside_bounds (&self, range: [Time; 2], input_shift: u32, bounds: [T; 2])->Option<Time> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    (0..T::DIMENSIONS).filter_map (| dimension | {
      let trajectory = self.coordinate_trajectory (dimension);
      let four = (T::Coordinate::one() + T::Coordinate::one()) + (T::Coordinate::one() + T::Coordinate::one());
      combine_options(
        trajectory.next_time_significantly_le (range, input_shift, bounds [0].coordinate (dimension) + four),
        trajectory.next_time_significantly_ge (range, input_shift, bounds [1].coordinate (dimension) - four),
        |a,b| min(a,b)
      )
    }).min()
  }

  #[cfg $multiplication]
  pub fn magnitude_squared_trajectory (&self)->Option<$ProductTrajectory <T::Coordinate>> {
    let mut coefficients = [T::Coordinate::zero(); $degree + $degree + 1];
    for dimension in 0..T::DIMENSIONS {
      let coordinate_coefficients = self.coordinate_coefficients (dimension);
      polynomial::add_product_into (& coordinate_coefficients, & coordinate_coefficients, &mut coefficients).ok()?;
    }
    Some($ProductTrajectory {
      origin: self.origin, coefficients
    })
  }
  #[cfg $multiplication]
  pub fn next_time_magnitude_significantly_gt (&self, range: [Time; 2], input_shift: u32, target: T::Coordinate)->Option<Time> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    //self.magnitude_squared_trajectory()?.next_time_significantly_gt (range, input_shift, target*target)
    let origin = self.origin << input_shift;
    let mut coordinate_polynomials: SmallVec<[[T::Coordinate; $degree + 1]; 4]> = SmallVec::with_capacity (T::DIMENSIONS);
    for dimension in 0..T::DIMENSIONS {
      coordinate_polynomials.push(self.coordinate_coefficients (dimension));
    }
    <[T::Coordinate; $degree + 1]>::next_time_magnitude_squared_passes(&coordinate_polynomials, FractionalInput::new(range [0] - origin, input_shift), input_shift, GreaterThanFilter::new(target as i64*target as i64, (target as i64+3)*(target as i64+3))).map(|a| a + origin)
  }
  #[cfg $multiplication]
  pub fn next_time_magnitude_significantly_lt (&self, range: [Time; 2], input_shift: u32, target: T::Coordinate)->Option<Time> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    //self.magnitude_squared_trajectory()?.next_time_significantly_lt (range, input_shift, target*target)
    let origin = self.origin << input_shift;
    let mut coordinate_polynomials: SmallVec<[[T::Coordinate; $degree + 1]; 4]> = SmallVec::with_capacity (T::DIMENSIONS);
    for dimension in 0..T::DIMENSIONS {
      coordinate_polynomials.push(self.coordinate_coefficients (dimension));
    }
    <[T::Coordinate; $degree + 1]>::next_time_magnitude_squared_passes(&coordinate_polynomials, FractionalInput::new(range [0] - origin, input_shift), input_shift, LessThanFilter::new(target as i64*target as i64, (target as i64-3)*(target as i64-3))).map(|a| a + origin)
  }
}


/*impl <T: Vector + Integer> $Trajectory <T> where Time: From <T::Coordinate> {
  #[cfg $multiplication]
  fn multiply_same_origin (&self, other: & Self)->$ProductTrajectory <T> {
    let mut coefficients = [T::zero(); $degree + $degree + 1];
    polynomial::add_product_into (& self.coefficients, & other.coefficients, &mut coefficients).unwrap();
    $ProductTrajectory {
      origin: self.origin, coefficients
    }
  }
}*/

impl <T: Integer + Signed> $Trajectory <T> {
  /*pub fn next_time_lt (&self, now: Time, value: T)->Option <Time> {
    if self.value (now) < value {Some (now)}
    else {
      unimplemented!()
    }
  }
  pub fn next_time_le (&self, now: Time, value: T)->Option <Time> {
    self.next_time_lt (now, value + T::one())
  }
  pub fn next_time_gt (&self, now: Time, value: T)->Option <Time> {
    (-self).next_time_lt (now, -value)
  }
  pub fn next_time_ge (&self, now: Time, value: T)->Option <Time> {
    self.next_time_gt (now, value - T::one())
  }*/
}

impl_trajectory_add_sub! ($Trajectory, Add, add, AddAssign, add_assign);
impl_trajectory_add_sub! ($Trajectory, Sub, sub, SubAssign, sub_assign);

impl<T: Vector, Coordinate: Copy> Mul<Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = Self;
  fn mul(mut self, other: Coordinate) -> Self {
    self *= other; self
  }
}

impl<'a, T: Vector, Coordinate: Copy> Mul<Coordinate> for & 'a $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = $Trajectory <T>;
  fn mul(self, other: Coordinate) ->$Trajectory <T> {
    self.clone()*other
  }
}

impl<T: Vector, Coordinate: Copy> MulAssign <Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  fn mul_assign (&mut self, other: Coordinate) {
    for coefficient in self.coefficients.iter_mut (){MulAssign::<Coordinate>::mul_assign (coefficient, other);}
  }
}

impl<T: Vector> PartialEq<$Trajectory<T>> for $Trajectory<T> {
  fn eq(&self, other: &$Trajectory<T>) -> bool {
    if self.origin < other.origin {other.eq(self)}
    else {
      let mut other = other.clone();
      other.set_origin(self.origin);
      self.coefficients == other.coefficients
    }
  }
}

impl<T: Vector> Eq for $Trajectory<T> {}

/*impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, T::Coordinate, $Trajectory <T>, Mul, mul, MulAssign, mul_assign, self, other,
{
  self *= other;
  self
},
{
  self *= other;
  self
},
{
  self.clone()*other
},
{
  self.clone()*other
},
{
  for term in self.terms.iter_mut (){*term *= other;}
},
{
  for term in self.terms.iter_mut (){*term *= other;}
},
}*/

/*#[cfg $multiplication]
impl_binop! {
[T: Vector + Integer], $Trajectory <T>, $Trajectory <T>, $ProductTrajectory <T>, Mul, mul, self, other,
{
  if self.origin < other.origin {self.set_origin (other.origin).unwrap(); }
  if other.origin < self.origin {other.set_origin (self.origin).unwrap(); }
  self.multiply_same_origin (&other)
},
{
  if other.origin < self.origin {let mut other = other.clone(); other.set_origin (self.origin).unwrap(); return self.multiply_same_origin (other)}
  if self.origin < other.origin {self.set_origin (other.origin).unwrap(); }
  self.multiply_same_origin (other)
},
{
  other + self
},
{
  if self.origin < other.origin {let mut me = self.clone(); me.set_origin (other.origin).unwrap(); return me.multiply_same_origin (other)}
  if other.origin < self.origin {let mut other = other.clone(); other.set_origin (self.origin).unwrap(); return self.multiply_same_origin (other)}
  self.multiply_same_origin (other)
},
}*/

impl <T: Vector + Neg <Output = T>> Neg for $Trajectory <T> {
  type Output = Self;
  fn neg (self)->Self {
    $Trajectory {
      origin: self.origin,
      coefficients: Array::from_fn (| index | self.coefficients [index].clone().neg()),
    }
  }
}
impl <'a, T: Vector> Neg for & 'a $Trajectory <T> where & 'a T: Neg <Output = T> {
  type Output = $Trajectory <T>;
  fn neg (self)->$Trajectory <T> {
    $Trajectory {
      origin: self.origin,
      coefficients: Array::from_fn (| index | (& self.coefficients [index]).neg()),
    }
  }
}

impl ScalarTrajectory for $Trajectory <Coordinate> {
  fn next_time_significantly_ge (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->Option<Time> {
    let origin = self.origin << input_shift;
    self.coefficients.next_time_value_passes(FractionalInput::new(range [0] - origin, input_shift), input_shift, GreaterThanEqualToFilter::new(target, target + 3)).map(|a| a + origin)
  }
}


  };
}

impl_trajectory!(LinearTrajectory, 1, (all()), QuadraticTrajectory);
impl_trajectory!(QuadraticTrajectory, 2, (all()), QuarticTrajectory);
impl_trajectory!(QuarticTrajectory, 4, (any()), Unused);

#[cfg(test)]
mod tests {
  use super::*;
  use nalgebra::Vector2;
  use proptest::prelude::*;

  fn assert_close(first: Vector2<i32>, second: Vector2<i32>) {
    let difference = first - second;
    assert!(
      difference[0].abs() <= 2 && difference[1].abs() <= 2,
      "vectors are too far apart: {:?}, {:?}",
      first,
      second
    );
  }
  fn assert_close_magsq(first: i32, second: i32) {
    let difference = (first as f64).sqrt() - (second as f64).sqrt();
    assert!(
      difference.abs() <= 2f64,
      "ints are too far apart: {:?}, {:?}",
      first,
      second
    );
  }

  fn assert_close_time(first: i64, second: i64) {
    let difference = first - second;
    assert!(
      difference.abs() <= 2,
      "ints are too far apart: {:?}, {:?}",
      first,
      second
    );
  }

  #[test]
  fn trajectory_unit_tests() {
    let mut foo = QuadraticTrajectory::constant(Vector2::new(30, 40));
    assert_close(foo.value(77, 5).unwrap(), Vector2::new(30, 40));
    foo.set_velocity(77, 5, Vector2::new(10, 20));
    assert_close(foo.value(77, 5).unwrap(), Vector2::new(30, 40));
    assert_close(foo.velocity(77, 5).unwrap(), Vector2::new(10, 20));
    assert_close(foo.velocity(0, 5).unwrap(), Vector2::new(10, 20));
    assert_close(foo.value(77 + 32, 5).unwrap(), Vector2::new(40, 60));

    let magsq = foo.magnitude_squared_trajectory().unwrap();

    assert_close_magsq(magsq.value(77, 5).unwrap(), 30 * 30 + 40 * 40);
    assert_close_magsq(magsq.value(77 + 32, 5).unwrap(), 40 * 40 + 60 * 60);

    assert_close_time(
      magsq
        .next_time_significantly_ge([0, 99999], 5, 40 * 40 + 60 * 60)
        .unwrap(),
      77 + 32,
    );
  }

  macro_rules! prop_assert_close {
    ($a:expr, $b:expr) => {{
      let a = $a;
      let b = $b;
      prop_assert!((a - b).abs() <= 2, "not close enough: {:?}, {:?}", a, b);
    }};
  }

  fn arbitrary_fractional_input() -> BoxedStrategy<(i64, u32)> {
    (0u32..16)
      .prop_flat_map(|shift| ((-16i64 << shift..16i64 << shift), Just(shift)))
      .boxed()
  }

  macro_rules! test_trajectory {
  ($mod: ident, $Trajectory: ident, $degree: expr, $uniform: ident, $multiplication: tt, $ProductTrajectory: ident) => {mod $mod { use super::*;

impl $Trajectory<i32> {
  fn arbitrary_trajectory() -> BoxedStrategy <$Trajectory<i32>> {
    (prop::array::$uniform(-16i32..16), -16i64..16).prop_map(|(coefficients, origin)| $Trajectory { coefficients, origin}).boxed()
  }
}

proptest! {
  #[test]
  fn randomly_test_set_origin_equal(ref first in $Trajectory::<i32>::arbitrary_trajectory(), new_origin in -16i64..16) {
    let mut second = first.clone();
    second.set_origin(new_origin);
    prop_assert_eq!(first, &second);
  }
  #[test]
  fn randomly_test_add_commutative(ref first in $Trajectory::<i32>::arbitrary_trajectory(), ref second in $Trajectory::<i32>::arbitrary_trajectory()) {
    prop_assert_eq!(first + second, second + first);
  }
  #[test]
  fn randomly_test_sub_anticommutative(ref first in $Trajectory::<i32>::arbitrary_trajectory(), ref second in $Trajectory::<i32>::arbitrary_trajectory()) {
    prop_assert_eq!(first- second, -(second - first));
  }
  /*#[cfg $multiplication]
  #[test]
  fn randomly_test_mul_commutative(ref first in $Trajectory::<i32>::arbitrary_trajectory(), ref second in $Trajectory::<i32>::arbitrary_trajectory()) {
    prop_assert_eq!(first * second, second * first);
  }*/
  #[test]
  fn randomly_test_add_associative(ref first in $Trajectory::<i32>::arbitrary_trajectory(), ref second in $Trajectory::<i32>::arbitrary_trajectory(), ref third in $Trajectory::<i32>::arbitrary_trajectory()) {
    prop_assert_eq!(first + (second + third), (first + second) + third);
  }
  #[test]
  fn randomly_test_nth_coefficient(ref first in $Trajectory::<i32>::arbitrary_trajectory(), (input, shift) in arbitrary_fractional_input(), which in 0usize..$degree+1, value in -16i32..16) {
    let mut second = first.clone();
    second.set_nth_coefficient(which, input, shift, value);
    prop_assert_close!(second.nth_coefficient(which, input, shift).unwrap(), value);
    for i in 0..$degree+1 { if i != which {
      prop_assert_close!(first.nth_coefficient(i, input, shift).unwrap(), second.nth_coefficient(i, input, shift).unwrap());
    }}

    let mut third = first.clone();
    third.add_nth_coefficient(which, input, shift, value);
    prop_assert_close!(third.nth_coefficient(which, input, shift).unwrap(), first.nth_coefficient(which, input, shift).unwrap() + value);
    for i in 0..$degree+1 { if i != which {
      prop_assert_close!(first.nth_coefficient(i, input, shift).unwrap(), third.nth_coefficient(i, input, shift).unwrap());
    }}
  }
}

  }}
}

  test_trajectory!(
    lin,
    LinearTrajectory,
    1,
    uniform2,
    (all()),
    QuadraticTrajectory
  );
  test_trajectory!(
    quad,
    QuadraticTrajectory,
    2,
    uniform3,
    (all()),
    QuarticTrajectory
  );
  test_trajectory!(quar, QuarticTrajectory, 4, uniform5, (any()), Unused);

}
