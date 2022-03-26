use crate::trajectory::Trajectory;
use crate::{DoubleSized, DoubleSizedSignedInteger, Integer, Vector};
use forward_ref_generic::forward_ref_binop;
use num::{CheckedAdd, CheckedSub, Signed, Zero};
use std::convert::TryInto;
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Add<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn add(self, rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) -> Self::Output {
    let (a, b) =
      Trajectory::with_later_origin(self, rhs).expect("overflow in Trajectory arithmetic");
    Trajectory {
      coordinates: a.coordinates.zip(b.coordinates).map(|(a, b)| a + b),
      origin: a.origin,
    }
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > CheckedAdd for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn checked_add(
    &self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>,
  ) -> Option<Self> {
    let (mut a, b) = Trajectory::with_later_origin(*self, *rhs)?;
    for (a, b) in a.coordinates.iter_mut().zip(b.coordinates) {
      *a = a.checked_add(&b)?;
    }
    Some(a)
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > AddAssign<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) {
    *self = *self + rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > AddAssign<&Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) {
    *self = *self + rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize]
  impl Add for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Add<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn add(mut self, rhs: Vector<Coefficient, DIMENSIONS>) -> Self::Output {
    for (dimension, coordinate) in self.coordinates.iter_mut().enumerate() {
      coordinate[0] += rhs[dimension];
    }
    self
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > AddAssign<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: Vector<Coefficient, DIMENSIONS>) {
    *self = *self + rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > AddAssign<&Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: &Vector<Coefficient, DIMENSIONS>) {
    *self = *self + rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize]
  impl Add for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>, Vector<Coefficient, DIMENSIONS>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Sub<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn sub(self, rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) -> Self::Output {
    let (a, b) =
      Trajectory::with_later_origin(self, rhs).expect("overflow in Trajectory arithmetic");
    Trajectory {
      coordinates: a.coordinates.zip(b.coordinates).map(|(a, b)| a - b),
      origin: a.origin,
    }
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > CheckedSub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn checked_sub(
    &self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>,
  ) -> Option<Self> {
    let (mut a, b) = Trajectory::with_later_origin(*self, *rhs)?;
    for (a, b) in a.coordinates.iter_mut().zip(b.coordinates) {
      *a = a.checked_sub(&b)?;
    }
    Some(a)
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > SubAssign<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) {
    *self = *self - rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > SubAssign<&Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>) {
    *self = *self - rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize]
  impl Sub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Sub<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn sub(mut self, rhs: Vector<Coefficient, DIMENSIONS>) -> Self::Output {
    for (dimension, coordinate) in self.coordinates.iter_mut().enumerate() {
      coordinate[0] -= rhs[dimension];
    }
    self
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > SubAssign<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: Vector<Coefficient, DIMENSIONS>) {
    *self = *self - rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > SubAssign<&Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: &Vector<Coefficient, DIMENSIONS>) {
    *self = *self - rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize]
  impl Sub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>, Vector<Coefficient, DIMENSIONS>
}

impl<Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize>
  Mul<Coefficient> for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn mul(mut self, rhs: Coefficient) -> Self::Output {
    for coordinate in self.coordinates.iter_mut() {
      *coordinate *= rhs;
    }
    self
  }
}

impl<Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize>
  Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  pub fn checked_mul(&self, rhs: Coefficient) -> Option<Self> {
    let mut result = *self;
    for result in result.coordinates.iter_mut() {
      *result = result.checked_mul(rhs)?;
    }
    Some(result)
  }
}

impl<Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize>
  MulAssign<Coefficient> for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn mul_assign(&mut self, rhs: Coefficient) {
    *self = *self * rhs
  }
}

impl<Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize>
  MulAssign<&Coefficient> for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn mul_assign(&mut self, rhs: &Coefficient) {
    *self = *self * rhs
  }
}

forward_ref_binop! {
  [Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize]
  impl Mul for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>, Coefficient
}

impl<
    Coefficient: Integer + Signed,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Neg for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn neg(mut self) -> Self::Output {
    for coordinate in self.coordinates.iter_mut() {
      *coordinate = -*coordinate;
    }
    self
  }
}

impl<
    'a,
    Coefficient: Integer + Signed,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Neg for &'a Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>;

  #[inline]
  fn neg(self) -> Self::Output {
    -*self
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Zero for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  fn zero() -> Self {
    Trajectory {
      coordinates: [Zero::zero(); DIMENSIONS],
      origin: Zero::zero(),
    }
  }

  #[inline]
  fn is_zero(&self) -> bool {
    *self == Self::zero()
  }
}
