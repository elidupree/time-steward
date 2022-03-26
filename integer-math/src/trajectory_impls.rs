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
    const TIME_SHIFT: u32,
  > Add<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

  #[inline]
  fn add(
    self,
    rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) -> Self::Output {
    let (a, b) = Trajectory::with_matching_rounded_origins(self, rhs)
      .expect("overflow in Trajectory arithmetic");
    Trajectory {
      coordinates: a.coordinates.zip(b.coordinates).map(|(a, b)| a + b),
      unrounded_origin: a.unrounded_origin,
    }
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > CheckedAdd for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn checked_add(
    &self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) -> Option<Self> {
    let (mut a, b) = Trajectory::with_matching_rounded_origins(*self, *rhs)?;
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
    const TIME_SHIFT: u32,
  > AddAssign<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn add_assign(
    &mut self,
    rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) {
    *self = *self + rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > AddAssign<&Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn add_assign(
    &mut self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) {
    *self = *self + rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize, const TIME_SHIFT: u32]
  impl Add for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Add<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

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
    const TIME_SHIFT: u32,
  > AddAssign<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
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
    const TIME_SHIFT: u32,
  > AddAssign<&Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn add_assign(&mut self, rhs: &Vector<Coefficient, DIMENSIONS>) {
    *self = *self + rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize, const TIME_SHIFT: u32]
  impl Add for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>, Vector<Coefficient, DIMENSIONS>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Sub<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

  #[inline]
  fn sub(
    self,
    rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) -> Self::Output {
    let (a, b) = Trajectory::with_matching_rounded_origins(self, rhs)
      .expect("overflow in Trajectory arithmetic");
    Trajectory {
      coordinates: a.coordinates.zip(b.coordinates).map(|(a, b)| a - b),
      unrounded_origin: a.unrounded_origin,
    }
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > CheckedSub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn checked_sub(
    &self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) -> Option<Self> {
    let (mut a, b) = Trajectory::with_matching_rounded_origins(*self, *rhs)?;
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
    const TIME_SHIFT: u32,
  > SubAssign<Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn sub_assign(
    &mut self,
    rhs: Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) {
    *self = *self - rhs
  }
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > SubAssign<&Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn sub_assign(
    &mut self,
    rhs: &Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>,
  ) {
    *self = *self - rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize, const TIME_SHIFT: u32]
  impl Sub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Sub<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

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
    const TIME_SHIFT: u32,
  > SubAssign<Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
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
    const TIME_SHIFT: u32,
  > SubAssign<&Vector<Coefficient, DIMENSIONS>>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn sub_assign(&mut self, rhs: &Vector<Coefficient, DIMENSIONS>) {
    *self = *self - rhs
  }
}

forward_ref_binop! {
  [Coefficient: DoubleSizedSignedInteger, Time: Integer + Signed + TryInto<Coefficient> + TryInto<DoubleSized<Coefficient>>, const DIMENSIONS: usize, const COEFFICIENTS: usize, const TIME_SHIFT: u32]
  impl Sub for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>, Vector<Coefficient, DIMENSIONS>
}

impl<
    Coefficient: Integer,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Mul<Coefficient> for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

  #[inline]
  fn mul(mut self, rhs: Coefficient) -> Self::Output {
    for coordinate in self.coordinates.iter_mut() {
      *coordinate *= rhs;
    }
    self
  }
}

impl<
    Coefficient: Integer,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
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

impl<
    Coefficient: Integer,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > MulAssign<Coefficient> for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn mul_assign(&mut self, rhs: Coefficient) {
    *self = *self * rhs
  }
}

impl<
    Coefficient: Integer,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > MulAssign<&Coefficient>
  for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn mul_assign(&mut self, rhs: &Coefficient) {
    *self = *self * rhs
  }
}

forward_ref_binop! {
  [Coefficient: Integer, Time: Integer, const DIMENSIONS: usize, const COEFFICIENTS: usize, const TIME_SHIFT: u32]
  impl Mul for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>, Coefficient
}

impl<
    Coefficient: Integer + Signed,
    Time: Integer,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Neg for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

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
    const TIME_SHIFT: u32,
  > Neg for &'a Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  type Output = Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>;

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
    const TIME_SHIFT: u32,
  > Zero for Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  fn zero() -> Self {
    Trajectory {
      coordinates: [Zero::zero(); DIMENSIONS],
      unrounded_origin: Zero::zero(),
    }
  }

  #[inline]
  fn is_zero(&self) -> bool {
    *self == Self::zero()
  }
}
