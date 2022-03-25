use crate::polynomial2::Polynomial;
use crate::Integer;
use forward_ref_generic::forward_ref_binop;
use num::{CheckedAdd, CheckedSub, Signed, Zero};
use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

impl<Coefficient: Integer, const COEFFICIENTS: usize> Add<Polynomial<Coefficient, COEFFICIENTS>>
  for Polynomial<Coefficient, COEFFICIENTS>
{
  type Output = Polynomial<Coefficient, COEFFICIENTS>;

  #[inline]
  fn add(self, rhs: Polynomial<Coefficient, COEFFICIENTS>) -> Self::Output {
    Polynomial(self.0.zip(rhs.0).map(|(a, b)| a + b))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> CheckedAdd
  for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn checked_add(&self, rhs: &Polynomial<Coefficient, COEFFICIENTS>) -> Option<Self> {
    let mut result = [Coefficient::default(); COEFFICIENTS];
    for (result, (a, b)) in result.iter_mut().zip(self.0.zip(rhs.0)) {
      *result = a.checked_add(&b)?;
    }
    Some(Polynomial(result))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize>
  AddAssign<Polynomial<Coefficient, COEFFICIENTS>> for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: Polynomial<Coefficient, COEFFICIENTS>) {
    *self = *self + rhs
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize>
  AddAssign<&Polynomial<Coefficient, COEFFICIENTS>> for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn add_assign(&mut self, rhs: &Polynomial<Coefficient, COEFFICIENTS>) {
    *self = *self + rhs
  }
}

forward_ref_binop! {
  [Coefficient: Integer, const COEFFICIENTS: usize]
  impl Add for Polynomial<Coefficient, COEFFICIENTS>
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> Sub<Polynomial<Coefficient, COEFFICIENTS>>
  for Polynomial<Coefficient, COEFFICIENTS>
{
  type Output = Polynomial<Coefficient, COEFFICIENTS>;

  #[inline]
  fn sub(self, rhs: Polynomial<Coefficient, COEFFICIENTS>) -> Self::Output {
    Polynomial(self.0.zip(rhs.0).map(|(a, b)| a - b))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> CheckedSub
  for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn checked_sub(&self, rhs: &Polynomial<Coefficient, COEFFICIENTS>) -> Option<Self> {
    let mut result = [Coefficient::default(); COEFFICIENTS];
    for (result, (a, b)) in result.iter_mut().zip(self.0.zip(rhs.0)) {
      *result = a.checked_sub(&b)?;
    }
    Some(Polynomial(result))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize>
  SubAssign<Polynomial<Coefficient, COEFFICIENTS>> for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: Polynomial<Coefficient, COEFFICIENTS>) {
    *self = *self - rhs
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize>
  SubAssign<&Polynomial<Coefficient, COEFFICIENTS>> for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn sub_assign(&mut self, rhs: &Polynomial<Coefficient, COEFFICIENTS>) {
    *self = *self - rhs
  }
}

forward_ref_binop! {
  [Coefficient: Integer, const COEFFICIENTS: usize]
  impl Sub for Polynomial<Coefficient, COEFFICIENTS>
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> Mul<Coefficient>
  for Polynomial<Coefficient, COEFFICIENTS>
{
  type Output = Polynomial<Coefficient, COEFFICIENTS>;

  #[inline]
  fn mul(self, rhs: Coefficient) -> Self::Output {
    Polynomial(self.0.map(|a| a * rhs))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> Polynomial<Coefficient, COEFFICIENTS> {
  #[inline]
  pub fn checked_mul(&self, rhs: Coefficient) -> Option<Self> {
    let mut result = [Coefficient::default(); COEFFICIENTS];
    for (result, a) in result.iter_mut().zip(self.0) {
      *result = a.checked_mul(&rhs)?;
    }
    Some(Polynomial(result))
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> MulAssign<Coefficient>
  for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn mul_assign(&mut self, rhs: Coefficient) {
    *self = *self * rhs
  }
}

impl<Coefficient: Integer, const COEFFICIENTS: usize> MulAssign<&Coefficient>
  for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn mul_assign(&mut self, rhs: &Coefficient) {
    *self = *self * rhs
  }
}

forward_ref_binop! {
  [Coefficient: Integer, const COEFFICIENTS: usize]
  impl Mul for Polynomial<Coefficient, COEFFICIENTS>, Coefficient
}

impl<Coefficient: Integer + Signed, const COEFFICIENTS: usize> Neg
  for Polynomial<Coefficient, COEFFICIENTS>
{
  type Output = Polynomial<Coefficient, COEFFICIENTS>;

  #[inline]
  fn neg(self) -> Self::Output {
    Polynomial(self.0.map(Neg::neg))
  }
}

impl<'a, Coefficient: Integer + Signed, const COEFFICIENTS: usize> Neg
  for &'a Polynomial<Coefficient, COEFFICIENTS>
{
  type Output = Polynomial<Coefficient, COEFFICIENTS>;

  #[inline]
  fn neg(self) -> Self::Output {
    Polynomial(self.0.map(Neg::neg))
  }
}

impl<Coefficient: Integer + Signed, const COEFFICIENTS: usize> Zero
  for Polynomial<Coefficient, COEFFICIENTS>
{
  #[inline]
  fn zero() -> Self {
    Polynomial([Zero::zero(); COEFFICIENTS])
  }

  #[inline]
  fn is_zero(&self) -> bool {
    *self == Self::zero()
  }
}
