use crate::polynomial2::{OverflowError, Polynomial};
use crate::{DoubleSized, DoubleSizedSignedInteger, Integer, Vector};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::cmp::Ordering;
use std::convert::TryInto;

#[serde_as]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize, Derivative)]
#[derivative(Default(
  bound = "Time: Default, [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS]: Default"
))]
#[serde(bound(serialize = "Coefficient: Serialize, Time: Serialize"))]
#[serde(bound(deserialize = "Coefficient: Deserialize<'de>, Time: Deserialize<'de>"))]
pub struct Trajectory<Coefficient, Time, const DIMENSIONS: usize, const COEFFICIENTS: usize> {
  #[serde_as(as = "[_; DIMENSIONS]")]
  pub(crate) coordinates: [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  pub(crate) origin: Time,
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer
      //+ From<Coefficient>
      //+ From<DoubleSized<Coefficient>>
      + TryInto<Coefficient>
      + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
  > Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  pub fn constant(value: &Vector<Coefficient, DIMENSIONS>) -> Self {
    Trajectory {
      coordinates: crate::vector_to_array(value).map(Polynomial::constant),
      origin: Time::zero(),
    }
  }
  #[inline]
  pub fn is_constant(&self) -> bool {
    self.coordinates.iter().all(Polynomial::is_constant)
  }

  pub fn set_origin(&mut self, new_origin: Time) {
    if !self.is_constant() {
      let duration: DoubleSized<Coefficient> = (new_origin - self.origin)
        .try_into()
        .ok()
        .expect("Overflow in Trajectory::set_origin");
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(duration)
          .expect("Overflow in Trajectory::set_origin");
      }
    }
    self.origin = new_origin;
  }

  pub fn try_set_origin(&mut self, new_origin: Time) -> Result<(), OverflowError> {
    if !self.is_constant() {
      let duration: DoubleSized<Coefficient> = (new_origin - self.origin)
        .try_into()
        .map_err(|_| OverflowError)?;
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(duration)
          .ok_or(OverflowError)?;
      }
    }
    self.origin = new_origin;
    Ok(())
  }

  #[inline]
  pub fn with_origin(self, new_origin: Time) -> Option<Self> {
    let mut result = self;
    result.try_set_origin(new_origin).ok()?;
    Some(result)
  }

  #[inline]
  pub fn with_later_origin(a: Self, b: Self) -> Option<(Self, Self)> {
    match a.origin.cmp(&b.origin) {
      Ordering::Less => Some((a.with_origin(b.origin)?, b)),
      Ordering::Equal => Some((a, b.with_origin(a.origin)?)),
      Ordering::Greater => Some((a, b)),
    }
  }
}
