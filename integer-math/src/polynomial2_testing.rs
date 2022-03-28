use crate::polynomial2::Polynomial;
use crate::{DoubleSized, DoubleSizedSignedInteger, Integer};
use live_prop_test::lpt_assert_eq;
use num::Signed;
use std::convert::TryInto;

impl<Coefficient: DoubleSizedSignedInteger, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  pub(crate) fn all_taylor_coefficients_postconditions(
    &self,
    input: impl Integer + Signed + TryInto<DoubleSized<Coefficient>>,
    result: &Option<Self>,
  ) -> Result<(), String> {
    if let Some(result) = result {
      if let Some(translated_back) = result.all_taylor_coefficients(-input) {
        lpt_assert_eq!(
          *self,
          translated_back,
          "all_taylor_coefficients should be reversible"
        )
      } else {
        return Err("all_taylor_coefficients should be reversible".to_string());
      }
    }
    Ok(())
  }
}
