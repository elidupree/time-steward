use num::{Signed, CheckedAdd, CheckedMul};
use array_ext::*;
use std::cmp::{min, max};

use super::*;

/// Evaluate all Taylor coefficients of a polynomial.
///
/// Returns None if any of the coefficients do not fit in the type.
pub trait AllTaylorCoefficients<Input>: Sized {
  fn all_taylor_coefficients(&self, input: Input)->Option<Self>;
}

macro_rules! impl_polynomials {
  ($($coefficients: expr),*) => {
$(
impl <Coefficient: DoubleSizedInteger> AllTaylorCoefficients<<Coefficient as DoubleSizedInteger>::Type> for [Coefficient; $coefficients] {
  fn all_taylor_coefficients(&self, input: <Coefficient as DoubleSizedInteger>::Type)->Option <Self> {
    let mut intermediates: [<Coefficient as DoubleSizedInteger>::Type; $coefficients] = self.map (| coefficient | coefficient.into());
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
    
)*  
    
  }
}


impl_polynomials!(1,2,3,4,5);

#[cfg(test)]
mod tests {

//use super::*;
//use proptest::prelude::*;


macro_rules! test_polynomials {
  ($($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident,)*) => {
$(
  mod $name {
    use super::super::*;
    use proptest::prelude::*;
    proptest! {
      #[test]
      fn randomly_test_polynomial_translation_inverts (coefficients in prop::array::$uniform(-16 as i32..16), input in -16 as i32..16) {
        let translated = coefficients.all_taylor_coefficients (input as $double);
        prop_assume! (translated.is_some());
        let translated = translated.unwrap();
        let translated_back = translated.all_taylor_coefficients (-input as $double);
        prop_assert! (translated_back.is_some(), "we know that the original value was in bounds, so translating back should return some");
        prop_assert_eq! (coefficients, translated_back.unwrap());
        
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
