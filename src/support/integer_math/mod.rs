use num::{self};
use num::traits::{WrappingAdd, WrappingSub, WrappingMul, CheckedShl, CheckedShr};
use std::mem;
use std::cmp::{min};
use std::ops::{Shl, Shr};

pub trait Integer: num::PrimInt + num::Integer + WrappingAdd + WrappingSub + WrappingMul + CheckedShl + CheckedShr + Shl <u32, Output = Self> + Shr <u32, Output = Self> {}
impl <T: num::PrimInt + num::Integer + WrappingAdd + WrappingSub + WrappingMul + CheckedShl + CheckedShr + Shl <u32, Output = Self> + Shr <u32, Output = Self> > Integer for T {}

/// Right-shift an integer, but round to nearest, with ties rounding to even.
///
/// This minimizes error, and avoids a directional bias.
pub fn shr_nicely_rounded <T: Integer> (input: T, shift: u32)->T {
  if shift == 0 {return input}
  let divisor = match T::one().checked_shl ( shift ) {Some (value) => value, None => return T::zero()};
  let half = T::one() << (shift - 1);
  let mask = divisor.wrapping_sub (&T::one());
  if (input & mask) == half {
    let shifted = input >> shift;
    shifted + (shifted & T::one())
  } else {
    (input + half) >> shift
  }
}

/// Left-shift an integer, returning Some(input*(2^shift)) if it fits within the type, None otherwise.
pub fn overflow_checked_shl <T: Integer> (input: T, shift: u32)->Option <T> {
  if input == T::zero() {return Some (T::zero())}
  let maximum = match T::max_value().checked_shr (shift) {
    None => return None,
    Some (value) => value,
  };
  if input > maximum {return None}
  let minimum = T::min_value() >> shift;
  if input < minimum {return None}
  Some (input << shift)
}

/// Compute the arithmetic mean of two integers, rounded towards negative infinity. Never overflows.
pub fn mean_floor <T: Integer> (first: T, second: T)->T {
  (first >> 1u32) + (second >> 1u32) + (first & second & T::one())
}

/// Compute the arithmetic mean of two integers, rounded towards positive infinity. Never overflows.
pub fn mean_ceil <T: Integer> (first: T, second: T)->T {
  (first >> 1u32) + (second >> 1u32) + ((first | second) & T::one())
}


pub mod polynomial;

#[cfg (test)]
mod tests {
  use super::*;
  use num::{One, ToPrimitive, Integer};
  use num::bigint::BigInt;
  use num::rational::{Ratio};
  use std::cmp::Ordering;
    
  fn perfect_shr_nicely_rounded <T: Integer> (input: T, shift: u32)->BigInt where BigInt: From <T> {
    let perfect_result = Ratio::new (BigInt::from (input), BigInt::one() << shift as usize);
    let rounded_down = perfect_result.floor();
    let fraction = & perfect_result - & rounded_down;
    let rounded_down = rounded_down.to_integer();
    match fraction.cmp (& Ratio::new (BigInt::one(), BigInt::one() << 1)) {
      Ordering::Less => rounded_down, Ordering::Greater => rounded_down + BigInt::one() ,
      Ordering::Equal => & rounded_down + rounded_down.mod_floor (& (BigInt::one() << 1)),
    }
  }
  
  #[test]
  fn test_shr_nicely_rounded() {
    let inputs: Vec<(i64, u32, i64)> = vec![
      (0, 0, 0), (0, 5, 0), (1, 3, 0), (4, 3, 0), (5, 3, 1),
      (999, 1, 500), (998, 1, 499), (997, 1, 498)
    ];
    for (input, shift, result) in inputs {
      assert_eq!(shr_nicely_rounded (input, shift), result);
      assert_eq!(shr_nicely_rounded (-input, shift), -result);
    }
  }
  
  quickcheck! {
    fn quickcheck_shr_nicely_rounded_signed (input: i32, shift: u8)->bool {
      let result = shr_nicely_rounded (input, shift as u32);
      let perfect_result = perfect_shr_nicely_rounded (input, shift as u32);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      perfect_result == BigInt::from (result)
    }
    
    fn quickcheck_shr_nicely_rounded_unsigned (input: u32, shift: u8)->bool {
      let result = shr_nicely_rounded (input, shift as u32);
      let perfect_result = perfect_shr_nicely_rounded (input, shift as u32);
      println!( "{:?}", (result, & perfect_result.to_str_radix (10)));
      perfect_result == BigInt::from (result)
    }
    
    fn quickcheck_overflow_checked_shl (input: i32, shift: u8)->bool {
      let result = overflow_checked_shl (input, shift as u32);
      let perfect_result = BigInt::from (input) << shift as usize;
      result == perfect_result.to_i32()
    }
  }
}
