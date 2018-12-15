use array_ext;
use arrayvec;
use smallvec;

pub trait Array:
  arrayvec::Array
  + smallvec::Array<Item = <Self as arrayvec::Array>::Item>
  + array_ext::Array<<Self as arrayvec::Array>::Item>
{
}

pub trait ReplaceItemType<U>: Array {
  type Type: Array + arrayvec::Array<Item = U> + array_ext::Array<U>;
}
pub trait SmallerArray: Array {
  type Type: Array;
}

macro_rules! array_impls {
  ($($size: expr,)*) => {$(

impl <T> Array for [T; $size] {}

impl <T,U> ReplaceItemType<U> for [T; $size] {
  type Type = [U; $size];
}



  )*};
}

macro_rules! smaller_array_impls {
  ($($size: expr,)*) => {$(

impl <T> SmallerArray for [T; $size] {
  type Type = [T; $size-1];
}

  )*};
}

array_impls!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,);
smaller_array_impls!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,);
