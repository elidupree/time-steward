use bincode;
use rand::{self, ChaChaRng, Rng, RngCore};
use rand_core;
use serde::Serialize;
use siphasher::sip::SipHasher;
use std;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io::{self, Write};

/// A 128-bit random ID used for rows and ExtendedTimes.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DeterministicRandomId {
  data: [u64; 2],
}
pub struct SiphashIdGenerator {
  data: [SipHasher; 2],
}
impl Write for SiphashIdGenerator {
  fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
    self.data[0].write(bytes);
    self.data[1].write(bytes);
    Ok(bytes.len())
  }
  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
impl Default for SiphashIdGenerator {
  fn default() -> Self {
    Self::new()
  }
}
impl SiphashIdGenerator {
  pub fn generate(&self) -> DeterministicRandomId {
    DeterministicRandomId {
      data: [self.data[0].finish(), self.data[1].finish()],
    }
  }
  pub fn new() -> SiphashIdGenerator {
    SiphashIdGenerator {
      data: [
        SipHasher::new_with_keys(0xb82a9426fd1a574f, 0x9d9d5b703dcb1bcc),
        SipHasher::new_with_keys(0x03e0d6037ff980a4, 0x65b790a0825b83bd),
      ],
    }
  }
}
impl DeterministicRandomId {
  /// Generates a new DeterministicRandomId from any Serialize type, using a cryptographic hash function.
  ///
  /// Generally, calling this with unique inputs is guaranteed to give unique outputs.
  /// However, take note: the serialization doesn't generally include any representation of the
  /// *type* of data serialized, so (for instance) two different structs with the
  /// same fields will produce the same result. If you want to make sure and ID is unique,
  /// a good technique is to use a tuple containing a unique numeric literal
  /// (randomly-generated to be unique to that part of your code):
  ///
  /// ```rust
  /// # type YourDataType = Vec<u64>;
  /// # use time_steward::DeterministicRandomId;
  /// let id = DeterministicRandomId::new(&(0xdefacab1e_bad_1du64, YourDataType::new()));
  /// ```
  ///
  /// Why do we use Serialize rather than Hash?
  /// We need to make sure that DeterministicRandomIds do not vary with CPU endianness.
  /// The trait [Hasher](https://doc.rust-lang.org/std/hash/trait.Hasher.html) "represents the ability to hash an arbitrary stream of bytes",
  /// and typical implementations of Hash submit the bytes in the order they appear
  /// on the current system, not in a standardized order.
  /// Therefore, we generate IDs from Serialize implementors,
  /// because Serialize IS meant to be compatible between platforms.
  pub fn new<T: Serialize>(data: &T) -> DeterministicRandomId {
    let mut writer = SiphashIdGenerator::new();
    bincode::serialize_into(&mut writer, data, bincode::Infinite).unwrap();
    writer.generate()
  }
  /// Rather than implement Rand for this type, we make sure that it can
  /// ONLY be generated from specific RNGs known to be cryptographically secure.
  pub fn from_rng(rng: &mut ChaChaRng) -> DeterministicRandomId {
    DeterministicRandomId {
      data: [rng.gen::<u64>(), rng.gen::<u64>()],
    }
  }
  pub fn to_rng(self) -> DeterministicRandomIdRng {
    let mut result = DeterministicRandomIdRng {
      state: self,
      index: 0,
    };
    result.reroll();
    result
  }
  /// Useful for creating out-of-band values which don't identify actual
  /// events, for implementations that need them. A common usage is to have
  /// "before all other events" sentinels.
  pub fn from_raw(data: [u64; 2]) -> DeterministicRandomId {
    DeterministicRandomId { data }
  }
  /// TimeSteward implementors use this internally to make sure fiat events have unique ids.
  ///
  /// We combine fiat event ids with unique random data so that TimeSteward impls
  /// can trust them not to collide with *other* TimeIds.
  /// We use + instead of XOR so that this won't fail if the user accidentally
  /// or maliciously calls this BEFORE passing the ids in, too.
  #[doc(hidden)]
  pub fn for_fiat_event_internal(&self) -> DeterministicRandomId {
    DeterministicRandomId {
      data: [
        self.data[0].wrapping_add(0xc1d40daaee67461d),
        self.data[1].wrapping_add(0xb23ce1f459edefff),
      ],
    }
  }
  /// Returns the internal data of the DeterministicRandomId.
  pub fn data(&self) -> &[u64; 2] {
    &self.data
  }
  pub const MIN: DeterministicRandomId = DeterministicRandomId {
    data: [std::u64::MIN, std::u64::MIN],
  };
  pub const MAX: DeterministicRandomId = DeterministicRandomId {
    data: [std::u64::MAX, std::u64::MAX],
  };
}
impl fmt::Display for DeterministicRandomId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "id:{:016x}{:016x}", self.data[0], self.data[1])
  }
}
impl fmt::Debug for DeterministicRandomId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "id:{:016x}{:016x}", self.data[0], self.data[1])
  }
}

// (this hash function is consistent with the default PartialEq)
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for DeterministicRandomId {
  /// Since this id is already random, we don't need to hash more than 64 bits of it.
  /// We use the lower bits to avoid the small bias created by using the ordering.
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.data[1].hash(state);
  }
}

pub struct DeterministicRandomIdRng {
  state: DeterministicRandomId,
  index: u32,
}
impl DeterministicRandomIdRng {
  fn reroll(&mut self) {
    self.state = DeterministicRandomId::new(&self.state);
  }
}
impl RngCore for DeterministicRandomIdRng {
  fn next_u32(&mut self) -> u32 {
    let result = (self.state.data[(self.index >> 1) as usize] >> (32 * (self.index & 1))) as u32;
    self.index += 1;
    if self.index >= 4 {
      self.reroll();
      self.index = 0;
    }
    result
  }
  fn next_u64(&mut self) -> u64 {
    rand_core::impls::next_u64_via_u32(self)
  }
  fn fill_bytes(&mut self, destination: &mut [u8]) {
    rand_core::impls::fill_bytes_via_next(self, destination)
  }
  fn try_fill_bytes(&mut self, destination: &mut [u8]) -> Result<(), rand::Error> {
    self.fill_bytes(destination);
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde::Serialize;
  use std::fmt::Debug;

  #[test]
  fn index_1_is_lower_bits() {
    assert!(DeterministicRandomId { data: [1, 0] } > DeterministicRandomId { data: [0, 1] });
  }

  fn test_id_endianness_impl<T: Serialize + Debug>(thing: T, confirm: DeterministicRandomId) {
    println!(
      "DeterministicRandomId::new({:?}) = {:?}",
      thing,
      DeterministicRandomId::new(&thing)
    );
    assert_eq!(DeterministicRandomId::new(&thing), confirm);
  }

  #[test]
  fn test_id_endianness() {
    // note: these values should be correct if bincode serializes in little-endian order,
    // which is the current default.
    test_id_endianness_impl(
      (),
      DeterministicRandomId {
        data: [18033283813966546569, 10131395250899649866],
      },
    );
    test_id_endianness_impl(
      1337,
      DeterministicRandomId {
        data: [13768737740279017279, 4442460339052638143],
      },
    );
    let a: (Option<Option<i32>>,) = (Some(None),);
    test_id_endianness_impl(
      a,
      DeterministicRandomId {
        data: [16808472249412258235, 2826611911447572457],
      },
    );
    test_id_endianness_impl(
      DeterministicRandomId::new(&0x70f7b85b08ba4fd5u64),
      DeterministicRandomId {
        data: [15806623539012513099, 2804789490945853517],
      },
    );
  }
}
