use std::cmp::Ordering;
use std::fmt;

use super::api::*;
use crate::EntityId;

impl EntityHandle for EntityId {
  fn id(&self) -> EntityId {
    *self
  }
}

impl<T: SimulationStateData> Modify<T> for ReplaceWith<T> {
  type UndoData = T;
  fn modify(self, entity: &mut T) -> Self::UndoData {
    ::std::mem::replace(entity, self.0)
  }
  fn undo(entity: &mut T, undo_data: &Self::UndoData) {
    *entity = undo_data.clone()
  }
}

impl<B: fmt::Display> fmt::Display for ValidSince<B> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ValidSince::TheBeginning => write!(f, "TheBeginning"),
      ValidSince::Before(something) => write!(f, "Before({})", something),
      ValidSince::After(something) => write!(f, "After({})", something),
    }
  }
}

impl<T: Ord> Ord for ValidSince<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&ValidSince::TheBeginning, &ValidSince::TheBeginning) => Ordering::Equal,
      (&ValidSince::TheBeginning, _) => Ordering::Less,
      (_, &ValidSince::TheBeginning) => Ordering::Greater,
      (&ValidSince::Before(ref something), &ValidSince::Before(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::After(ref something), &ValidSince::After(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::Before(ref something), &ValidSince::After(ref anything)) => {
        if something <= anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      (&ValidSince::After(ref something), &ValidSince::Before(ref anything)) => {
        if something < anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T> PartialEq<T> for ValidSince<T> {
  fn eq(&self, _: &T) -> bool {
    false
  }
}

impl<T: Ord> PartialOrd<T> for ValidSince<T> {
  fn partial_cmp(&self, other: &T) -> Option<Ordering> {
    Some(match self {
      ValidSince::TheBeginning => Ordering::Less,
      ValidSince::Before(something) => {
        if something <= other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      ValidSince::After(something) => {
        if something < other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    })
  }
}
impl<T: Ord> PartialOrd for ValidSince<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
// impl <T: Ord> PartialOrd <ValidSince <T>> for T {
//  fn partial_cmp (&self, other: & ValidSince <T>)->Option <Ordering> {
//    Some (other.partial_cmp (self).unwrap().reverse());
//  }
// }
