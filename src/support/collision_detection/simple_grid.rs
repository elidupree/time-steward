use std::collections::HashSet;
use super::Nearness;
use {RowId, ColumnId, Column, Accessor, MomentaryAccessor, Mutator,
     ColumnType, EventType, PredictorType};
use std::marker::PhantomData;
use serde::Serialize;

#[derive (Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Bounds {
  pub min: [i64; 2],
  pub max: [i64; 2],
}

pub trait Basics: super::Basics {
  fn get_bounds<A: MomentaryAccessor<<Self as super::Basics>::StewardBasics>>(accessor: &A,
                                                                              who: RowId, detector: <Self as super::Basics>::DetectorId)
                                                                              -> Bounds;
  fn when_escapes <A: Accessor <<Self as super::Basics>::StewardBasics>>(accessor: & A, who: RowId, bounds: Bounds, detector: <Self as super::Basics>::DetectorId)->Option <<<Self as super::Basics>::StewardBasics as ::Basics>::Time>;
}



struct Cell<B: Basics> {
  _marker: PhantomData<B>,
}
impl<B: Basics> Column for Cell<B> {
  type FieldType = HashSet<RowId>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0x030fc8868af34f2c)
  }
}

// Work around private-in-public rules so that our TimeSteward types
// (which are implementation details) are not visible elsewhere.
mod hack {
  use super::*;
  use {RowId, PredictorId, EventId, Column};
  #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
  pub struct Member<B: Basics> {
    pub row: RowId,
    pub bounds: Bounds,
    pub detector: B::DetectorId,
  }
  time_steward_event! (
    pub struct BoundsChange <[B: Basics]> {id: RowId, member: Member <B>},
    <B as super::super::Basics>::StewardBasics,
    EventId (<B as super::super::Basics>::nearness_column_id().0 ^ 0x1be13fa07975f552),
    | &self, mutator | {
        //TODO: optimize remove-then-insert
        remove::<B,_> (mutator, self.member.row, self.member.detector, self.id, self.member.bounds);
        insert::<B,_> (mutator, self.member.row, self.member.detector);
      });
time_steward_predictor! {
  pub struct BoundsChangePredictor <[B: Basics]>,
  <B as super::super::Basics>::StewardBasics,
  PredictorId (<B as super::super::Basics>::nearness_column_id().0 ^ 0x3686689daa651bf3),
  Member::<B>::column_id(),
  | accessor, id | {
    let member;
    {
      let member_reference = accessor.get::<Member <B>> (id).expect ("row is missing the field the predictor triggered on");
      member = (*member_reference).clone();
    }
    if let Some (time) = B::when_escapes (accessor, member.row, member.bounds, member.detector) {
      accessor.predict_at_time (time, BoundsChange::new (id, member));
    }
  }
}
}
use self::hack::{Member, BoundsChange, BoundsChangePredictor};

impl<B: Basics> Column for Member<B> {
  type FieldType = Member<B>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0xe844c90eaf2e20ab)
  }
}


fn cell_row<DetectorId: Serialize>(x: i64, y: i64, me: DetectorId) -> RowId {
  RowId::new(&(x, y, me))
}
fn get_bounds<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                       who: RowId,
                                                       me: B::DetectorId)
                                                       -> Bounds {
  B::get_bounds(mutator, who, me)
}
pub fn insert<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                       who: RowId,
                                                       me: B::DetectorId) {
  let bounds = get_bounds::<B, M>(mutator, who, me);
  mutator.set::<Member<B>>(RowId::new(&(who, me)),
                           Some(Member {
                             row: who,
                             bounds: bounds,
                             detector: me,
                           }));
  for next in bounds.min[0]..bounds.max[0] + 1 {
    for what in bounds.min[1]..bounds.max[1] + 1 {
      let row = cell_row(next, what, me);
      let mut members = mutator.get::<Cell<B>>(row).cloned().unwrap_or(HashSet::new());
      for member in members.iter() {
        let (id, contents) = Nearness::new(who, member.clone(), me);
        // check first: no need to update last_change
        if mutator.get::<Nearness<B>>(id).is_none() {
          mutator.set::<Nearness<B>>(id, Some(contents));
        }
      }
      members.insert(who);
      mutator.set::<Cell<B>>(row, Some(members));
    }
  }
}

pub fn remove<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                      who: RowId,
                                                      me: B::DetectorId,
                                                      membership_id: RowId,
                                                      bounds: Bounds) {

  mutator.set::<Member<B>>(membership_id, None);
  for next in bounds.min[0]..bounds.max[0] + 1 {
    for what in bounds.min[1]..bounds.max[1] + 1 {
      let row = cell_row(next, what, me);
      let mut members = mutator.get::<Cell<B>>(row).cloned().unwrap_or(HashSet::new());
      for member in members.iter() {
        if *member != who {
          let (id, _) = Nearness::<B>::new(who, member.clone(), me);
          mutator.set::<Nearness<B>>(id, None);
        }
      }
      members.remove(&who);
      mutator.set::<Cell<B>>(row,
                             if members.is_empty() {
                               None
                             } else {
                               Some(members)
                             });
    }
  }
}



pub type TimeStewardTypes <B> = (ColumnType<Member<B>>, EventType <BoundsChange <B>>, PredictorType <BoundsChangePredictor <B>>);

