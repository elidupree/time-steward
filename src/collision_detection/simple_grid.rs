use std::collections::HashSet;
use super::Nearness;
use {RowId, ColumnId, Column, Accessor, MomentaryAccessor, Mutator};
use std::marker::PhantomData;
use std::hash::Hash;

#[derive (Copy, Clone, Hash)]
pub struct Bounds {
  pub min: [i64; 2],
  pub max: [i64; 2],
}

pub trait Basics: super::Basics {
  fn get_bounds<A: MomentaryAccessor<<Self as super::Basics>::StewardBasics>>(accessor: &A,
                                                                              who: RowId)
                                                                              -> Bounds;
  fn when_escapes <A: Accessor <<Self as super::Basics>::StewardBasics>>(accessor: & A, who: RowId, bounds: Bounds)->Option <<<Self as super::Basics>::StewardBasics as ::Basics>::Time>;
}



struct Cell<B: Basics> {
  _marker: PhantomData<B>,
}
impl<B: Basics> Column for Cell<B> {
  type FieldType = HashSet <RowId>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0x030fc8868af34f2c)
  }
}

///TODO: this should not be public; it's only public so that it can be used in a macro expansion,
///which would be a generic function if rust had better polymorphism
#[derive (Clone)]
pub struct Member<B: Basics> {
  pub row: RowId,
  pub bounds: Bounds,
  pub detector: B::DetectorId,
  _marker: PhantomData<B>,
}
impl<B: Basics> Column for Member<B> {
  type FieldType = Member <B>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0xe844c90eaf2e20ab)
  }
}


fn cell_row<DetectorId: Hash>(x: i64, y: i64, me: DetectorId) -> RowId {
  RowId::new(&(x, y, me))
}
fn get_bounds<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                       who: RowId,
                                                       me: B::DetectorId)
                                                       -> Bounds {
  B::get_bounds(mutator, who)
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
                             _marker: PhantomData,
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

pub fn erase<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
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

#[macro_export]
macro_rules! simple_grid_predictor {
($module: ident, $basics: ident) => {{
  $module::Predictor::<<$basics as $crate::collision_detection::Basics>::StewardBasics> {
    predictor_id: PredictorId (<$basics as $crate::collision_detection::Basics>::nearness_column_id().0 ^ 0x3686689daa651bf3),
    column_id: $crate::collision_detection::simple_grid::Member::<$basics>::column_id(),
    function: Rc::new (| accessor, id | {
      let member;
      {
        let member_reference = accessor.get ::<$crate::collision_detection::simple_grid::Member <$basics>> (id).expect ("row is missing the field the predictor triggered on");
        member = (*member_reference).clone();
      }
      if let Some (time) = <$basics as $crate::collision_detection::simple_grid::Basics>::when_escapes (accessor, member.row, member.bounds) {
        accessor.predict_at_time (time, Rc::new (move | mutator | {
          //TODO: optimize erase-then-insert
          $crate::collision_detection::simple_grid::erase::<$basics,_> (mutator, member.row, member.detector, id, member.bounds);
          $crate::collision_detection::simple_grid::insert::<$basics,_> (mutator, member.row, member.detector);
        }));
      }
    }),
  }
}}
}
