
use ::{Basics, TimeSteward, Accessor};


struct Mutator <B: Basics> {
  
}

struct DataTimelineHandle <B: Basics, Timeline: DataTimeline> {
  data: TypedArc::<B::GlobalLists, B::DataTimelinesList, DataTimelineId, T>>;
}

impl <B: Basics> ::Mutator for Mutator {
  fn create <T: DataTimeline> (&mut self, constants: D::Constants)->DataTimelineHandle <B, T> {
    DataTimelineHandle {
      data: TypedArc::<GlobalLists, StewardsTimesDataTimelines, DataTimelineId, Times <<Self as Accessor>::Steward, T>>::new (self.next_id(), T::from_constants (constants))
    }
  }
  fn do_operation <T: DataTimeline> (&mut self, timeline: &DataTimelineHandle <B, T>, operation: T::Operation) {
    // stewards::simplest always uhh...  reverts all later changes before uh...?
    timeline.differentiated_mut().insert_operation (self.extended_now(), operation, snapshots????);
  }
}

struct Steward <B: Basics> {
  snapshots: SnapshotTree <B>,
  constants: B::Constants,
}

impl <B: Basics> TimeSteward for Steward <B> {
  fn snapshot_before (&mut self, time: & Basics::Time)->Option <Self::Snapshot> {
    
  }
}
