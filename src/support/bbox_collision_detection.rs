use ::{DeterministicRandomId, PersistentTypeId, ListedType, PersistentlyIdentifiedType, SimulationStateData, DataHandleTrait, DataTimelineCellTrait, ExtendedTime};
use ::stewards::{simple_full as steward_module};
use self::steward_module::{TimeSteward, Event, DataTimelineCellReadGuard, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use self::simple_timeline::{SimpleTimeline, GetVarying, tracking_query, tracking_query_ref, set, unset};


pub type Coordinate = u32;
pub type NumDimensions = u32;


pub trait BoundingBoxCollisionDetectable: PersistentlyIdentifiedType {
  type Steward: TimeSteward;
  type Object: SimulationStateData + PersistentlyIdentifiedType;
  type Space; // a way to distinguish if the same object can be put in multiple collision detectors of the same type. Can easily be () if not.
  
  const DIMENSIONS: NumDimensions;

  // An Object generally has to store some opaque data for the collision detector.
  // It would normally include a DataHandle to a tree node.
  // These are getter and setter methods for that data.
  fn get_collision_detector_data<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, object: &DataHandle<Self::Object>, space: &Self::Space)->Option<DataTimelineCellReadGuard<ObjectVarying<Self>>>;
  fn set_collision_detector_data<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, object: &DataHandle<Self::Object>, space: &Self::Space, data: Option<ObjectVarying<Self>>);

  fn calculate_current_bounding_box<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, object: &DataHandle<Self::Object>, space: &Self::Space)->BoundingBox;
  fn when_escapes<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, object: &DataHandle<Self::Object>, space: &Self::Space, BoundingBox)-><Self::Steward as TimeSteward>::Basics::Time;
  
  fn become_nearby<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, objects: [&DataHandle<Self::Object>; 2], space: &Self::Space) {}
  fn stop_being_nearby<A: EventAccessor <Steward = Self::Steward>>(accessor: &A, objects: [&DataHandle<Self::Object>; 2], space: &Self::Space) {}
}

pub struct BoundingBox<B: BoundingBoxCollisionDetectable> {
  pub bounds: [[Coordinate; 2]; B::DIMENSIONS],
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Node<B: BoundingBoxCollisionDetectable> {
  size_shift: u32,
  center: [Coordinate; 2],
  parent: Option<DataHandle <Node <B>>>,
  larger_cousins: [[Option<DataHandle <Node <B>>>; 2]; B::DIMENSIONS],
  varying: NodeVarying <B>,
}
impl<B: BoundingBoxCollisionDetectable> PersistentlyIdentifiedType for Node<B> {
  const ID: PersistentTypeId = PersistentTypeId(B::ID.0 ^ 0x7c4c8993671ba023);
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying<B: BoundingBoxCollisionDetectable> {
  children: [Option<DataHandle <Node <B>>>; 1<<B::DIMENSIONS],
  overlapping_live_nodes: Vec<DataHandle <B::Object>>,
  objects: Vec<DataHandle <B::Object>>,
}

pub struct ObjectVarying<B: BoundingBoxCollisionDetectable> {
  node: DataHandle <Node <B>>,
}
pub struct BoundingBoxCollisionDetector<B: BoundingBoxCollisionDetectable> {
  root: DataHandle <Node <B>>,
}

impl<B: BoundingBoxCollisionDetectable> BoundingBoxCollisionDetector<B> {
  fn insert<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space, location_hint: Option <& DataHandle<B::Object>>) {
  
  }
  fn remove<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space) {
  
  }
  fn neighbors<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space) -> Iter {
  
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct EscapesBounds<B: BoundingBoxCollisionDetectable> {}
impl<B: BoundingBoxCollisionDetectable> PersistentlyIdentifiedType for EscapesBounds<B> {
  const ID: PersistentTypeId = PersistentTypeId(B::ID.0 ^ 0xb0cdbe951b688b70);
}
impl<B: BoundingBoxCollisionDetectable> Event for EscapesBounds<B> {
  type Steward = B::Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {

  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

