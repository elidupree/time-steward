#[doc(hidden)]
#[macro_export]
macro_rules! time_steward_define_bbox_collision_detection {
  () => {
    time_steward_define_bbox_collision_detection!(2, bbox_collision_detection_2d);
    time_steward_define_bbox_collision_detection!(3, bbox_collision_detection_3d);
  };

  ($dims: expr, $mod: ident) => {
    pub mod $mod {
      use super::simple_timeline::{query, set, SimpleTimeline};
      use super::*;
      use super::{
        DataHandle, DataTimelineCell, Event, EventAccessor, FutureCleanupAccessor, TimeSteward,
      };
      use array_ext::*;
      use rpds::RedBlackTreeMap;
      use std::collections::HashSet;
      use crate::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
      use crate::type_utils::list_of_types::{ListedType};
      use crate::{DeterministicRandomId, QueryResult, SimulationStateData};

      pub type Coordinate = u64;
      pub type NumDimensions = u32;

      const DIMENSIONS: NumDimensions = $dims;

      /// If there's only one interpretation of how your objects are arranged in space, it makes sense to implement this on a unit-like struct.
      pub trait Space: SimulationStateData + PersistentlyIdentifiedType {
        type Steward: TimeSteward;
        type Object: SimulationStateData + PersistentlyIdentifiedType;
        type DetectorDataPerObject: QueryResult;
        type UniqueId: SimulationStateData;

        const DIMENSIONS: NumDimensions;

        // An Object generally has to store some opaque data for the collision detector.
        // It would normally include a DataHandle to a tree node.
        // These are getter and setter methods for that data.
        fn get_detector_data<A: Accessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          object: &DataHandle<Self::Object>,
        ) -> Option<Self::DetectorDataPerObject>;
        fn set_detector_data<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          object: &DataHandle<Self::Object>,
          data: Option<Self::DetectorDataPerObject>,
        );

        // must be unique among ALL objects/space pairs
        fn unique_id<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          object: &DataHandle<Self::Object>,
        ) -> Self::UniqueId;

        fn current_bounding_box<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          object: &DataHandle<Self::Object>,
        ) -> BoundingBox<Self>;
        fn when_escapes<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          object: &DataHandle<Self::Object>,
          _: BoundingBox<Self>,
        ) -> Option<<<Self::Steward as TimeSteward>::Basics as Basics>::Time>;

        #[allow(unused_variables)]
        fn become_neighbors<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          objects: [&DataHandle<Self::Object>; 2],
        ) {
        }
        #[allow(unused_variables)]
        fn stop_being_neighbors<A: EventAccessor<Steward = Self::Steward>>(
          &self,
          accessor: &A,
          objects: [&DataHandle<Self::Object>; 2],
        ) {
        }
      }

      pub trait Detector: SimulationStateData + PersistentlyIdentifiedType {
        type Space: Space;
        //type DetectorDataPerObject: SimulationStateData;

        fn insert<A: EventAccessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          object: &DataHandle<<Self::Space as Space>::Object>,
          location_hint: Option<&DataHandle<<Self::Space as Space>::Object>>,
        );
        fn remove<A: EventAccessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          object: &DataHandle<<Self::Space as Space>::Object>,
        );
        fn changed_position<A: EventAccessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          object: &DataHandle<<Self::Space as Space>::Object>,
        );
        fn changed_course<A: EventAccessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          object: &DataHandle<<Self::Space as Space>::Object>,
        );
        fn objects_near_object<A: Accessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          object: &DataHandle<<Self::Space as Space>::Object>,
        ) -> Vec<DataHandle<<Self::Space as Space>::Object>>;
        fn objects_near_box<A: Accessor<Steward = <Self::Space as Space>::Steward>>(
          accessor: &A,
          detector: &DataHandle<Self>,
          bounds: BoundingBox<Self::Space>,
          location_hint: Option<&DataHandle<<Self::Space as Space>::Object>>,
        ) -> Vec<DataHandle<<Self::Space as Space>::Object>>;
      }

      #[derive(Serialize, Deserialize, Debug, Derivative)]
      #[derivative(Clone(bound = ""), PartialEq(bound = ""), Eq(bound = ""))]
      #[serde(bound = "")]
      pub struct BoundingBox<S: Space> {
        pub bounds: [[Coordinate; 2]; DIMENSIONS as usize],
        pub _marker: PhantomData<S>, // waiting for the DIMENSIONS associated constant to be usable
      }

      impl<S: Space> BoundingBox<S> {
        pub fn new(bounds: [[Coordinate; 2]; DIMENSIONS as usize]) -> Self {
          BoundingBox {
            bounds: bounds,
            _marker: PhantomData,
          }
        }
        pub fn centered(center: [Coordinate; DIMENSIONS as usize], radius: Coordinate) -> Self {
          Self::new(Array::from_fn(|dimension| {
            [center[dimension] - radius, center[dimension] + radius]
          }))
        }
        pub fn locations(&self) -> Vec<[Coordinate; DIMENSIONS as usize]> {
          for bounds in self.bounds.iter() {
            assert!(bounds[0] <= bounds[1], "invalid bounding box");
          }
          let mut result = Vec::new();
          let mut position = self.bounds.map(|bounds| bounds[0]);
          'outer: loop {
            result.push(position);
            for dimension in 0..DIMENSIONS as usize {
              position[dimension] += 1;
              if position[dimension] <= self.bounds[dimension][1] {
                continue 'outer;
              }
              position[dimension] = self.bounds[dimension][0];
            }
            break 'outer;
          }
          result
        }

        pub fn contains_location(&self, location: [Coordinate; DIMENSIONS as usize]) -> bool {
          self.bounds.iter().enumerate().all(|(dimension, bounds)| {
            location[dimension] >= bounds[0] && location[dimension] <= bounds[1]
          })
        }
      }

      pub mod simple_grid {
        use super::*;
        //use array_ext::*;

        pub type Types<S> = (ListedType<SimpleGridDetector<S>>, ListedType<Escape<S>>);

        #[derive(Serialize, Deserialize, Debug)]
        #[serde(bound = "")]
        pub struct SimpleGridDetector<S: Space> {
          space: S,
          cell_size: Coordinate,
          cells: DataTimelineCell<
            SimpleTimeline<RedBlackTreeMap<[Coordinate; DIMENSIONS as usize], Cell<S>>, S::Steward>,
          >,
        }
        impl<S: Space> PersistentlyIdentifiedType for SimpleGridDetector<S> {
          const ID: PersistentTypeId = PersistentTypeId(0x6763f785bae6fe43 ^ S::ID.0);
        }

        #[derive(Serialize, Deserialize, Debug, Derivative)]
        #[derivative(Clone(bound = ""), PartialEq(bound = ""), Eq(bound = ""))]
        #[serde(bound = "")]
        pub struct DetectorDataPerObject<S: Space> {
          current_grid_bounds: BoundingBox<S>,
          escapes_bounds_prediction: Option<<S::Steward as TimeSteward>::EventHandle>,
        }
        #[derive(Serialize, Deserialize, Debug, Derivative)]
        #[derivative(
          Clone(bound = ""),
          PartialEq(bound = ""),
          Eq(bound = ""),
          Default(bound = "")
        )]
        #[serde(bound = "")]
        struct Cell<S: Space> {
          objects: Vec<DataHandle<S::Object>>,
        }
        #[derive(Serialize, Deserialize, Debug, Derivative)]
        #[derivative(Clone(bound = ""))]
        #[serde(bound = "")]
        pub struct Escape<S: Space> {
          detector: DataHandle<SimpleGridDetector<S>>,
          object: DataHandle<S::Object>,
        }
        impl<S: Space> PersistentlyIdentifiedType for Escape<S> {
          const ID: PersistentTypeId = PersistentTypeId(0xf693c99eca6bee45 ^ S::ID.0);
        }
        impl<S: Space<DetectorDataPerObject = DetectorDataPerObject<S>>> Event for Escape<S> {
          type Steward = S::Steward;
          type ExecutionData = ();
          fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(
            &self,
            accessor: &mut Accessor,
          ) {
            SimpleGridDetector::changed_position(accessor, &self.detector, &self.object);
          }
          fn undo<Accessor: FutureCleanupAccessor<Steward = Self::Steward>>(
            &self,
            _accessor: &mut Accessor,
            _: (),
          ) {
            unimplemented!()
          }
        }

        impl<S: Space<DetectorDataPerObject = DetectorDataPerObject<S>>> Detector
          for SimpleGridDetector<S>
        {
          type Space = S;
          //type DetectorDataPerObject = DetectorDataPerObject;

          fn insert<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
            _location_hint: Option<&DataHandle<S::Object>>,
          ) {
            let new_bounds = detector.space.current_bounding_box(accessor, object);
            let new_grid_bounds = detector.grid_box(&new_bounds);
            Self::update(accessor, detector, object, Some(new_grid_bounds));
          }
          fn remove<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
          ) {
            Self::update(accessor, detector, object, None);
          }

          fn changed_position<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
          ) {
            Self::insert(accessor, detector, object, None);
          }

          fn changed_course<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
          ) {
            let mut data = match detector.space.get_detector_data(accessor, object) {
              None => return,
              Some(a) => a,
            }
            .clone();

            data.escapes_bounds_prediction =
              Self::create_prediction(accessor, detector, object, &data.current_grid_bounds);

            detector
              .space
              .set_detector_data(accessor, object, Some(data));
          }

          fn objects_near_object<A: Accessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
          ) -> Vec<DataHandle<S::Object>> {
            let data = match detector.space.get_detector_data(accessor, object) {
              None => return Vec::new(),
              Some(a) => a,
            };
            let cells = query(accessor, &detector.cells);
            let mut result = Vec::new();
            for location in data.current_grid_bounds.locations() {
              if let Some(cell) = cells.get(&location) {
                for neighbor in cell.objects.iter() {
                  if neighbor != object && !result.contains(neighbor) {
                    result.push(neighbor.clone());
                  }
                }
              }
            }
            result
          }

          fn objects_near_box<A: Accessor<Steward = <Self::Space as Space>::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            bounds: BoundingBox<Self::Space>,
            _location_hint: Option<&DataHandle<<Self::Space as Space>::Object>>,
          ) -> Vec<DataHandle<<Self::Space as Space>::Object>> {
            let cells = query(accessor, &detector.cells);
            let mut result_existences = HashSet::new();
            let mut result = Vec::new();
            for location in detector.grid_box(&bounds).locations() {
              if let Some(cell) = cells.get(&location) {
                for neighbor in cell.objects.iter() {
                  if result_existences.insert(neighbor) {
                    result.push(neighbor.clone());
                  }
                }
              }
            }
            result
          }
        }

        impl<S: Space<DetectorDataPerObject = DetectorDataPerObject<S>>> SimpleGridDetector<S> {
          pub fn new<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            space: S,
            cell_size: Coordinate,
          ) -> DataHandle<Self> {
            let result = accessor.new_handle(SimpleGridDetector {
              space: space,
              cell_size: cell_size,
              cells: DataTimelineCell::new(SimpleTimeline::new()),
            });
            set(accessor, &result.cells, RedBlackTreeMap::new());
            result
          }
          fn grid_box(&self, exact_box: &BoundingBox<S>) -> BoundingBox<S> {
            BoundingBox {
              bounds: Array::from_fn(|dimension| {
                Array::from_fn(|direction| {
                  (exact_box.bounds[dimension][direction]
                    + (direction as Coordinate) * (self.cell_size - 1))
                    / self.cell_size
                })
              }),
              _marker: PhantomData,
            }
          }
          fn real_box_from_grid(&self, grid_box: &BoundingBox<S>) -> BoundingBox<S> {
            BoundingBox {
              bounds: Array::from_fn(|dimension| {
                Array::from_fn(|direction| grid_box.bounds[dimension][direction] * self.cell_size)
              }),
              _marker: PhantomData,
            }
          }
          fn update<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
            new_bounds: Option<BoundingBox<S>>,
          ) {
            let old_data = detector.space.get_detector_data(accessor, object);

            let mut cells = query(accessor, &detector.cells);
            let mut new_neighbors = Vec::new();
            let mut old_neighbors = Vec::new();
            //printlnerr!("{:?}", (& detector.space. unique_id (accessor, object), &new_bounds));
            if let Some(new_bounds) = new_bounds.as_ref() {
              for location in new_bounds.locations() {
                let mut cell = cells.get(&location).cloned().unwrap_or(Default::default());
                //printlnerr!("{:?}", (& location, cell.objects.iter().map (| object |detector.space. unique_id (accessor, object)).collect::<Vec<_>>()));
                for neighbor in cell.objects.iter() {
                  if neighbor != object && !new_neighbors.contains(neighbor) {
                    new_neighbors.push(neighbor.clone());
                  }
                }
                if !cell.objects.contains(object) {
                  cell.objects.push(object.clone());
                  cells = cells.insert(location, cell);
                }
              }
            }
            if let Some(old_data) = old_data.as_ref() {
              for location in old_data.current_grid_bounds.locations() {
                let mut cell = cells.get(&location).unwrap().clone();
                for neighbor in cell.objects.iter() {
                  if neighbor != object && !old_neighbors.contains(neighbor) {
                    old_neighbors.push(neighbor.clone());
                  }
                }
                if new_bounds
                  .as_ref()
                  .map_or(true, |new_bounds| !new_bounds.contains_location(location))
                {
                  cell.objects.retain(|a| a != object);
                  if cell.objects.is_empty() {
                    cells = cells.remove(&location);
                  } else {
                    cells = cells.insert(location, cell);
                  }
                }
              }
            }

            for neighbor in old_neighbors.iter() {
              if !new_neighbors.contains(&neighbor) {
                detector
                  .space
                  .stop_being_neighbors(accessor, [object, neighbor]);
              }
            }
            for neighbor in new_neighbors.iter() {
              if !old_neighbors.contains(&neighbor) {
                detector
                  .space
                  .become_neighbors(accessor, [object, neighbor]);
              }
            }

            let new_data = new_bounds.map(|new_bounds| DetectorDataPerObject {
              current_grid_bounds: new_bounds.clone(),
              escapes_bounds_prediction: Self::create_prediction(
                accessor,
                detector,
                object,
                &new_bounds,
              ),
            });

            detector.space.set_detector_data(accessor, object, new_data);
            set(accessor, &detector.cells, cells);
          }
          fn create_prediction<A: EventAccessor<Steward = S::Steward>>(
            accessor: &A,
            detector: &DataHandle<Self>,
            object: &DataHandle<S::Object>,
            grid_box: &BoundingBox<S>,
          ) -> Option<<S::Steward as TimeSteward>::EventHandle> {
            detector
              .space
              .when_escapes(accessor, object, detector.real_box_from_grid(&grid_box))
              .map(|time| {
                let time_id = accessor.extended_now().id;
                accessor.create_prediction(
                  time,
                  DeterministicRandomId::new(&(
                    0xe5d3c2856ad28befu64,
                    time_id,
                    detector.space.unique_id(accessor, object),
                  )),
                  Escape {
                    detector: detector.clone(),
                    object: object.clone(),
                  },
                )
              })
          }
        }
      }

      /*
      pub mod tree {

      use super::*;

      trait NodeAugmentation <S: Space>: Default {
        fn start_overlapping <A: EventAccessor <Steward = <Self::Space as Space>::Steward> (&mut self, accessor: &A, object: & DataHandle <S::Object>) ;
        fn stop_overlapping <A: EventAccessor <Steward = <Self::Space as Space>::Steward> (&mut self, accessor: &A, object: & DataHandle <S::Object>) ;
      }
      trait AugmentedSearchable: Detector {
        fn search <A: Accessor <Steward = <Self::Space as Space>::Steward>, F: Fn (& augmentation)->bool>(accessor: &A, detector: &DataHandle<Self>, bounds: BoundingBox <Self::Space>, location_hint: Option < &DataHandle<<Self::Space as Space>::Object>>, filter: F) ->Vec<DataHandle<<Self::Space as Space>::Object>>;

      }

      }*/

      /*
      struct NodeBounds<B: BoundingBoxCollisionDetectable> {
        half_size_shift: u32,
        center: [Coordinate; B::DIMENSIONS],
      }
      fn smallest_containing_node_bounds <B: BoundingBoxCollisionDetectable> (bounds: & BoundingBox<B>)->NodeBounds <B> {
        unimplemented!()
      }

      #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
      struct Node<B: BoundingBoxCollisionDetectable> {
        bounds: NodeBounds<B>,
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

      pub struct ObjectData<B: BoundingBoxCollisionDetectable> {
        varying: ObjectVarying<B>,
      }
      struct ObjectVarying<B: BoundingBoxCollisionDetectable> {
        node: DataHandle <Node <B>>,
      }
      pub struct BoundingBoxCollisionDetector<B: BoundingBoxCollisionDetectable> {
        root: DataHandle <Node <B>>,
      }

      impl<B: BoundingBoxCollisionDetectable> BoundingBoxCollisionDetector<B> {
        pub fn insert<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space, location_hint: Option <& DataHandle<B::Object>>) {

        }
        pub fn remove<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space) {

        }
        pub fn neighbors<A: EventAccessor <Steward = B::Steward>>(accessor: &A, object: &DataHandle<B::Object>, space: &B::Space) -> Iter {

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


      impl<B: BoundingBoxCollisionDetectable> BoundingBoxCollisionDetector<B> {
        fn reposition <A: EventAccessor <Steward = B::Steward>> (accessor: &A, object: &DataHandle<B::Object>) {
          let varying = tracking_query (accessor, object.varying);
          let mut current_node = varying.node;
          let new_bounds = B::calculate_current_bounding_box (object);
          let destination_node_bounds = smallest_containing_node_bounds (new_bounds) ;
          let bigger_shift = max (current_node.bounds.half_size_shift, destination_node_bounds.half_size_shift);

          while current_node.bounds.half_size_shift < bigger_shift {
            current_node = current_node.parent.expect("All nodes except the root node have to have a parent, and we should never be trying to navigate to a node bigger than the root");
          }

          let mut destination_ancestor_bounds = destination_node_bounds;
          while destination_ancestor_bounds.half_size_shift < bigger_shift {
            destination_ancestor_bounds = parent_bounds(destination_ancestor_bounds);
          }

          while largest_single_dimension_distance (current_node.bounds, destination_ancestor_bounds) > (1 << destination_ancestor_bounds.half_size_shift) {
            destination_ancestor_bounds = parent_bounds(destination_ancestor_bounds);
            current_node = current_node.parent.expect("All nodes except the root node have to have a parent, and we should never be trying to navigate to a node bigger than the root");
          }

          if current_node.bounds != destination_ancestor_bounds {
            // they should now be the same size and half- or all-overlapping in each dimension.
          }
        }
      }

      */
    }
  };
}
