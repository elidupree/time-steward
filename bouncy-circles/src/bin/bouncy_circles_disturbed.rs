fn main() {
  bouncy_circles::benchmarks::bouncy_circles_disturbed::<
    time_steward_simple_flat::Steward<bouncy_circles::physics::BouncyCirclesSpec>,
  >();
}
