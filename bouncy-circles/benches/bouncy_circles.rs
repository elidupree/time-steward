use criterion::{criterion_group, criterion_main, Criterion};

fn bouncy_circles_straightforward(c: &mut Criterion) {
  c.bench_function("bouncy_circles_straightforward", |b| {
    b.iter(|| {
      bouncy_circles::benchmarks::bouncy_circles_straightforward::<
        time_steward_simple_flat::Steward<bouncy_circles::physics::BouncyCirclesSpec>,
      >()
    })
  });
}

fn bouncy_circles_disturbed(c: &mut Criterion) {
  c.bench_function("bouncy_circles_disturbed", |b| {
    b.iter(|| {
      bouncy_circles::benchmarks::bouncy_circles_disturbed::<
        time_steward_simple_flat::Steward<bouncy_circles::physics::BouncyCirclesSpec>,
      >()
    })
  });
}
/*
#[bench]
fn bouncy_circles_disturbed_retroactive (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: amortized::Steward<SimulationSpec> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, EntityId::hash_of(&0), Initialize::new()).unwrap();
    steward.snapshot_before(& (10*SECOND)).expect("steward failed to provide snapshot");
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND, EntityId::hash_of (& index), Disturb::new ([ARENA_SIZE/3,ARENA_SIZE/3])).unwrap();
      steward.snapshot_before(& (10*SECOND)).expect("steward failed to provide snapshot");
    }
  })
}
*/

criterion_group!(
  benches,
  bouncy_circles_straightforward,
  bouncy_circles_disturbed
);
criterion_main!(benches);
