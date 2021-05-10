# TimeSteward (under construction)

A game/simulation backend that automatically supports:
* [lockstep multiplayer](http://clintonbrennan.com/2013/12/lockstep-implementation-in-unity3d/) with reliable synchronization
* lag hiding
* non-blocking autosaves*
* parallel computation*
* and other features.

The main catch is that you have to write the physics within the TimeSteward model. Any physics is possible to write using TimeSteward, but it may not be easy to convert an existing simulation to use it.


## Short overview

TimeSteward has one core trick: It can change an event in the past, then cheaply update the present. It doesn't need to redo any computations that weren't dependent on that change.

Every time any event occurs, the TimeSteward records what data that event examined and modified. Thus, it can maintain a dependency graph between events. Ideally, all events only access or modify data within a small neighborhood, and dependencies don't propagate very fast. If these conditions are met, making a change to the recent past is very cheap.

This naturally supports lockstep multiplayer. Each client simply runs the simulation in real-time, handling local inputs immediately. When it receives input from other clients, it inserts that input into history *at the time the input was sent*. Thus, all clients ultimately end up with the same input history.

Individual clients can also speculatively simulate into the future, which lets them smooth over moments when very costly computations occur for only a short time.

Because TimeSteward retains old data, you can also cheaply take out a handle to a snapshot of the simulation state. You can trust that the snapshot won't change as the simulation continues. This allows, for instance, saving the snapshot to disk in a different thread*, without freezing the game while the user waits for the save to finish.


## Gotchas

In order to remain synchronized, all code executing within the simulation must be **deterministic**. (This doesn't apply to inputs which are manually shared between the clients.) Being deterministic means it can only depend on data from within the simulation. It cannot depend on other things, such as:

* The local system time
* System random number generation
* The floating-point implementation of the local processor
* The endianness of the local processor
* Mutable global (static/thread_local) variables
* Whether data has been reloaded from a serialized version

In particular, your physics cannot depend on `f32` or `f64` arithmetic, the iteration order of `std::collections::HashMap`**, or the capacity of `Vec`.

TimeSteward provides some features to work around these limitations. It has a built-in deterministic PRNG. It will eventually also provide a deterministic alternative to HashMap and a deterministic plug-and-play replacement for f32/f64. (However, using floats may still be undesirable because floating-point emulation is much slower.)

TimeSteward also provides a convenient system for running test simulations synchronized over one or more computers. If synchronization fails, the system can report the exact event where the first failure occurred.


## Detailed design


### DataTimelines

All data that can *change over time* is stored in implementors of `trait DataTimeline`.

A DataTimeline is a **retroactive data structure**. You can insert operations in the present or past, and query it at any time in the present or past. The results of queries must depend only on the operations that exist at times earlier than the query – and not, for instance, on the order the operations were inserted. (You don't need to implement retroactive data structures yourself – TimeSteward has built-in types for common use cases. And if you do build your own, TimeSteward has features for testing that they behave correctly.)

A DataTimeline can also report *all* data existing as a specific time, as a snapshot. A snapshot taken at a specific time can be used to compute an exactly identical simulation, if the same user inputs are supplied after that.

DataTimelines can only change at discrete moments, and it is good to make those changes infrequent. If you want to represent, say, a moving object, the inner data should not just be the location of the object, but a representation of its trajectory over time:

```rust
struct Ball {
  // location at the last time the ball was modified
  location: [i64; 3],
  // velocity at the last time the ball was modified
  velocity: [i64; 3],
  // current constant acceleration – for instance, due to gravity or other forces
  acceleration: [i64; 3],
}
```

Thus, the data only needs to change when the forces on the ball change, such as when it runs into an object.

(In practice, the TimeSteward library provides implementations of a few trajectory types, so you may not have to implement this yourself. We will continue expanding the support libraries as development continues.)


### Predictions and Events

If the data doesn't normally change over time, how do we know when to make things happen?

An **Event** is sort of like an object of type `Fn(simulation state) -> results`. When called, it can query and modify DataTimelines at a specific time. It is also allowed to create and destroy **Predictions**. A Prediction is an Event, combined with a time at which it's expected to happen. If the Prediction isn't destroyed by the time, the event happens.

Imagine that one Event makes a ball is move towards a wall. From the current trajectory of the ball and the location of the wall, the event computes the time when the ball will hit the wall, then creates a Prediction of a collision at that time.

```rust
let ball: Ball = accessor.query (...);
let time = ... // Examine various fields and compute the time when the ball hits the wall
let prediction = accessor.create_prediction (time, BallHitsWallEvent {...});
```

If a later Event changes the motion of the ball, it should then destroy the original Prediction and create a new one based on the new trajectory.


As shown above, Events interact with the simulation through "accessor" objects. These objects are the way we track what queries and operations were made. Generally, it is an error for an Event to get information by any means other than the accessor (and `self`).

This system – Events automatically creating Predictions, Predictions automatically running Events – can implement a complete ongoing physics. The only thing missing is the way to add user input.

### FiatEvents

Events are the only thing that can change field data, but there are two ways Events can be created. One is to be predicted in a Prediction. The other is to be inserted from the outside by fiat. We call these FiatEvents. They usually represent user input, but they can also be based on the local time, instructions from a server, or other things. To keep simulations synchronized over a network, all FiatEvents, and *only* the FiatEvents, need to be shared between all clients.

### Ordering and EntityIds

If two Events are scheduled to happen at the same time, one of them technically has to happen before the other. For the simulation to be deterministic, the order has to be deterministic as well.

We accomplish this by using a cryptographic hash function. Each Event is given a EntityId – a unique 128 bit ID. Events happen in order by ID. For both Predictions and FiatEvents, the caller has to provide a unique random id. EntityId can easily be generated from any type that implements Serialize:

```rust
for time in 0..50 {
  if the user is holding down the red button {
    steward.insert_fiat_event(
      time,
      EntityId::hash_of(&time),
      UserContinuesHoldingdownRedButtonEvent::new());
  }
}
```

A typical choice for FiatEvents would be to hash together a tuple of `(time, ID of user who gave the input, enum indicating the type of input)`. A typical choice for Predictions would be to hash together the time-id of the creating event with something unique to the prediction, like a unique id of an object being predicted about, or coordinates of a cell in a grid that's being predicted about.


### ExtendedTime

There's a special case when and Event creates a Prediction at the same time as the Event. For instance, imagine that a ball is going to collide with *two* walls at the same time, like in a corner. One of the events happens first, and the ball is deflected away from the one wall. That event then predicts when the ball will collide with the other wall – which is zero time from now. It might generate a time ID that comes *before* the ID of the first Event!

To deal with this, we still make the second Event happen at the same numerical time, but in a later **iteration**. This gives rise to the concept of an **ExtendedTime**, which is defined approximately as follows:

```rust
struct ExtendedTime {
  base: Time,
  iteration: u32,
  id: TimeId,
}
```

ExtendedTimes are lexicographically ordered by the fields listed above. TimeSteward users usually don't need to be aware of ExtendedTimes (just implement your Events in terms of regular time, and they will likely turn out fine). However, it *is* possible for TimeSteward users to examine ExtendedTimes, which can be useful for debugging and loop detection.


### PersistentTypeIds and serialization

All the physics-related data of a TimeSteward forms a network of DataTimelines, Events, and Predictions. These objects can contain handles to each other. DataTimeline changes are a form of interior mutability, which allows the formation of cyclic data structures. This makes them tricky to serialize, but TimeSteward does provide a way to serialize and deserialize the collection as a whole.

For various reasons, it's sometimes convenient to allow the handles to *not know the concrete type* of the objects they're pointing to, like trait objects. This complicates serialization, because we need to store some record of what type the objects are. Rust exposes TypeIds, but they're explicitly nondeterministic over multiple builds.

So, we require that some of the types used in simulation state to have hard-coded IDs. These IDs are simply one random u64 for each type. (Because there are fewer of them, they don't need to have as many bits to stay unique. Thanks to the [birthday problem](https://en.wikipedia.org/wiki/Birthday_problem), this would have a >1% chance of a collision with a mere 700 million types. I don't think we need to worry about this. 128 bit IDs are necessary for events, because computers can generate billions of them easily, but this isn't the same situation.) The documentation provides a convenient way to generate these IDs. We could theoretically have these IDs be automatically generated from the type name and module path, which would make them unique, but hard-coding them helps keep serialization consistent from version to version of your program. (You wouldn't want savefiles to be incompatible just because you reorganized some modules.)


### Invalidation, invariants, and auditing

A TimeSteward may run events out of order. Let's define the **canonical state** to be the exact history that results if you run all of the events **in order**. We want the history to eventually reach the canonical state regardless of what order the events are run. In particular, if a later event runs first, then an earlier event may change some data that the later event queried, making the later event invalid. If that happens, the later event must be rerun with the new inputs.

Each Event must:
* implement a way for the event to be undone.
* whenever the event modifies a DataTimeline, inform the TimeSteward of all future events whose queries to that DataTimeline would return a different result. (False-positives are okay, but false-negatives are not.)

TimeSteward *could* automatically track which events queried which DataTimelines, but that would be inefficient – in most cases, it would store many redundant dependencies. For instance, a certain type of event might be centered on a certain tile of a grid, and query the 25 tiles closest to it. An automatic system would store 25 handles to the event. But it would be possible to only store one, in the center tile. The implementor would simply know that when you modify one tile, you need to invalidate the events in surrounding tiles as well.

So instead of automatically tracking everything *during normal simulations*, TimeSteward provides features to *audit that your more efficient behavior was correct* after-the-fact.

To guarantee determinism, we require this invariant: "The history **prior to the first invalid event** is always in the canonical state." Note that for this definition, a Prediction that exists but hasn't been run yet is also considered "invalid", because the "valid" state is the one where it **has** been run.

This is sufficient to uphold determinism (we can simply keep running/undoing/rerunning invalid events until we reach the canonical state), but hard to audit in a helpful way. On one hand, whenever we run the first invalid event, we know that it receives canonical inputs, and we can audit that it leaves the history in the canonical state up through the next invalid event. On the other hand, imagine that, near the beginning of the simulation, an unimportant event E was invalidated, but not undone. Then the simulation continued for a while, executing hundreds of events unrelated to E. Only after that, E was rerun – and TimeSteward detected that it the history was now inconsistent with the canonical state! That isn't very useful. We have no idea which of the hundreds of events was responsible for the inconsistency.

Invalid events raise a problem for tightening this condition. We'd like to be able to say, "The history depends solely on the executed-events (events that have been executed but not undone)." But if you had a timeline like this:

Time 0 – Undone event E – Invalidated, but not undone, event F – Valid event G

Then the state after F could legitimately depend on whether F's execution happened *before* or *after* E was undone. So we need a weaker invariant.

Luckily, we can check the canonicity of individual DataTimelines. So specifically, "For any DataTimeline D, D's history **prior to the first noncanonical event that modifies D** is always in the canonical state." Again, we need to clarify the definition of noncanonical event – this includes both **events that have been executed with noncanonical inputs and modified D**, and also **events that canonically modify D, but have not been executed canonically**. (TODO: Can we prove that this is stricter than the first invariant? What do we need to know in order to know that there can't be any noncanonical events prior to the first invalid event?)




## Example

Coming soon...


## Optimizing TimeSteward simulations

Coming later...


## Keywords

TimeSteward uses **incremental processing** to be a **retroactive data structure**. It's also a **partially persistent data structure**, in the sense that snapshots don't change even if you make retroactive modifications after taking them. I didn't need these terms for the explanation, but I want them to appear in this document to attract people who are doing web searches for "incremental processing game physics" or similar.


## License

MIT


## Footnotes

*Not yet, but it is in the works.

**Even if you use a deterministic hasher, Hash implementations are endian-unsafe, which makes the ordering of the elements nondeterministic across systems. Also, the default Serialize and Deserialize impls for HashMap do not record the current capacity, which makes ordering of the elements nondeterministic under serialization.
