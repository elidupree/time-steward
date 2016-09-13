# TimeSteward (under construction)

A game/simulation backend that automatically supports:
* [lockstep multiplayer](http://clintonbrennan.com/2013/12/lockstep-implementation-in-unity3d/) with reliable synchronization
* lag hiding
* asynchronous autosaves*
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
* Whether it was serialized and then deserialized at any particular time

In particular, you cannot use `f32`, `f64`, or `std::collections::HashMap`**.

TimeSteward provides some features work around these limitations. It has a built-in deterministic PRNG. It will eventually also provide a deterministic alternative to HashMap and a deterministic plug-and-play replacement for f32/f64. (However, using floats may still be undesirable because floating-point emulation is much slower.)

TimeSteward also provides a convenient system for running test simulations synchronized over one or more computers. If synchronization fails, the system can report the exact event where the first failure occurred.


## Detailed design

Coming soon...


## Example

Coming soon...


## Optimizing TimeSteward simulations

Coming later...


## License

MIT


## Footnotes

*Not yet, but it is in the works.

**Even if you use a deterministic hasher, Hash implementations are endian-unsafe, which makes the ordering of the elements nondeterministic across systems. Also, the default Serialize and Deserialize impls for HashMap do not record the current capacity, which makes ordering of the elements nondeterministic under serialization.
