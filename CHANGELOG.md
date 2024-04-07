# Change Log

## 0.5.0 - 2024-04-07

- Redesign component access checking. Complex queries will now pass the component access checker in more cases.
- Improve diagnostics for component access errors.
- Redesign `Config` interface to hide implementation details. (Now named `HandlerConfig`).
- Removed `bit_set` and `sparse` modules from public API.
- Added `Fetcher::get_many_mut`.
- Removed `AliasedMutability` variant from `GetError` and added `GetManyMutError`.

## 0.4.2 - 2024-03-24

- Fixed bug in entity despawning.

## 0.4.1 - 2024-03-24

- Impl `Send` and `Sync` for `UnsafeWorldCell` and `Fetcher`.
- Removed dependency on `memoffset`.

## 0.4.0 - 2024-03-09

- Renamed "system" to "handler" to avoid confusion with other ECS libraries.
- Removed `Column::component_index`. To obtain the component index of a column, use `Archetype::component_indices`.
- Fixed bug related to handler ordering. Handlers with the same priority always run in the order they were added to the world.
- Changed alignment of identifier types to match `u64`.
- Renamed `Priority::{Before, Normal, After}` to `Priority::{High, Medium, Low}`.
- Renamed `World::{get_component, get_component_mut}` to `World::{get, get_mut}`.
- Fixed empty fetcher iteration bug.

## 0.3.0 - 2024-02-13

- Fixed bug that occurs when changing event target archetype while broadcasting.
- Changed behavior of event queue. The next event in the queue is handled only once the current event has finished broadcasting.
- Changed `EventPtr` interface and relaxed `Send` and `Sync` bounds for `EventMut`.
- Fixed buggy behavior when mutating the target of a broadcasting event.

## 0.2.2 - 2024-02-08

- Update documentation

## 0.2.1 - 2024-02-02

- Added Rayon parallel iterator for `Fetcher`, available behind the `rayon` feature flag.
- Fixed archetype refresh bugs and changed semantics of `refresh_archetype`.
- Fixed potential "cannot shadow tuple struct with let binding" errors in derived `Query` and `SystemParam` impls.
- Added `#[must_use]` attribute to iterators.
- Relaxed auto trait impls for various types.
- Minor performance improvements.

## 0.1.1 - 2024-01-25

Initial release
