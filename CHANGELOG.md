# Change Log

## Unreleased

- Removed `Column::component_index`. To obtain the component index of a column, use `Archetype::component_indices`.
- Fixed bug related to system ordering. Systems with the same priority always run in the order they were added to the world.

## 0.3.0 - 2024-02-13

- Fixed bug that occurs when changing event target archetype while broadcasting.
- Changed behavior of event queue. The next event in the queue is handled only once the current event has finished broadcasting.
- Changed `EventPtr` interface and relaxed `Send` and `Sync` bounds for `EventMut`.
- Fixed buggy behavior when mutating the target of a broadcasting event.

## 0.2.2 - 2024-02-8

- Update documentation

## 0.2.1 - 2024-02-2

- Added Rayon parallel iterator for `Fetcher`, available behind the `rayon` feature flag.
- Fixed archetype refresh bugs and changed semantics of `refresh_archetype`.
- Fixed potential "cannot shadow tuple struct with let binding" errors in derived `Query` and `SystemParam` impls.
- Added `#[must_use]` attribute to iterators.
- Relaxed auto trait impls for various types.
- Minor performance improvements.

## 0.1.1 - 2024-01-25

Initial release
