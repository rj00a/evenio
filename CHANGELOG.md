# Change Log

## Unreleased

- Fixed a bug in event propagation.
- The behavior of the event queue has changed. The next event in the queue is handled only once the current event has finished broadcasting.
- Changed `EventPtr` interface and relaxed `Send` and `Sync` bounds for `EventMut`.

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
