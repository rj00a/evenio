# Change Log

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
