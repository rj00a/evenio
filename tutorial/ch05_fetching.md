# Fetching

In the previous chapter, we've seen how to create entities and add components to them.
But components aren't very useful unless we have some way to access them from within systems.

This is where the [`Fetcher`] system parameter comes in.
`Fetcher<Q>` allows for both random access entity lookups using an `EntityId` and iteration over all entities matching some [`Query`] `Q`.

```rust
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(Event)]
struct E;

#[derive(Component, Debug)]
struct A(&'static str);

#[derive(Component, Debug)]
struct B(i32);

let e1 = world.spawn();
world.insert(e1, A("foo"));
world.insert(e1, B(123));

let e2 = world.spawn();
world.insert(e2, A("bar"));

let e3 = world.spawn();
world.insert(e3, B(456));

world.add_system(move |_: Receiver<E>, fetcher: Fetcher<&A>| {
    // Get a reference to the `A` component on entity `e2`.
    // Returns `None` if the query doesn't match.
    let a = fetcher.get(e1).unwrap();
    println!("fetcher.get(): {a:?}");
    
    // `e3` doesn't have component `A`.
    assert!(fetcher.get(e3).is_err());

    // Iterate over all entities with the `A` component.
    // Entities are visited in a deterministic but unspecified order.
    for a in fetcher.iter() {
        println!("fetcher.iter(): {a:?}");
    }
});

world.send(E);
```

Output:
```txt
fetcher.get(): A("foo")
fetcher.iter(): A("bar")
fetcher.iter(): A("foo")
```

## Queries

Queries are expressions used to filter and access data from entities.
In the previous example, we used the `&A` query to match entities with the `A` component, but there are many other queries to choose from:

| Query               | Description                                                                                       |
|---------------------|---------------------------------------------------------------------------------------------------|
| `&C`                | Matches entities with component `C` and returns an immutable reference to `C`.                    |
| `&mut C`            | Matches entities with component `C` and returns a mutable reference to `C`.                       |
| `(Q₁, Q₂, …, Qₙ)`   | Matches if all queries `Q₁` through `Qₙ` match. Returns a tuple of the results.                   |
| `()`                | The empty tuple matches all entities and returns nothing.                                         |
| `Option<Q>`         | Returns the result of query `Q` as `Some`, or `None` if `Q` does not match. Matches all entities. |
| `Or<Q₁, Q₂>`        | Matches if `Q₁` or `Q₂` matches. Returns the matched query results.                               |
| `Xor<Q₁, Q₂>`       | Matches if `Q₁` or `Q₂` matches, but not both. Returns the matched query results.                 |
| `EntityId`          | Returns the [`EntityId`] of the matched entity. Matches all entities.                             |
| `Has<Q>`            | Returns a boolean indicating whether the query `Q` matches. Matches all entities.                 |
| `With<Q>`           | Matches if `Q` matches. Returns nothing and does not access the result of `Q`.                    |
| `Not<Q>`            | Matches if `Q` does not match. Returns nothing.                                                   |

[`Fetcher`]: crate::fetch::Fetcher
[`Query`]: crate::query::Query
[`EntityId`]: crate::entity::EntityId

## Derived Queries

`Query` can be derived on structs so long as every field is a `Query`.
Derived queries act like tuples — every field must match in order for the whole query to match.

Compared to tuples, however, derived query structs offer some advantages.
Fields are be named and methods can be implemented on the struct – handy for complex or frequently used queries.

```rust
# use evenio::prelude::*;
# #[derive(Event)] struct E;
# #[derive(Component, Debug)] struct A;
# #[derive(Component, Debug)] struct B;
# #[derive(Component, Debug)] struct C;
# let mut world = World::new();
#
#[derive(Query, Debug)]
struct MyQuery<'a> {
    foo: &'a A,
    bar: &'a mut B,
    _baz: With<&'static C>,
}

impl MyQuery<'_> {
    fn print(&self) {
        println!("foo: {:?}, bar: {:?}", self.foo, self.bar);
    }
}

world.add_system(|_: Receiver<E>, fetcher: Fetcher<MyQuery>| {
    for my_query in fetcher {
        my_query.print();
    }
});
```

## Access Conflicts

Within a query or set of queries, it is possible to create component accesses that violate Rust's aliasing rules.
Consider:

```should_panic
# use evenio::prelude::*;
# #[derive(Event)] struct E;
# #[derive(Component)] struct A;
# let mut world = World::new();
// Panics!
world.add_system(|_: Receiver<E>, _: Fetcher<&mut A>, _: Fetcher<&A>| {});
```

Because both fetchers are capable of accessing the same entity, it is possible to create `&mut A` and `&A` references to the same component, thus triggering Undefined Behavior.
`add_system` guards against this by panicking if the system parameters have any possibility of causing unsoundness.

There are a number of ways to resolve this situation, but let's look at one way using `With` and `Not`:

```rust
# use evenio::prelude::*;
# #[derive(Event)] struct E;
# #[derive(Component)] struct A;
# #[derive(Component)] struct B;
# let mut world = World::new();
// Doesn't panic.
world.add_system(|_: Receiver<E>, _: Fetcher<(&mut A, With<&B>)>, _: Fetcher<(&A, Not<&B>)>| {});
```

The set of entities matched by both queries are now disjoint.
Therefore, there is no possibility of overlapping access to `A`, and the system can be added to the world without panicking.

## Singleton Entities

Through the course of development, we may find ourselves wanting to store global data in a location that is easily accessed by systems.

To facilitate this, `evenio` has the [`Single`] system parameter.
`Single` is parameterized by a query that must match a single entity in the world.
If the query does not match exactly one entity, then a runtime panic occurs.

To create our global variable, we
1. create a component to hold the data
2. initialize the component and attach it to an entity
3. let `Single` match against it.

```rust
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Event)] struct E;
#[derive(Component)]
struct MyGlobalData {
    foo: i32,
    bar: &'static str,
}

let e = world.spawn();
world.insert(e, MyGlobalData { foo: 123, bar: "data" });

world.add_system(|_: Receiver<E>, Single(g): Single<&MyGlobalData>| {
    println!("foo: {}, bar: {}", g.foo, g.bar);
});

world.send(E);
```

For handling the situation where `Single` fails, see [`TrySingle`].

For global data scoped to a single system, see [`Local`].

[`Single`]: crate::fetch::Single
[`TrySingle`]: crate::fetch::TrySingle
[`Local`]: crate::system::Local
