use evenio::access::ComponentAccessExpr;
use evenio::prelude::*;

#[derive(Event)]
struct MyEvent;

#[derive(Component, Debug)]
struct MyComponent;

#[derive(Query, Debug)]
struct MyQuery<'a> {
    _foo: &'a MyComponent,
}

pub fn main() {}
