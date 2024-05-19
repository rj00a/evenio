#![doc = include_str!("../README.md")]

use proc_macro::TokenStream;

mod all_tuples;
mod component;
mod event;
mod handler_param;
mod query;
mod util;

/// Helper macro which repeatedly invokes a given macro with an increasing list
/// of identifiers.
///
/// Invoked with `all_tuples!(my_macro, start, end, A, B, ...)` where `start`
/// and `end` are integer literals and `A`, `B`, ... are identifiers.
///
/// `all_tuples!(my_macro, 0, 15, A)` expands to:
///
/// ```ignore
/// my_macro!();
/// my_macro!(A0);
/// my_macro!(A0, A1);
/// my_macro!(A0, A1, A2);
/// ...
/// my_macro!(A0, ..., A14);
/// ```
///
/// Now with multiple identifiers. `all_tuples!(my_macro, 0, 15, A, a)` expands
/// to:
///
/// ```ignore
/// my_macro!();
/// my_macro!((A0, a0));
/// my_macro!((A0, a0), (A1, a1));
/// my_macro!((A0, a0), (A1, a1), (A2, a2));
/// ...
/// my_macro!((A0, a0), ..., (A14, a14));
/// ```
#[proc_macro]
pub fn all_tuples(input: TokenStream) -> TokenStream {
    all_tuples::all_tuples(input)
}

/// Derive macro for `Event`. See `Event` and `GlobalEvent`'s documentation for
/// more information.
#[proc_macro_derive(GlobalEvent, attributes(event))]
pub fn derive_global_event(input: TokenStream) -> TokenStream {
    event::derive_event(input.into(), false)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Derive macro for `Event`. See `Event` and `TargetedEvent`'s documentation
/// for more information.
#[proc_macro_derive(TargetedEvent, attributes(event))]
pub fn derive_targeted_event(input: TokenStream) -> TokenStream {
    event::derive_event(input.into(), true)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Derive macro for `Component`. See `Component`'s documentation for more
/// information.
#[proc_macro_derive(Component, attributes(component))]
pub fn derive_component(input: TokenStream) -> TokenStream {
    component::derive_component(input.into())
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Derive macro for `Query`. See `Query`'s documentation for more
/// information.
#[proc_macro_derive(Query, attributes(query))]
pub fn derive_query(input: TokenStream) -> TokenStream {
    query::derive_query(input.into())
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Derive macro for `HandlerParam`. See `HandlerParam`'s documentation for more
/// information.
#[proc_macro_derive(HandlerParam)]
pub fn derive_handler_param(input: TokenStream) -> TokenStream {
    handler_param::derive_handler_param(input.into())
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}
