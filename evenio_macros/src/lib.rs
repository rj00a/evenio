use proc_macro::TokenStream;

mod all_tuples;
mod component;
mod event;
mod query;
mod system_param;
mod util;

#[proc_macro]
pub fn all_tuples(input: TokenStream) -> TokenStream {
    all_tuples::all_tuples(input)
}

/// Derive macro for `Event`. See `Event`'s documentation for more information.
#[proc_macro_derive(Event, attributes(event))]
pub fn derive_event(input: TokenStream) -> TokenStream {
    event::derive_event(input.into())
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

#[proc_macro_derive(SystemParam)]
pub fn derive_system_param(input: TokenStream) -> TokenStream {
    system_param::derive_system_param(input.into())
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}
