use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse2, parse_quote, DeriveInput, Result};

use crate::parse_is_mutable;

pub(crate) fn derive_component(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;

    input
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: Send + Sync + 'static));

    let is_mutable = parse_is_mutable("component", &input.attrs)?;

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics ::evenio::component::Component for #name #ty_generics #where_clause {
            const IS_MUTABLE: bool = #is_mutable;
        }
    })
}
