use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse2, parse_quote, DeriveInput, Result, Type};

use crate::util::parse_attr_immutable;

pub(crate) fn derive_component(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;

    input
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: 'static));

    let mutability_type: Type = if parse_attr_immutable("component", &input.attrs)? {
        parse_quote!(::evenio::mutability::Immutable)
    } else {
        parse_quote!(::evenio::mutability::Mutable)
    };

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics ::evenio::component::Component for #name #ty_generics #where_clause {
            type Mutability = #mutability_type;
        }
    })
}
