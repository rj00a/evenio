use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse2, parse_macro_input, parse_quote, DeriveInput, Error, Result};

pub(crate) fn derive_event(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;

    input
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: Send + Sync + 'static));

    match input.data {
        syn::Data::Struct(struct_) => {
            for f in struct_.fields {
                for attr in f.attrs {
                    if attr.path().is_ident("target") {
                        todo!()
                    }
                }
            }
        }
        syn::Data::Enum(enum_) => {}
        syn::Data::Union(union_) => {
            todo!()
        }
    }

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics ::evenio::event::Event for #name #ty_generics #where_clause {
            const IS_TARGETED: bool = false;

            fn target(&self) -> ::evenio::entity::EntityId {
                ::core::unimplemented!()
            }
        }
    })
}
