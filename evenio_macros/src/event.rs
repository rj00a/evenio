use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse2, parse_quote, DeriveInput, Result, Type};

use crate::util::{parse_attr_immutable, replace_lifetime};

pub(crate) fn derive_event(input: TokenStream, is_targeted: bool) -> Result<TokenStream> {
    let input = parse2::<DeriveInput>(input)?;

    let is_immutable = parse_attr_immutable("event", &input.attrs)?;

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut this: Type = parse_quote!(#name #ty_generics);

    let new_lifetime_ident = parse_quote!(__a);
    for life in input.generics.lifetimes() {
        replace_lifetime(&mut this, &life.lifetime.ident, &new_lifetime_ident);
    }

    let event_idx_type: Type = if is_targeted {
        parse_quote!(::evenio::event::TargetedEventIdx)
    } else {
        parse_quote!(::evenio::event::GlobalEventIdx)
    };

    Ok(quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::evenio::event::Event for #name #ty_generics #where_clause {
            type This<'__a> = #this;

            type EventIdx = #event_idx_type;

            const IS_IMMUTABLE: bool = #is_immutable;
        }
    })
}
