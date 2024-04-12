use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{parse2, parse_quote, Data, DeriveInput, LitInt, Result, Type};

use crate::util::{parse_attr_immutable, replace_lifetime};

pub(crate) fn derive_event(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;

    input
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(Self: Send + Sync));

    let mut target_field = None;

    match &input.data {
        Data::Struct(struct_) => {
            for (idx, field) in struct_.fields.iter().enumerate() {
                for attr in &field.attrs {
                    if attr.path().is_ident("event") {
                        attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("target") {
                                if target_field.is_some() {
                                    return Err(meta.error("target already assigned"));
                                }

                                target_field = Some((idx, field.clone()));
                                Ok(())
                            } else {
                                Err(meta.error("unrecognized argument"))
                            }
                        })?;
                    }
                }
            }
        }
        Data::Enum(enum_) => {
            for variant in &enum_.variants {
                for field in &variant.fields {
                    for attr in &field.attrs {
                        if attr.path().is_ident("event") {
                            attr.parse_nested_meta(|meta| {
                                if meta.path.is_ident("target") {
                                    Err(meta.error("cannot set target on enum"))
                                } else {
                                    Err(meta.error("unrecognized argument"))
                                }
                            })?;
                        }
                    }
                }
            }
        }
        Data::Union(union_) => {
            for field in &union_.fields.named {
                for attr in &field.attrs {
                    if attr.path().is_ident("event") {
                        attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("target") {
                                Err(meta.error("cannot set target on union"))
                            } else {
                                Err(meta.error("unrecognized argument"))
                            }
                        })?;
                    }
                }
            }
        }
    }

    let is_targeted = target_field.is_some();

    let is_immutable = parse_attr_immutable("event", &input.attrs)?;

    let target_fn_body = if let Some((idx, field)) = target_field {
        let f = match field.ident {
            Some(ident) => ident.into_token_stream(),
            None => LitInt::new(&idx.to_string(), Span::call_site()).to_token_stream(),
        };

        quote!(self.#f)
    } else {
        let message = format!("`{}` is not a targeted event", &input.ident);
        quote!(::core::unreachable!(#message))
    };

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut this: Type = parse_quote!(#name #ty_generics);

    let new_lifetime_ident = parse_quote!(__a);
    for life in input.generics.lifetimes() {
        replace_lifetime(&mut this, &life.lifetime.ident, &new_lifetime_ident);
    }

    Ok(quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::evenio::event::Event for #name #ty_generics #where_clause {
            type This<'__a> = #this;

            const IS_TARGETED: bool = #is_targeted;
            const IS_IMMUTABLE: bool = #is_immutable;

            #[track_caller]
            fn target(&self) -> ::evenio::entity::EntityId {
                #target_fn_body
            }
        }
    })
}
