use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse2, parse_quote, Data, DeriveInput, Error, GenericParam, LitInt, Result, Type};

use crate::util::{make_tuple, replace_lifetime};

pub(crate) fn derive_system_param(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;
    let name = &input.ident;

    let lifetimes;
    let tuple_ty;
    let get_param_body;

    match &input.data {
        Data::Struct(struct_) => {
            lifetimes = input
                .generics
                .params
                .iter()
                .filter_map(|p| match p {
                    GenericParam::Lifetime(life) => Some(life.clone()),
                    GenericParam::Type(_) => None,
                    GenericParam::Const(_) => None,
                })
                .collect::<Vec<_>>();

            tuple_ty = make_tuple(struct_.fields.iter().map(|f| &f.ty));

            let where_clause = input.generics.make_where_clause();

            for field in &struct_.fields {
                let ty = &field.ty;

                let mut replaced_ty = ty.clone();

                for life in &lifetimes {
                    replace_lifetime(&mut replaced_ty, &life.lifetime.ident, &parse_quote!(__a));
                }

                where_clause.predicates.push(
                    parse_quote!(#ty: for<'__a> ::evenio::system::SystemParam<Item<'__a> = #replaced_ty>),
                );
            }

            get_param_body = match &struct_.fields {
                syn::Fields::Named(fields) => {
                    let idents: Vec<_> = fields
                        .named
                        .iter()
                        .map(|f| f.ident.clone().unwrap())
                        .collect();

                    quote! {
                        let (#(#idents,)*) = <#tuple_ty as ::evenio::system::SystemParam>::get_param(
                            state,
                            info,
                            event_ptr,
                            world
                        );

                        #name {
                            #(#idents),*
                        }
                    }
                }
                syn::Fields::Unnamed(fields) => {
                    let indices = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| LitInt::new(&format!("{i}"), Span::call_site()));

                    quote! {
                        let tuple = <#tuple_ty as ::evenio::system::SystemParam>::get_param(
                            state,
                            info,
                            event_ptr,
                            world
                        );

                        #name(#(tuple.#indices),*)
                    }
                }
                syn::Fields::Unit => quote!(#name),
            };
        }
        Data::Enum(_) => {
            return Err(Error::new(
                Span::call_site(),
                "cannot derive `SystemParam` on enums",
            ))
        }
        Data::Union(_) => {
            return Err(Error::new(
                Span::call_site(),
                "cannot derive `SystemParam` on unions",
            ))
        }
    }

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut item: Type = parse_quote!(#name #ty_generics);
    for life in &lifetimes {
        replace_lifetime(&mut item, &life.lifetime.ident, &parse_quote!(__a));
    }

    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics ::evenio::system::SystemParam for #name #ty_generics #where_clause {
            type State = <#tuple_ty as ::evenio::system::SystemParam>::State;

            type Item<'__a> = #item;

            fn init(
                world: &mut ::evenio::world::World,
                config: &mut ::evenio::system::Config,
            ) -> ::core::result::Result<Self::State, ::evenio::system::InitError>
            {
                <#tuple_ty as ::evenio::system::SystemParam>::init(world, config)
            }

            unsafe fn get_param<'__a>(
                state: &'__a mut Self::State,
                info: &'__a ::evenio::system::SystemInfo,
                event_ptr: ::evenio::event::EventPtr<'__a>,
                world: ::evenio::world::UnsafeWorldCell<'__a>,
            ) -> Self::Item<'__a> {
                #get_param_body
            }

            unsafe fn refresh_archetype(
                state: &mut Self::State,
                arch: &::evenio::archetype::Archetype
            ) {
                <#tuple_ty as ::evenio::system::SystemParam>::refresh_archetype(
                    state,
                    arch
                )
            }

            unsafe fn remove_archetype(
                state: &mut Self::State,
                arch: &::evenio::archetype::Archetype
            ) {
                <#tuple_ty as ::evenio::system::SystemParam>::remove_archetype(
                    state,
                    arch
                )
            }
        }
    })
}
