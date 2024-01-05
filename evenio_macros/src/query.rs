use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{
    parse2, parse_quote, Data, DeriveInput, Error, GenericArgument, GenericParam, Path, Result,
    ReturnType, Type, TypeParamBound, TypeTuple,
};

pub(crate) fn derive_query(input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<DeriveInput>(input)?;

    let life;

    let tuple_ty;
    let field_idents;

    let mut ro_generics = input.generics.clone();

    match input.data {
        Data::Struct(struct_) => {
            {
                let mut life_iter = input.generics.params.iter().filter_map(|p| match p {
                    GenericParam::Lifetime(life) => Some(life),
                    GenericParam::Type(_) => None,
                    GenericParam::Const(_) => None,
                });

                life = life_iter.next().cloned();

                if life_iter.next().is_some() {
                    return Err(Error::new(
                        input.generics.params.span(),
                        "derived query cannot have more than one lifetime parameter",
                    ));
                }
            }

            tuple_ty = make_tuple(struct_.fields.iter().map(|f| &f.ty));

            let where_clause = input.generics.make_where_clause();

            let ro_where_clause = ro_generics.make_where_clause();

            for field in &struct_.fields {
                let ty = &field.ty;

                let mut replaced_ty = ty.clone();
                if let Some(life) = life.as_ref() {
                    replace_lifetime(&mut replaced_ty, &life.lifetime.ident, &parse_quote!(__a));
                }

                where_clause.predicates.push(
                    parse_quote!(#ty: for<'__a> ::evenio::query::Query<Item<'__a> = #replaced_ty>),
                );

                ro_where_clause
                    .predicates
                    .push(parse_quote!(#ty: for<'__a> ::evenio::query::ReadOnlyQuery<Item<'__a> = #replaced_ty>));
            }

            field_idents = struct_
                .fields
                .iter()
                .map(|f| f.ident.clone())
                .collect::<Vec<_>>();
        }
        Data::Enum(_) => {
            return Err(Error::new(
                Span::call_site(),
                "cannot derive `Query` on enums",
            ))
        }
        Data::Union(_) => {
            return Err(Error::new(
                Span::call_site(),
                "cannot derive `Query` on unions",
            ))
        }
    }

    let (ro_impl_generics, ro_ty_generics, ro_where_clause) = ro_generics.split_for_impl();

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let item_life_arg = life.as_ref().map(|l| quote!(<#l>));

    Ok(quote! {
        #[automatically_derived]
        unsafe impl #impl_generics ::evenio::query::Query for #name #ty_generics #where_clause {
            type Item<'__a> = #name #item_life_arg;

            type ArchState = <#tuple_ty as ::evenio::query::Query>::ArchState;

            type State = <#tuple_ty as ::evenio::query::Query>::State;

            fn init(
                world: &mut ::evenio::world::World,
                config: &mut ::evenio::system::Config
            ) -> ::core::result::Result<(::evenio::access::ComponentAccessExpr, Self::State), ::evenio::system::InitError>
            {
                <#tuple_ty as ::evenio::query::Query>::init(world, config)
            }

            fn new_state(world: &mut ::evenio::world::World) -> Self::State {
                <#tuple_ty as ::evenio::query::Query>::new_state(world)
            }

            fn new_arch_state(arch: &::evenio::archetype::Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
                <#tuple_ty as ::evenio::query::Query>::new_arch_state(arch, state)
            }

            unsafe fn fetch<'__a>(state: &Self::ArchState, row: ::evenio::archetype::ArchetypeRow) -> Self::Item<'__a> {
                let (#(#field_idents,)*) = <#tuple_ty as ::evenio::query::Query>::fetch(state, row);

                #name {
                    #(#field_idents),*
                }
            }
        }

        #[automatically_derived]
        unsafe impl #ro_impl_generics ::evenio::query::ReadOnlyQuery for #name #ro_ty_generics #ro_where_clause {}
    })
}

fn make_tuple(types: impl Iterator<Item = impl ToTokens>) -> TypeTuple {
    parse_quote! {
        (#(#types,)*)
    }
}

/// Replace all occurrences of the lifetime `old` with `new` in `ty`.
fn replace_lifetime(ty: &mut Type, old: &Ident, new: &Ident) {
    match ty {
        Type::Array(t) => replace_lifetime(&mut t.elem, old, new),
        Type::BareFn(t) => {
            for input in &mut t.inputs {
                replace_lifetime(&mut input.ty, old, new);
            }

            if let ReturnType::Type(_, t) = &mut t.output {
                replace_lifetime(t, old, new);
            }
        }
        Type::Group(t) => replace_lifetime(&mut t.elem, old, new),
        Type::ImplTrait(t) => handle_type_param_bounds(t.bounds.iter_mut(), old, new),
        Type::Infer(_) => {}
        Type::Macro(_) => {}
        Type::Never(_) => {}
        Type::Paren(t) => replace_lifetime(&mut t.elem, old, new),
        Type::Path(p) => {
            if let Some(qself) = &mut p.qself {
                replace_lifetime(&mut qself.ty, old, new);
            }

            handle_path(&mut p.path, old, new);
        }
        Type::Ptr(p) => replace_lifetime(&mut p.elem, old, new),
        Type::Reference(r) => {
            if let Some(life) = &mut r.lifetime {
                if life.ident == *old {
                    life.ident = new.clone();
                }
            }

            replace_lifetime(&mut r.elem, old, new);
        }
        Type::Slice(s) => replace_lifetime(&mut s.elem, old, new),
        Type::TraitObject(tobj) => {
            handle_type_param_bounds(tobj.bounds.iter_mut(), old, new);
        }
        Type::Tuple(t) => {
            for elem in &mut t.elems {
                replace_lifetime(elem, old, new);
            }
        }
        Type::Verbatim(_) => {}
        _ => {}
    }

    fn handle_path(path: &mut Path, old: &Ident, new: &Ident) {
        for seg in &mut path.segments {
            match &mut seg.arguments {
                syn::PathArguments::None => {}
                syn::PathArguments::AngleBracketed(args) => {
                    handle_generic_args(args.args.iter_mut(), old, new)
                }
                syn::PathArguments::Parenthesized(args) => {
                    for input in &mut args.inputs {
                        replace_lifetime(input, old, new);
                    }

                    if let ReturnType::Type(_, t) = &mut args.output {
                        replace_lifetime(t, old, new);
                    }
                }
            }
        }
    }

    fn handle_generic_args<'a>(
        args: impl Iterator<Item = &'a mut GenericArgument>,
        old: &Ident,
        new: &Ident,
    ) {
        for arg in args {
            match arg {
                syn::GenericArgument::Lifetime(l) => {
                    if l.ident == *old {
                        l.ident = new.clone();
                    }
                }
                syn::GenericArgument::Type(t) => replace_lifetime(t, old, new),
                syn::GenericArgument::Const(_) => {
                    // TODO exprs
                }
                syn::GenericArgument::AssocType(t) => {
                    if let Some(args) = &mut t.generics {
                        handle_generic_args(args.args.iter_mut(), old, new);
                    }
                }
                syn::GenericArgument::AssocConst(c) => {
                    if let Some(args) = &mut c.generics {
                        handle_generic_args(args.args.iter_mut(), old, new);
                    }
                }
                syn::GenericArgument::Constraint(c) => {
                    if let Some(args) = &mut c.generics {
                        handle_generic_args(args.args.iter_mut(), old, new);
                    }

                    handle_type_param_bounds(c.bounds.iter_mut(), old, new);
                }
                _ => {}
            }
        }
    }

    fn handle_type_param_bounds<'a>(
        bounds: impl Iterator<Item = &'a mut TypeParamBound>,
        old: &Ident,
        new: &Ident,
    ) {
        for bound in bounds {
            match bound {
                syn::TypeParamBound::Trait(t) => handle_path(&mut t.path, old, new),
                syn::TypeParamBound::Lifetime(l) => {
                    if l.ident == *old {
                        l.ident = new.clone();
                    }
                }
                syn::TypeParamBound::Verbatim(_) => {}
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(mut a: Type, b: Type) {
        replace_lifetime(&mut a, &parse_quote!(a), &parse_quote!(b));

        assert_eq!(
            a.into_token_stream().to_string(),
            b.into_token_stream().to_string()
        );
    }

    #[test]
    fn lifetime_replacement() {
        check(parse_quote!(A), parse_quote!(A));
        check(parse_quote!(&'a A), parse_quote!(&'b A));
        check(
            parse_quote!(for<'c> fn(&'a str, &'c str)),
            parse_quote!(for<'c> fn(&'b str, &'c str)),
        );
        check(parse_quote!(a::b::C<'a>), parse_quote!(a::b::C<'b>));
        check(
            parse_quote!(impl Foo<'a> + 'a),
            parse_quote!(impl Foo<'b> + 'b),
        );
    }
}
