use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, Fields, GenericArgument, Path, Result, ReturnType, Type,
    TypeParamBound, TypeTuple,
};

/// Parse a `#[foo(immutable)]` attribute where `outer` is `foo`.
pub(crate) fn parse_attr_immutable(outer: &str, attrs: &[Attribute]) -> Result<bool> {
    let mut res = false;

    for attr in attrs {
        if attr.path().is_ident(outer) {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("immutable") {
                    res = true;
                    Ok(())
                } else {
                    Err(meta.error("unrecognized argument"))
                }
            })?;
        }
    }

    Ok(res)
}

/// Make a tuple from a list of the tuple's element types.
pub(crate) fn make_tuple(types: impl Iterator<Item = impl ToTokens>) -> TypeTuple {
    parse_quote! {
        (#(#types,)*)
    }
}

/// Replace all occurrences of the lifetime `old` with `new` in `ty`.
pub(crate) fn replace_lifetime(ty: &mut Type, old: &Ident, new: &Ident) {
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

/// Returns a list of field identifiers. Tuple fields are named `_0`, `_1`, etc.
pub(crate) fn collect_field_idents(fields: &Fields) -> Vec<Ident> {
    match fields {
        Fields::Named(fields) => fields
            .named
            .iter()
            .map(|f| f.ident.clone().unwrap())
            .collect(),
        Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, f)| Ident::new(&format!("_{i}"), f.span()))
            .collect(),
        Fields::Unit => vec![],
    }
}

/// Returns a list of field names. Tuple fields are named `0`, `1`, etc.
pub(crate) fn collect_field_names(fields: &Fields) -> Vec<TokenStream> {
    match fields {
        Fields::Named(fields) => fields
            .named
            .iter()
            .map(|f| f.ident.to_token_stream())
            .collect(),
        Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, _)| quote!(#i))
            .collect(),
        Fields::Unit => vec![],
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
