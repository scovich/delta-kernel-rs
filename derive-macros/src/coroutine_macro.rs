use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{ParseStream, Parser};
use syn::{
    parse2, parse_quote, Attribute, Error, Fields, GenericParam, ItemEnum, ItemTrait, Meta, Token,
    Type, TypeParam,
};

struct RequestVariant {
    attrs: Vec<Attribute>,
    cfg_attrs: Vec<Attribute>,
    ident: syn::Ident,
    operation: Type,
    state: Option<Type>,
}

pub(crate) fn expand_request(attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let output = (|input: ParseStream| parse_type_assignment(input, "output", "coroutine_request"))
        .parse2(attr)?;
    let input: ItemEnum = parse2(item)?;
    let enum_ident = &input.ident;
    let visibility = &input.vis;
    let generics = &input.generics;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let variants = input
        .variants
        .iter()
        .map(parse_request_variant)
        .collect::<syn::Result<Vec<_>>>()?;

    let expanded_variants = variants.iter().map(|variant| {
        let RequestVariant {
            attrs,
            ident,
            operation,
            state,
            ..
        } = variant;
        match state {
            Some(state) => quote! {
                #(#attrs)*
                #ident(
                    ::delta_kernel::coroutine::Pagination<
                        <#operation as ::delta_kernel::coroutine::Operation>::Work,
                        #state,
                    >,
                    ::delta_kernel::coroutine::Resume<
                        #output,
                        #enum_ident #type_generics,
                        (
                            <#operation as ::delta_kernel::coroutine::Operation>::Response,
                            ::std::option::Option<#state>,
                        ),
                    >,
                )
            },
            None => quote! {
                #(#attrs)*
                #ident(
                    <#operation as ::delta_kernel::coroutine::Operation>::Work,
                    ::delta_kernel::coroutine::OperationResume<
                        #output,
                        #enum_ident #type_generics,
                        #operation,
                    >,
                )
            },
        }
    });

    let request_impls = variants.iter().map(|variant| {
        let RequestVariant {
            cfg_attrs,
            ident,
            operation,
            state,
            ..
        } = variant;
        match state {
            Some(state) => quote! {
                #(#cfg_attrs)*
                impl #impl_generics
                    ::delta_kernel::coroutine::CanRequestPaginated<#output, #operation>
                    for #enum_ident #type_generics
                    #where_clause
                {
                    type State = #state;

                    fn request(
                        pagination: ::delta_kernel::coroutine::Pagination<
                            <#operation as ::delta_kernel::coroutine::Operation>::Work,
                            Self::State,
                        >,
                        resume: ::delta_kernel::coroutine::Resume<
                            #output,
                            Self,
                            (
                                <#operation as ::delta_kernel::coroutine::Operation>::Response,
                                ::std::option::Option<Self::State>,
                            ),
                        >,
                    ) -> Self {
                        Self::#ident(pagination, resume)
                    }
                }
            },
            None => quote! {
                #(#cfg_attrs)*
                impl #impl_generics ::delta_kernel::coroutine::CanRequest<#output, #operation>
                    for #enum_ident #type_generics
                    #where_clause
                {
                    fn request(
                        work: <#operation as ::delta_kernel::coroutine::Operation>::Work,
                        resume: ::delta_kernel::coroutine::OperationResume<
                            #output,
                            Self,
                            #operation,
                        >,
                    ) -> Self {
                        Self::#ident(work, resume)
                    }
                }
            },
        }
    });

    let connector_ident = fresh_generic_ident(generics, "C");
    let mut connector_bounds = Vec::new();
    let mut maybe_capabilities = Vec::new();
    for variant in &variants {
        let cfg_attrs = &variant.cfg_attrs;
        let operation = &variant.operation;
        let capability = match &variant.state {
            Some(state) => quote! {
                #connector_ident: ::delta_kernel::coroutine::SupportsPaginated<
                    #operation,
                    State = #state,
                >
            },
            None => quote! {
                #connector_ident: ::delta_kernel::coroutine::Supports<#operation>
            },
        };
        let Some(condition) = cfg_condition(cfg_attrs)? else {
            connector_bounds.push(capability);
            continue;
        };

        let helper_ident = format_ident!("__{}{}Capability", enum_ident, variant.ident,);
        let helper_bound = match &variant.state {
            Some(state) => quote! {
                ::delta_kernel::coroutine::SupportsPaginated<
                    #operation,
                    State = #state,
                >
            },
            None => quote! {
                ::delta_kernel::coroutine::Supports<#operation>
            },
        };
        maybe_capabilities.push(quote! {
            #(#cfg_attrs)*
            #[doc(hidden)]
            #visibility trait #helper_ident: #helper_bound {}

            #(#cfg_attrs)*
            impl<T> #helper_ident for T
            where
                T: #helper_bound,
            {
            }

            #[cfg(not(#condition))]
            #[doc(hidden)]
            #visibility trait #helper_ident {}

            #[cfg(not(#condition))]
            impl<T> #helper_ident for T {}
        });
        connector_bounds.push(quote! {
            #connector_ident: #helper_ident
        });
    }
    let resume_arms = variants.iter().map(|variant| {
        let cfg_attrs = &variant.cfg_attrs;
        let ident = &variant.ident;
        let operation = &variant.operation;
        match &variant.state {
            Some(_) => quote! {
                #(#cfg_attrs)*
                Self::#ident(pagination, resume) => resume.resume_with(|| {
                    match pagination {
                        ::delta_kernel::coroutine::Pagination::Start(work) =>
                        {
                            let state =
                                <#connector_ident as ::delta_kernel::coroutine::SupportsPaginated<
                                    #operation,
                                >>::start(connector, work)?;
                            <#connector_ident as ::delta_kernel::coroutine::SupportsPaginated<
                                #operation,
                            >>::next(connector, state)
                        },
                        ::delta_kernel::coroutine::Pagination::Continue(state) =>
                            <#connector_ident as ::delta_kernel::coroutine::SupportsPaginated<
                                #operation,
                            >>::next(connector, state),
                    }
                })
            },
            None => quote! {
                #(#cfg_attrs)*
                Self::#ident(work, resume) => resume.resume_with(|| {
                    <#connector_ident as ::delta_kernel::coroutine::Supports<#operation>>::execute(
                        connector,
                        work,
                    )
                })
            },
        }
    });

    Ok(quote! {
        #visibility enum #enum_ident #generics {
            #(#expanded_variants),*
        }

        #(#request_impls)*

        #(#maybe_capabilities)*

        impl #impl_generics #enum_ident #type_generics #where_clause {
            /// Execute this delegated request and resume the kernel workflow.
            #visibility fn resume<#connector_ident>(
                self,
                connector: &mut #connector_ident,
            ) -> ::delta_kernel::DeltaResult<
                ::std::ops::ControlFlow<#output, Self>,
            >
            where
                #(#connector_bounds),*
            {
                match self {
                    #(#resume_arms),*
                }
            }
        }
    })
}

pub(crate) fn expand_capabilities(
    attr: TokenStream,
    item: TokenStream,
) -> syn::Result<TokenStream> {
    if !attr.is_empty() {
        return Err(Error::new_spanned(
            attr,
            "coroutine_capabilities takes no arguments",
        ));
    }
    let input: ItemTrait = parse2(item)?;
    if let Some(item) = input.items.first() {
        return Err(Error::new_spanned(
            item,
            "coroutine capability traits cannot define items",
        ));
    }
    if input.supertraits.is_empty() {
        return Err(Error::new_spanned(
            &input.ident,
            "coroutine capability traits require at least one capability bound",
        ));
    }

    let trait_ident = &input.ident;
    let trait_generics = &input.generics;
    let (_, trait_type_generics, _) = trait_generics.split_for_impl();
    let supertraits = &input.supertraits;

    let mut impl_generics = trait_generics.clone();
    let request_ident = fresh_generic_ident(trait_generics, "T");
    impl_generics
        .params
        .push(GenericParam::Type(TypeParam::from(request_ident.clone())));
    impl_generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(#request_ident: #supertraits));
    let (impl_generics, _, impl_where_clause) = impl_generics.split_for_impl();

    Ok(quote! {
        #input

        impl #impl_generics #trait_ident #trait_type_generics for #request_ident
            #impl_where_clause
        {
        }
    })
}

fn parse_request_variant(variant: &syn::Variant) -> syn::Result<RequestVariant> {
    let Fields::Unnamed(fields) = &variant.fields else {
        return Err(Error::new_spanned(
            &variant.fields,
            "coroutine request variants must contain exactly one operation type",
        ));
    };
    if fields.unnamed.len() != 1 {
        return Err(Error::new_spanned(
            fields,
            "coroutine request variants must contain exactly one operation type",
        ));
    }

    let mut state = None;
    let mut attrs = Vec::new();
    let mut cfg_attrs = Vec::new();
    for attr in &variant.attrs {
        if attr.path().is_ident("paginated") {
            if state.is_some() {
                return Err(Error::new_spanned(attr, "duplicate paginated attribute"));
            }
            state = Some(parse_paginated_state(attr)?);
        } else {
            if attr.path().is_ident("cfg") || attr.path().is_ident("cfg_attr") {
                cfg_attrs.push(attr.clone());
            }
            attrs.push(attr.clone());
        }
    }

    Ok(RequestVariant {
        attrs,
        cfg_attrs,
        ident: variant.ident.clone(),
        operation: fields.unnamed[0].ty.clone(),
        state,
    })
}

fn parse_paginated_state(attr: &Attribute) -> syn::Result<Type> {
    let Meta::List(list) = &attr.meta else {
        return Err(Error::new_spanned(
            attr,
            "expected #[paginated(state = Type)]",
        ));
    };
    (|input: ParseStream| parse_type_assignment(input, "state", "paginated"))
        .parse2(list.tokens.clone())
}

fn parse_type_assignment(input: ParseStream, expected: &str, context: &str) -> syn::Result<Type> {
    let name: syn::Ident = input.parse()?;
    if name != expected {
        return Err(Error::new(
            name.span(),
            format!("expected `{expected} = Type`"),
        ));
    }
    input.parse::<Token![=]>()?;
    let value = input.parse()?;
    if !input.is_empty() {
        return Err(input.error(format!("unexpected {context} arguments")));
    }
    Ok(value)
}

fn cfg_condition(attrs: &[Attribute]) -> syn::Result<Option<TokenStream>> {
    if attrs.is_empty() {
        return Ok(None);
    }
    let mut conditions = Vec::with_capacity(attrs.len());
    for attr in attrs {
        if attr.path().is_ident("cfg_attr") {
            return Err(Error::new_spanned(
                attr,
                "cfg_attr is not yet supported on coroutine request variants",
            ));
        }
        let Meta::List(list) = &attr.meta else {
            return Err(Error::new_spanned(attr, "expected #[cfg(...)]"));
        };
        conditions.push(list.tokens.clone());
    }
    Ok(Some(quote! { all(#(#conditions),*) }))
}

fn fresh_generic_ident(generics: &syn::Generics, base: &str) -> syn::Ident {
    let mut name = base.to_string();
    loop {
        let candidate = format_ident!("{name}");
        let already_used = generics.params.iter().any(|param| match param {
            GenericParam::Type(param) => param.ident == candidate,
            GenericParam::Lifetime(param) => param.lifetime.ident == candidate,
            GenericParam::Const(param) => param.ident == candidate,
        });
        if !already_used {
            return candidate;
        }
        name.push('0');
    }
}
