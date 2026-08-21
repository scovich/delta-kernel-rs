use std::fmt::Display;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse2, parse_quote, Attribute, Error, Fields, GenericParam, ItemEnum, ItemTrait, Meta, Type,
    TypeParam,
};

struct WorkflowVariant {
    attrs: Vec<Attribute>,
    cfg_attrs: Vec<Attribute>,
    ident: syn::Ident,
    field_type: Type,
    state: Option<Type>,
    is_output: bool,
}

pub(crate) fn expand_workflow(attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    if !attr.is_empty() {
        return spanned_err(attr, "coroutine_workflow takes no arguments");
    }
    let input: ItemEnum = parse2(item)?;
    let variants = input
        .variants
        .iter()
        .map(parse_workflow_variant)
        .collect::<syn::Result<Vec<_>>>()?;
    let (output_variants, operation_variants): (Vec<_>, Vec<_>) =
        variants.into_iter().partition(|variant| variant.is_output);
    let Ok([output_variant]): Result<[_; 1], _> = output_variants.try_into() else {
        return spanned_err(&input.ident, "exactly one #[output] variant required");
    };
    if operation_variants.is_empty() {
        return spanned_err(&input.ident, "at least one operation variant is required");
    }
    let output_ident = &output_variant.ident;
    let output_type = &output_variant.field_type;
    let output_attrs = &output_variant.attrs;
    let expanded_output = quote! {
        #(#output_attrs)*
        #output_ident(#output_type)
    };

    let enum_ident = &input.ident;
    let generics = &input.generics;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let expansions = operation_variants.iter().map(|variant| {
        let WorkflowVariant {
            attrs,
            cfg_attrs,
            ident,
            field_type: operation,
            state,
            ..
        } = variant;
        let (capability, request_type, resume_type, resume_state, state_definition) = match state {
            Some(state) => (
                quote!(::delta_kernel::coroutine::CanRequestPaginated<#operation>),
                quote!(::delta_kernel::coroutine::Pagination<#operation, #state>),
                quote!(::delta_kernel::coroutine::PaginatedResume),
                quote!(, #state),
                quote!(type State = #state;),
            ),
            None => (
                quote!(::delta_kernel::coroutine::CanRequest<#operation>),
                quote!(#operation),
                quote!(::delta_kernel::coroutine::Resume),
                quote!(),
                quote!(),
            ),
        };
        let expanded_operation = quote! {
            #(#attrs)*
            #ident(
                #request_type,
                #resume_type<
                    #enum_ident #type_generics,
                    #operation
                    #resume_state
                >,
            )
        };
        let request_impl = quote! {
            #(#cfg_attrs)*
            impl #impl_generics #capability
                for #enum_ident #type_generics
                #where_clause
            {
                #state_definition

                fn request(
                    request: #request_type,
                    resume: #resume_type<
                        Self,
                        #operation
                        #resume_state
                    >,
                ) -> Self {
                    Self::#ident(request, resume)
                }
            }
        };
        (expanded_operation, request_impl)
    });
    let (expanded_operations, request_impls): (Vec<_>, Vec<_>) = expansions.unzip();

    let visibility = &input.vis;
    Ok(quote! {
        #visibility enum #enum_ident #generics {
            #expanded_output,
            #(#expanded_operations),*
        }

        #(#request_impls)*

        impl #impl_generics ::delta_kernel::coroutine::Workflow
            for #enum_ident #type_generics
            #where_clause
        {
            type Output = #output_type;

            fn finish(output: Self::Output) -> Self {
                Self::#output_ident(output)
            }
        }
    })
}

pub(crate) fn expand_capabilities(
    attr: TokenStream,
    item: TokenStream,
) -> syn::Result<TokenStream> {
    if !attr.is_empty() {
        return spanned_err(attr, "coroutine_capabilities takes no arguments");
    }
    let input: ItemTrait = parse2(item)?;
    if let Some(item) = input.items.first() {
        return spanned_err(item, "coroutine capability traits cannot define items");
    }
    if input.supertraits.is_empty() {
        return spanned_err(
            &input.ident,
            "coroutine capability traits require at least one capability bound",
        );
    }

    let trait_ident = &input.ident;
    let trait_generics = &input.generics;
    let (_, trait_type_generics, _) = trait_generics.split_for_impl();
    let supertraits = &input.supertraits;

    let mut impl_generics = trait_generics.clone();
    let workflow_ident = fresh_generic_ident(trait_generics, "W");
    impl_generics
        .params
        .push(GenericParam::Type(TypeParam::from(workflow_ident.clone())));
    impl_generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(#workflow_ident: #supertraits));
    let (impl_generics, _, impl_where_clause) = impl_generics.split_for_impl();

    Ok(quote! {
        #input

        impl #impl_generics #trait_ident #trait_type_generics for #workflow_ident
            #impl_where_clause
        {
        }
    })
}

fn parse_workflow_variant(variant: &syn::Variant) -> syn::Result<WorkflowVariant> {
    let Fields::Unnamed(fields) = &variant.fields else {
        return spanned_err(&variant.fields, "variants must use tuple fields");
    };

    let mut is_paginated = false;
    let mut is_output = false;
    let mut attrs = Vec::new();
    let mut cfg_attrs = Vec::new();
    for attr in &variant.attrs {
        if attr.path().is_ident("paginated") {
            if is_paginated {
                return spanned_err(attr, "duplicate paginated attribute");
            }
            if !matches!(attr.meta, Meta::Path(_)) {
                return spanned_err(attr, "expected #[paginated]");
            }
            is_paginated = true;
        } else if attr.path().is_ident("output") {
            if is_output {
                return spanned_err(attr, "duplicate output attribute");
            }
            if !matches!(attr.meta, Meta::Path(_)) {
                return spanned_err(attr, "expected #[output]");
            }
            is_output = true;
        } else {
            if attr.path().is_ident("cfg") || attr.path().is_ident("cfg_attr") {
                cfg_attrs.push(attr.clone());
            }
            attrs.push(attr.clone());
        }
    }
    let ident = &variant.ident;
    if is_output && is_paginated {
        return spanned_err(ident, "output variant cannot be paginated");
    }
    if is_output && !cfg_attrs.is_empty() {
        return spanned_err(ident, "output variant cannot be cfg-gated");
    }
    let (expected_fields, message) = if is_paginated {
        (2, "paginated variant must contain (Operation, State)")
    } else {
        (1, "variant must contain exactly one field")
    };
    if fields.unnamed.len() != expected_fields {
        return spanned_err(fields, message);
    }
    let mut field_types = fields.unnamed.iter().map(|field| field.ty.clone());
    let Some(field_type) = field_types.next() else {
        return spanned_err(fields, "workflow variant has no fields");
    };
    Ok(WorkflowVariant {
        attrs,
        cfg_attrs,
        ident: ident.clone(),
        field_type,
        state: field_types.next(),
        is_output,
    })
}

fn spanned_err<T>(tokens: impl ToTokens, message: impl Display) -> syn::Result<T> {
    Err(Error::new_spanned(tokens, message))
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
