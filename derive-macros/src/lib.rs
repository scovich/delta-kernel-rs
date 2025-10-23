use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::{
    Data, DataStruct, DeriveInput, Error, Fields, ImplItemFn, Item, ItemFn, Meta, PathArguments,
    TraitItemFn, Type, Visibility,
};

/// Parses a dot-delimited column name into an array of field names. See
/// `delta_kernel::expressions::column_name::column_name` macro for details.
#[proc_macro]
pub fn parse_column_name(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let is_valid = |c: char| c.is_ascii_alphanumeric() || c == '_' || c == '.';
    let err = match syn::parse(input) {
        Ok(syn::Lit::Str(name)) => match name.value().chars().find(|c| !is_valid(*c)) {
            Some(bad_char) => Error::new(name.span(), format!("Invalid character: {bad_char:?}")),
            _ => {
                let path = name.value();
                let path = path.split('.').map(proc_macro2::Literal::string);
                return quote_spanned! { name.span() => [#(#path),*] }.into();
            }
        },
        Ok(lit) => Error::new(lit.span(), "Expected a string literal"),
        Err(err) => err,
    };
    err.into_compile_error().into()
}

/// Derive a `delta_kernel::schemas::ToSchema` implementation for the annotated struct. The actual
/// field names in the schema (and therefore of the struct members) are all mandated by the Delta
/// spec, and so the user of this macro is responsible for ensuring that
/// e.g. `Metadata::schema_string` is the snake_case-ified version of `schemaString` from [Delta's
/// Change Metadata](https://github.com/delta-io/delta/blob/master/PROTOCOL.md#change-metadata)
/// action (this macro allows the use of standard rust snake_case, and will convert to the correct
/// delta schema camelCase version).
///
/// If a field sets `allow_null_container_values`, it means the underlying data can contain null in
/// the values of the container (i.e. a `key` -> `null` in a `HashMap`). Therefore the schema should
/// mark the value field as nullable, but those mappings will be dropped when converting to an
/// actual rust `HashMap`. Currently this can _only_ be set on `HashMap` fields.
#[proc_macro_derive(ToSchema, attributes(allow_null_container_values))]
pub fn derive_to_schema(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_ident = input.ident;

    let schema_fields = gen_schema_fields(&input.data);
    let output = quote! {
        #[automatically_derived]
        impl delta_kernel::schema::ToSchema for #struct_ident {
            fn to_schema() -> delta_kernel::schema::StructType {
                use delta_kernel::schema::derive_macro_utils::{
                    ToDataType as _, GetStructField as _, GetNullableContainerStructField as _,
                };
                delta_kernel::schema::StructType::new_unchecked([
                    #schema_fields
                ])
            }
        }
    };
    proc_macro::TokenStream::from(output)
}

// turn our struct name into the schema name, goes from snake_case to camelCase
fn get_schema_name(name: &Ident) -> Ident {
    let snake_name = name.to_string();
    let mut next_caps = false;
    let ret: String = snake_name
        .chars()
        .filter_map(|c| {
            if c == '_' {
                next_caps = true;
                None
            } else if next_caps {
                next_caps = false;
                // This assumes we're using ascii, should be okay
                Some(c.to_ascii_uppercase())
            } else {
                Some(c)
            }
        })
        .collect();
    Ident::new(&ret, name.span())
}

fn gen_schema_fields(data: &Data) -> TokenStream {
    let fields = match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => {
            return Error::new(
                Span::call_site(),
                "this derive macro only works on structs with named fields",
            )
            .to_compile_error()
        }
    };

    let schema_fields = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap(); // we know these are named fields
        let name = get_schema_name(name);
        let have_schema_null = field.attrs.iter().any(|attr| {
            // check if we have allow_null_container_values attr
            match &attr.meta {
                Meta::Path(path) => path.get_ident().is_some_and(|ident| ident == "allow_null_container_values"),
                _ => false,
            }
        });

        match field.ty {
            Type::Path(ref type_path) => {
                let type_path_quoted = type_path.path.segments.iter().map(|segment| {
                    let segment_ident = &segment.ident;
                    match &segment.arguments {
                        PathArguments::None => quote! { #segment_ident :: },
                        PathArguments::AngleBracketed(angle_args) => quote! { #segment_ident::#angle_args :: },
                        _ => Error::new(segment.arguments.span(), "Can only handle <> type path args").to_compile_error()
                    }
                });
                if have_schema_null {
                    if let Some(last_ident) = type_path.path.segments.last().map(|seg| &seg.ident) {
                        if last_ident != "HashMap" {
                           return Error::new(
                                last_ident.span(),
                                format!("Can only use allow_null_container_values on HashMap fields, not {last_ident}")
                            ).to_compile_error()
                        }
                    }
                    quote_spanned! { field.span() => #(#type_path_quoted)* get_nullable_container_struct_field(stringify!(#name))}
                } else {
                    quote_spanned! { field.span() => #(#type_path_quoted)* get_struct_field(stringify!(#name))}
                }
            }
            _ => Error::new(field.span(), format!("Can't handle type: {:?}", field.ty)).to_compile_error()
        }
    });
    quote! { #(#schema_fields),* }
}

/// Derive an IntoEngineData trait for a struct that has all fields implement `Into<Scalar>`.
///
/// This is a relatively simple macro to produce the boilerplate for converting a struct into
/// EngineData using the `create_one` method. TODO: (doc)tests included in the delta_kernel crate:
/// `IntoEngineData` trait.
#[proc_macro_derive(IntoEngineData)]
pub fn into_engine_data_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let Data::Struct(DataStruct {
        fields: Fields::Named(fields),
        ..
    }) = &input.data
    else {
        return Error::new(
            struct_name.span(),
            "IntoEngineData can only be derived for structs with named fields",
        )
        .to_compile_error()
        .into();
    };

    let fields = &fields.named;
    let field_idents = fields.iter().map(|f| &f.ident);
    let field_types = fields.iter().map(|f| &f.ty);

    let expanded = quote! {
        #[automatically_derived]
        impl crate::IntoEngineData for #struct_name
        where
            #(#field_types: Into<crate::expressions::Scalar>),*
        {
            fn into_engine_data(
                self,
                schema: crate::schema::SchemaRef,
                engine: &dyn crate::Engine)
            -> crate::DeltaResult<Box<dyn crate::EngineData>> {
                // NB: we `use` here to avoid polluting the caller's namespace
                use crate::EvaluationHandlerExtension as _;
                let values = [
                    #(self.#field_idents.into()),*
                ];
                let evaluator = engine.evaluation_handler();
                evaluator.create_one(schema, &values)
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

/// Mark items as `internal_api` to make them public iff the `internal-api` feature is enabled.
/// Note this doesn't work for inline module definitions (see `internal_mod!` macro in delta_kernel
/// crate - can't export macro_rules! from proc macro crate).
/// Ref: <https://github.com/rust-lang/rust/issues/54727>
#[proc_macro_attribute]
pub fn internal_api(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as Item);

    // Create a version with public visibility for the unstable feature
    let public_version = make_public(input.clone());

    // The original item stays as-is for the non-unstable case
    let output = quote! {
        #[cfg(feature = "internal-api")]
        #public_version

        #[cfg(not(feature = "internal-api"))]
        #input
    };

    output.into()
}

fn make_public(mut item: Item) -> Item {
    fn set_pub(vis: &mut Visibility) -> Result<(), syn::Error> {
        if matches!(vis, Visibility::Public(_)) {
            return Err(Error::new(
                vis.span(),
                "ineligible for #[internal_api]: item is already public",
            ));
        }
        *vis = syn::parse_quote!(pub);
        Ok(())
    }

    let result = match &mut item {
        Item::Fn(f) => set_pub(&mut f.vis),
        Item::Struct(s) => set_pub(&mut s.vis),
        Item::Enum(e) => set_pub(&mut e.vis),
        Item::Trait(t) => set_pub(&mut t.vis),
        Item::Type(t) => set_pub(&mut t.vis),
        Item::Mod(m) => set_pub(&mut m.vis),
        Item::Static(s) => set_pub(&mut s.vis),
        Item::Const(c) => set_pub(&mut c.vis),
        Item::Union(u) => set_pub(&mut u.vis),
        // foreign mod, impl block, and all others not handled
        _ => Err(Error::new(
            item.span(),
            format!("unsupported item type for #[internal_api]: {item:?}"),
        )),
    };

    if let Err(err) = result {
        let error = err.to_compile_error();
        let mut tokens = item.to_token_stream();
        tokens.extend(error);
        return syn::parse_quote!(#tokens);
    }

    item
}

/// Conditionally adds the `async` keyword to a function based on the `async` feature flag.
///
/// Use this for regular functions and struct impl methods (where there's no `#[async_trait]`).
/// For trait definitions and trait impls, use `#[async_trait_fn]` instead.
///
/// # Convention
///
/// Write WITHOUT `async` in source. The macro adds it in async mode.
///
/// # Examples
///
/// ```ignore
/// // Regular function
/// #[async_fn]
/// fn helper() { await_!(operation())?; }
///
/// // Struct impl method
/// impl MyStruct {
///     #[async_fn]
///     fn process(&self) { await_!(operation())?; }
/// }
/// ```
#[proc_macro_attribute]
pub fn async_fn(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let asyncness = cfg!(feature = "async").then(syn::token::Async::default);

    // Try regular function first (most common case)
    if let Ok(mut item_fn) = syn::parse::<ItemFn>(item.clone()) {
        item_fn.sig.asyncness = asyncness;
        return quote! { #item_fn }.into();
    }

    // Try struct impl method (NOT trait impl - use #[async_trait_fn] for those)
    if let Ok(mut impl_fn) = syn::parse::<ImplItemFn>(item.clone()) {
        impl_fn.sig.asyncness = asyncness;
        return quote! { #impl_fn }.into();
    }

    // Neither function nor impl method - emit item first, then diagnostic
    let item: proc_macro2::TokenStream = item.into();
    let diagnostic = Error::new(
        proc_macro2::Span::call_site(),
        "#[async_fn] could not parse this item as a function or impl method.\n\
         \n\
         This macro only works on:\n\
         - Regular functions: `#[async_fn] fn foo() { ... }`\n\
         - Struct impl methods: `impl Foo { #[async_fn] fn bar() { ... } }`\n\
         \n\
         For trait definitions and trait impls, use `#[async_trait_fn]` instead.\n\
         \n\
         If you believe you're using it correctly, check for syntax errors in the function body.",
    )
    .to_compile_error();

    // Emit item FIRST (so syntax errors appear first), diagnostic SECOND
    quote! {
        #item
        #diagnostic
    }
    .into()
}

/// Removes the `async` keyword from trait methods to enable dyn-compatibility.
///
/// This macro works in conjunction with `#[async_trait]` to handle trait methods correctly
/// in both sync and async modes. Use this for trait definitions AND trait implementations.
///
/// **IMPORTANT**: This is specifically for trait-related code. For regular functions and
/// struct impl methods, use `#[async_fn]` instead.
///
/// # Why Two Macros?
///
/// Trait impl methods (`ImplItemFn` in syn) need different behavior depending on context:
/// - Trait impls (with `#[async_trait]`): Must clear `async` (this macro)
/// - Struct impls (no `#[async_trait]`): Must use feature flag (`#[async_fn]`)
///
/// Since proc macros cannot see the parent `impl` block context, we need two separate macros
/// where the user chooses based on whether they're implementing a trait or not.
///
/// # How It Works
///
/// - **Async mode**: `#[async_trait]` (from async-trait crate) processes the trait first,
///   removing `async` and boxing the futures. This macro then has no effect since the
///   `async` keyword is already gone.
///
/// - **Sync mode**: `#[async_trait]` (our no-op) does nothing, so this macro removes the
///   `async` keyword to create a standard synchronous trait method.
///
/// # Examples
///
/// ```ignore
/// #[async_trait]
/// pub trait Handler {
///     #[async_trait_fn]  // For trait definitions
///     async fn handle(...) -> Result<()>;
/// }
///
/// #[async_trait]
/// impl Handler for MyHandler {
///     #[async_trait_fn]  // For trait implementations
///     async fn handle(...) -> Result<()> {
///         // implementation
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn async_trait_fn(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Try trait method definition
    if let Ok(mut trait_fn) = syn::parse::<TraitItemFn>(item.clone()) {
        trait_fn.sig.asyncness = None;
        return quote! { #trait_fn }.into();
    }

    // Try trait impl method
    if let Ok(mut impl_fn) = syn::parse::<ImplItemFn>(item.clone()) {
        impl_fn.sig.asyncness = None;
        return quote! { #impl_fn }.into();
    }

    // Neither - emit item first, then diagnostic
    let item: proc_macro2::TokenStream = item.into();
    let diagnostic = Error::new(
        proc_macro2::Span::call_site(),
        "#[async_trait_fn] could not parse this item as a trait method.\n\
         \n\
         This macro only works on:\n\
         - Trait method definitions: `trait Foo { #[async_trait_fn] async fn bar(); }`\n\
         - Trait impl methods: `impl Foo for Bar { #[async_trait_fn] async fn bar() { ... } }`\n\
         \n\
         For regular functions and struct impls, use `#[async_fn]` instead.\n\
         \n\
         If you believe you're using it correctly, check for syntax errors in the function body.",
    )
    .to_compile_error();

    quote! {
        #item
        #diagnostic
    }
    .into()
}

/// No-op proc macro that stands in for async-trait in sync mode.
///
/// In async mode, the kernel imports the real `async-trait` crate using this name,
/// which boxes futures to make traits with async methods dyn-compatible.
///
/// This macro should be applied to both trait definitions AND implementations.
///
/// # Example
///
/// ```ignore
/// #[async_trait]
/// pub trait ParquetHandler {
///     #[async_trait_fn]
///     async fn read_files(...) -> DeltaResult<FileDataReadResultIterator>;
/// }
///
/// #[async_trait]
/// impl ParquetHandler for DefaultParquetHandler {
///     #[async_trait_fn]
///     async fn read_files(...) -> DeltaResult<FileDataReadResultIterator> {
///         // implementation
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn async_trait(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Sync mode: no-op, return item unchanged
    item
}

/// Mode-agnostic test macro that works in both sync and async modes.
///
/// This macro combines `#[async_fn]` with conditional test attributes to create
/// tests that run correctly in both sync mode (regular `#[test]`) and async mode
/// (using `#[tokio::test]`).
///
/// # Basic Usage
///
/// ```ignore
/// #[async_test]
/// fn my_test() {
///     let result = await_!(async_operation())?;
///     assert_eq!(result, expected);
/// }
/// ```
///
/// # With Test Wrapper
///
/// For tests that need a wrapper (like `test_log::test` for logging), provide the
/// wrapper's name as an optional argument.
///
/// # Expansion
///
/// Without arguments:
/// ```ignore
/// #[async_test]
/// fn my_test() { ... }
/// ```
/// Expands to:
/// ```ignore
/// #[async_fn]
/// #[cfg_attr(not(feature = "async"), test)]
/// #[cfg_attr(feature = "async", ::tokio::test)]
/// fn my_test() { ... }
/// ```
///
/// With wrapper argument:
/// ```ignore
/// #[async_test(test_log::test)]
/// fn my_test() { ... }
/// ```
/// Expands to:
/// ```ignore
/// #[async_fn]
/// #[cfg_attr(not(feature = "async"), test_log::test)]
/// #[cfg_attr(feature = "async", test_log::test(::tokio::test))]
/// fn my_test() { ... }
/// ```
#[proc_macro_attribute]
pub fn async_test(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item: proc_macro2::TokenStream = item.into();
    
    // Determine the test attributes based on whether a wrapper is provided
    let (sync_test, async_test) = if attr.is_empty() {
        // No wrapper - use built-in test
        (quote! { test }, quote! { ::tokio::test })
    } else {
        // Has wrapper - user provides the full path (e.g., test_log::test)
        let wrapper: proc_macro2::TokenStream = attr.into();
        (quote! { #wrapper }, quote! { #wrapper(::tokio::test) })
    };
    
    let output = quote! {
        #[async_fn]
        #[cfg_attr(not(feature = "async"), #sync_test)]
        #[cfg_attr(feature = "async", #async_test)]
        #item
    };
    
    output.into()
}
