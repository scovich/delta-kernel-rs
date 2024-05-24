use proc_macro::{TokenStream};
use proc_macro2::{Span, TokenStream as TokenStream2, TokenTree as TokenTree2};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    Error, Fields, FieldsNamed, FieldsUnnamed, Ident, ItemStruct, LitBool,
    Result, Token, Type, TypePath,
};

// target, mutable, sized
struct Params(Type, Option<LitBool>, Option<LitBool>);

impl Parse for Params {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse the "target=..." field (always first)
        let key: Ident = input.parse()?;
        if let "target" = key.to_string().as_str() {
            let _: Token![=] = input.parse()?;
        } else {
            return Err(Error::new(key.span(), "expected `target` field"));
        }
        let target: Type = input.parse()?;

        // Parse the remaining fields (unordered)
        let mut mutable = None;
        let mut sized = None;
        while !input.is_empty() {
            let _: Token![,] = input.parse()?;
            let ident: Ident = input.parse()?;
            let _: Token![=] = input.parse()?;
            let value: LitBool = input.parse()?;
            match ident.to_string().as_str() {
                "mutable" if mutable.is_none() => mutable = Some(value),
                "sized" if sized.is_none() => sized = Some(value),
                other => {
                    return Err(Error::new(
                        ident.span(),
                        format!("unknown or duplicated argument `{}`", other),
                    ));
                }
            }
        }
        Ok(Self(target, mutable, sized))
    }
}

fn bool_to_boolean(b: bool) -> TokenStream2 {
    let name = if b {
        quote! { True }
    } else {
        quote! { False }
    };
    quote! { crate::handle::#name }
}

fn compile_error(span: Span, msg: impl AsRef<str>) -> Result<TokenStream2> {
    Err(Error::new(span, msg.as_ref()))
}

// Check whether the requested type is `Self`
fn is_self(path: &TypePath) -> bool {
    let tokens: Vec<_> = path.to_token_stream().into_iter().collect();
    if let [TokenTree2::Ident(ident)] = tokens.as_slice() {
        *ident == "Self"
    } else {
        false
    }
}

fn emit_handle_descriptor(st: &mut ItemStruct, target: Type, mutable: bool, sized: bool) -> Result<TokenStream2> {
    let ident = &st.ident;
    match st.fields {
        Fields::Named(FieldsNamed {
            named: ref fields, ..
        })
            | Fields::Unnamed(FieldsUnnamed {
                unnamed: ref fields,
                ..
            }) if !fields.is_empty() => {
                return compile_error(fields.span(), "A handle struct must not define any fields")
            }
        _ => {}
    };

    // Inject a single unconstructible member to make it a NZST
    let new_struct = quote! { struct #ident(crate::handle::Unconstructable); };
    let new_struct: ItemStruct = syn::parse2(new_struct)?;
    st.fields = new_struct.fields;

    let mutable = bool_to_boolean(mutable);
    let sized = bool_to_boolean(sized);

    Ok(quote! {
        #[automatically_derived]
        impl crate::handle::HandleDescriptor for #ident {
            type Target = #target;
            type Mutable = #mutable;
            type Sized = #sized;
        }
    })
}

/// Macro for conveniently deriving a `delta_kernel_ffi::handle::HandleDescriptor`.
#[proc_macro_attribute]
pub fn handle_descriptor(attr: TokenStream, item: TokenStream) -> TokenStream {
    handle_descriptor2(attr.into(), item.into()).unwrap_or_else(Error::into_compile_error).into()
}
fn handle_descriptor2(attr: TokenStream2, item: TokenStream2) -> Result<TokenStream2> {
    let argspan = attr.span().clone();
    let params: Params = syn::parse2(attr)?;
    let mut st: ItemStruct = syn::parse2(item)?;
    let ident = &st.ident;
    let descriptor_impl = match params {
        // special case: target=Self
        Params(Type::Path(ref path), mutable, sized) if is_self(path) => {
            if let Some(LitBool { value: false, span }) = mutable {
                return compile_error(span, "self handles are always mutable")
            } else if let Some(LitBool { value: false, span }) = sized {
                return compile_error(span, "self handles are always sized")
            } else {
                quote! {
                    #[automatically_derived]
                    impl crate::handle::SelfHandle for #ident {}
                }
            }
        }

        Params(target @ Type::Path(_), mutable, sized) => {
            let mutable = match mutable {
                Some(mutable) => mutable.value(),
                _ => return compile_error(argspan, "missing `mutable=<bool>` argument"),
            };
            let sized = match sized {
                Some(sized) => sized.value(),
                _ => return compile_error(argspan, "missing `sized=<bool>` argument"),
            };
            emit_handle_descriptor(&mut st, target, mutable, sized)?
        }
        Params(target @ Type::TraitObject(_), mutable, sized) => {
            let mutable = match mutable {
                Some(mutable) => mutable.value(),
                _ => return compile_error(argspan, "missing `mutable=<bool>` argument"),
            };
            if let Some(LitBool { value: true, span }) = sized {
                return compile_error(span, "trait objects are never sized");
            };
            emit_handle_descriptor(&mut st, target, mutable, false)?
        }

        Params(target, _, _) => return compile_error(target.span(), "expected a type or a trait object"),
    };

    Ok(quote_spanned! { st.span() => #st #descriptor_impl })
}
