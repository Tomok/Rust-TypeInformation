extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;
use std::convert::TryInto;

use quote::quote;
use syn;

extern crate serde_meta;

#[proc_macro_derive(SerdeMeta)]
pub fn derive_serde_meta(_item: TokenStream) -> TokenStream {
    internal_derive_serde_meta(_item.into()).into()
}

fn build_static_variable_name_str(ident: &dyn ToString) -> String {
    format!("_{}_META_INFO", ident.to_string())
}

fn build_static_variable_name(ident: &syn::Ident) -> syn::Ident {
    let meta_info_name_str = build_static_variable_name_str(ident);
    syn::Ident::new(&meta_info_name_str, ident.span())
}

fn build_static_variable_path(path: &syn::Path) -> syn::Path {
    let mut res = path.clone();
    let item = res.segments.pop().unwrap();
    let last_value = item.value().ident.clone();
    let new_value = build_static_variable_name(&last_value);
    let mut v = item.into_value();
    v.ident = new_value;
    res.segments.push(v);
    assert!(res.segments.last().unwrap().value().ident == build_static_variable_name(&last_value));
    res
}

///using a separate function with proc_macro2::TokenStreams to implement the
///logic, to make it unit testable, since proc_macro can not be used in the
///context of unit tests.
fn internal_derive_serde_meta(item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    //TODO: Implement something usefull here
    let input: syn::DeriveInput = syn::parse2(item).unwrap();

    //println!("Input {}: {:#?}", input.ident, input.data);
    let ident = input.ident;
    let gen = match input.data {
        syn::Data::Struct(data_struct) => derive_struct(&ident, data_struct),
        _ => panic!("internal_derive_serde_meta: Not implemented for {:#?}", input.data),
    };
    let meta_info_name_ident = build_static_variable_name(&ident);
    let res = quote! {
        pub static #meta_info_name_ident: TypeInformation = #gen;

        impl SerdeMeta for #ident {
            fn meta() -> &'static serde_meta::TypeInformation {
                &#meta_info_name_ident
            }
        }
    };
    //println!("#######\nOutput:\n{}", res);
    res
}

fn derive_struct(ident: &syn::Ident, data_struct: syn::DataStruct) -> proc_macro2::TokenStream {
    let strident = format!("{}", ident);
    match data_struct.fields {
        syn::Fields::Named(f) => {
            let fields = derive_fields_named(f);
            let res = quote! {
                serde_meta::TypeInformation::StructValue {
                    name: #strident,
                    fields: #fields
                }
            };
            res
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            let res = quote! {
                serde_meta::TypeInformation::TupleStructValue {
                    name: #strident,
                    inner_types: #fields
                }
            };
            res
        }
        syn::Fields::Unit => {
            let res = quote! {
                serde_meta::TypeInformation::UnitStructValue{ name: #strident }
            };
            res
        }
    }
}

fn derive_fields_named(fields: syn::FieldsNamed) -> proc_macro2::TokenStream {
    let fields_iter = fields.named.iter().map(|f| derive_named_field(f));

    quote! {
        &[#(#fields_iter),*]
    }
}

fn derive_fields_unnamed(fields: syn::FieldsUnnamed) -> proc_macro2::TokenStream {
    let fields_iter = fields.unnamed.iter().map(|f| type_to_meta(&f.ty));

    quote! {
        &[#(&#fields_iter),*]
    }
}

fn handle_simple_type(type_name: &str) -> Option<proc_macro2::TokenStream> {
    match type_name {
        "bool" => Some(quote! { serde_meta::TypeInformation::BoolValue() }),
        "i8" => Some(quote! { serde_meta::TypeInformation::I8Value() }),
        "i16" => Some(quote! { serde_meta::TypeInformation::I16Value() }),
        "i32" => Some(quote! { serde_meta::TypeInformation::I32Value() }),
        "i64" => Some(quote! { serde_meta::TypeInformation::I64Value() }),
        "i128" => Some(quote! { serde_meta::TypeInformation::I128Value() }),

        "u8" => Some(quote! { serde_meta::TypeInformation::U8Value() }),
        "u16" => Some(quote! { serde_meta::TypeInformation::U16Value() }),
        "u32" => Some(quote! { serde_meta::TypeInformation::U32Value() }),
        "u64" => Some(quote! { serde_meta::TypeInformation::U64Value() }),
        "u128" => Some(quote! { serde_meta::TypeInformation::U128Value() }),

        "f32" => Some(quote! { serde_meta::TypeInformation::F32Value() }),
        "f64" => Some(quote! { serde_meta::TypeInformation::F64Value() }),

        "char" => Some(quote! { serde_meta::TypeInformation::CharValue() }),

        "str" => Some(quote! { serde_meta::TypeInformation::StringValue() }),
        _ => None,
    }
}

fn path_to_meta(path: &syn::Path) -> proc_macro2::TokenStream {
    if path.segments.len() == 1 {
        //could be a basic type
        let simple_type_res =
            handle_simple_type(&path.segments.first().unwrap().value().ident.to_string());
        if let Some(res) = simple_type_res {
            return res;
        }
    }
    let path_name = build_static_variable_path(&path);
    let res = quote! { #path_name };
    res
}

fn array_to_meta(a: &syn::TypeArray) -> proc_macro2::TokenStream {
    if let syn::Expr::Lit(syn::ExprLit {
        attrs: _,
        lit: syn::Lit::Int(lit),
    }) = &a.len
    {
        let l = lit.value();
        let t = type_to_meta(&*a.elem);
        let size: usize = l
            .try_into()
            .unwrap_or_else(|s| panic!("Array size to big: {:#?}", s));
        let fields_iter = std::iter::repeat(t).take(size);
        quote! {
            serde_meta::TypeInformation::TupleValue { inner_types: &[#(&#fields_iter),*] }
        }
    } else {
        panic!(
            "Only integer literals are supported as array length right now, but found a {:#?}",
            a.len
        );
    }
}

fn type_to_meta(ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Path(p) => path_to_meta(&p.path),
        syn::Type::Array(a) => array_to_meta(&a),
        _ => panic!("type_to_meta: Not implemented for {:#?}", ty),
    }
}

fn derive_named_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let x = field.ident.clone(); //TODO: is clone realy the only option here?
    let ident = format!("{}", x.unwrap());
    let type_info = type_to_meta(&field.ty);
    let map_res = quote! {
        Field {
            name: #ident,
            inner_type: &#type_info,
        }
    };
    map_res
}

#[cfg(test)]
mod test {
    use super::*;

    use quote::quote;

    #[test]
    fn test_derive_serde_unit_struct() {
        let input = quote! { struct A; };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            pub static _A_META_INFO: TypeInformation = serde_meta::TypeInformation::UnitStructValue {
                    name: "A"
            };

            impl SerdeMeta for A {
                fn meta() -> &'static serde_meta::TypeInformation {
                    & _A_META_INFO
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_serde_empty_struct() {
        let input = quote! { struct A {} };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            pub static _A_META_INFO: TypeInformation = serde_meta::TypeInformation::StructValue {
                name: "A",
                fields: &[]
            };

            impl SerdeMeta for A {
                fn meta() -> &'static serde_meta::TypeInformation {
                    & _A_META_INFO
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
