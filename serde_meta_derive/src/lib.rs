extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;
use quote::quote;
use syn;

extern crate serde_meta;

#[proc_macro_derive(SerdeMeta)]
/// automatically generates a `meta()` function for a given struct
///
/// This generates a static variable `_<struct_name>_META_INFO`
/// a reference to which is returned by that function.
pub fn derive_serde_meta(_item: TokenStream) -> TokenStream {
    internal_derive_serde_meta(_item.into()).into()
}

/// converts the ident of a struct to a String with the name of the
/// corresponding internal meta-data variable name.
///
/// For example "A" will return "_A_META_INFO"
fn build_static_variable_name_str(ident: &dyn ToString) -> String {
    // TODO: change parameter type to &syn::Ident?
    format!("_{}_META_INFO", ident.to_string())
}

/// converts the ident of a struct to a syn::Ident with the name of the
/// corresponding internal meta-data variable name.
fn build_static_variable_name(ident: &syn::Ident) -> syn::Ident {
    let meta_info_name_str = build_static_variable_name_str(ident);
    syn::Ident::new(&meta_info_name_str, ident.span())
}

/// takes a given `syn::Path` and returns the corresponding
/// `syn::Path` to the meta object.
///
/// Does not check, if a meta object exists for that path!
fn build_static_variable_path(path: &syn::Path) -> syn::Path {
    let mut res = path.clone();
    let item = res.segments.pop().unwrap();
    let last_value = item.value().ident.clone();
    let new_value = build_static_variable_name(&last_value);
    let mut v = item.into_value();
    v.ident = new_value;
    res.segments.push(v);
    assert!(res.segments.last().unwrap().ident == build_static_variable_name(&last_value));
    res
}

/// library internal function to automatically generate a `meta()` function for a given struct
///
/// using a separate function with proc_macro2::TokenStreams to implement the
/// logic, to make it unit testable, since proc_macro can not be used in the
/// context of unit tests.
fn internal_derive_serde_meta(item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input: syn::DeriveInput = syn::parse2(item).unwrap();

    //println!("Input {}: {:#?}", input.ident, input.data);
    let ident = input.ident;

    let lifetimes: Vec<&syn::Lifetime> = input.generics.lifetimes().map(|x| &x.lifetime).collect();
    let has_lifetimes = !lifetimes.is_empty();

    let lifet = if has_lifetimes {
        quote! {<#(#lifetimes),*>}
    } else {
        quote! {}
    };

    let gen = match input.data {
        syn::Data::Struct(data_struct) => derive_struct(&ident, data_struct),
        syn::Data::Enum(data_enum) => derive_enum(&ident, data_enum),
        _ => panic!(
            "internal_derive_serde_meta: Not implemented for {:#?}",
            input.data
        ),
    };
    let meta_info_name_ident = build_static_variable_name(&ident);
    let res = quote! {
        impl #lifet SerdeMeta for #ident #lifet {
            fn meta() -> serde_meta::TypeInformation<'static> {
                #gen
            }
        }
    };
    //println!("#######\nOutput:\n{}", res);
    res
}

/// converts an enum token stream into tokens
/// generating the corresponding `TypeInformation::EnumValue` meta data object
fn derive_enum(ident: &syn::Ident, data_enum: syn::DataEnum) -> proc_macro2::TokenStream {
    let strident = format!("{}", ident);
    let variants = derive_enum_variants(data_enum.variants);
    quote! {
        serde_meta::TypeInformation::EnumValue(NamedTypeInformation::new(#strident, EnumType::new(#variants)))
    }
}

/// converts the variants in an enum token stream into tokens
/// generating the corresponding array of EnumVariantTypes for a meta data object
fn derive_enum_variants(
    variants: syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> proc_macro2::TokenStream {
    let variants_iter = variants.iter().map(|v| derive_enum_variant(v));

    quote! {
        Box::new([#(#variants_iter),*])
    }
}

/// converts an enum variant token stream into tokens
/// generating the corresponding meta data object
/// Which is a `EnumVariantType`
fn derive_enum_variant(variant: &syn::Variant) -> proc_macro2::TokenStream {
    let strident = format!("{}", variant.ident);
    let inner_type = match &variant.fields {
        syn::Fields::Named(f) => {
            let fields = derive_fields_named(f);
            quote! {
                serde_meta::EnumVariantType::StructVariant(#fields)
            }
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            quote! {
                serde_meta::EnumVariantType::TupleVariant(TupleTypes::new( #fields ))
            }
        }
        syn::Fields::Unit => {
            quote! {
                serde_meta::EnumVariantType::UnitVariant()
            }
        }
    };

    quote! {
        serde_meta::EnumVariant::new(#strident, #inner_type)
    }
}

/// Converts the tokens of a named struct into tokens
/// generating the corresponding meta object,
/// which is one of:
/// * TypeInformation::StructValue
/// * TypeInformation::TupleStructValue
/// * TypeInformation::UnitStructValue
fn derive_struct(ident: &syn::Ident, data_struct: syn::DataStruct) -> proc_macro2::TokenStream {
    let strident = format!("{}", ident);
    match &data_struct.fields {
        syn::Fields::Named(f) => {
            let fields = derive_fields_named(f);
            let res = quote! {
                serde_meta::TypeInformation::StructValue(
                    serde_meta::NamedTypeInformation::new( #strident, #fields)
                )
            };
            res
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            let res = quote! {
                serde_meta::TypeInformation::TupleStructValue(
                    serde_meta::NamedTypeInformation::new(#strident,
                        serde_meta::TupleTypes::new(#fields))
                )
            };
            res
        }
        syn::Fields::Unit => {
            let res = quote! {
                serde_meta::TypeInformation::UnitStructValue( serde_meta::UnitStructType::new( #strident, () ))
            };
            res
        }
    }
}

/// Converts the tokens describing named fields of a struct
/// (or struct variant of an enum), into a token stream generating
/// the corresponding meta data, i.e. a Fields object
fn derive_fields_named(fields: &syn::FieldsNamed) -> proc_macro2::TokenStream {
    let fields_iter = fields.named.iter().map(|f| derive_named_field(f));

    quote! {
        serde_meta::Fields::new(Box::new([#(#fields_iter),*]))
    }
}

/// Converts the tokens describing named fields of a tuple
/// (or tuple variant of an enum), into a token stream generating
/// the corresponding meta data, i.e. a reference to an array of references
/// to the corresponding meta data type objects
fn derive_fields_unnamed(fields: &syn::FieldsUnnamed) -> proc_macro2::TokenStream {
    let fields_iter = fields
        .unnamed
        .iter()
        .map(|f| derive_unnamed_tuple_element(&f.ty));

    quote! {
        Box::new([#(#fields_iter),*])
    }
}

/// Tries to convert a type_name to tokens generating a corresponding
/// `TypeInformation` object.
/// Returns `None` if the conversion was not possible.
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

/// takes a given `syn::Path` and returns the corresponding
/// `TokenStream` describing the path to the meta object,
/// or if it is a simple type a corresponding TypeInformation
/// object.
///
/// Does not check, if a meta object exists for that path!
fn path_to_meta(path: &syn::Path) -> proc_macro2::TokenStream {
    let res = quote! { #path::meta() };
    res
}

/// takes a given `syn::TypeArray` and returns the corresponding
/// `TokenStream` generating a `serde_meta::TypeInformation::TupleValue`
/// representing the array.
fn array_to_meta(a: &syn::TypeArray) -> proc_macro2::TokenStream {
    if let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Int(lit),
        ..
    }) = &a.len
    {
        let size = lit.base10_parse().expect("Could not parse array size");
        let t = type_to_meta(&*a.elem);
        let tt = quote! { serde_meta::TupleType::new( || #t ) };
        let fields_iter = std::iter::repeat(tt).take(size);
        let repeated = quote! { Box::new([#(#fields_iter),*]) };
        let res = quote! {
            serde_meta::TypeInformation::TupleValue(serde_meta::TupleTypes::new( #repeated ))
        };
        res
    } else {
        panic!(
            "Only integer literals are supported as array length right now, but found a {:#?}",
            a.len
        );
    }
}

/// takes a `syn::Type` and returns a `TokenStream` generating the corresponding
/// meta object
fn type_to_meta(ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Path(p) => path_to_meta(&p.path),
        syn::Type::Array(a) => array_to_meta(&a),
        syn::Type::Reference(syn::TypeReference { elem: t, .. }) => type_to_meta(&*t),
        syn::Type::Slice(syn::TypeSlice { elem: t, .. }) => {
            let inner = type_to_meta(&*t);
            quote! {
                serde_meta::TypeInformation::SeqValue(serde_meta::SeqType::new( || #inner ))
            }
        }
        _ => panic!("type_to_meta: Not implemented for {:#?}", ty),
    }
}

/// generates a `TokenStream` that creates the meta data for a given named field.
fn derive_named_field(field: &syn::Field) -> proc_macro2::TokenStream {
    let x = field.ident.clone(); //TODO: is clone realy the only option here?
    let ident = format!("{}", x.unwrap());
    let type_info = type_to_meta(&field.ty);
    let map_res = quote! {
        serde_meta::Field::new(
            #ident,
            || #type_info
        )
    };
    map_res
}

/// generates a `TokenStream` that creates the meta data for a given tuple element.
fn derive_unnamed_tuple_element(ty: &syn::Type) -> proc_macro2::TokenStream {
    let type_info_meta = type_to_meta(ty);
    let map_res = quote! {
        serde_meta::TupleType::new(
            || #type_info_meta
        )
    };
    map_res
}

#[cfg(test)]
mod test {
    use super::*;

    use quote::quote;
    use syn::parse_quote;

    #[test]
    fn test_build_static_variable_name_str() {
        assert_eq!("_A_META_INFO", build_static_variable_name_str(&"A"));
    }

    #[test]
    fn test_handle_simple_type_return_none() {
        assert_eq!(handle_simple_type("NotASimpleType").is_none(), true);
    }

    #[test]
    fn test_handle_simple_type_return_u8() {
        let res = handle_simple_type("u8");
        assert_eq!(res.is_some(), true);
        let expectation = quote! { serde_meta::TypeInformation::U8Value() };
        assert_eq!(res.unwrap().to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_serde_unit_struct() {
        let input = quote! { struct A; };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            impl SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation<'static> {
                    serde_meta::TypeInformation::UnitStructValue( serde_meta::UnitStructType::new( "A", () ) )
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
            impl SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation<'static> {
                    serde_meta::TypeInformation::StructValue(
                        serde_meta::NamedTypeInformation::new("A",
                            serde_meta::Fields::new(Box::new([]))
                        )
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_tuple_struct() {
        let input = quote! {struct A(u8, u16, u32); };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            impl SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation<'static> {
                    serde_meta::TypeInformation::TupleStructValue(
                        serde_meta::NamedTypeInformation::new("A", serde_meta::TupleTypes::new(Box::new([
                            serde_meta::TupleType::new( || u8::meta() ),
                            serde_meta::TupleType::new( || u16::meta() ),
                            serde_meta::TupleType::new( || u32::meta() )
                        ])))
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_type_to_meta_slice() {
        let input = parse_quote! {[u8]};
        let expectation = quote! {
            serde_meta::TypeInformation::SeqValue(serde_meta::SeqType::new( || u8::meta() ))
        };

        let res = type_to_meta(&input);

        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_array_to_meta() {
        // as parse is not implemented for TypeArray, parse an ExprType and get the TypeArray from there
        let typedef: syn::ExprType = parse_quote! { a: [u8; 2]};
        let input = match *typedef.ty {
            syn::Type::Array(a) => a,
            _ => panic!("typedef was messed up and did not return an Array as type..."),
        };

        let expectation = quote! {
            serde_meta::TypeInformation::TupleValue(
                serde_meta::TupleTypes::new(Box::new([
                    serde_meta::TupleType::new( || u8::meta() ),
                    serde_meta::TupleType::new( || u8::meta() )
                ]))
            )
        };

        let res = array_to_meta(&input);

        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_array_struct() {
        let input = quote! { struct A {f: [u8; 3]} };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            impl SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation<'static> {
                    serde_meta::TypeInformation::StructValue(
                        serde_meta::NamedTypeInformation::new("A",
                            serde_meta::Fields::new(Box::new([serde_meta::Field::new("f",
                                || serde_meta::TypeInformation::TupleValue(
                                    serde_meta::TupleTypes::new(Box::new([
                                        serde_meta::TupleType::new( || u8::meta() ),
                                        serde_meta::TupleType::new( || u8::meta() ),
                                        serde_meta::TupleType::new( || u8::meta() )
                                    ]))
                                )
                            )]))
                        )
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_self_referencing_struct() {
        let input = quote! { struct A { f: &A } };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            impl SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation<'static> {
                    serde_meta::TypeInformation::StructValue(
                        serde_meta::NamedTypeInformation::new("A",
                            serde_meta::Fields::new(Box::new([ serde_meta::Field::new("f", || A::meta()) ]))
                        )
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
