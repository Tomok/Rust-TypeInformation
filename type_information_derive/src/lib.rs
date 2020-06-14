extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;
use quote::quote;
use syn;

extern crate type_information;

#[proc_macro_derive(Meta)]
/// automatically generates a `meta()` function for a given struct
///
/// This generates a static variable `_<struct_name>_META_INFO`
/// a reference to which is returned by that function.
pub fn derive_type_information(_item: TokenStream) -> TokenStream {
    internal_derive_type_information(_item.into()).into()
}

/// library internal function to automatically generate a `meta()` function for a given struct
///
/// using a separate function with proc_macro2::TokenStreams to implement the
/// logic, to make it unit testable, since proc_macro can not be used in the
/// context of unit tests.
fn internal_derive_type_information(item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
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
            "internal_derive_type_information: Not implemented for {:#?}",
            input.data
        ),
    };
    let res = quote! {
        impl #lifet Meta for #ident #lifet {
            fn meta() -> type_information::TypeInformation {
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
        type_information::TypeInformation::EnumValue(NamedTypeInformation::new(#strident.to_owned(), EnumType::new(#variants)))
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
                type_information::EnumVariantType::StructVariant(#fields)
            }
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            quote! {
                type_information::EnumVariantType::TupleVariant(TupleTypes::new( #fields ))
            }
        }
        syn::Fields::Unit => {
            quote! {
                type_information::EnumVariantType::UnitVariant()
            }
        }
    };

    quote! {
        type_information::EnumVariant::new(#strident.to_owned(), #inner_type)
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
                type_information::TypeInformation::StructValue(
                    type_information::NamedTypeInformation::new( #strident.to_owned(), #fields)
                )
            };
            res
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            let res = quote! {
                type_information::TypeInformation::TupleStructValue(
                    type_information::NamedTypeInformation::new(#strident.to_owned(),
                        type_information::TupleTypes::new(#fields))
                )
            };
            res
        }
        syn::Fields::Unit => {
            let res = quote! {
                type_information::TypeInformation::UnitStructValue( type_information::UnitStructType::new( #strident.to_owned(), () ))
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
        type_information::Fields::new(Box::new([#(#fields_iter),*]))
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
/// `TokenStream` generating a `type_information::TypeInformation::TupleValue`
/// representing the array.
fn array_to_meta(a: &syn::TypeArray) -> proc_macro2::TokenStream {
    if let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Int(lit),
        ..
    }) = &a.len
    {
        let size = lit.base10_parse().expect("Could not parse array size");
        let t = type_to_meta(&*a.elem);
        let tt = quote! { type_information::TupleType::new( || #t ) };
        let fields_iter = std::iter::repeat(tt).take(size);
        let repeated = quote! { Box::new([#(#fields_iter),*]) };
        let res = quote! {
            type_information::TypeInformation::TupleValue(type_information::TupleTypes::new( #repeated ))
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
                type_information::TypeInformation::SeqValue(type_information::SeqType::new( || #inner ))
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
        type_information::Field::new(
            #ident.to_owned(),
            || #type_info
        )
    };
    map_res
}

/// generates a `TokenStream` that creates the meta data for a given tuple element.
fn derive_unnamed_tuple_element(ty: &syn::Type) -> proc_macro2::TokenStream {
    let type_info_meta = type_to_meta(ty);
    let map_res = quote! {
        type_information::TupleType::new(
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
    fn test_derive_serde_unit_struct() {
        let input = quote! { struct A; };
        let res = internal_derive_type_information(input);
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation {
                    type_information::TypeInformation::UnitStructValue( type_information::UnitStructType::new( "A".to_owned(), () ) )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_serde_empty_struct() {
        let input = quote! { struct A {} };
        let res = internal_derive_type_information(input);
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation {
                    type_information::TypeInformation::StructValue(
                        type_information::NamedTypeInformation::new("A".to_owned(),
                            type_information::Fields::new(Box::new([]))
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
        let res = internal_derive_type_information(input);
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation {
                    type_information::TypeInformation::TupleStructValue(
                        type_information::NamedTypeInformation::new("A".to_owned(), type_information::TupleTypes::new(Box::new([
                            type_information::TupleType::new( || u8::meta() ),
                            type_information::TupleType::new( || u16::meta() ),
                            type_information::TupleType::new( || u32::meta() )
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
            type_information::TypeInformation::SeqValue(type_information::SeqType::new( || u8::meta() ))
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
            type_information::TypeInformation::TupleValue(
                type_information::TupleTypes::new(Box::new([
                    type_information::TupleType::new( || u8::meta() ),
                    type_information::TupleType::new( || u8::meta() )
                ]))
            )
        };

        let res = array_to_meta(&input);

        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_array_struct() {
        let input = quote! { struct A {f: [u8; 3]} };
        let res = internal_derive_type_information(input);
        let expected_fields = quote! {
            type_information::Fields::new(Box::new([type_information::Field::new("f".to_owned(),
                || type_information::TypeInformation::TupleValue(
                    type_information::TupleTypes::new(Box::new([
                        type_information::TupleType::new( || u8::meta() ),
                        type_information::TupleType::new( || u8::meta() ),
                        type_information::TupleType::new( || u8::meta() )
                    ]))
                )
            )]))
        };
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation {
                    type_information::TypeInformation::StructValue(
                        type_information::NamedTypeInformation::new("A".to_owned(), #expected_fields)
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_derive_self_referencing_struct() {
        let input = quote! { struct A { f: &A } };
        let res = internal_derive_type_information(input);
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation {
                    type_information::TypeInformation::StructValue(
                        type_information::NamedTypeInformation::new("A".to_owned(),
                            type_information::Fields::new(Box::new([ type_information::Field::new("f".to_owned(), || A::meta()) ]))
                        )
                    )
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
