use super::extracted_type_information::*;

pub fn extract_type_information(item: proc_macro2::TokenStream) -> ExtractedTypeInformation {
    let input: syn::DeriveInput = syn::parse2(item).unwrap();

    let ident = format!("{}", input.ident);

    let lifetimes = input.generics.lifetimes().map(|x| x.into()).collect();

    let type_info = match input.data {
        syn::Data::Struct(data_struct) => derive_struct(&input.ident, data_struct),
        syn::Data::Enum(data_enum) => derive_enum(&input.ident, data_enum),
        _ => panic!(
            "internal_derive_type_information: Not implemented for {:#?}",
            input.data
        ),
    };
    ExtractedTypeInformation {
        ident,
        lifetimes,
        type_info,
    }
}

/// converts an enum token stream into
/// the corresponding `TypeInformation::EnumValue` meta data object
fn derive_enum(ident: &syn::Ident, data_enum: syn::DataEnum) -> TypeInformation {
    let strident = format!("{}", ident);
    let variants = derive_enum_variants(data_enum.variants);
    TypeInformation::EnumValue(NamedTypeInformation::new(strident, EnumType::new(variants)))
}

/// converts the variants in an enum token stream into
/// the corresponding array of EnumVariantTypes for a meta data object
fn derive_enum_variants(
    variants: syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> Box<[EnumVariant]> {
    let variants_iter = variants.iter().map(|v| derive_enum_variant(v));
    let mut variants = Vec::with_capacity(variants.len());
    variants.extend(variants_iter);
    variants.into_boxed_slice()
}

/// converts an enum variant token stream into
/// the corresponding meta data object
/// Which is a `EnumVariant`
fn derive_enum_variant(variant: &syn::Variant) -> EnumVariant {
    let strident = format!("{}", variant.ident);
    let inner_type = match &variant.fields {
        syn::Fields::Named(f) => {
            let fields = derive_fields_named(f);
            EnumVariantType::StructVariant(fields)
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            EnumVariantType::TupleVariant(TupleTypes::new(fields))
        }
        syn::Fields::Unit => EnumVariantType::UnitVariant(),
    };

    EnumVariant::new(strident, inner_type)
}

/// Converts the tokens of a named struct into
/// the corresponding meta object,
/// which is one of:
/// * TypeInformation::StructValue
/// * TypeInformation::TupleStructValue
/// * TypeInformation::UnitStructValue
fn derive_struct(ident: &syn::Ident, data_struct: syn::DataStruct) -> TypeInformation {
    let strident = format!("{}", ident);
    match &data_struct.fields {
        syn::Fields::Named(f) => {
            let fields = derive_fields_named(f);
            let res = TypeInformation::StructValue(NamedTypeInformation::new(strident, fields));
            res
        }
        syn::Fields::Unnamed(f) => {
            let fields = derive_fields_unnamed(f);
            let res = TypeInformation::TupleStructValue(NamedTypeInformation::new(
                strident,
                TupleTypes::new(fields),
            ));
            res
        }
        syn::Fields::Unit => TypeInformation::UnitStructValue(UnitStructType::new(strident, ())),
    }
}

/// Converts the tokens describing named fields of a struct
/// (or struct variant of an enum), into
/// the corresponding meta data, i.e. a Fields object
fn derive_fields_named(fields: &syn::FieldsNamed) -> Fields {
    let fields_iter = fields.named.iter().map(|f| derive_named_field(f));
    let mut fields = std::vec::Vec::with_capacity(fields.named.len());
    fields.extend(fields_iter);
    Fields::new(fields.into_boxed_slice())
}

/// Converts the tokens describing named fields of a tuple
/// (or tuple variant of an enum), into
/// the corresponding meta data, i.e. a reference to an array of references
/// to the corresponding meta data type objects
fn derive_fields_unnamed(fields: &syn::FieldsUnnamed) -> Box<[TupleType]> {
    let fields_iter = fields
        .unnamed
        .iter()
        .map(|f| derive_unnamed_tuple_element(&f.ty));

    let mut res_vec = Vec::with_capacity(fields.unnamed.len());
    res_vec.extend(fields_iter);
    res_vec.into_boxed_slice()
}

/// takes a given `syn::Path` and returns the corresponding
/// TypeInfoOrRef::Reference describing the path to the referenced object.
/// Does not check, if a meta object exists for that path!
fn path_to_meta(path: &syn::TypePath) -> TypeInfoOrRef {
    TypeInfoOrRef::Reference(path.into())
}

/// takes a given `syn::TypeArray` and returns the corresponding
/// `type_information::TypeInformation::TupleValue`
/// representing the array.
fn array_to_meta(a: &syn::TypeArray) -> TypeInformation {
    if let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Int(lit),
        ..
    }) = &a.len
    {
        let size = lit.base10_parse().expect("Could not parse array size");
        let t = type_to_meta(&*a.elem);
        let tt = TupleType::new(t);
        //quote! { type_information::TupleType::new( || #t ) };
        let mut repeated_vec = std::vec::Vec::with_capacity(size);
        repeated_vec.extend(std::iter::repeat(tt).take(size));
        assert_eq!(size, repeated_vec.len());
        let repeated = repeated_vec.into_boxed_slice();
        TypeInformation::TupleValue(TupleTypes::new(repeated))
    } else {
        panic!(
            "Only integer literals are supported as array length right now, but found a {:#?}",
            a.len
        );
    }
}

/// takes a `syn::Type` and returns a corresponding TypeInfoOrRef
fn type_to_meta(ty: &syn::Type) -> TypeInfoOrRef {
    match ty {
        syn::Type::Path(p) => path_to_meta(&p),
        syn::Type::Array(a) => array_to_meta(&a).into(),
        syn::Type::Reference(syn::TypeReference { elem: t, .. }) => type_to_meta(&*t),
        syn::Type::Slice(syn::TypeSlice { elem: t, .. }) => {
            let inner = type_to_meta(&*t);
            let seq_type = SeqType::new(inner);
            TypeInformation::SeqValue(seq_type).into()
        }
        _ => panic!("type_to_meta: Not implemented for {:#?}", ty),
    }
}

/// generates a `TokenStream` that creates the meta data for a given named field.
fn derive_named_field(field: &syn::Field) -> Field {
    let x = field.ident.clone(); //TODO: is clone realy the only option here?
    let ident = format!("{}", x.unwrap());
    let type_info = type_to_meta(&field.ty);
    Field::new(ident, type_info)
}

/// generates a `TokenStream` that creates the meta data for a given tuple element.
fn derive_unnamed_tuple_element(ty: &syn::Type) -> TupleType {
    let type_info_meta = type_to_meta(ty);
    TupleType::new(type_info_meta)
}

#[cfg(test)]
mod test {
    use super::*;

    use super::IntoTypeRef;
    use quote::quote;
    use syn::parse_quote;

    #[test]
    fn test_derive_serde_unit_struct() {
        let input = quote! { struct A; };
        let res = extract_type_information(input);
        let expectation = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![],
            type_info: TypeInformation::UnitStructValue(UnitStructType::new("A".to_owned(), ())),
        };
        assert_eq!(res, expectation);
    }

    #[test]
    fn test_derive_serde_empty_struct() {
        let input = quote! { struct A {} };
        let res = extract_type_information(input);
        let expectation = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![],
            type_info: TypeInformation::StructValue(NamedTypeInformation::new(
                "A".to_owned(),
                Fields::new(Box::new([])),
            )),
        };
        assert_eq!(res, expectation);
    }

    #[test]
    fn test_derive_tuple_struct() {
        let input = quote! {struct A(u8, u16, u32); };
        let res = extract_type_information(input);
        let expectation = TypeInformation::TupleStructValue(NamedTypeInformation::new(
            "A".to_owned(),
            TupleTypes::new(Box::new([
                TupleType::new("u8".into_type_ref()),
                TupleType::new("u16".into_type_ref()),
                TupleType::new("u32".into_type_ref()),
            ])),
        ));
        assert_eq!(res.type_info, expectation);
    }

    #[test]
    fn test_type_to_meta_slice() {
        let input = parse_quote! {[u8]};
        let expectation: TypeInfoOrRef =
            TypeInformation::SeqValue(SeqType::new("u8".into_type_ref())).into();

        let res = type_to_meta(&input);

        assert_eq!(res, expectation);
    }

    #[test]
    fn test_array_to_meta() {
        // as parse is not implemented for TypeArray, parse an ExprType and get the TypeArray from there
        let typedef: syn::ExprType = parse_quote! { a: [u8; 2]};
        let input = match *typedef.ty {
            syn::Type::Array(a) => a,
            _ => panic!("typedef was messed up and did not return an Array as type..."),
        };

        let expectation = TypeInformation::TupleValue(TupleTypes::new(Box::new([
            TupleType::new("u8".into_type_ref()),
            TupleType::new("u8".into_type_ref()),
        ])));

        let res = array_to_meta(&input);

        assert_eq!(res, expectation);
    }

    #[test]
    fn test_derive_array_struct() {
        let input = quote! { struct A {f: [u8; 3]} };
        let res = extract_type_information(input);
        let expected_fields = Fields::new(Box::new([Field::new(
            "f".to_owned(),
            TypeInformation::TupleValue(TupleTypes::new(Box::new([
                TupleType::new("u8".into_type_ref()),
                TupleType::new("u8".into_type_ref()),
                TupleType::new("u8".into_type_ref()),
            ])))
            .into(),
        )]));
        let expectation = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![],
            type_info: TypeInformation::StructValue(NamedTypeInformation::new(
                "A".to_owned(),
                expected_fields,
            )),
        };
        assert_eq!(res, expectation);
    }

    #[test]
    fn test_derive_self_referencing_struct() {
        let input = quote! { struct A { f: &A } };
        let res = extract_type_information(input);
        let expectation = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![],
            type_info: TypeInformation::StructValue(NamedTypeInformation::new(
                "A".to_owned(),
                Fields::new(Box::new([Field::new("f".to_owned(), "A".into_type_ref())])),
            )),
        };
        assert_eq!(res, expectation);
    }
}
