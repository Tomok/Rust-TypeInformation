extern crate proc_macro2;

use super::extracted_type_information::*;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};

pub trait ToTokens {
    fn to_tokens(&self) -> TokenStream;
}

impl ToTokens for ExtractedTypeInformation {
    fn to_tokens(&self) -> TokenStream {
        let lifetimes = self
            .lifetimes
            .iter()
            .map(|x| syn::Lifetime::new((*x.0).into(), Span::call_site()));
        let has_lifetimes = !self.lifetimes.is_empty();
        let lifet = if has_lifetimes {
            quote! { <#(#lifetimes),*> }
        } else {
            quote! {}
        };
        let ident = format_ident!("{}", &self.ident);
        let ti = self.type_info.to_tokens();
        let res = quote! {
            impl #lifet Meta for #ident #lifet {
                fn meta() -> type_information::TypeInformation<'static> {
                    #ti
                }
            }
        };
        res
    }
}

impl ToTokens for TypeInformation {
    fn to_tokens(&self) -> TokenStream {
        match self {
            TypeInformation::BoolValue() => {
                quote! { bool::meta() }
            }
            TypeInformation::I8Value() => {
                quote! { i8::meta() }
            }
            TypeInformation::I16Value() => {
                quote! { i16::meta() }
            }
            TypeInformation::I32Value() => {
                quote! { i32::meta() }
            }
            TypeInformation::I64Value() => {
                quote! { i64::meta() }
            }
            TypeInformation::I128Value() => {
                quote! { i128::meta() }
            }
            TypeInformation::U8Value() => {
                quote! { u8::meta() }
            }
            TypeInformation::U16Value() => {
                quote! { u16::meta() }
            }
            TypeInformation::U32Value() => {
                quote! { u32::meta() }
            }
            TypeInformation::U64Value() => {
                quote! { u64::meta() }
            }
            TypeInformation::U128Value() => {
                quote! { u128::meta() }
            }
            TypeInformation::F32Value() => {
                quote! { f32::meta() }
            }
            TypeInformation::F64Value() => {
                quote! { f64::meta() }
            }
            TypeInformation::CharValue() => {
                quote! { char::meta() }
            }
            TypeInformation::StringValue() => {
                quote! { string::meta() }
            }
            TypeInformation::ByteArray() => {
                quote! { [u8]::meta() }
            }
            TypeInformation::OptionValue { inner_type } => {
                let inner = inner_type.to_tokens();
                quote! { type_information::OptionValue { #inner }}
            }
            TypeInformation::UnitStructValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! { TypeInformation::UnitStructValue ( #inner ) }
            }
            TypeInformation::SeqValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! {TypeInformation::SeqValue( #inner ) }
            }
            TypeInformation::TupleValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! {TypeInformation::TupleValue( #inner ) }
            }
            TypeInformation::TupleStructValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! {TypeInformation::TupleStructValue( #inner ) }
            }
            TypeInformation::StructValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! {TypeInformation::StructValue( #inner ) }
            }
            TypeInformation::EnumValue(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! {TypeInformation::EnumValue( #inner ) }
            }
        }
    }
}

impl ToTokens for TypeInfoOrRef {
    fn to_tokens(&self) -> TokenStream {
        // since TypeInfoOrRef is the TypeInformationRef and that is a
        // function () -> TypeInformation this needs function creates functions
        // returning that function
        match self {
            TypeInfoOrRef::TypeInfo(ti) => {
                let inner = ti.to_tokens();
                quote! { || #inner }
            }
            TypeInfoOrRef::Reference(r) => {
                let p = r.to_tokens();
                quote! { || #p::meta() }
            }
        }
    }
}

impl ToTokens for TypePath {
    fn to_tokens(&self) -> TokenStream {
        let tp = self.0.iter().map(|x| format_ident!("{}", x));
        quote! { #(#tp)::* }
    }
}

impl<I: ToTokens> ToTokens for NamedTypeInformation<I> {
    fn to_tokens(&self) -> TokenStream {
        let name = &self.name;
        let ti = self.type_info.to_tokens();
        quote! { type_information::NamedTypeInformation::new( #name, #ti ) }
    }
}

impl<I: ToTokens> ToTokens for Box<[I]> {
    fn to_tokens(&self) -> TokenStream {
        let iter = self.iter().map(|x| x.to_tokens());
        let array = quote! { [#(#iter),*] };
        quote! { Box::new( #array ) }
    }
}

impl ToTokens for () {
    fn to_tokens(&self) -> TokenStream {
        quote! { () }
    }
}

impl ToTokens for SeqType {
    fn to_tokens(&self) -> TokenStream {
        let inner = self.inner_type.to_tokens();
        quote! { type_information::SeqType::new( #inner ) }
    }
}

impl ToTokens for TupleType {
    fn to_tokens(&self) -> TokenStream {
        let inner = self.inner_type.to_tokens();
        quote! { type_information::TupleType::new( #inner ) }
    }
}

impl ToTokens for TupleTypes {
    fn to_tokens(&self) -> TokenStream {
        let inner = self.inner_types.to_tokens();
        quote! { type_information::TupleTypes::new( #inner ) }
    }
}

impl ToTokens for Fields {
    fn to_tokens(&self) -> TokenStream {
        let inner = self.fields.to_tokens();
        quote! { type_information::Fields::new( #inner )}
    }
}

impl ToTokens for Field {
    fn to_tokens(&self) -> TokenStream {
        let name = &self.name;
        let inner = self.inner_type.to_tokens();
        quote! { type_information::Field::new( #name, #inner )}
    }
}

impl ToTokens for EnumType {
    fn to_tokens(&self) -> TokenStream {
        let variants = self.possible_variants.to_tokens();
        quote! { type_information::EnumType::new( #variants ) }
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self) -> TokenStream {
        let name = &self.name;
        let inner = self.inner_type.to_tokens();
        quote! { type_information::EnumVariant::new( #name, #inner ) }
    }
}

impl ToTokens for EnumVariantType {
    fn to_tokens(&self) -> TokenStream {
        match self {
            EnumVariantType::UnitVariant() => {
                quote! { type_information::EnumVariantType::UnitVariant() }
            }
            EnumVariantType::TupleVariant(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! { type_information::EnumVariantType::TupleVariant( #inner ) }
            }
            EnumVariantType::StructVariant(inner_type) => {
                let inner = inner_type.to_tokens();
                quote! { type_information::EnumVariantType::StructVariant( #inner ) }
            }
        }
    }
}
#[cfg(test)]
mod test {
    use super::*;

    use super::IntoTypeRef;
    use quote::quote;

    #[test]
    fn test_tokenize_unit_struct() {
        let input = UnitStructType::new("A".to_owned(), ());
        let expectation = quote! { type_information::NamedTypeInformation::new("A", ()) };
        let res = input.to_tokens();
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_tokenize_field() {
        let input = Field::new("f".to_owned(), "u32".into_type_ref());
        let expectation = quote! { type_information::Field::new("f", || u32::meta()) };

        let res = input.to_tokens();
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_tokenize_simple_extracted_type_information() {
        let input = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![],
            type_info: TypeInformation::BoolValue(),
        };
        let expectation = quote! {
            impl Meta for A {
                fn meta() -> type_information::TypeInformation<'static> {
                    bool::meta()
                }
            }
        };
        let res = input.to_tokens();
        assert_eq!(res.to_string(), expectation.to_string());
    }

    #[test]
    fn test_tokenize_extracted_type_information_with_lifetime() {
        let input = ExtractedTypeInformation {
            ident: "A".to_owned(),
            lifetimes: vec![Lifetime("'a".to_owned())],
            type_info: TypeInformation::BoolValue(),
        };
        let expectation = quote! {
            impl<'a> Meta for A<'a> {
                fn meta() -> type_information::TypeInformation<'static> {
                    bool::meta()
                }
            }
        };
        let res = input.to_tokens();
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
