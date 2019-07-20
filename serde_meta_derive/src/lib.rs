extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;
use quote::quote;
use syn;

extern crate serde_meta;

#[proc_macro_derive(SerdeMeta)]
pub fn derive_serde_meta(_item: TokenStream) -> TokenStream {
    internal_derive_serde_meta(_item.into()).into()
}

///using a separate function with proc_macro2::TokenStreams to implement the
///logic, to make it unit testable, since proc_macro can not be used in the
///context of unit tests.
fn internal_derive_serde_meta(item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    //TODO: Implement something usefull here
    let input: syn::DeriveInput = syn::parse2(item).unwrap();

    println!("{}: {:#?}", input.ident, input.data);
    let ident = input.ident;
    println!("#ident = {}", quote! { #ident });
    let gen = match input.data {
        syn::Data::Struct(data_struct) => derive_struct(&ident, data_struct),
        _ => panic!("Not implemented"),
    };
    quote! {
        impl serde_meta::SerdeMeta for #ident {
            fn meta() -> serde_meta::TypeInformation {
                #gen
            }
        }
    }
}

fn derive_struct(ident: &syn::Ident, data_struct: syn::DataStruct) -> proc_macro2::TokenStream {
    let def = quote! { "abc" };
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
        syn::Fields::Unnamed(f) => panic!("Not implemented"),
        syn::Fields::Unit => {
            let res = quote! {
                serde_meta::TypeInformation::UnitStructValue{ name: #strident }
            };
            res
        }
    }
}

fn derive_fields_named(fields: syn::FieldsNamed) -> proc_macro2::TokenStream {
    let fields_iter = fields.named.iter().map(|f| {
        let ident = &f.ident;
        let type_info = 1; //TODO
        let map_res = quote! {
            Field {
                name: #ident,
                inner_type: #type_info,
            }
        };
        map_res
    });

    quote! {
        &[#(#fields_iter),*]
    }
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
            impl serde_meta::SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation {
                    serde_meta::TypeInformation::UnitStructValue {
                        name: "A"
                    }
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
            impl serde_meta::SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation {
                    serde_meta::TypeInformation::StructValue {
                        name: "A",
                        fields: &[]
                    }
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
