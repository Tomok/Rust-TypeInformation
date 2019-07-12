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
        syn::Data::Struct(dataStruct) => derive_struct(&ident, dataStruct),
        _ => panic!("Not implemented"),
    };
    quote! {
        use serde_meta;
        impl serde_meta::SerdeMeta for #ident {
            fn meta() -> serde_meta::TypeInformation {
                #gen
            }
        }
    }
}

fn derive_struct(ident: &syn::Ident, dataStruct: syn::DataStruct) -> proc_macro2::TokenStream {
    let def = quote! { "abc"};
    let strident = format!("{}", ident);
    match dataStruct.fields {
        syn::Fields::Named(f) => panic!("Not implemented for {:#?}", f),
        syn::Fields::Unnamed(f) => panic!("Not implemented"),
        syn::Fields::Unit => {
            let res = quote! {
                TypeInformation::UnitStructValue{ name: #strident }
            };
            res
        }
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
            use serde_meta;
            impl serde_meta::SerdeMeta for A {
                fn meta() -> serde_meta::TypeInformation {
                    TypeInformation::UnitStructValue {
                        name: "A"
                    }
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
