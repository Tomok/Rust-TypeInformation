extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;

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
    item
}

#[cfg(test)]
mod test {
    use super::*;

    use quote::quote;

    #[test]
    fn test_derive_serde_empty_struct() {
        let input = quote! { struct A{} };
        let res = internal_derive_serde_meta(input);
        let expectation = quote! {
            use serde_meta::SerdeMeta;
            impl SerdeMeta for A {
                use serde_meta;
                fn meta() -> serde_meta::TypeInformation {
                    TypeInformation::TupleStructValue {
                        name: "A",
                        inner_types: []
                    }
                }
            }
        };
        assert_eq!(res.to_string(), expectation.to_string());
    }
}
