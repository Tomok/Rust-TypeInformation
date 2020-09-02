extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;

mod extracted_type_information;
mod type_info_extraction;
mod type_info_to_tokenstream;
use type_info_to_tokenstream::ToTokens;

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
    let extracted_info = type_info_extraction::extract_type_information(item);
    extracted_info.to_tokens()
}
