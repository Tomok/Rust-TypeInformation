extern crate type_information;
use type_information::generic_indexed_type_information;

//#[derive(Clone)]
pub type TypeInformationRef = TypeInfoOrRef;

//re-publish types from generic_indexed_type_information with TypeInformatioRef type filled
pub type Field = generic_indexed_type_information::Field<TypeInformationRef>;
pub type Fields = generic_indexed_type_information::Fields<TypeInformationRef>;
pub type EnumVariantType = generic_indexed_type_information::EnumVariantType<TypeInformationRef>;
pub type EnumVariant = generic_indexed_type_information::EnumVariant<TypeInformationRef>;
pub use generic_indexed_type_information::NamedTypeInformation;
pub use generic_indexed_type_information::UnitStructType;
pub type SeqType = generic_indexed_type_information::SeqType<TypeInformationRef>;
pub type TupleType = generic_indexed_type_information::TupleType<TypeInformationRef>;
pub type TupleTypes = generic_indexed_type_information::TupleTypes<TypeInformationRef>;
pub type EnumType = generic_indexed_type_information::EnumType<TypeInformationRef>;
pub type TypeInformation = generic_indexed_type_information::TypeInformation<TypeInformationRef>;

/// Path to another type, might be relative to current code position
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypePath(pub Box<[String]>);

impl TypePath {
    //convert to TypeInfoOrRef::Refernce
    pub fn as_ti_ref(self) -> TypeInfoOrRef {
        TypeInfoOrRef::Reference(self)
    }
}

impl From<&syn::TypePath> for TypePath {
    fn from(path: &syn::TypePath) -> Self {
        let mut segment_count = path.path.segments.len();
        let leading_colon = path.path.leading_colon.is_some();
        if leading_colon {
            segment_count += 1;
        }
        let mut path_segments: Vec<String> = Vec::with_capacity(segment_count);
        if leading_colon {
            path_segments.push("".to_owned());
        }
        path_segments.extend(path.path.segments.iter().map(|x| format!("{}", x.ident)));
        Self(path_segments.into_boxed_slice())
    }
}

/// Lifetime definition
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lifetime(pub String);

impl From<&syn::LifetimeDef> for Lifetime {
    fn from(lt: &syn::LifetimeDef) -> Self {
        Self(format!("{}", lt.lifetime))
    }
}

/// hardcoded TypeInformation or TypeInformationReference
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeInfoOrRef {
    TypeInfo(Box<TypeInformation>),
    Reference(TypePath), //Path to reference
}

impl Into<TypeInfoOrRef> for TypeInformation {
    fn into(self) -> TypeInfoOrRef {
        TypeInfoOrRef::TypeInfo(Box::new(self))
    }
}

///small helper to make writing tests easier, just use "std::vec::Vec".into_type_ref() to generate a type path to that
pub trait IntoTypeRef<'a> {
    fn into_type_ref(&'a self) -> TypeInfoOrRef;
}

impl<'a> IntoTypeRef<'a> for str {
    fn into_type_ref(&'a self) -> TypeInfoOrRef {
        let mut segments: Vec<String> = Vec::new();
        segments.extend(self.split("::").map(|x| x.to_owned()));
        TypeInfoOrRef::Reference(TypePath(segments.into_boxed_slice()))
    }
}

/// TypeInformation gathered from a (single) TokenStream
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExtractedTypeInformation {
    pub ident: String,
    pub lifetimes: Vec<Lifetime>,
    pub type_info: TypeInformation,
}
