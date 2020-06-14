/// serde serializer support
#[cfg(feature = "serde_ser")]
mod number_indexed_type_information;
#[cfg(feature = "serde_ser")]
mod serde_ser;

mod implementations;
pub use implementations::*;

type TypeInformationRef = fn() -> TypeInformation;

type TIBox<T> = Box<T>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Field inside a struct
pub struct Field {
    name: String,
    inner_type: TypeInformationRef,
}

impl Field {
    pub fn new(name: String, inner_type: TypeInformationRef) -> Self {
        Self { name, inner_type }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        &self.name
    }

    pub fn inner_type(&self) -> TypeInformation {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Fields {
    fields: Box<[Field]>,
}

impl Fields {
    pub const fn new(fields: Box<[Field]>) -> Self {
        //TODO: maybe check for duplicate names?
        Self { fields }
    }

    pub fn fields(& self) -> &[Field] {
        self.fields.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Possible types contained in a enum.
///
/// If it contains no fields, it will be a `UnitVariant`.
/// For tuple enums variants like A in:
/// ```
/// enum Foo {
///   A(u8,u8),
/// }
/// ```
/// A `TupleVariant` is used.
///
/// For struct enum variants like B in:
/// ```
/// enum Bar {
///   B{ field: u8 },
/// }
/// ```
/// A `StructVariant` is used.
pub enum EnumVariantType {
    /// Enum variant without any fields.
    UnitVariant(),
    /// Enum variant for tuples.
    TupleVariant(TupleTypes),
    /// Enum variant for variants containing named fields.
    StructVariant(Fields),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Meta Data for an enum variant
pub struct EnumVariant {
    name: String,
    inner_type: EnumVariantType,
}

impl EnumVariant {
    pub const fn new(name: String, inner_type: EnumVariantType) -> Self {
        Self { name, inner_type }
    }

    /// the name of the enums variant
    pub fn name(&self) -> &str {
        &self.name
    }

    /// the possible kind and if used the fields inside the enum variant.
    pub fn inner_type(&self) -> &EnumVariantType {
        &self.inner_type
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct NamedTypeInformation<I: Sized> {
    name: String,
    type_info: I,
}

impl<I: Sized> NamedTypeInformation<I> {
    pub const fn new(name: String, type_info: I) -> Self {
        Self { name, type_info }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_info(&self) -> &I {
        &self.type_info
    }
}

pub type UnitStructType = NamedTypeInformation<()>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SeqType {
    inner_type: TypeInformationRef,
}

impl SeqType {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }

    pub fn inner_type(& self) -> TypeInformation {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TupleType {
    inner_type: TypeInformationRef,
}

impl TupleType {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }

    pub fn inner_type<'a>(&self) -> TypeInformation {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TupleTypes {
    inner_types: TIBox<[TupleType]>,
}

impl TupleTypes {
    pub fn new(inner_types: TIBox<[TupleType]>) -> Self {
        Self { inner_types }
    }

    //TODO: Decide on return type...
    pub fn inner_types<'a>(&'a self) -> &'a [TupleType] {
        self.inner_types.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct EnumType {
    possible_variants: Box<[EnumVariant]>,
}

impl EnumType {
    pub const fn new(possible_variants: Box<[EnumVariant]>) -> Self {
        Self { possible_variants }
    }

    pub fn possible_variants(& self) -> &[EnumVariant] {
        self.possible_variants.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// All possible kinds of TypeInformation delivered by `meta` function
/// or contained in the structs returned by it.
pub enum TypeInformation {
    /// used for `bool` fields
    BoolValue(),

    /// used for `i8` fields
    I8Value(),
    /// used for `i16` fields
    I16Value(),
    /// used for `i32` fields
    I32Value(),
    /// used for `i64` fields
    I64Value(),
    /// used for `i128` fields
    I128Value(),

    /// used for `u8` fields
    U8Value(),
    /// used for `u16` fields
    U16Value(),
    /// used for `u32` fields
    U32Value(),
    /// used for `u64` fields
    U64Value(),
    /// used for `u128` fields
    U128Value(),

    /// used for `f32` fields
    F32Value(),
    /// used for `f64` fields
    F64Value(),

    /// used for `char` fields
    CharValue(),

    /// used for `str`
    StringValue(),
    // TODO should this remain, or is it just there because serde has it?
    ByteArray(),

    // TODO should this remain, or is it just there because serde has it?
    OptionValue {
        inner_type: TypeInformationRef,
    },

    // TODO: Unused -> Remove??
    // used for empty types, i.e. `()`
    // UnitValue(),
    /// Used for Unit type fields inside a struct
    ///
    /// e.g. `f` in:
    /// ```
    /// struct Foo {
    ///   f: (),
    /// }
    /// ```
    UnitStructValue(UnitStructType),

    // TODO: Unused -> Remove??
    // NewTypeStructValue {
    //     inner_type: &'static TypeInformation,
    // },
    // TODO: Unused -> Remove??
    // NewTypeVariantValue {
    //     inner_type: &'static TypeInformation,
    // },
    /// Used for all kinds of dynamically sized sequences,
    /// e.g. `[u8]`
    SeqValue(SeqType),

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue(TupleTypes),
    TupleStructValue(NamedTypeInformation<TupleTypes>),

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue(NamedTypeInformation<Fields>),

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue(NamedTypeInformation<EnumType>),
}

/// Implement this trait or derive `Meta` to add a
/// `meta` function to your class providing meta data about it.
pub trait Meta {
    /// Provide Meta Data for this struct.
    fn meta() -> TypeInformation;
}

#[cfg(test)]
mod tests {
    use super::*;
    use lazy_static::lazy_static;
    use TypeInformation::*;

    mod test_structs_can_reference_themselves {
        use super::*;

        fn get_test_struct() -> TypeInformation {
            LOOPED_TEST_STRUCT.clone()
        }

        lazy_static! {
            static ref FIELDS: Fields = Fields::new(Box::new([Field {
                name: "a".to_owned(),
                inner_type: get_test_struct,
            }]));
            static ref LOOPED_TEST_STRUCT: TypeInformation =
                TypeInformation::StructValue(NamedTypeInformation::new("A".to_owned(), FIELDS.clone()));
        }

        #[test]
        fn test_structs_can_reference_themselves() {
            if let StructValue(NamedTypeInformation {
                name: _,
                type_info: fields,
            }) = get_test_struct()
            {
                let test_struct = get_test_struct();
                //make sure test_struct and the one in the field of the let statement above are equal
                assert_eq!(&test_struct, &fields.fields()[0].inner_type());
            } else {
                panic!("TEST_STRUCT did not contain a TEST_STRUCT as field");
            }
        }
    }

    /// tests that two structs with identical structure, but not identical themselves are
    /// not determined to be equal
    mod structs_with_identical_structure {
        use super::*;
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        fn get_test_struct() -> TypeInformation {
            LOOPED_TEST_STRUCT.clone()
        }

        lazy_static! {
            static ref FIELDS: Fields = Fields::new(Box::new([Field {
                name: "a".to_owned(),
                inner_type: get_test_struct,
            }]));
            static ref LOOPED_TEST_STRUCT: TypeInformation =
                TypeInformation::StructValue(NamedTypeInformation::new("A".to_owned(), FIELDS.clone()));
        }

        fn get_test_struct2() -> TypeInformation {
            LOOPED_TEST_STRUCT2.clone()
        }

        lazy_static! {
            static ref FIELDS2: Fields = Fields::new(Box::new([Field {
                name: "a".to_owned(),
                inner_type: get_test_struct2,
            }]));
            static ref LOOPED_TEST_STRUCT2: TypeInformation =
                TypeInformation::StructValue(NamedTypeInformation::new("A".to_owned(), FIELDS2.clone()));
        }
        #[test]
        fn test_equal() {
            assert_ne!(get_test_struct(), get_test_struct2());
        }

        fn calculate_hash<T: Hash>(t: &T) -> u64 {
            let mut s = DefaultHasher::new();
            t.hash(&mut s);
            s.finish()
        }

        #[test]
        fn test_hash() {
            assert_ne!(
                calculate_hash(&get_test_struct()),
                calculate_hash(&get_test_struct2())
            );
        }
    }
}
