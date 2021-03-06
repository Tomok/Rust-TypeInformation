//TODO: is there a way to make this private and still use it in the _derive module??
pub mod generic_indexed_type_information;

/// serde serializer support
#[cfg(feature = "serde_ser")]
mod number_indexed_type_information;
#[cfg(feature = "serde_ser")]
mod serde_ser;

mod implementations;
pub use implementations::*;

type TypeInformationRef<'a> = fn() -> TypeInformation<'a>;

type TIBox<T> = Box<T>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Field inside a struct
pub struct Field<'a> {
    name: &'a str,
    inner_type: TypeInformationRef<'a>,
}

impl<'a> Field<'a> {
    pub fn new(name: &'a str, inner_type: TypeInformationRef<'a>) -> Self {
        Self { name, inner_type }
    }

    pub fn name(&'a self) -> &'a str {
        self.name
    }

    pub fn inner_type(&'a self) -> TypeInformation<'a> {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Fields<'a> {
    fields: Box<[Field<'a>]>,
}

impl<'a> Fields<'a> {
    pub const fn new(fields: Box<[Field<'a>]>) -> Self {
        //TODO: maybe check for duplicate names?
        Self { fields }
    }

    pub fn fields(&'a self) -> &'a [Field<'a>] {
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
pub enum EnumVariantType<'a> {
    /// Enum variant without any fields.
    UnitVariant(),
    /// Enum variant for tuples.
    TupleVariant(TupleTypes<'a>),
    /// Enum variant for variants containing named fields.
    StructVariant(Fields<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Meta Data for an enum variant
pub struct EnumVariant<'a> {
    name: &'a str,
    inner_type: EnumVariantType<'a>,
}

impl<'a> EnumVariant<'a> {
    pub const fn new(name: &'a str, inner_type: EnumVariantType<'a>) -> Self {
        Self { name, inner_type }
    }

    /// the name of the enums variant
    pub fn name(&'a self) -> &'a str {
        self.name
    }

    /// the possible kind and if used the fields inside the enum variant.
    pub fn inner_type(&'a self) -> &EnumVariantType<'a> {
        &self.inner_type
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct NamedTypeInformation<'a, I: Sized> {
    name: &'a str,
    type_info: I,
}

impl<'a, I: Sized> NamedTypeInformation<'a, I> {
    pub const fn new(name: &'a str, type_info: I) -> Self {
        Self { name, type_info }
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn type_info(&self) -> &I {
        &self.type_info
    }
}

pub type UnitStructType<'a> = NamedTypeInformation<'a, ()>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SeqType<'a> {
    inner_type: TypeInformationRef<'a>,
}

impl<'a> SeqType<'a> {
    pub fn new(inner_type: TypeInformationRef<'a>) -> Self {
        Self { inner_type }
    }

    pub fn inner_type(&'a self) -> TypeInformation<'a> {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TupleType<'a> {
    inner_type: TypeInformationRef<'a>,
}

impl<'a> TupleType<'a> {
    pub fn new(inner_type: TypeInformationRef<'a>) -> Self {
        Self { inner_type }
    }

    pub fn inner_type(&self) -> TypeInformation {
        (self.inner_type)()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TupleTypes<'a> {
    inner_types: TIBox<[TupleType<'a>]>,
}

impl<'a> TupleTypes<'a> {
    pub fn new(inner_types: TIBox<[TupleType<'a>]>) -> Self {
        Self { inner_types }
    }

    //TODO: Decide on return type...
    pub fn inner_types(&'a self) -> &'a [TupleType<'a>] {
        self.inner_types.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct EnumType<'a> {
    possible_variants: Box<[EnumVariant<'a>]>,
}

impl<'a> EnumType<'a> {
    pub const fn new(possible_variants: Box<[EnumVariant<'a>]>) -> Self {
        Self { possible_variants }
    }

    pub fn possible_variants(&'a self) -> &'a [EnumVariant<'a>] {
        self.possible_variants.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// All possible kinds of TypeInformation delivered by `meta` function
/// or contained in the structs returned by it.
pub enum TypeInformation<'a> {
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
        inner_type: TypeInformationRef<'a>,
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
    UnitStructValue(UnitStructType<'a>),

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
    SeqValue(SeqType<'a>),

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue(TupleTypes<'a>),
    TupleStructValue(NamedTypeInformation<'a, TupleTypes<'a>>),

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue(NamedTypeInformation<'a, Fields<'a>>),

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue(NamedTypeInformation<'a, EnumType<'a>>),
}

/// Implement this trait or derive `Meta` to add a
/// `meta` function to your class providing meta data about it.
pub trait Meta {
    /// Provide Meta Data for this struct.
    fn meta() -> TypeInformation<'static>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use lazy_static::lazy_static;
    use TypeInformation::*;

    mod test_structs_can_reference_themselves {
        use super::*;

        fn get_test_struct() -> TypeInformation<'static> {
            LOOPED_TEST_STRUCT.clone()
        }

        lazy_static! {
            static ref FIELDS: Fields<'static> = Fields::new(Box::new([Field {
                name: "a",
                inner_type: get_test_struct,
            }]));
            static ref LOOPED_TEST_STRUCT: TypeInformation<'static> =
                TypeInformation::StructValue(NamedTypeInformation::new(&"A", FIELDS.clone()));
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

        fn get_test_struct() -> TypeInformation<'static> {
            LOOPED_TEST_STRUCT.clone()
        }

        lazy_static! {
            static ref FIELDS: Fields<'static> = Fields::new(Box::new([Field {
                name: "a",
                inner_type: get_test_struct,
            }]));
            static ref LOOPED_TEST_STRUCT: TypeInformation<'static> =
                TypeInformation::StructValue(NamedTypeInformation::new(&"A", FIELDS.clone()));
        }

        fn get_test_struct2() -> TypeInformation<'static> {
            LOOPED_TEST_STRUCT2.clone()
        }

        lazy_static! {
            static ref FIELDS2: Fields<'static> = Fields::new(Box::new([Field {
                name: "a",
                inner_type: get_test_struct2,
            }]));
            static ref LOOPED_TEST_STRUCT2: TypeInformation<'static> =
                TypeInformation::StructValue(NamedTypeInformation::new(&"A", FIELDS2.clone()));
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
