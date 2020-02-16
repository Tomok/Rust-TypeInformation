/// serde serializer support
#[cfg(feature = "serde_ser")]
mod serde_ser;

#[derive(Debug, PartialEq, Eq)]
/// Field inside a struct
pub struct Field<'a> {
    name: &'a str,
    inner_type: &'a TypeInformation<'a>,
}

impl<'a> Field<'a> {
    pub const fn new(name: &'a str, inner_type: &'a TypeInformation<'a>) -> Self {
        Self { name, inner_type }
    }

    pub fn name(&'a self) -> &'a str {
        self.name
    }

    pub fn inner_type(&'a self) -> &'a TypeInformation<'a> {
        self.inner_type
    }
}

#[derive(Debug, PartialEq, Eq)]
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
    TupleVariant {
        fields: &'a [&'a TypeInformation<'a>],
    },
    /// Enum variant for variants containing named fields.
    StructVariant { fields: &'a [Field<'a>] },
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct SeqType<'a> {
    inner_type: &'a TypeInformation<'a>,
}

impl<'a> SeqType<'a> {
    pub const fn new(inner_type: &'a TypeInformation<'a>) -> Self {
        Self { inner_type }
    }

    pub fn inner_type(&'a self) -> &'a TypeInformation<'a> {
        self.inner_type
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleTypes<'a> {
    inner_types: &'a [&'a TypeInformation<'a>],
}

impl<'a> TupleTypes<'a> {
    pub const fn new(inner_types: &'a [&'a TypeInformation<'a>]) -> Self {
        Self { inner_types }
    }

    pub fn inner_types(&'a self) -> &'a [&'a TypeInformation<'a>] {
        self.inner_types
    }
}

#[derive(Debug, PartialEq, Eq)]
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
        inner_type: &'a TypeInformation<'a>,
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
    TupleStructValue {
        name: &'a str,
        inner_types: &'a [&'a TypeInformation<'a>],
    },

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue {
        name: &'a str,
        fields: &'a [Field<'a>],
    },

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue {
        name: &'a str,
        possible_variants: &'a [EnumVariant<'a>],
    },
}

/// Implement this trait or derive `SerdeMeta` to add a
/// `meta` function to your class providing meta data about it.
pub trait SerdeMeta {
    /// Provide Meta Data for this struct.
    fn meta() -> &'static TypeInformation<'static>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use TypeInformation::*;

    mod test_structs_can_reference_themselves {
        use super::*;

        use std::ptr;

        static FIELDS: &[Field] = &[Field {
            name: "a",
            inner_type: &TEST_STRUCT,
        }];
        static TEST_STRUCT: TypeInformation = StructValue {
            name: &"A",
            fields: FIELDS,
        };

        #[test]
        fn test_structs_can_reference_themselves() {
            if let StructValue {
                name: _,
                fields: [field_struct],
            } = TEST_STRUCT
            {
                //make sure it is the same instance
                assert!(ptr::eq(&TEST_STRUCT, field_struct.inner_type));
            } else {
                panic!("TEST_STRUCT did not contain a TEST_STRUCT as field");
            }
        }
    }
}
