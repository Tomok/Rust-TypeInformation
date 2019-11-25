#[derive(Debug, PartialEq, Eq)]
/// Field inside a struct
pub struct Field {
    pub name: &'static str,
    pub inner_type: &'static TypeInformation,
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
pub enum EnumVariantType {
    /// Enum variant without any fields.
    UnitVariant(),
    /// Enum variant for tuples.
    TupleVariant {
        fields: &'static [&'static TypeInformation],
    },
    /// Enum variant for variants containing named fields.
    StructVariant { fields: &'static [Field] },
}

#[derive(Debug, PartialEq, Eq)]
/// Meta Data for an enum variant
pub struct EnumVariant {
    /// the name of the enums variant
    pub name: &'static str,
    /// the possible kind and if used the fields inside the enum variant.
    pub inner_type: EnumVariantType,
}

#[derive(Debug, PartialEq, Eq)]
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
        inner_type: &'static TypeInformation,
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
    UnitStructValue {
        name: &'static str,
    },

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
    SeqValue {
        inner_type: &'static TypeInformation,
    },

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue {
        inner_types: &'static [&'static TypeInformation],
    },
    TupleStructValue {
        name: &'static str,
        inner_types: &'static [&'static TypeInformation],
    },

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue {
        name: &'static str,
        fields: &'static [Field],
    },

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue {
        name: &'static str,
        possible_variants: &'static [EnumVariant],
    },
}

/// Implement this trait or derive `SerdeMeta` to add a
/// `meta` function to your class providing meta data about it.
pub trait SerdeMeta {
    /// Provide Meta Data for this struct.
    fn meta() -> &'static TypeInformation;
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
