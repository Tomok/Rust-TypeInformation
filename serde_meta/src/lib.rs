#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    pub name: &'static str,
    pub inner_type: &'static TypeInformation,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeInformation {
    BoolValue(),

    I8Value(),
    I16Value(),
    I32Value(),
    I64Value(),
    I128Value(),

    U8Value(),
    U16Value(),
    U32Value(),
    U64Value(),
    U128Value(),

    F32Value(),
    F64Value(),

    CharValue(),

    StringValue(),
    ByteArray(),

    OptionValue {
        inner_type: &'static TypeInformation,
    },

    UnitValue(),
    UnitStructValue {
        name: &'static str,
    },
    UnitVariant(),

    NewTypeStructValue {
        inner_type: &'static TypeInformation,
    },
    NewTypeVariantValue {
        inner_type: &'static TypeInformation,
    },

    SeqValue {
        inner_type: &'static TypeInformation,
    },

    TupleValue {
        inner_types: &'static [&'static TypeInformation],
    },
    TupleStructValue {
        name: &'static str,
        inner_types: &'static [&'static TypeInformation],
    },
    TupleVariantValue {
        name: &'static str,
        inner_types: &'static [&'static TypeInformation],
    },

    MapValue {
        key_type: &'static TypeInformation,
        value_type: &'static TypeInformation,
    },

    StructValue {
        name: &'static str,
        fields: &'static [Field],
    },
    StructVariant {
        fields: &'static [Field],
    },
}

pub trait SerdeMeta {
    fn meta() -> TypeInformation;
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
            fields: FIELDS
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
