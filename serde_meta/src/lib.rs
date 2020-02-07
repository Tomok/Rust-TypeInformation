#[derive(Debug, PartialEq, Eq)]
/// Field inside a struct
pub struct Field<'a> {
    pub name: &'a str,
    pub inner_type: &'a TypeInformation<'a>,
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
    /// the name of the enums variant
    pub name: &'a str,
    /// the possible kind and if used the fields inside the enum variant.
    pub inner_type: EnumVariantType<'a>,
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
    UnitStructValue {
        name: &'a str,
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
        inner_type: &'a TypeInformation<'a>,
    },

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue {
        inner_types: &'a [&'a TypeInformation<'a>],
    },
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

mod serde_support {
    use super::*;
    use serde::ser::*;
    use std::cell::RefCell;
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashMap;
    use std::hash::Hasher;

    type Id = usize;

    struct VisitedMap(RefCell<HashMap<u64, Id>>);

    impl VisitedMap {
        fn get(&self, mem_pos: &u64) -> Option<Id> {
            self.0.borrow().get(mem_pos).map(|x| *x)
        }

        fn register(&self, mem_pos: u64) -> Id {
            let mut v = self.0.borrow_mut();
            let res = v.len();
            let existing = v.insert(mem_pos, res);
            assert!(existing.is_none()); //only new items should be registered
            res
        }
    }

    struct SerializeableTypeInformation<'a, 'b> {
        type_info: &'a TypeInformation<'a>,
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableTypeInformation<'a, 'b> {
        fn new(type_info: &'a TypeInformation<'a>, visited: &'b VisitedMap) -> Self {
            Self { type_info, visited }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableTypeInformation<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut hasher = DefaultHasher::new();
            core::ptr::hash(self.type_info, &mut hasher);
            let self_mem_pos = hasher.finish();
            if let Some(id) = self.visited.get(&self_mem_pos) {
                let mut st = serializer.serialize_struct("TypeInformationRef", 1)?;
                st.serialize_field("id", &id)?;
                st.end()
            } else {
                let visited = self.visited;
                match self.type_info {
                    TypeInformation::BoolValue() => {
                        serializer.serialize_unit_variant("TypeInformation", 0, "BoolValue")
                    }
                    TypeInformation::I8Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 1, "I8Value")
                    }
                    TypeInformation::I16Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 2, "I16Value")
                    }
                    TypeInformation::I32Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 3, "I32Value")
                    }
                    TypeInformation::I64Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 4, "I64Value")
                    }
                    TypeInformation::I128Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 5, "I128Value")
                    }
                    TypeInformation::U8Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 6, "U8Value")
                    }
                    TypeInformation::U16Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 7, "U16Value")
                    }
                    TypeInformation::U32Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 8, "U32Value")
                    }
                    TypeInformation::U64Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 9, "U64Value")
                    }
                    TypeInformation::U128Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 10, "U128Value")
                    }
                    TypeInformation::F32Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 11, "F32Value")
                    }
                    TypeInformation::F64Value() => {
                        serializer.serialize_unit_variant("TypeInformation", 12, "F64Value")
                    }
                    TypeInformation::CharValue() => {
                        serializer.serialize_unit_variant("TypeInformation", 13, "CharValue")
                    }
                    TypeInformation::StringValue() => {
                        serializer.serialize_unit_variant("TypeInformation", 14, "StringValue")
                    }
                    TypeInformation::ByteArray() => {
                        serializer.serialize_unit_variant("TypeInformation", 15, "ByteArray")
                    }
                    TypeInformation::OptionValue { inner_type } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            16,
                            "OptionValue",
                            1,
                        )?;
                        let serializeable_inner_type =
                            SerializeableTypeInformation::new(inner_type, visited);
                        st.serialize_field("inner_type", &serializeable_inner_type)?;
                        st.end()
                    }
                    TypeInformation::UnitStructValue { name } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            17,
                            "UnitStructValue",
                            1,
                        )?;
                        st.serialize_field("name", name)?;
                        st.end()
                    }
                    TypeInformation::SeqValue { inner_type } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            18,
                            "SeqValue",
                            1,
                        )?;
                        let serializeable_inner_type =
                            SerializeableTypeInformation::new(inner_type, visited);
                        st.serialize_field("inner_type", &serializeable_inner_type)?;
                        st.end()
                    }
                    TypeInformation::TupleValue { inner_types } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            18,
                            "SeqValue",
                            1,
                        )?;
                        let serializeable_inner_types =
                            SerializeableTypeInformations::new(inner_types, visited);
                        st.serialize_field("inner_types", &serializeable_inner_types)?;
                        st.end()
                    }
                    TypeInformation::TupleStructValue { name, inner_types } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            19,
                            "TupleStructValue",
                            3,
                        )?;
                        let id = visited.register(self_mem_pos);
                        st.serialize_field("id", &id)?;
                        st.serialize_field("name", name)?;
                        let serializeable_inner_types =
                            SerializeableTypeInformations::new(inner_types, visited);
                        st.serialize_field("inner_types", &serializeable_inner_types)?;
                        st.end()
                    }
                    TypeInformation::StructValue { name, fields } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            19,
                            "StructValue",
                            3,
                        )?;
                        let id = visited.register(self_mem_pos);
                        st.serialize_field("id", &id)?;
                        st.serialize_field("name", name)?;
                        let serializeable_fields = SerializeableFields::new(fields, visited);
                        st.serialize_field("fields", &serializeable_fields)?;
                        st.end()
                    }
                    TypeInformation::EnumValue {
                        name,
                        possible_variants,
                    } => {
                        let mut st = serializer.serialize_struct_variant(
                            "TypeInformation",
                            20,
                            "EnumValue",
                            3,
                        )?;
                        let id = visited.register(self_mem_pos);
                        st.serialize_field("id", &id)?;
                        st.serialize_field("name", name)?;
                        let serializeable =
                            SerializeableEnumVariants::new(possible_variants, visited);
                        st.serialize_field("possible_variants", &serializeable)?;
                        st.end()
                    }
                }
            }
        }
    }

    struct SerializeableTypeInformations<'a, 'b> {
        type_info: &'a [&'a TypeInformation<'a>],
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableTypeInformations<'a, 'b> {
        fn new(type_info: &'a [&'a TypeInformation<'a>], visited: &'b VisitedMap) -> Self {
            Self { type_info, visited }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableTypeInformations<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.type_info.len()))?;
            for element in self.type_info {
                let serializeable = SerializeableTypeInformation::new(element, self.visited);
                seq.serialize_element(&serializeable)?;
            }
            seq.end()
        }
    }

    struct SerializeableField<'a, 'b> {
        field: &'a Field<'a>,
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableField<'a, 'b> {
        fn new(field: &'a Field<'a>, visited: &'b VisitedMap) -> Self {
            Self { field, visited }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableField<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut st = serializer.serialize_struct("Field", 2)?;
            st.serialize_field("name", self.field.name)?;
            let serializeable =
                SerializeableTypeInformation::new(self.field.inner_type, self.visited);
            st.serialize_field("inner_type", &serializeable)?;
            st.end()
        }
    }
    struct SerializeableFields<'a, 'b> {
        fields: &'a [Field<'a>],
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableFields<'a, 'b> {
        fn new(fields: &'a [Field<'a>], visited: &'b VisitedMap) -> Self {
            Self { fields, visited }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableFields<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.fields.len()))?;
            for element in self.fields {
                let serializeable = SerializeableField::new(element, self.visited);
                seq.serialize_element(&serializeable)?;
            }
            seq.end()
        }
    }

    struct SerializeableEnumVariantType<'a, 'b> {
        enum_variant_type: &'a EnumVariantType<'a>,
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableEnumVariantType<'a, 'b> {
        fn new(enum_variant_type: &'a EnumVariantType<'a>, visited: &'b VisitedMap) -> Self {
            Self {
                enum_variant_type,
                visited,
            }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableEnumVariantType<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self.enum_variant_type {
                EnumVariantType::UnitVariant() => {
                    (serializer.serialize_unit_variant("EnumVariantType", 0, "UnitVariant"))
                }
                EnumVariantType::TupleVariant { fields } => {
                    let mut st = serializer.serialize_struct_variant(
                        "EnumVariantType",
                        1,
                        "TupleVariant",
                        1,
                    )?;
                    let serializeable = SerializeableTypeInformations::new(fields, self.visited);
                    st.serialize_field("fields", &serializeable)?;
                    st.end()
                }
                EnumVariantType::StructVariant { fields } => {
                    let mut st = serializer.serialize_struct_variant(
                        "EnumVariantType",
                        2,
                        "TupleVariant",
                        1,
                    )?;
                    let serializeable = SerializeableFields::new(fields, self.visited);
                    st.serialize_field("fields", &serializeable)?;
                    st.end()
                }
            }
        }
    }

    struct SerializeableEnumVariant<'a, 'b> {
        enum_variant: &'a EnumVariant<'a>,
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableEnumVariant<'a, 'b> {
        fn new(enum_variant: &'a EnumVariant<'a>, visited: &'b VisitedMap) -> Self {
            Self {
                enum_variant,
                visited,
            }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableEnumVariant<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut st = serializer.serialize_struct("EnumVariant", 2)?;
            st.serialize_field("name", self.enum_variant.name)?;
            let serializeable =
                SerializeableEnumVariantType::new(&self.enum_variant.inner_type, self.visited);
            st.serialize_field("inner_type", &serializeable)?;
            st.end()
        }
    }
    struct SerializeableEnumVariants<'a, 'b> {
        enum_variants: &'a [EnumVariant<'a>],
        visited: &'b VisitedMap,
    }

    impl<'a, 'b> SerializeableEnumVariants<'a, 'b> {
        fn new(enum_variants: &'a [EnumVariant<'a>], visited: &'b VisitedMap) -> Self {
            Self {
                enum_variants,
                visited,
            }
        }
    }

    impl<'a, 'b, 'c> Serialize for SerializeableEnumVariants<'b, 'c> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.enum_variants.len()))?;
            for element in self.enum_variants {
                let serializeable = SerializeableEnumVariant::new(element, self.visited);
                seq.serialize_element(&serializeable)?;
            }
            seq.end()
        }
    }

    impl<'a> Serialize for TypeInformation<'a> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let visited = VisitedMap(RefCell::new(HashMap::new()));
            let serializeable = SerializeableTypeInformation::new(&self, &visited);
            serializeable.serialize(serializer)
        }
    }
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn simple_struct_serialize_test() {
            let to = TypeInformation::StructValue {
                name: "TestObject",
                fields: &[],
            };
            let res = serde_json::to_string(&to).unwrap();
            assert_eq!(
                "{\"StructValue\":{\"id\":0,\"name\":\"TestObject\",\"fields\":[]}}",
                res
            );
        }

        static FIELDS: &[Field] = &[Field {
            name: "a",
            inner_type: &LOOPED_TEST_STRUCT,
        }];
        static LOOPED_TEST_STRUCT: TypeInformation = TypeInformation::StructValue {
            name: &"A",
            fields: FIELDS,
        };

        #[test]
        fn looped_struct_serialize_test() {
            let res = serde_json::to_string(&LOOPED_TEST_STRUCT).unwrap();
            assert_eq!("{\"StructValue\":{\"id\":0,\"name\":\"A\",\"fields\":[{\"name\":\"a\",\"inner_type\":{\"id\":0}}]}}", res);
        }
    }
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
