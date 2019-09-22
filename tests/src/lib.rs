#[cfg(test)]
mod tests {
    use serde_meta::*;
    use serde_meta_derive;

    mod test_derive_u32_field {
        use super::*;

        #[allow(unused)] //dummy struct just to check meta information generated for it
        #[derive(serde_meta_derive::SerdeMeta)]
        struct A {
            f: u32,
        }

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue { name, fields } = meta {
                assert_eq!(&"A", name);
                assert_eq!(1, fields.len());
                assert_eq!("f", fields[0].name);
                assert_eq!(&TypeInformation::U32Value(), fields[0].inner_type);
            } else {
                panic!("Expected StructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_empty_struct {
        use super::*;

        #[derive(serde_meta_derive::SerdeMeta)]
        struct A {}

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue { name, fields } = meta {
                assert_eq!(&"A", name);
                let expected_fields: &[Field] = &[];
                assert_eq!(&expected_fields, fields);
            } else {
                panic!("Expected StructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_single_field_struct {
        use super::*;

        #[derive(serde_meta_derive::SerdeMeta)]
        struct B;

        #[allow(unused)] //dummy struct just to check meta information generated for it
        #[derive(serde_meta_derive::SerdeMeta)]
        struct A {
            f: B,
        }

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue { name, fields } = meta {
                assert_eq!(&"A", name);
                assert_eq!(1, fields.len());
                assert_eq!("f", fields[0].name);
                assert_eq!(B::meta(), fields[0].inner_type);
            } else {
                panic!("Expected StructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_multiple_unnamed_fields_struct {
        use super::*;

        #[allow(unused)]
        #[derive(serde_meta_derive::SerdeMeta)]
        struct A(u8, u16, u32);

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::TupleStructValue { name, inner_types } = meta {
                assert_eq!(&"A", name);
                assert_eq!(3, inner_types.len());
                assert_eq!(&TypeInformation::U8Value(), inner_types[0]);
                assert_eq!(&TypeInformation::U16Value(), inner_types[1]);
                assert_eq!(&TypeInformation::U32Value(), inner_types[2]);
            } else {
                panic!("Expected TupleStructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_array_struct {
        use super::*;

        #[allow(unused)]
        #[derive(serde_meta_derive::SerdeMeta)]
        struct A {
            f: [u8; 3],
        }

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue { name, fields } = meta {
                assert_eq!(&"A", name);
                assert_eq!(1, fields.len());
                assert_eq!("f", fields[0].name);
                if let TypeInformation::TupleValue { inner_types } = fields[0].inner_type {
                    assert_eq!(3, inner_types.len());
                    for &t in inner_types.iter() {
                        assert_eq!(&TypeInformation::U8Value(), t);
                    }
                } else {
                    panic!("Expected TupleValue, but got {:#?}", fields[0].inner_type);
                }
            } else {
                panic!("Expected StructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_array_ref {
        use super::*;

        #[allow(unused)]
        #[derive(serde_meta_derive::SerdeMeta)]
        struct A<'a> {
            f: &'a [u16],
        }

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue { name, fields } = meta {
                assert_eq!(&"A", name);
                assert_eq!(1, fields.len());
                assert_eq!("f", fields[0].name);
                if let TypeInformation::SeqValue { inner_type } = fields[0].inner_type {
                    //as if let adds a reference, to the reference in SeqValue.inner_type,
                    // a check against &&TypeInformation::U16Value is necessary`:w
                    assert_eq!(&&TypeInformation::U16Value(), inner_type);
                } else {
                    panic!("Expected SeqValue, but got {:#?}", fields[0].inner_type);
                }
            } else {
                panic!("Expected StructValue, but got {:#?}", meta);
            }
        }
    }

    mod test_derive_enum {
        use super::*;

        #[allow(unused)]
        #[derive(serde_meta_derive::SerdeMeta)]
        enum A {
            IntVal(i32),
            StructVal { field: bool },
            UnitVal,
        }

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::EnumValue {
                name,
                possible_variants,
            } = meta
            {
                assert_eq!(&"A", name);
                assert_eq!(3, possible_variants.len());

                assert_eq!("IntVal", possible_variants[0].name);
                if let EnumVariantType::TupleVariant { fields: x } = possible_variants[0].inner_type
                {
                    assert_eq!(1, x.len());
                    assert_eq!(&TypeInformation::I32Value(), x[0]);
                } else {
                    panic!(
                        "Expected TupleVariant, but found {:#?}",
                        possible_variants[0].inner_type
                    );
                }

                assert_eq!("StructVal", possible_variants[1].name);
                if let EnumVariantType::StructVariant { fields: x } =
                    possible_variants[1].inner_type
                {
                    assert_eq!(1, x.len());
                    assert_eq!(&TypeInformation::BoolValue(), x[0].inner_type);
                    assert_eq!("field", x[0].name);
                } else {
                    panic!(
                        "Expected StructVariant, but found {:#?}",
                        possible_variants[0].inner_type
                    );
                }

                assert_eq!("UnitVal", possible_variants[2].name);
                assert_eq!(
                    EnumVariantType::UnitVariant(),
                    possible_variants[2].inner_type
                )
            } else {
                panic!("Expected EnumValue but got {:#?}", meta);
            }
        }
    }
}
