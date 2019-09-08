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
}
