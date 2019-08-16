#[cfg(test)]
mod tests {
    use serde_meta::*;
    use serde_meta_derive;

    mod test_derive_u32_field {
        use super::*;

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
}
