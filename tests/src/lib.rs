#[cfg(test)]
mod tests {
    use serde_meta_derive;
    use serde_meta::*;

    mod test_derive_empty_struct {
        use super::*;

        #[derive(serde_meta_derive::SerdeMeta)]
        struct A {}

        #[test]
        fn test() {
            let meta = A::meta();
            if let TypeInformation::StructValue{name, fields} = meta {
                assert_eq!("A", name);
                let expected_fields: &[Field] = &[];
                assert_eq!(expected_fields, fields);
            } else {
                panic!("Expected StructValue, but got {:#?}",
                       meta
                );
            }
        }
    }
}
