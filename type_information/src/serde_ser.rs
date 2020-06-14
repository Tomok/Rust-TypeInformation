use super::number_indexed_type_information::numbered_type_informations_from_type_information;
use super::TypeInformation;
use serde::{Serialize, Serializer};

impl Serialize for TypeInformation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let serializeable = numbered_type_informations_from_type_information(self);
        serializeable.serialize(serializer)
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;

    use lazy_static::lazy_static;

    #[test]
    fn simple_struct_serialize_test() {
        let to = TypeInformation::StructValue(NamedTypeInformation::new(
            "TestObject".to_owned(),
            Fields::new(Box::new([])),
        ));
        let res = serde_json::to_string(&to).unwrap();
        assert_eq!(
            "{\"0\":{\"StructValue\":{\"name\":\"TestObject\",\"type_info\":{\"fields\":[]}}}}",
            res
        );
    }

    mod looped {
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
        fn looped_struct_serialize_test() {
            let looped = LOOPED_TEST_STRUCT.clone();
            let res = serde_json::to_string(&looped).unwrap();
            assert_eq!("{\"0\":{\"StructValue\":{\"name\":\"A\",\"type_info\":{\"fields\":[{\"name\":\"a\",\"inner_type\":0}]}}}}", res);
        }
    }
}
