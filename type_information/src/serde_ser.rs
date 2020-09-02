use super::number_indexed_type_information::numbered_type_information_map_from_type_information;
use super::TypeInformation;
use serde::{Serialize, Serializer};

impl<'a> Serialize for TypeInformation<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        //use NumberedTypeInformationMap instead of NumberedTypeInformation, to get explicit indices into serialized data,
        // uses more space but makes it easier to read for text-based formats
        let serializeable = numbered_type_information_map_from_type_information(self);
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
            "TestObject",
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
        fn looped_struct_serialize_test() {
            let looped = LOOPED_TEST_STRUCT.clone();
            let res = serde_json::to_string(&looped).unwrap();
            assert_eq!("{\"0\":{\"StructValue\":{\"name\":\"A\",\"type_info\":{\"fields\":[{\"name\":\"a\",\"inner_type\":0}]}}}}", res);
        }
    }
}
