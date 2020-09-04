use super::generic_indexed_type_information;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::hash::{Hash, Hasher};

type TypeInformationRef = usize;
//re-publish types from generic_indexed_type_information with TypeInformatioRef type filled
pub type Field = generic_indexed_type_information::Field<TypeInformationRef>;
pub type Fields = generic_indexed_type_information::Fields<TypeInformationRef>;
pub type EnumVariantType = generic_indexed_type_information::EnumVariantType<TypeInformationRef>;
pub type EnumVariant = generic_indexed_type_information::EnumVariant<TypeInformationRef>;
pub use generic_indexed_type_information::NamedTypeInformation;
pub use generic_indexed_type_information::UnitStructType;
pub type SeqType = generic_indexed_type_information::SeqType<TypeInformationRef>;
pub type TupleType = generic_indexed_type_information::TupleType<TypeInformationRef>;
pub type TupleTypes = generic_indexed_type_information::TupleTypes<TypeInformationRef>;
pub type EnumType = generic_indexed_type_information::EnumType<TypeInformationRef>;
pub type TypeInformation = generic_indexed_type_information::TypeInformation<TypeInformationRef>;

pub type NumberedTypeInformationMap = std::collections::BTreeMap<usize, TypeInformation>;

pub fn numbered_type_information_map_from_type_information(
    ti: &super::TypeInformation,
) -> NumberedTypeInformationMap {
    let mut known_infos = KnownTypeInfos::new();
    let index = make_numbered(ti, &mut known_infos);
    assert_eq!(
        0, index,
        "Expected 0 to be the index of the root TypeInformation"
    );
    //switch to BTreeMap, to have an ordered map
    let mut indexed_map = BTreeMap::new();
    for (_, (index, ti_option)) in known_infos.drain() {
        if indexed_map
            .insert(index, ti_option.expect("None in known types map"))
            .is_some()
        {
            panic!("Same index appeared twice in known_infos");
        }
    }
    indexed_map
}

type KnownTypeInfos = HashMap<u64, (TypeInformationRef, Option<TypeInformation>)>;

/// takes the passed in typeInformation and converts it to a numbered type information,
/// which is added to known_infos, unless it already was in there
fn make_numbered<'a>(
    ti: &super::TypeInformation<'a>,
    known_infos: &mut KnownTypeInfos,
) -> TypeInformationRef {
    let mut hasher = DefaultHasher::new();
    ti.hash(&mut hasher);
    let ti_hash = hasher.finish();

    let known_infos_entry = known_infos.get(&ti_hash);

    match known_infos_entry {
        Some((id, _)) => *id,
        None => {
            let index_no = known_infos.len();
            //temporarely register self in known_infos
            known_infos.insert(ti_hash, (index_no, None));
            let new_type_info = match ti {
                super::TypeInformation::BoolValue() => TypeInformation::BoolValue(),
                super::TypeInformation::I8Value() => TypeInformation::I8Value(),
                super::TypeInformation::I16Value() => TypeInformation::I16Value(),
                super::TypeInformation::I32Value() => TypeInformation::I32Value(),
                super::TypeInformation::I64Value() => TypeInformation::I64Value(),
                super::TypeInformation::I128Value() => TypeInformation::I128Value(),
                super::TypeInformation::U8Value() => TypeInformation::U8Value(),
                super::TypeInformation::U16Value() => TypeInformation::U16Value(),
                super::TypeInformation::U32Value() => TypeInformation::U32Value(),
                super::TypeInformation::U64Value() => TypeInformation::U64Value(),
                super::TypeInformation::U128Value() => TypeInformation::U128Value(),
                super::TypeInformation::F32Value() => TypeInformation::F32Value(),
                super::TypeInformation::F64Value() => TypeInformation::F64Value(),
                super::TypeInformation::CharValue() => TypeInformation::CharValue(),
                super::TypeInformation::StringValue() => TypeInformation::StringValue(),
                super::TypeInformation::ByteArray() => TypeInformation::ByteArray(),
                super::TypeInformation::OptionValue { inner_type } => {
                    let inner_type = make_numbered(&(inner_type)(), known_infos);
                    TypeInformation::OptionValue { inner_type }
                }
                super::TypeInformation::UnitStructValue(named_info) => {
                    TypeInformation::UnitStructValue(NamedTypeInformation::new(
                        named_info.name().into(),
                        (),
                    ))
                }
                super::TypeInformation::SeqValue(seq_type) => {
                    let inner_type = make_numbered(&seq_type.inner_type(), known_infos);
                    TypeInformation::SeqValue(SeqType::new(inner_type))
                }
                super::TypeInformation::TupleValue(tuple_types) => {
                    TypeInformation::TupleValue(convert_tuple_types(tuple_types, known_infos))
                }
                super::TypeInformation::TupleStructValue(super::NamedTypeInformation {
                    name,
                    type_info,
                }) => TypeInformation::TupleStructValue(NamedTypeInformation::new(
                    (*name).into(),
                    convert_tuple_types(type_info, known_infos),
                )),
                super::TypeInformation::StructValue(super::NamedTypeInformation {
                    name,
                    type_info,
                }) => TypeInformation::StructValue(NamedTypeInformation::new(
                    (*name).into(),
                    convert_fields(type_info, known_infos),
                )),
                super::TypeInformation::EnumValue(super::NamedTypeInformation {
                    name,
                    type_info,
                }) => TypeInformation::EnumValue(NamedTypeInformation::new(
                    (*name).into(),
                    convert_enum_type(type_info, known_infos),
                )),
            };
            known_infos.insert(ti_hash, (index_no, Some(new_type_info)));
            index_no
        }
    }
}

fn convert_tuple_types(
    tuple_types: &super::TupleTypes,
    known_infos: &mut KnownTypeInfos,
) -> TupleTypes {
    let mut inner_types_vec = std::vec::Vec::with_capacity(tuple_types.inner_types().len());
    for t in tuple_types.inner_types().iter() {
        let type_ref = make_numbered(&t.inner_type(), known_infos);
        inner_types_vec.push(TupleType::new(type_ref));
    }
    assert_eq!(
        tuple_types.inner_types().len(),
        inner_types_vec.len(),
        "Length of new vec {} did not match old len {}",
        inner_types_vec.len(),
        tuple_types.inner_types().len()
    );
    TupleTypes::new(inner_types_vec.into_boxed_slice())
}

fn convert_fields(fields: &super::Fields, known_infos: &mut KnownTypeInfos) -> Fields {
    let mut fields_vec = std::vec::Vec::with_capacity(fields.fields.len());
    for f in fields.fields.iter() {
        let new_field = Field::new(f.name().into(), make_numbered(&f.inner_type(), known_infos));
        fields_vec.push(new_field);
    }
    Fields::new(fields_vec.into_boxed_slice())
}

fn convert_enum_type(enum_type: &super::EnumType, known_infos: &mut KnownTypeInfos) -> EnumType {
    let mut variants_vec = std::vec::Vec::with_capacity(enum_type.possible_variants().len());
    for v in enum_type.possible_variants().iter() {
        let evt = convert_enum_variant_type(v.inner_type(), known_infos);
        let new_variant = EnumVariant::new(v.name().into(), evt);
        variants_vec.push(new_variant);
    }
    EnumType::new(variants_vec.into_boxed_slice())
}

fn convert_enum_variant_type(
    enum_variant_type: &super::EnumVariantType,
    known_infos: &mut KnownTypeInfos,
) -> EnumVariantType {
    match enum_variant_type {
        super::EnumVariantType::UnitVariant() => EnumVariantType::UnitVariant(),
        super::EnumVariantType::TupleVariant(tuple_types) => {
            EnumVariantType::TupleVariant(convert_tuple_types(tuple_types, known_infos))
        }
        super::EnumVariantType::StructVariant(fields) => {
            EnumVariantType::StructVariant(convert_fields(fields, known_infos))
        }
    }
}
