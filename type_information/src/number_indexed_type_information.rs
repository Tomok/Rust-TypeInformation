use serde::{Deserialize, Serialize};

use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::convert::{TryInto, TryFrom};
use std::hash::{Hash, Hasher};

type TypeInformationRef = usize;

type TIBox<T> = Box<T>;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
/// Field inside a struct
pub struct Field {
    name: String,
    inner_type: TypeInformationRef,
}

impl<'a> Field {
    pub fn new(name: String, inner_type: TypeInformationRef) -> Self {
        Self { name, inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct Fields {
    fields: Box<[Field]>,
}

impl<'a> Fields {
    pub const fn new(fields: Box<[Field]>) -> Self {
        Self { fields }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
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
    TupleVariant(TupleTypes),
    /// Enum variant for variants containing named fields.
    StructVariant(Fields),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
/// Meta Data for an enum variant
pub struct EnumVariant {
    name: String,
    inner_type: EnumVariantType,
}

impl<'a> EnumVariant {
    pub const fn new(name: String, inner_type: EnumVariantType) -> Self {
        Self { name, inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct NamedTypeInformation<I: Sized> {
    name: String,
    type_info: I,
}

impl<'a, I: Sized> NamedTypeInformation<I> {
    pub const fn new(name: String, type_info: I) -> Self {
        Self { name, type_info }
    }
}

pub type UnitStructType = NamedTypeInformation<()>;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct SeqType {
    inner_type: TypeInformationRef,
}

impl SeqType {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TupleType {
    inner_type: TypeInformationRef,
}

impl<'a> TupleType {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TupleTypes {
    inner_types: TIBox<[TupleType]>,
}

impl TupleTypes {
    pub fn new(inner_types: TIBox<[TupleType]>) -> Self {
        Self { inner_types }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct EnumType {
    possible_variants: Box<[EnumVariant]>,
}

impl<'a> EnumType {
    pub const fn new(possible_variants: Box<[EnumVariant]>) -> Self {
        Self { possible_variants }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
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
        inner_type: TypeInformationRef,
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
    UnitStructValue(UnitStructType),

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
    SeqValue(SeqType),

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue(TupleTypes),
    TupleStructValue(NamedTypeInformation<TupleTypes>),

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue(NamedTypeInformation<Fields>),

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue(NamedTypeInformation<EnumType>),
}


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

pub struct NumberedTypeInformations(Vec<TypeInformation>);

impl TryFrom<NumberedTypeInformationMap> for NumberedTypeInformations {
    type Error = &'static str;
    fn try_from(mut value: NumberedTypeInformationMap) -> Result<Self, Self::Error> {
        let ti_count = value.len();
        let mut res_vec = Vec::with_capacity(ti_count);
        for index in 0..ti_count {
            //use remove instead of get to gain ownership
            if let Some(type_info) = value.remove(&index) {
                res_vec.push(type_info);
            } else {
                return Err("NumberedTypeInformationMap was not consecutive");
            }
        }
        Ok(Self(res_vec))
    }

}

pub fn numbered_type_informations_from_type_information(
    ti: &super::TypeInformation
) -> NumberedTypeInformations {
    let ordered_map = numbered_type_information_map_from_type_information(ti);
    ordered_map.try_into().expect("Internal Error: NumberedTypeInformationMap was not consecutive")
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
        Some((id, _)) => id.clone(),
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
