use serde::{Deserialize, Serialize};
type TIBox<T> = Box<T>;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
/// Field inside a struct
pub struct Field<TypeInformationRef> {
    name: String,
    inner_type: TypeInformationRef,
}

impl<'a, TypeInformationRef> Field<TypeInformationRef> {
    pub fn new(name: String, inner_type: TypeInformationRef) -> Self {
        Self { name, inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct Fields<TypeInformationRef> {
    fields: Box<[Field<TypeInformationRef>]>,
}

impl<'a, TypeInformationRef> Fields<TypeInformationRef> {
    pub const fn new(fields: Box<[Field<TypeInformationRef>]>) -> Self {
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
pub enum EnumVariantType<TypeInformationRef> {
    /// Enum variant without any fields.
    UnitVariant(),
    /// Enum variant for tuples.
    TupleVariant(TupleTypes<TypeInformationRef>),
    /// Enum variant for variants containing named fields.
    StructVariant(Fields<TypeInformationRef>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
/// Meta Data for an enum variant
pub struct EnumVariant<TypeInformationRef> {
    name: String,
    inner_type: EnumVariantType<TypeInformationRef>,
}

impl<'a, TypeInformationRef> EnumVariant<TypeInformationRef> {
    pub const fn new(name: String, inner_type: EnumVariantType<TypeInformationRef>) -> Self {
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
pub struct SeqType<TypeInformationRef> {
    inner_type: TypeInformationRef,
}

impl<TypeInformationRef> SeqType<TypeInformationRef> {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TupleType<TypeInformationRef> {
    inner_type: TypeInformationRef,
}

impl<'a, TypeInformationRef> TupleType<TypeInformationRef> {
    pub fn new(inner_type: TypeInformationRef) -> Self {
        Self { inner_type }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TupleTypes<TypeInformationRef> {
    inner_types: TIBox<[TupleType<TypeInformationRef>]>,
}

impl<TypeInformationRef> TupleTypes<TypeInformationRef> {
    pub fn new(inner_types: TIBox<[TupleType<TypeInformationRef>]>) -> Self {
        Self { inner_types }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct EnumType<TypeInformationRef> {
    possible_variants: Box<[EnumVariant<TypeInformationRef>]>,
}

impl<'a, TypeInformationRef> EnumType<TypeInformationRef> {
    pub const fn new(possible_variants: Box<[EnumVariant<TypeInformationRef>]>) -> Self {
        Self { possible_variants }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
/// All possible kinds of TypeInformation delivered by `meta` function
/// or contained in the structs returned by it.
pub enum TypeInformation<TypeInformationRef> {
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
    SeqValue(SeqType<TypeInformationRef>),

    /// TupleValue used for tuples, also used for
    /// arrays with known length, to be able to reflect
    /// the length information
    TupleValue(TupleTypes<TypeInformationRef>),
    TupleStructValue(NamedTypeInformation<TupleTypes<TypeInformationRef>>),

    // TODO: Unused -> Remove??
    // MapValue {
    //     key_type: &'static TypeInformation,
    //     value_type: &'static TypeInformation,
    // },
    StructValue(NamedTypeInformation<Fields<TypeInformationRef>>),

    /// Used for an Enum and contains all possible variants
    /// of it.
    ///
    /// Note that SERDE does not know this type, as it does
    /// not need to transfer the information, that a enum was
    /// used, just which value it had.
    EnumValue(NamedTypeInformation<EnumType<TypeInformationRef>>),
}
