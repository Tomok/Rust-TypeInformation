use super::{Meta, TypeInformation};

impl Meta for u8 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U8Value()
    }
}

impl Meta for u16 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U16Value()
    }
}

impl Meta for u32 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U32Value()
    }
}

impl Meta for u64 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U64Value()
    }
}

impl Meta for bool {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::BoolValue()
    }
}
impl Meta for i8 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I8Value()
    }
}
impl Meta for i16 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I16Value()
    }
}
impl Meta for i32 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I32Value()
    }
}
impl Meta for i64 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I64Value()
    }
}
