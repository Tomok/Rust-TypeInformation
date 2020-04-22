use super::{SerdeMeta, TypeInformation};

impl SerdeMeta for u8 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U8Value()
    }
}

impl SerdeMeta for u16 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U16Value()
    }
}

impl SerdeMeta for u32 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U32Value()
    }
}

impl SerdeMeta for u64 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::U64Value()
    }
}

impl SerdeMeta for bool {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::BoolValue()
    }
}
impl SerdeMeta for i8 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I8Value()
    }
}
impl SerdeMeta for i16 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I16Value()
    }
}
impl SerdeMeta for i32 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I32Value()
    }
}
impl SerdeMeta for i64 {
    fn meta() -> TypeInformation<'static> {
        TypeInformation::I64Value()
    }
}
