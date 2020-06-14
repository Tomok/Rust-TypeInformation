use super::{Meta, TypeInformation};

impl Meta for u8 {
    fn meta() -> TypeInformation {
        TypeInformation::U8Value()
    }
}

impl Meta for u16 {
    fn meta() -> TypeInformation {
        TypeInformation::U16Value()
    }
}

impl Meta for u32 {
    fn meta() -> TypeInformation {
        TypeInformation::U32Value()
    }
}

impl Meta for u64 {
    fn meta() -> TypeInformation {
        TypeInformation::U64Value()
    }
}

impl Meta for bool {
    fn meta() -> TypeInformation {
        TypeInformation::BoolValue()
    }
}
impl Meta for i8 {
    fn meta() -> TypeInformation {
        TypeInformation::I8Value()
    }
}
impl Meta for i16 {
    fn meta() -> TypeInformation {
        TypeInformation::I16Value()
    }
}
impl Meta for i32 {
    fn meta() -> TypeInformation {
        TypeInformation::I32Value()
    }
}
impl Meta for i64 {
    fn meta() -> TypeInformation {
        TypeInformation::I64Value()
    }
}
