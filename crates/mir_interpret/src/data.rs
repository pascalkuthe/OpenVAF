use std::intrinsics::transmute;

use lasso::Spur;

#[derive(Clone, Copy)]
#[repr(C)]
pub union Data {
    raw: [u8; 8],
    float: f64,
    int: i32,
    str: Spur,
    bool: bool,
}

impl Data {
    pub const UNDEF: Data = Data { raw: [0; 8] };

    pub fn f64(self) -> f64 {
        self.into()
    }

    pub fn i32(self) -> i32 {
        self.into()
    }

    pub fn bool(self) -> bool {
        self.into()
    }

    pub fn str(self) -> Spur {
        self.into()
    }

    pub fn from_f64_slice(data: &[f64]) -> &[Data] {
        unsafe { transmute(data) }
    }
}

impl From<i32> for Data {
    fn from(val: i32) -> Self {
        let mut res = Data::UNDEF;
        res.int = val;
        res
    }
}

impl From<Data> for f64 {
    fn from(data: Data) -> Self {
        // Safey: This is save because we only ever hand out zero inizalized unions
        // So the memory is always inizalized to zero.
        // For f64 all bit patterns are valid
        unsafe { data.float }
    }
}

impl From<Data> for i32 {
    fn from(data: Data) -> Self {
        // Safey: This is save because we only ever hand out zero inizalized unions
        // So the memory is always inizalized to zero.
        // For i32 all bit patterns are valid
        unsafe { data.int }
    }
}

impl From<Data> for Spur {
    fn from(data: Data) -> Self {
        // Safey: This is save because we only ever hand out zero inizalized unions
        // So the memory is always inizalized to zero.
        // For Spur all non zero bit patterns are valid (checked with assert)

        unsafe {
            assert_ne!(&data.raw[..4], &[0; 4]);
            data.str
        }
    }
}

impl From<Data> for bool {
    fn from(data: Data) -> Self {
        // Safey: This is save because we only ever hand out zero inizalized unions
        // So the memory is always inizalized to zero.
        // For bool all bit patterns are valid

        unsafe { data.bool }
    }
}

impl From<f64> for Data {
    fn from(val: f64) -> Self {
        let mut res = Data::UNDEF;
        res.float = val;
        res
    }
}

impl From<Spur> for Data {
    fn from(val: Spur) -> Self {
        let mut res = Data::UNDEF;
        res.str = val;
        res
    }
}

impl From<bool> for Data {
    fn from(val: bool) -> Self {
        let mut res = Data::UNDEF;
        res.bool = val;
        res
    }
}

impl From<mir::Const> for Data {
    fn from(val: mir::Const) -> Self {
        match val {
            mir::Const::Float(val) => f64::from(val).into(),
            mir::Const::Int(val) => val.into(),
            mir::Const::Str(val) => val.into(),
            mir::Const::Bool(val) => val.into(),
        }
    }
}
