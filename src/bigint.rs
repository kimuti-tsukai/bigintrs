use crate::BigUint;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Sign {
    Positive = 1,
    Zero = 0,
    Negative = -1
}

impl Default for Sign {
    fn default() -> Self {
        Self::Positive
    }
}

#[derive(Clone, Debug, Hash, Default, PartialEq, Eq)]
pub struct BigInt {
    sign: Sign,
    value: BigUint
}

// Private methods
impl BigInt {
    fn is_zero_and_set_sign(&mut self) {
        if self.is_zero() {
            self.sign = Sign::Zero
        }
    }

    fn unwrap_zero_and_sign(mut self) -> Self {
        if self.is_zero() {
            self.is_zero_and_set_sign();
        }

        self
    }
}

// Public methods
impl BigInt {
    pub fn new() -> Self {
        BigInt {
            sign: Sign::Zero,
            value: BigUint::new()
        }
    }

    pub fn zero() -> Self {
        Self::new()
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    pub fn set_zero(&mut self) {
        *self = Self::zero()
    }

    pub fn one() -> Self {
        BigInt {
            sign: Sign::Positive,
            value: BigUint::one()
        }
    }

    pub fn is_one(&self) -> bool {
        self == &Self::one()
    }

    pub fn set_one(&mut self) {
        *self = Self::one()
    }

    pub fn is_positive(&self) -> bool {
        self.sign == Sign::Positive
    }

    pub fn is_negative(&self) -> bool {
        self.sign == Sign::Negative
    }
}

impl From<BigUint> for BigInt {
    fn from(value: BigUint) -> Self {
        BigInt {
            sign: if value.is_zero() { Sign::Zero } else { Sign::Positive },
            value
        }
    }
}

macro_rules! impl_from_unsigned_int {
    ($($type: ty),+) => {$(
        impl From<$type> for BigInt {
            fn from(value: $type) -> Self {
                Self::from(BigUint::from(value))
            }
        }
    )+};
}

impl_from_unsigned_int!(u8, u16, u32, u64, u128, usize);

macro_rules! impl_from_signed_int {
    ($($type: ty),+) => {$(
        impl From<$type> for BigInt {
            fn from(value: $type) -> Self {
                match value.signum() {
                    0 => Self::zero(),
                    1 => Self::from(BigUint::try_from(value).unwrap()),
                    -1 => Self {
                        sign: Sign::Negative,
                        value: BigUint::try_from(-value).unwrap()
                    },
                    _ => unreachable!()
                }
            }
        }
    )+};
}

impl_from_signed_int!(i8, i16, i32, i64, i128, isize);
