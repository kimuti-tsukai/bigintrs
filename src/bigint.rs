use std::ops::{Add, Neg};

use crate::BigUint;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Sign {
    Positive = 1,
    Zero = 0,
    Negative = -1,
}

impl Default for Sign {
    fn default() -> Self {
        Self::Positive
    }
}

#[derive(Clone, Debug, Hash, Default, PartialEq, Eq)]
pub struct BigInt {
    sign: Sign,
    value: BigUint,
}

// Public methods
impl BigInt {
    pub fn new() -> Self {
        BigInt {
            sign: Sign::Zero,
            value: BigUint::new(),
        }
    }

    pub fn zero() -> Self {
        Self::new()
    }

    pub fn is_zero(&self) -> bool {
        self.sign == Sign::Zero
    }

    pub fn set_zero(&mut self) {
        *self = Self::zero()
    }

    pub fn one() -> Self {
        BigInt {
            sign: Sign::Positive,
            value: BigUint::one(),
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
            sign: if value.is_zero() {
                Sign::Zero
            } else {
                Sign::Positive
            },
            value,
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

impl Neg for BigInt {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self.sign {
            Sign::Positive => Self {
                sign: Sign::Negative,
                ..self
            },
            Sign::Negative => Self {
                sign: Sign::Positive,
                ..self
            },
            Sign::Zero => self,
        }
    }
}

impl Add for BigInt {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else if self.sign == rhs.sign {
            Self {
                sign: self.sign,
                value: &self.value + &rhs.value,
            }
        } else if self.value == rhs.value {
            Self::zero()
        } else {
            let value = self.value.clone().abs_diff(rhs.value.clone());
            Self {
                sign: std::cmp::max_by(self, rhs, |a, b| a.value.cmp(&b.value)).sign,
                value,
            }
        }
    }
}
