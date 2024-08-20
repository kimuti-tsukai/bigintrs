use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use crate::BigUint;

macro_rules! impl_for_ref_to_ref {
    ($trait: ty, $method: ident) => {
        impl $trait for &BigInt {
            type Output = BigInt;

            fn $method(self, rhs: Self) -> Self::Output {
                self.clone().$method(rhs.clone())
            }
        }
    };
    ($($trait: ty, $method: ident);+) => {
        $(
            impl_for_ref_to_ref!($trait, $method);
        )+
    };
}

macro_rules! impl_for_owned_to_ref {
    ($trait: tt, $method: ident) => {
        impl $trait<&Self> for BigInt {
            type Output = BigInt;

            fn $method(self, rhs: &Self) -> Self::Output {
                self.$method(rhs.clone())
            }
        }
    };
    ($($trait: tt, $method: ident);+) => {
        $(
            impl_for_owned_to_ref!($trait, $method);
        )+
    };
}

macro_rules! impl_for_ref_to_owned {
    ($trait: tt, $method: ident) => {
        impl $trait<BigInt> for &BigInt {
            type Output = BigInt;

            fn $method(self, rhs: BigInt) -> Self::Output {
                self.clone().$method(rhs)
            }
        }
    };
    ($($trait: tt, $method: ident);+) => {
        $(
            impl_for_ref_to_owned!($trait, $method);
        )+
    };
}

macro_rules! impl_assign_for_ref {
    ($trait: tt, $method: ident) => {
        impl $trait<&Self> for BigInt {
            fn $method(&mut self, rhs: &Self) {
                self.$method(rhs.clone())
            }
        }
    };
    ($($trait: tt, $method: ident);+) => {
        $(
            impl_assign_for_ref!($trait, $method);
        )+
    };
}

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

impl Sub for BigInt {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + (-rhs)
    }
}

impl Mul for BigInt {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_zero() || rhs.is_zero() {
            Self::zero()
        } else {
            Self {
                sign: if self.sign == rhs.sign {
                    Sign::Positive
                } else {
                    Sign::Negative
                },
                value: &self.value * &rhs.value,
            }
        }
    }
}

impl Div for BigInt {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            panic!("attempt to divide by zero");
        }

        if self.is_zero() {
            Self::zero()
        } else {
            Self {
                sign: if self.sign == rhs.sign {
                    Sign::Positive
                } else {
                    Sign::Negative
                },
                value: &self.value / &rhs.value,
            }
        }
    }
}

impl Rem for BigInt {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        &self - (&self / &rhs) * &rhs
    }
}

impl_for_ref_to_ref!(
    Add, add;
    Sub, sub;
    Mul, mul;
    Div, div;
    Rem, rem
);

impl_for_owned_to_ref!(
    Add, add;
    Sub, sub;
    Mul, mul;
    Div, div;
    Rem, rem
);

impl_for_ref_to_owned!(
    Add, add;
    Sub, sub;
    Mul, mul;
    Div, div;
    Rem, rem
);
