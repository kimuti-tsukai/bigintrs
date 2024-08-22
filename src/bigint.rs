use std::{
    cmp::Ordering,
    fmt::{Binary, Display, LowerHex, Octal, UpperHex},
    num::IntErrorKind,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign},
    str::FromStr,
};

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

macro_rules! impl_assign_ops {
    ($($trait: ty, $assign_method: ident, $method: ident);+) => {$(
        impl $trait for BigInt {
            fn $assign_method(&mut self, rhs: Self) {
                *self = self.clone().$method(rhs);
            }
        }
    )+};
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

impl PartialOrd for Sign {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sign {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as i8).cmp(&(*other as i8))
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

    pub fn from_str_radix(src: &str, radix: u32) -> Result<Self, IntErrorKind> {
        if !(2..=36).contains(&radix) {
            panic!(
                "from_str_radix_int: must lie in the range `[2, 36]` - found {}",
                radix
            );
        }

        if src.is_empty() {
            return Err(IntErrorKind::Empty);
        }

        let src: &[u8] = src.as_bytes();

        let (is_positive, mut digits): (bool, &[u8]) = match src {
            [b'+' | b'-'] => return Err(IntErrorKind::InvalidDigit),
            [b'+', rest @ ..] => (true, rest),
            [b'-', rest @ ..] => (false, rest),
            _ => (true, src),
        };

        let mut result: BigInt = Self::zero();

        macro_rules! c_loop {
            ($assign_op: tt) => {
                while let [c, rest @ ..] = digits {
                    result *= Self::from(radix);
                    let Some(x) = (*c as char).to_digit(radix) else {
                        return Err(IntErrorKind::InvalidDigit);
                    };
                    result $assign_op Self::from(x);
                    digits = rest;
                }
            };
        }

        if is_positive {
            c_loop!(+=);
        } else {
            c_loop!(-=);
        }

        Ok(result)
    }

    pub fn to_str_radix_lower(self, radix: u32) -> String {
        if !(2..=36).contains(&radix) {
            panic!(
                "from_str_radix_int: must lie in the range `[2, 36]` - found {}",
                radix
            );
        }

        match self.sign {
            Sign::Zero => String::from("0"),
            Sign::Positive => self.value.to_str_radix_lower(radix),
            Sign::Negative => format!("-{}", self.value.to_str_radix_lower(radix)),
        }
    }

    pub fn to_str_radix_upper(self, radix: u32) -> String {
        self.to_str_radix_lower(radix).to_ascii_uppercase()
    }

    pub fn abs(self) -> Self {
        if self.is_negative() {
            -self
        } else {
            self
        }
    }

    pub fn abs_diff(self, rhs: Self) -> Self {
        (self - rhs).abs()
    }

    pub fn div_euclid(self, rhs: Self) -> Self {
        if self.is_negative() && !(&self % &rhs).is_zero() {
            self / rhs + Self::one()
        } else {
            self / rhs
        }
    }

    pub fn rem_euclid(self, rhs: Self) -> Self {
        let rem: BigInt = &self % &rhs;

        if rem.is_negative() {
            rhs + rem
        } else {
            rem
        }
    }

    pub fn pow(self, rhs: u32) -> Self {
        let Self { sign, value }: Self = self;
        let unsigned_pow: BigUint = value.pow(rhs);

        if sign == Sign::Negative && rhs % 2 == 1 {
            -Self::from(unsigned_pow)
        } else {
            Self::from(unsigned_pow)
        }
    }

    pub fn pow_big(self, rhs: BigUint) -> Self {
        let Self { sign, value }: Self = self;
        let unsigned_pow: BigUint = value.pow_big(rhs.clone());

        if sign == Sign::Negative && (rhs % BigUint::from(2u8)).is_one() {
            -Self::from(unsigned_pow)
        } else {
            Self::from(unsigned_pow)
        }
    }

    pub fn checked_ilog(self, base: Self) -> Option<u32> {
        if self.is_negative() {
            None
        } else {
            self.value.checked_ilog(base.value)
        }
    }

    pub fn checked_ilog2(self) -> Option<u32> {
        if self.is_negative() {
            None
        } else {
            self.value.checked_ilog2()
        }
    }

    pub fn checked_ilog10(self) -> Option<u32> {
        if self.is_negative() {
            None
        } else {
            self.value.checked_ilog10()
        }
    }

    pub fn ilog(self, base: Self) -> u32 {
        assert!(
            base >= Self::from(2u8),
            "base of integer logarithm must be at least 2"
        );
        self.checked_ilog(base)
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
    }

    pub fn ilog2(self) -> u32 {
        self.checked_ilog2()
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
    }

    pub fn ilog10(self) -> u32 {
        self.checked_ilog10()
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
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

impl TryFrom<BigInt> for BigUint {
    type Error = IntErrorKind;

    fn try_from(value: BigInt) -> Result<Self, Self::Error> {
        match value.sign {
            Sign::Zero => Ok(BigUint::zero()),
            Sign::Positive => Ok(value.value),
            Sign::Negative => Err(IntErrorKind::NegOverflow),
        }
    }
}

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

macro_rules! impl_try_from_BigInt_for_unsigned {
    ($($type: ty),+) => {$(
        impl TryFrom<BigInt> for $type {
            type Error = IntErrorKind;

            fn try_from(value: BigInt) -> Result<Self, Self::Error> {
                match value.sign {
                    Sign::Zero => Ok(0),
                    Sign::Positive => Self::try_from(value.value),
                    Sign::Negative => Err(IntErrorKind::NegOverflow),
                }
            }
        }
    )+};
}

impl_try_from_BigInt_for_unsigned!(u8, u16, u32, u64, u128, usize);

macro_rules! impl_try_from_BigInt_for_signed {
    ($($type: ty, $unsigned: ty);+) => {$(
        impl TryFrom<BigInt> for $type {
            type Error = IntErrorKind;

            fn try_from(value: BigInt) -> Result<Self, Self::Error> {
                if value < BigInt::from(Self::MIN) {
                    Err(IntErrorKind::NegOverflow)
                } else if BigInt::from(Self::MAX) < value {
                    Err(IntErrorKind::PosOverflow)
                } else {
                    match value.sign {
                        Sign::Zero => Ok(0),
                        Sign::Positive => Ok(<$unsigned>::try_from(value.value).unwrap() as $type),
                        Sign::Negative => Ok(-(<$unsigned>::try_from(value.value).unwrap() as $type)),
                    }
                }
            }
        }
    )+};
}

impl_try_from_BigInt_for_signed!(
    i8, u8;
    i16, u16;
    i32, u32;
    i64, u64;
    i128, u128;
    isize, usize
);

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

impl Neg for &BigInt {
    type Output = BigInt;

    fn neg(self) -> Self::Output {
        -self.clone()
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
            let value: BigUint = self.value.clone().abs_diff(rhs.value.clone());
            Self {
                sign: std::cmp::max_by(self, rhs, |a: &BigInt, b: &BigInt| a.value.cmp(&b.value)).sign,
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

impl_assign_ops!(
    AddAssign, add_assign, add;
    SubAssign, sub_assign, sub;
    MulAssign, mul_assign, mul;
    DivAssign, div_assign, div;
    RemAssign, rem_assign, rem
);

impl_assign_for_ref!(
    AddAssign, add_assign;
    SubAssign, sub_assign;
    MulAssign, mul_assign;
    DivAssign, div_assign;
    RemAssign, rem_assign
);

impl PartialOrd for BigInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BigInt {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sign.cmp(&other.sign).then_with(|| match self.sign {
            Sign::Zero => Ordering::Equal,
            Sign::Positive => self.value.cmp(&other.value),
            Sign::Negative => self.value.cmp(&other.value).reverse(),
        })
    }
}

impl FromStr for BigInt {
    type Err = IntErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        BigInt::from_str_radix(s, 10)
    }
}

macro_rules! impl_fmt_radix_lower {
    ($trait: ty, $radix: expr) => {
        impl $trait for BigInt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let write: String = self.clone().to_str_radix_lower($radix);

                write!(f, "{}", write)
            }
        }
    };
    ($($trait: ty, $radix: expr);+) => {
        $(
            impl_fmt_radix_lower!($trait, $radix);
        )+
    }
}

impl_fmt_radix_lower!(
    Display, 10;
    Binary, 2;
    Octal, 8;
    LowerHex, 16
);

impl UpperHex for BigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let write: String = self.clone().to_str_radix_upper(16);

        write!(f, "{}", write)
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn addition() {
        // 100 + 200 = 300
        assert_eq!(
            BigInt::from(100u32) + BigInt::from(200u32),
            BigInt::from(300u32)
        );

        // -50 + 50 = 0
        assert_eq!(BigInt::from(-50) + BigInt::from(50), BigInt::zero());

        // -100 + (-200) = -300
        assert_eq!(BigInt::from(-100) + BigInt::from(-200), BigInt::from(-300));
    }

    #[test]
    fn subtraction() {
        // 300 - 100 = 200
        assert_eq!(
            BigInt::from(300u32) - BigInt::from(100u32),
            BigInt::from(200u32)
        );

        // -50 - 50 = -100
        assert_eq!(BigInt::from(-50) - BigInt::from(50), BigInt::from(-100));

        // 100 - (-200) = 300
        assert_eq!(BigInt::from(100) - BigInt::from(-200), BigInt::from(300));
    }

    #[test]
    fn multiplication() {
        // 50 * 2 = 100
        assert_eq!(
            BigInt::from(50u32) * BigInt::from(2u32),
            BigInt::from(100u32)
        );

        // -50 * 2 = -100
        assert_eq!(BigInt::from(-50) * BigInt::from(2), BigInt::from(-100));

        // -50 * (-2) = 100
        assert_eq!(BigInt::from(-50) * BigInt::from(-2), BigInt::from(100));
    }

    #[test]
    fn division() {
        // 100 / 2 = 50
        assert_eq!(
            BigInt::from(100u32) / BigInt::from(2u32),
            BigInt::from(50u32)
        );

        // -100 / 2 = -50
        assert_eq!(BigInt::from(-100) / BigInt::from(2), BigInt::from(-50));

        // -100 / -2 = 50
        assert_eq!(BigInt::from(-100) / BigInt::from(-2), BigInt::from(50));
    }

    #[test]
    fn remainder() {
        // 100 % 3 = 1
        assert_eq!(
            BigInt::from(100u32) % BigInt::from(3u32),
            BigInt::from(1u32)
        );

        // -100 % 3 = -1
        assert_eq!(BigInt::from(-100) % BigInt::from(3), BigInt::from(-1));

        // 100 % -3 = 1
        assert_eq!(BigInt::from(100) % BigInt::from(-3), BigInt::from(1));

        // -100 % -3 = -1
        assert_eq!(BigInt::from(-100) % BigInt::from(-3), BigInt::from(-1));
    }

    #[test]
    fn from_str_radix() {
        // 10進数の文字列を BigInt に変換
        assert_eq!(
            BigInt::from_str_radix("1234567890", 10).unwrap(),
            BigInt::from(1234567890u32)
        );

        // 16進数の文字列を BigInt に変換 (小文字)
        assert_eq!(
            BigInt::from_str_radix("abcdef", 16).unwrap(),
            BigInt::from(0xabcdefu32)
        );

        // 16進数の文字列を BigInt に変換 (大文字)
        assert_eq!(
            BigInt::from_str_radix("ABCDEF", 16).unwrap(),
            BigInt::from(0xABCDEFu32)
        );

        // 2進数の文字列を BigInt に変換
        assert_eq!(
            BigInt::from_str_radix("101010", 2).unwrap(),
            BigInt::from(42u32)
        );

        // 8進数の文字列を BigInt に変換
        assert_eq!(
            BigInt::from_str_radix("1234567", 8).unwrap(),
            BigInt::from(342391u32)
        );

        // 負の数の16進数の文字列を BigInt に変換
        assert_eq!(
            BigInt::from_str_radix("-ABCDEF", 16).unwrap(),
            BigInt::from(-0xABCDEFi32)
        );

        // 無効な文字列のパース (基数外の文字)
        assert!(BigInt::from_str_radix("GHIJKL", 16).is_err());
    }

    #[test]
    fn to_str_radix_lower() {
        // 10進数での出力
        assert_eq!(
            BigInt::from(1234567890u32).to_str_radix_lower(10),
            "1234567890"
        );

        // 16進数での出力 (小文字)
        assert_eq!(BigInt::from(0xabcdefu32).to_str_radix_lower(16), "abcdef");

        // 2進数での出力
        assert_eq!(BigInt::from(42u32).to_str_radix_lower(2), "101010");

        // 8進数での出力
        assert_eq!(BigInt::from(342391u32).to_str_radix_lower(8), "1234567");

        // 負の数の16進数での出力 (小文字)
        assert_eq!(BigInt::from(-0xabcdefi32).to_str_radix_lower(16), "-abcdef");
    }

    #[test]
    fn to_str_radix_upper() {
        // 10進数での出力
        assert_eq!(
            BigInt::from(1234567890u32).to_str_radix_upper(10),
            "1234567890"
        );

        // 16進数での出力 (大文字)
        assert_eq!(BigInt::from(0xabcdefu32).to_str_radix_upper(16), "ABCDEF");

        // 2進数での出力
        assert_eq!(BigInt::from(42u32).to_str_radix_upper(2), "101010");

        // 8進数での出力
        assert_eq!(BigInt::from(342391u32).to_str_radix_upper(8), "1234567");

        // 負の数の16進数での出力 (大文字)
        assert_eq!(BigInt::from(-0xabcdefi32).to_str_radix_upper(16), "-ABCDEF");
    }

    #[test]
    fn ord() {
        // 大きな正の数と小さな正の数
        assert_eq!(
            BigInt::from(1000u32).cmp(&BigInt::from(100u32)),
            std::cmp::Ordering::Greater
        );

        // 同じ値の比較
        assert_eq!(
            BigInt::from(500u32).cmp(&BigInt::from(500u32)),
            std::cmp::Ordering::Equal
        );

        // 小さな正の数と大きな正の数
        assert_eq!(
            BigInt::from(100u32).cmp(&BigInt::from(1000u32)),
            std::cmp::Ordering::Less
        );

        // 負の数と正の数
        assert_eq!(
            BigInt::from(-100).cmp(&BigInt::from(100)),
            std::cmp::Ordering::Less
        );

        // 同じ負の数
        assert_eq!(
            BigInt::from(-500).cmp(&BigInt::from(-500)),
            std::cmp::Ordering::Equal
        );

        // 負の数同士の比較（絶対値が大きい方が小さいと評価される）
        assert_eq!(
            BigInt::from(-1000).cmp(&BigInt::from(-100)),
            std::cmp::Ordering::Less
        );

        // 0と正の数
        assert_eq!(
            BigInt::zero().cmp(&BigInt::from(100)),
            std::cmp::Ordering::Less
        );

        // 0と負の数
        assert_eq!(
            BigInt::zero().cmp(&BigInt::from(-100)),
            std::cmp::Ordering::Greater
        );

        // 0と0
        assert_eq!(
            BigInt::zero().cmp(&BigInt::zero()),
            std::cmp::Ordering::Equal
        );
    }
}
