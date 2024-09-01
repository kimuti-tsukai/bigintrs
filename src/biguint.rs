use std::{
    borrow::{Borrow, BorrowMut},
    cmp::{self, Ordering},
    fmt::{Binary, Display, LowerHex, Octal, UpperHex},
    num::IntErrorKind,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Div,
        DivAssign, Mul, MulAssign, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
    },
    slice::Iter,
    str::FromStr,
};

#[allow(unused_macros)]
macro_rules! dbg_biguint {
    ($i: expr) => {{
        let r = $i;

        eprint!("{:<43}", format!("[ {} ] =", stringify!($i)));
        for i in &r.value {
            eprint!("{:0>8b} ", i);
        }
        eprintln!();
        eprintln!();
        r
    }};
}

trait CastUnsigned {
    type Output;

    fn my_cast_unsigned(self) -> Self::Output;
}

macro_rules! impl_cast_unsigned {
    ($from: ty, $to: ty) => {
        impl CastUnsigned for $from {
            type Output = $to;

            fn my_cast_unsigned(self) -> Self::Output {
                self as $to
            }
        }
    };
    ($(($from: ty, $to: ty)),+) => {
        $(
            impl_cast_unsigned!($from, $to);
        )+
    }
}

impl_cast_unsigned!(
    (i8, u8),
    (i16, u16),
    (i32, u32),
    (i64, u64),
    (i128, u128),
    (isize, usize)
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BigUint {
    value: Vec<u8>,
}

impl BigUint {
    pub const ZERO: Self = Self::zero();

    pub const fn new() -> Self {
        BigUint { value: Vec::new() }
    }

    pub const fn min_value() -> Self {
        BigUint::zero()
    }

    pub const fn zero() -> Self {
        BigUint::new()
    }

    pub fn is_zero(&self) -> bool {
        *self == Self::zero()
    }

    pub fn set_zero(&mut self) {
        *self = BigUint::zero()
    }

    pub fn one() -> Self {
        BigUint::from(1u8)
    }

    pub fn is_one(&self) -> bool {
        *self == BigUint::one()
    }

    pub fn set_one(&mut self) {
        *self = BigUint::one()
    }

    fn bytes(&self) -> u32 {
        self.value.len() as u32
    }

    fn bits(&self) -> u32 {
        self.bytes() * 8
    }

    pub fn valid_bits(&self) -> u32 {
        if let Some(first) = self.value.first() {
            self.bits() - first.leading_zeros()
        } else {
            0
        }
    }

    pub fn count_ones(&self) -> usize {
        let mut result: usize = 0;

        for i in &self.value {
            result += i.count_ones() as usize;
        }

        result
    }

    pub fn long_mul(self, mut rhs: Self) -> Self {
        let mut result: BigUint = BigUint::new();

        for i in 0..rhs.valid_bits() {
            if rhs.value.last().unwrap() & 1 == 1 {
                result += &self << i;
            }

            rhs >>= 1u8;
        }

        result
    }

    pub fn karatsuba_mul(self, rhs: Self) -> Self {
        self._karatsuba_mul(&rhs)
    }

    fn _karatsuba_mul(&self, other: &BigUint) -> Self {
        let n: u32 = self.valid_bits().max(other.valid_bits());

        if n <= 32 {
            return self.clone().long_mul(other.clone());
        }

        let m: u32 = n / 2;

        let (x1, x0): (BigUint, BigUint) = self.split(m);
        let (y1, y0): (BigUint, BigUint) = other.split(m);

        let z2: BigUint = x1._karatsuba_mul(&y1);
        let z0: BigUint = x0._karatsuba_mul(&y0);
        let z1_f: BigUint = (x1 + x0)._karatsuba_mul(&(y1 + y0));
        let z1_s: BigUint = &z2 + &z0;

        if z1_f >= z1_s {
            let z1: BigUint = z1_f - z1_s;
            (z2 << (m << 1)) + z0 + (z1 << m)
        } else {
            let z1: BigUint = z1_s - z1_f;
            (z2 << (m << 1)) + z0 - (z1 << m)
        }
    }

    fn split(&self, mid: u32) -> (BigUint, BigUint) {
        let low_part: BigUint = self >> mid;
        let high_part: BigUint = self - (&low_part << mid);
        (low_part, high_part)
    }

    pub fn pow(self, rhs: u32) -> Self {
        if rhs == 0 {
            BigUint::one()
        } else if rhs % 2 == 1 {
            &self * self.clone().pow(rhs - 1)
        } else {
            let a: BigUint = self.pow(rhs / 2);
            &a * &a
        }
    }

    pub fn pow_big(self, rhs: Self) -> Self {
        if rhs.is_zero() {
            BigUint::one()
        } else if (&rhs % BigUint::from(2u8)).is_one() {
            &self * self.clone().pow_big(rhs - BigUint::one())
        } else {
            let a: BigUint = self.pow_big(rhs / BigUint::from(2u8));
            &a * &a
        }
    }

    const fn none() -> Self {
        BigUint { value: Vec::new() }
    }

    fn push(&mut self, value: u8) {
        self.value.push(value)
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

        if !is_positive {
            return Err(IntErrorKind::NegOverflow);
        }

        let mut result: BigUint = BigUint::zero();

        while let [c, rest @ ..] = digits {
            result *= BigUint::from(radix);
            let Some(x) = (*c as char).to_digit(radix) else {
                return Err(IntErrorKind::InvalidDigit);
            };
            result += BigUint::from(x);
            digits = rest;
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

        let mut chars: Vec<char> = Vec::new();

        let mut src: BigUint = self;

        while !src.is_zero() {
            let push: char =
                char::from_digit((&src % BigUint::from(radix)).try_into().unwrap(), radix).unwrap();

            chars.push(push);

            println!("{:?}", &chars);

            src /= BigUint::from(radix);
        }

        if chars.is_empty() {
            String::from("0")
        } else {
            chars.into_iter().rev().collect()
        }
    }

    pub fn to_str_radix_upper(self, radix: u32) -> String {
        self.to_str_radix_lower(radix).to_ascii_uppercase()
    }

    pub fn normal_div(mut self, rhs: Self) -> Self {
        assert!(!rhs.is_zero(), "attempt to divide by zero");

        let mut result: BigUint = BigUint::zero();

        while self >= rhs {
            let mut add: BigUint = BigUint::one();
            let mut sub: BigUint = rhs.clone();

            while self >= sub {
                self -= &sub;

                result += &add;

                sub <<= 1u8;
                add <<= 1u8;
            }
        }

        result
    }

    pub fn binary_div(self, rhs: Self) -> Self {
        assert!(!rhs.is_zero(), "attempt to divide by zero");

        if rhs.is_one() {
            return self;
        }

        let (mut l, mut r) = (Self::zero(), self.clone());

        while &r - &l > Self::one() {
            let next = (&l + &r) >> 1;

            if &next * &rhs <= self {
                l = next;
            } else {
                r = next
            }
        }

        l
    }

    pub fn div_ceil(self, rhs: Self) -> Self {
        if (&self % &rhs).is_zero() {
            self / rhs
        } else {
            self / rhs + BigUint::one()
        }
    }

    pub fn div_euclid(self, rhs: Self) -> Self {
        self / rhs
    }

    pub fn rem_euclid(self, rhs: Self) -> Self {
        self % rhs
    }

    pub fn increment(&mut self) -> &Self {
        *self += BigUint::one();

        &*self
    }

    pub fn decrement(&mut self) -> &Self {
        *self -= BigUint::one();

        &*self
    }

    pub fn abs_diff(self, rhs: Self) -> Self {
        if self > rhs {
            self - rhs
        } else {
            rhs - self
        }
    }

    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        Some(self + rhs)
    }

    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        if rhs > self {
            None
        } else {
            Some(self - rhs)
        }
    }

    pub fn checked_mul(self, rhs: Self) -> Option<Self> {
        Some(self * rhs)
    }

    pub fn checked_div(self, rhs: Self) -> Option<Self> {
        Some(self / rhs)
    }

    pub fn checked_neg(self) -> Option<Self> {
        if self.is_zero() {
            Some(self)
        } else {
            None
        }
    }

    pub fn checked_pow(self, rhs: u32) -> Option<Self> {
        Some(self.pow(rhs))
    }

    pub fn checked_pow_big(self, rhs: Self) -> Option<Self> {
        Some(self.pow_big(rhs))
    }

    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        Some(self % rhs)
    }

    pub fn checked_div_euclid(self, rhs: Self) -> Option<Self> {
        Some(self.div_euclid(rhs))
    }

    pub fn checked_rem_euclid(self, rhs: Self) -> Option<Self> {
        Some(self.rem_euclid(rhs))
    }

    pub fn checked_shr(self, rhs: u32) -> Option<Self> {
        Some(self >> rhs)
    }

    pub fn checked_shl(self, rhs: u32) -> Option<Self> {
        Some(self << rhs)
    }

    fn swap_bytes(self) -> Self {
        BigUint {
            value: self
                .value
                .into_iter()
                .rev()
                .skip_while(|v: &u8| *v == 0)
                .collect(),
        }
    }

    pub fn from_be(self) -> Self {
        self
    }

    pub fn from_be_bytes(bytes: &[u8]) -> Self {
        let mut result: BigUint = BigUint::none();

        for i in bytes.iter().skip_while(|&v: &&u8| *v == 0) {
            result.push(*i)
        }

        result
    }

    pub fn from_le(self) -> Self {
        self.swap_bytes()
    }

    pub fn from_le_bytes(bytes: &[u8]) -> Self {
        let mut result: BigUint = BigUint::none();

        for i in bytes.iter().rev().skip_while(|&v: &&u8| *v == 0) {
            result.push(*i)
        }

        result
    }

    pub fn from_ne_bytes(bytes: &[u8]) -> Self {
        if cfg!(target_endian = "big") {
            BigUint::from_be_bytes(bytes)
        } else {
            BigUint::from_le_bytes(bytes)
        }
    }

    pub fn to_be(self) -> Self {
        self
    }

    pub fn to_be_bytes(self) -> Box<[u8]> {
        self.value.as_slice().into()
    }

    pub fn to_be_bytes_vec(self) -> Vec<u8> {
        self.value
    }

    pub fn to_le(self) -> Self {
        self.swap_bytes()
    }

    pub fn to_le_bytes(self) -> Box<[u8]> {
        let mut vec: Vec<u8> = self.value;
        vec.reverse();

        vec.as_slice().into()
    }

    pub fn to_le_bytes_vec(self) -> Vec<u8> {
        let mut vec: Vec<u8> = self.value;
        vec.reverse();

        vec
    }

    pub fn to_ne_bytes(self) -> Box<[u8]> {
        if cfg!(target_endian = "big") {
            self.to_be_bytes()
        } else {
            self.to_le_bytes()
        }
    }

    pub fn to_ne_bytes_vec(self) -> Vec<u8> {
        if cfg!(target_endian = "big") {
            self.to_be_bytes_vec()
        } else {
            self.to_le_bytes_vec()
        }
    }

    pub fn leading_ones(&self) -> u32 {
        let mut result: u32 = 0;

        let mut it: Iter<u8> = self.value.iter();
        while it.next().unwrap_or(&0).leading_ones() == 8 {
            result += 8;
        }

        result += if let Some(v) = it.next() {
            v.leading_ones()
        } else {
            0
        };

        result
    }

    pub fn trailing_ones(&self) -> u32 {
        let mut result: u32 = 0;

        let mut it: Iter<u8> = self.value.iter();
        while it.next_back().unwrap_or(&0).trailing_ones() == 8 {
            result += 8;
        }

        result += if let Some(v) = it.next() {
            v.trailing_ones()
        } else {
            0
        };

        result
    }

    pub fn trailing_zeros(&self) -> u32 {
        assert!(!self.is_zero(), "trailing zero is infinity");

        let mut result: u32 = 0;

        let mut it: Iter<u8> = self.value.iter();
        while it.next_back().unwrap_or(&0).trailing_zeros() == 8 {
            result += 8;
        }

        result += if let Some(v) = it.next() {
            v.trailing_zeros()
        } else {
            0
        };

        result
    }

    pub fn checked_ilog2(self) -> Option<u32> {
        if self.is_zero() {
            None
        } else {
            Some(self.valid_bits())
        }
    }

    pub fn checked_ilog(self, base: Self) -> Option<u32> {
        if self.is_zero() || base < Self::one() {
            None
        } else if self < base {
            Some(0)
        } else {
            let mut pow: BigUint = Self::one();

            let mut counter: u32 = 0;

            if self.bytes() >= 16 {
                counter = self.clone().ilog2() / (base.clone().ilog2() + 1)
            }

            while pow <= &self / &base {
                pow *= &base;
                counter += 1;
            }

            Some(counter)
        }
    }

    pub fn checked_ilog10(self) -> Option<u32> {
        self.checked_ilog(Self::from(10u8))
    }

    pub fn ilog2(self) -> u32 {
        self.checked_ilog2()
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
    }

    pub fn ilog(self, base: Self) -> u32 {
        assert!(
            base >= Self::from(2u8),
            "base of integer logarithm must be at least 2"
        );
        self.checked_ilog(base)
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
    }

    pub fn ilog10(self) -> u32 {
        self.checked_ilog10()
            .unwrap_or_else(|| panic!("argument of integer logarithm must be positive"))
    }

    pub fn is_power_of_two(&self) -> bool {
        self.count_ones() == 1
    }

    pub fn next_power_of_two(self) -> Self {
        if self.is_power_of_two() {
            self
        } else {
            Self::one() << (self.valid_bits() + 1)
        }
    }

    pub fn checked_next_power_of_two(self) -> Option<Self> {
        Some(self.next_power_of_two())
    }

    pub fn saturating_add(self, rhs: Self) -> Self {
        self + rhs
    }

    pub fn saturating_sub(self, rhs: Self) -> Self {
        self.checked_sub(rhs).unwrap_or_default()
    }

    pub fn saturating_mul(self, rhs: Self) -> Self {
        self * rhs
    }

    pub fn saturating_div(self, rhs: Self) -> Self {
        self / rhs
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        self.pow(exp)
    }
}

macro_rules! impl_from_unsigned_int {
    ($type: ty) => {
        impl From<$type> for BigUint {
            fn from(value: $type) -> BigUint {
                let mut result: BigUint = BigUint::none();

                for i in value.to_be_bytes().into_iter().skip_while(|v: &u8| *v == 0) {
                    result.push(i);
                }

                result
            }
        }
    };
    ($($type: ty),+) => {
        $(
            impl_from_unsigned_int!($type);
        )+
    };
}

impl_from_unsigned_int!(u8, u16, u32, u64, u128, usize);

macro_rules! impl_try_from_signed_int {
    ($type: ty) => {
        impl TryFrom<$type> for BigUint {
            type Error = IntErrorKind;

            fn try_from(value: $type) -> Result<Self, Self::Error> {
                if value < 0 {
                    return Err(IntErrorKind::NegOverflow);
                }

                Ok(BigUint::from(value.my_cast_unsigned()))
            }
        }
    };
    ($($type: ty),+) => {
        $(
            impl_try_from_signed_int!($type);
        )+
    };
}

impl_try_from_signed_int!(i8, i16, i32, i64, i128, isize);

macro_rules! impl_try_from_BigUint_for_unsigned {
    ($type: ty) => {
        impl TryFrom<BigUint> for $type {
            type Error = IntErrorKind;

            fn try_from(value: BigUint) -> Result<Self, Self::Error> {
                if value.bytes() > <$type>::BITS / 8 {
                    Err(IntErrorKind::PosOverflow)
                } else {
                    let mut array = [0; (<$type>::BITS / 8) as usize];

                    for (index,i) in value.value.into_iter().rev().enumerate() {
                        array[index] = i;
                    }

                    Ok(<$type>::from_le_bytes(array))
                }
            }
        }
    };
    ($($type: ty),+) => {
        $(
            impl_try_from_BigUint_for_unsigned!($type);
        )+
    };
}

impl_try_from_BigUint_for_unsigned!(u8, u16, u32, u64, u128, usize);

impl AsRef<Self> for BigUint {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsMut<Self> for BigUint {
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

impl<T> AsRef<T> for BigUint
where
    Vec<u8>: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        self.value.as_ref()
    }
}

impl<T> AsMut<T> for BigUint
where
    Vec<u8>: AsMut<T>,
{
    fn as_mut(&mut self) -> &mut T {
        self.value.as_mut()
    }
}

impl<T> Borrow<T> for BigUint
where
    Vec<u8>: Borrow<T>,
{
    fn borrow(&self) -> &T {
        self.value.borrow()
    }
}

impl<T> BorrowMut<T> for BigUint
where
    Vec<u8>: BorrowMut<T>,
{
    fn borrow_mut(&mut self) -> &mut T {
        self.value.borrow_mut()
    }
}

macro_rules! impl_for_ref_to_ref {
    ($trait: ty, $method: ident) => {
        impl $trait for &BigUint {
            type Output = BigUint;

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
        impl $trait<&Self> for BigUint {
            type Output = BigUint;

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
        impl $trait<BigUint> for &BigUint {
            type Output = BigUint;

            fn $method(self, rhs: BigUint) -> Self::Output {
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
        impl $trait<&Self> for BigUint {
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

macro_rules! impl_bit_ops {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for BigUint {
            type Output = Self;

            fn $method(mut self, mut rhs: Self) -> Self::Output {
                let mut new: Vec<u8> = Vec::new();

                for _ in 0..cmp::max(self.value.len(), rhs.value.len()) {
                    let push: u8 = self.value.pop().unwrap_or_default() $op rhs.value.pop().unwrap_or_default();

                    new.push(push);
                }

                let result: Vec<u8> = new.into_iter().rev().skip_while(|v: &u8| *v == 0).collect();

                BigUint { value: result }
            }
        }
    };
    ($($trait: ty, $method: ident, $op: tt);+) => {
        $(
            impl_bit_ops!($trait, $method, $op);
        )+
    };
}

impl_bit_ops!(
    BitAnd, bitand, &;
    BitOr, bitor, |;
    BitXor, bitxor, ^
);

impl_for_ref_to_ref!(
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor
);

impl_for_owned_to_ref!(
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor
);

impl_for_ref_to_owned!(
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor
);

macro_rules! impl_bit_assign_ops {
    ($trait:ty, $method:ident, $op: tt) => {
        impl $trait for BigUint {
            fn $method(&mut self, rhs: Self) {
                let change: BigUint = self.clone() $op rhs;

                *self = change;
            }
        }
    };
    ($($trait: ty, $method: ident, $op: tt);+) => {
        $(
            impl_bit_assign_ops!($trait, $method, $op);
        )+
    };
}

impl_bit_assign_ops!(
    BitAndAssign, bitand_assign, &;
    BitOrAssign, bitor_assign, |;
    BitXorAssign, bitxor_assign, ^
);

impl_assign_for_ref!(
    BitAndAssign, bitand_assign;
    BitOrAssign, bitor_assign;
    BitXorAssign, bitxor_assign
);

impl PartialOrd for BigUint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BigUint {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.value.len().cmp(&other.value.len()) {
            Ordering::Equal => {
                for (s, o) in self.value.iter().zip(other.value.iter()) {
                    let order: Ordering = s.cmp(o);
                    if order != Ordering::Equal {
                        return order;
                    }
                }

                Ordering::Equal
            }
            order => order,
        }
    }
}

macro_rules! impl_shr_and_shl_unsigned {
    ($type: ty) => {
        impl Shr<$type> for BigUint {
            type Output = Self;

            fn shr(mut self, mut rhs: $type) -> Self::Output {
                if self.valid_bits() as $type <= rhs {
                    return BigUint::new();
                }

                for _ in 0..rhs/8 {
                    self.value.pop();
                }

                rhs %= 8;

                if rhs == 0 {
                    self
                } else {
                    let mut result: BigUint = BigUint::none();

                    let mut is_firstloop: bool = true;
                    let mut next: u8 = 0;
                    for i in self.value {
                        if is_firstloop {
                            if i >> rhs != 0 {
                                result.push(i >> rhs);
                            }

                            is_firstloop = false;
                        } else {
                            result.push((i >> rhs) + next);
                        }

                        next = (i - ((i >> rhs) << rhs)) << (8 - rhs);
                    }

                    result
                }
            }
        }

        impl Shl<$type> for BigUint {
            type Output = Self;

            fn shl(self, rhs: $type) -> Self::Output {
                if self.is_zero() {
                    return Self::zero()
                }

                let mut result: BigUint = self;

                for _ in 0..rhs.div_ceil(8) {
                    result.push(0);
                }

                if rhs % 8 == 0 {
                    result
                } else {
                    result >> (8 - (rhs % 8))
                }
            }
        }

        impl Shr<$type> for &BigUint {
            type Output = BigUint;

            fn shr(self, rhs: $type) -> Self::Output {
                self.clone() >> rhs
            }
        }

        impl Shl<$type> for &BigUint {
            type Output = BigUint;

            fn shl(self, rhs: $type) -> Self::Output {
                self.clone() << rhs
            }
        }

        impl ShrAssign<$type> for BigUint {
            fn shr_assign(&mut self, rhs: $type) {
                *self = self.clone() >> rhs;
            }
        }

        impl ShlAssign<$type> for BigUint {
            fn shl_assign(&mut self, rhs: $type) {
                *self = self.clone() << rhs;
            }
        }
    };
    ($($type: ty),+) => {
        $(
            impl_shr_and_shl_unsigned!($type);
        )+
    };
}

impl_shr_and_shl_unsigned!(usize, u8, u16, u32, u64, u128);

impl Shr for BigUint {
    type Output = Self;

    fn shr(mut self, rhs: Self) -> Self::Output {
        if BigUint::from(self.valid_bits()) <= rhs {
            return BigUint::new();
        }

        let mut cnt: BigUint = &rhs / BigUint::from(8u8);
        while cnt > BigUint::zero() {
            self.value.pop();
            cnt.decrement();
        }

        let rhs: u8 = u8::try_from(rhs % BigUint::from(8u8)).unwrap();

        if rhs == 0 {
            self
        } else {
            let mut result: BigUint = BigUint::none();

            let mut is_firstloop: bool = true;
            let mut next: u8 = 0;
            for i in self.value {
                if is_firstloop {
                    if i >> rhs != 0 {
                        result.push(i >> rhs);
                    }

                    is_firstloop = false;
                } else {
                    result.push((i >> rhs) + next);
                }

                next = (i - ((i >> rhs) << rhs)) << (8 - rhs);
            }

            result
        }
    }
}

impl Shl for BigUint {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        let mut result: BigUint = self;

        let mut counter: BigUint = rhs.clone().div_ceil(BigUint::from(8u8));
        while counter > BigUint::zero() {
            result.push(0);

            counter.decrement();
        }

        if (&rhs % BigUint::from(8u8)).is_zero() {
            result
        } else {
            result >> (8u8 - u8::try_from(rhs % BigUint::from(8u8)).unwrap())
        }
    }
}

impl_for_ref_to_ref!(
    Shr, shr;
    Shl, shl
);

impl_for_owned_to_ref!(
    Shr, shr;
    Shl, shl
);

impl_for_ref_to_owned!(
    Shr, shr;
    Shl, shl
);

macro_rules! impl_shr_and_shl_signed {
    ($type: ty) => {
        impl Shr<$type> for BigUint {
            type Output = Self;

            fn shr(self, rhs: $type) -> Self::Output {
                assert!(rhs >= 0, "attempt to shift right with overflow");

                self >> rhs.my_cast_unsigned()
            }
        }

        impl Shl<$type> for BigUint {
            type Output = Self;

            fn shl(self, rhs: $type) -> Self::Output {
                assert!(rhs >= 0, "attempt to shift left with overflow");

                self << rhs.my_cast_unsigned()
            }
        }

        impl Shr<$type> for &BigUint {
            type Output = BigUint;

            fn shr(self, rhs: $type) -> Self::Output {
                self.clone() >> rhs
            }
        }

        impl Shl<$type> for &BigUint {
            type Output = BigUint;

            fn shl(self, rhs: $type) -> Self::Output {
                self.clone() << rhs
            }
        }

        impl ShrAssign<$type> for BigUint {
            fn shr_assign(&mut self, rhs: $type) {
                assert!(rhs >= 0, "attempt to shift right with overflow");

                *self >>= rhs.my_cast_unsigned();
            }
        }

        impl ShlAssign<$type> for BigUint {
            fn shl_assign(&mut self, rhs: $type) {
                assert!(rhs >= 0, "attempt to shift left with overflow");

                *self <<= rhs.my_cast_unsigned();
            }
        }
    };
    ($($type: ty),+) => {
        $(
            impl_shr_and_shl_signed!($type);
        )+
    };
}

impl_shr_and_shl_signed!(i8, i16, i32, i64, i128, isize);

impl Add for BigUint {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let xor: BigUint = &self ^ &rhs;

        let carry: BigUint = (&self & &rhs) << 1u8;

        if carry.is_zero() {
            xor
        } else {
            xor + carry
        }
    }
}

impl Sub for BigUint {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        assert!(self >= rhs, "attempt to subtract with overflow");

        let xor: BigUint = &self ^ &rhs;

        let carry: BigUint = ((&self ^ &rhs) & &rhs) << 1u8;

        if carry.is_zero() {
            xor
        } else {
            xor.sub(carry)
        }
    }
}

impl_for_ref_to_ref!(
    Add, add;
    Sub, sub
);

impl_for_owned_to_ref!(
    Add, add;
    Sub, sub
);

impl_for_ref_to_owned!(
    Add, add;
    Sub, sub
);

impl AddAssign for BigUint {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl SubAssign for BigUint {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.clone() - rhs;
    }
}

impl_assign_for_ref!(
    AddAssign, add_assign;
    SubAssign, sub_assign
);

impl Mul for BigUint {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.karatsuba_mul(rhs)
    }
}

impl_for_ref_to_ref!(Mul, mul);

impl_for_owned_to_ref!(Mul, mul);

impl_for_ref_to_owned!(Mul, mul);

impl_bit_assign_ops!(MulAssign, mul_assign, *);

impl_assign_for_ref!(MulAssign, mul_assign);

impl Div for BigUint {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self.binary_div(rhs)
    }
}

impl_for_ref_to_ref!(Div, div);

impl_for_owned_to_ref!(Div, div);

impl_for_ref_to_owned!(Div, div);

impl_bit_assign_ops!(DivAssign, div_assign, /);

impl_assign_for_ref!(DivAssign, div_assign);

impl Rem for BigUint {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        &self - (&rhs * (&self / &rhs))
    }
}

impl_for_ref_to_ref!(Rem, rem);

impl_for_owned_to_ref!(Rem, rem);

impl_for_ref_to_owned!(Rem, rem);

impl_bit_assign_ops!(RemAssign, rem_assign, %);

impl_assign_for_ref!(RemAssign, rem_assign);

impl FromStr for BigUint {
    type Err = IntErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        BigUint::from_str_radix(s, 10)
    }
}

macro_rules! impl_fmt_radix_lower {
    ($trait: ty, $radix: expr) => {
        impl $trait for BigUint {
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

impl UpperHex for BigUint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let write: String = self.clone().to_str_radix_upper(16);

        write!(f, "{}", write)
    }
}

impl Default for BigUint {
    fn default() -> Self {
        BigUint::new()
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn run() {
        dbg_biguint!(BigUint::from(0u32));
    }

    #[test]
    fn valid_bits() {
        assert_eq!(
            BigUint::from(0b_00000000_00000011_11111100_00000000_u32).valid_bits(),
            18
        )
    }

    #[test]
    fn bit_and() {
        assert_eq!(
            BigUint::from(0b_11001100_u8) & BigUint::from(0b_10001110_u8),
            BigUint::from(0b_10001100_u8)
        );

        assert_eq!(
            BigUint::from(0b_11001100_u8) & BigUint::from(0b_11010111_10001110_u16),
            BigUint::from(0b_10001100_u8)
        );
    }

    #[test]
    fn bit_or() {
        assert_eq!(
            BigUint::from(0b_11001100_u8) | BigUint::from(0b_10001110_u8),
            BigUint::from(0b_11001110_u8)
        );

        assert_eq!(
            BigUint::from(0b_11001100_u8) | BigUint::from(0b_11010111_10001110_u16),
            BigUint::from(0b_11010111_11001110_u16)
        );
    }

    #[test]
    fn bit_xor() {
        assert_eq!(
            BigUint::from(0b_11001100_u8) ^ BigUint::from(0b_10001110_u8),
            BigUint::from(0b_01000010_u8)
        );

        assert_eq!(
            BigUint::from(0b_11001100_u8) ^ BigUint::from(0b_11010111_10001110_u16),
            BigUint::from(0b_11010111_01000010_u16)
        );
    }

    #[test]
    fn shift_right() {
        assert_eq!(
            BigUint::from(0b_00000001_u8) >> 1u8,
            BigUint::from(0b_00000000_u8)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_01111111_10000000_u16) >> 3u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_00000000_00000011_11111100_00000000_u32) >> 6u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_00000000_00001111_11110000_00000000_u32) >> 8u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_00000011_11111100_00000000_00000000_u32) >> 14u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_00001111_11110000_00000000_00000000_u32) >> 16u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16),
            BigUint::from(0b_00111111_11000000_00000000_00000000_u32) >> 18u8
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) >> 30,
            BigUint::zero()
        );
    }

    #[test]
    fn shift_left() {
        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 3u8,
            BigUint::from(0b_01111111_10000000_u16)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 6u8,
            BigUint::from(0b_00000000_00000011_11111100_00000000_u32)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 8u8,
            BigUint::from(0b_00000000_00001111_11110000_00000000_u32)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 14u8,
            BigUint::from(0b_00000011_11111100_00000000_00000000_u32)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 16u8,
            BigUint::from(0b_00001111_11110000_00000000_00000000_u32)
        );

        assert_eq!(
            BigUint::from(0b_00001111_11110000_u16) << 18u8,
            BigUint::from(0b_00111111_11000000_00000000_00000000_u32)
        );
    }

    #[test]
    fn ordering() {
        assert!(BigUint::from(100u8) > BigUint::from(30u8));

        assert!(BigUint::from(890u32) > BigUint::from(30u8));

        assert!(BigUint::from(54u8) < BigUint::from(891u32));
    }

    #[test]
    fn add() {
        assert_eq!(BigUint::from(8u8) + BigUint::from(3u8), BigUint::from(11u8));

        assert_eq!(
            BigUint::from(11u8) + BigUint::from(3u8),
            BigUint::from(14u8)
        );

        assert_eq!(
            BigUint::from(11u8) + BigUint::from(25u8),
            BigUint::from(36u8)
        );

        assert_eq!(
            BigUint::from(37u8) + BigUint::from(183u8),
            BigUint::from(220u8)
        );
    }

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn overflow_subtract() {
        let a: BigUint = BigUint::from(891u32);
        let b: BigUint = BigUint::from(54u8);
        let _ = b - a;
    }

    #[test]
    fn subtract() {
        assert_eq!(BigUint::from(8u8) - BigUint::from(3u8), BigUint::from(5u8));

        assert_eq!(
            BigUint::from(531u16) - BigUint::from(238u8),
            BigUint::from(293u16)
        );

        assert_eq!(
            BigUint::from(531u16) - BigUint::from(260u16),
            BigUint::from(271u16)
        );
    }

    #[test]
    fn multiple() {
        assert_eq!(
            BigUint::from(12u8) * BigUint::from(11u8),
            BigUint::from(132u8)
        );

        assert_eq!(
            BigUint::from(100u8) * BigUint::from(1000u16),
            BigUint::from(100000u32)
        );

        assert_eq!(
            BigUint::from(12345u16) * BigUint::from(6789u16),
            BigUint::from(83810205u32)
        );

        assert_eq!(
            BigUint::from(987654321u32) * BigUint::from(123456789u32),
            BigUint::from(121932631112635269u64)
        );

        assert_eq!(BigUint::zero() * BigUint::from(123456u32), BigUint::zero());

        assert_eq!(BigUint::one() * BigUint::one(), BigUint::one());

        assert_eq!(
            BigUint::from_str_radix("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2).unwrap() * BigUint::from(0b11u8),
            BigUint::from_str_radix("11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2).unwrap()
        );

        assert_eq!(
            BigUint::from_str("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000").unwrap() * BigUint::from_str("1000000000000000000000000000000000000000000000000").unwrap(),
            BigUint::from_str("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000").unwrap()
        );
    }

    #[test]
    fn divide() {
        assert_eq!(
            BigUint::from(100000u32) / BigUint::from(1000u16),
            BigUint::from(100u8)
        );

        assert_eq!(
            BigUint::from(83810205u32) / BigUint::from(6789u16),
            BigUint::from(12345u16)
        );

        assert_eq!(
            BigUint::from(121932631112635269u64) / BigUint::from(123456789u32),
            BigUint::from(987654321u32)
        );

        assert_eq!(BigUint::zero() / BigUint::from(123456u32), BigUint::zero());

        assert_eq!(BigUint::one() / BigUint::one(), BigUint::one());

        assert_eq!(
            BigUint::from(123456789012345678900u128) / BigUint::from(10u8),
            BigUint::from(12345678901234567890u64)
        );

        assert_eq!(
            BigUint::from_str_radix("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2).unwrap() / BigUint::from(0b10u8),
            BigUint::from_str_radix("100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2).unwrap()
        );
    }

    #[test]
    fn remainder() {
        assert_eq!(BigUint::from(100u8) % BigUint::from(2u8), BigUint::zero());

        assert_eq!(
            BigUint::from(100000u32) % BigUint::from(1000u16),
            BigUint::zero()
        );

        assert_eq!(
            BigUint::from(83810205u32) % BigUint::from(6789u16),
            BigUint::zero()
        );

        assert_eq!(
            BigUint::from(121932631112635269u64) % BigUint::from(123456789u32),
            BigUint::zero()
        );

        assert_eq!(BigUint::from(100u8) % BigUint::from(3u8), BigUint::one());

        assert_eq!(BigUint::zero() % BigUint::from(123456u32), BigUint::zero());

        assert_eq!(
            BigUint::from(12345678901234567890u64) % BigUint::from(10u8),
            BigUint::zero()
        );

        assert_eq!(
            BigUint::from_str_radix("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001", 2).unwrap() % BigUint::from(0b10u8),
            BigUint::one()
        );
    }

    #[test]
    fn from_str_radix() {
        assert_eq!(
            BigUint::from_str_radix("123", 10).unwrap(),
            BigUint::from(123u8)
        );

        assert_eq!(
            BigUint::from_str_radix("1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2).unwrap(),
            BigUint::one() << (64 * 3)
        );
    }

    #[test]
    fn display() {
        assert_eq!(BigUint::from(102u8).to_string(), String::from("102"));

        assert_eq!(BigUint::from(100000u32).to_string(), String::from("100000"));

        assert_eq!(
            BigUint::from(83810205u32).to_string(),
            String::from("83810205")
        );

        assert_eq!(
            BigUint::from(121932631112635269u64).to_string(),
            String::from("121932631112635269")
        );

        assert_eq!(BigUint::zero().to_string(), String::from("0"));

        assert_eq!(BigUint::one().to_string(), String::from("1"));

        assert_eq!(
            BigUint::from(12345678901234567890u64).to_string(),
            String::from("12345678901234567890")
        );
    }

    #[test]
    fn to_str_radix_lower() {
        // 10進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_lower(10),
            String::from("255")
        );

        // 16進数 (小文字)
        assert_eq!(
            BigUint::from(255u32).to_str_radix_lower(16),
            String::from("ff")
        );

        // 2進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_lower(2),
            String::from("11111111")
        );

        // 8進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_lower(8),
            String::from("377")
        );

        // 大きな数の16進数 (小文字)
        assert_eq!(
            BigUint::from(12345678901234567890u64).to_str_radix_lower(16),
            String::from("ab54a98ceb1f0ad2")
        );

        // 2進数 (小さな数)
        assert_eq!(BigUint::one().to_str_radix_lower(2), String::from("1"));

        // 10進数 (ゼロ)
        assert_eq!(BigUint::zero().to_str_radix_lower(10), String::from("0"));
    }

    #[test]
    fn to_str_radix_upper() {
        // 10進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_upper(10),
            String::from("255")
        );

        // 16進数 (大文字)
        assert_eq!(
            BigUint::from(255u32).to_str_radix_upper(16),
            String::from("FF")
        );

        // 2進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_upper(2),
            String::from("11111111")
        );

        // 8進数
        assert_eq!(
            BigUint::from(255u32).to_str_radix_upper(8),
            String::from("377")
        );

        // 大きな数の16進数 (大文字)
        assert_eq!(
            BigUint::from(12345678901234567890u64).to_str_radix_upper(16),
            String::from("AB54A98CEB1F0AD2")
        );

        // 2進数 (小さな数)
        assert_eq!(BigUint::one().to_str_radix_upper(2), String::from("1"));

        // 10進数 (ゼロ)
        assert_eq!(BigUint::zero().to_str_radix_upper(10), String::from("0"));
    }

    #[test]
    fn power() {
        // 2^3 = 8
        assert_eq!(BigUint::from(2u32).pow(3), BigUint::from(8u32));

        // 5^0 = 1 (ゼロ乗)
        assert_eq!(BigUint::from(5u32).pow(0), BigUint::one());

        // 7^1 = 7 (1乗)
        assert_eq!(BigUint::from(7u32).pow(1), BigUint::from(7u32));

        // 10^5 = 100000
        assert_eq!(BigUint::from(10u32).pow(5), BigUint::from(100000u32));

        // 0^10 = 0 (0のべき乗)
        assert_eq!(BigUint::zero().pow(10), BigUint::zero());

        // 大きな数のべき乗: 2^64
        assert_eq!(
            BigUint::from(2u32).pow(64),
            BigUint::from(18446744073709551616u128)
        );
    }

    #[test]
    fn memory() {
        let _ = BigUint::from(10u8)
            .pow_big(BigUint::from(10u8).pow_big(BigUint::from(10u8).pow_big(BigUint::from(10u8))));
    }
}
