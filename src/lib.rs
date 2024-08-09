use std::{
    borrow::Borrow,
    cmp::{self, Ordering},
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Shl, Shr},
    slice::SliceIndex,
};

#[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct BigUint {
    value: Vec<u8>,
}

impl BigUint {
    pub fn new() -> Self {
        BigUint { value: vec![0] }
    }

    fn none() -> Self {
        BigUint { value: Vec::new() }
    }

    fn push(&mut self, value: u8) {
        self.value.push(value)
    }

    fn get<I: SliceIndex<[u8]>>(&self, index: I) -> Option<&<I as SliceIndex<[u8]>>::Output> {
        self.value.get(index)
    }

    fn get_mut<I: SliceIndex<[u8]>>(
        &mut self,
        index: I,
    ) -> Option<&mut <I as SliceIndex<[u8]>>::Output> {
        self.value.get_mut(index)
    }
}

impl From<u8> for BigUint {
    fn from(value: u8) -> Self {
        BigUint { value: vec![value] }
    }
}

macro_rules! impl_from_primitive {
    ($type: ty) => {
        impl From<$type> for BigUint {
            fn from(value: $type) -> BigUint {
                let mut result = BigUint::none();

                for i in value.to_be_bytes() {
                    result.push(i);
                }

                result
            }
        }
    };
}

impl_from_primitive!(u16);

impl_from_primitive!(u32);

impl_from_primitive!(u64);

impl_from_primitive!(u128);

impl_from_primitive!(usize);

impl AsRef<Vec<u8>> for BigUint {
    fn as_ref(&self) -> &Vec<u8> {
        &self.value
    }
}

impl Borrow<Vec<u8>> for BigUint {
    fn borrow(&self) -> &Vec<u8> {
        &self.value
    }
}

macro_rules! impl_bit_ops {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for BigUint {
            type Output = Self;

            fn $method(self, rhs: Self) -> Self::Output {
                let mut new = BigUint::none();

                for i in 0..cmp::max(self.value.len(), rhs.value.len()) {
                    new.push(self.get(i).unwrap_or(&0) $op rhs.get(i).unwrap_or(&0));
                }

                new
            }
        }
    };
}

impl_bit_ops!(BitAnd, bitand, &);

impl_bit_ops!(BitOr, bitor, |);

impl_bit_ops!(BitXor, bitxor, ^);

macro_rules! impl_bit_assign_ops {
    ($trait:ty, $method:ident, $op: tt) => {
        impl $trait for BigUint {
            fn $method(&mut self, rhs: Self) {
                let change = self.clone() $op rhs;

                *self = change;
            }
        }
    };
}

impl_bit_assign_ops!(BitAndAssign, bitand_assign, &);

impl_bit_assign_ops!(BitOrAssign, bitor_assign, |);

impl_bit_assign_ops!(BitXorAssign, bitxor_assign, ^);

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
                    match s.cmp(o) {
                        Ordering::Equal => {}
                        order => return order,
                    }
                }

                Ordering::Equal
            }
            order => order,
        }
    }
}
