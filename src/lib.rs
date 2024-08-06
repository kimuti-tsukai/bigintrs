use std::{
    borrow::Borrow,
    cmp,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign},
    slice::SliceIndex,
};

#[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct BigUint {
    value: Vec<u8>,
}

impl BigUint {
    pub fn new() -> Self {
        BigUint { value: Vec::new() }
    }

    fn new_with_bytes(b: usize) -> Self {
        BigUint { value: vec![0; b] }
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
                let mut new = BigUint::new();

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

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;
}
