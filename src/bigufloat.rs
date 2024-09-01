use crate::BigUint;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct BigUfloat {
    value: BigUint,
    prec: BigUint
}

impl Default for BigUfloat {
    fn default() -> Self {
        Self::new()
    }
}

impl BigUfloat {
    pub fn new() -> Self {
        Self {
            value: BigUint::new(),
            prec: BigUint::from(50u8)
        }
    }
}
