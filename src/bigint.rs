use crate::BigUint;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BigInt {
    sign: bool,
    value: BigUint
}
