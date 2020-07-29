use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub(crate) enum Type {
    Bool,
    Arr(Rc<Type>, Rc<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Arr(from, to) => write!(f, "({} -> {})", from, to),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Arr(l1, l2), Type::Arr(r1, r2)) => (l1 == r1) && (l2 == r2),
            _ => false,
        }
    }
}
