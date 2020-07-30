use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub(crate) enum Type {
    Bool,
    Int,
    Arr(Rc<Type>, Rc<Type>),
    Tuple(Vec<Rc<Type>>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Arr(from, to) => write!(f, "({} -> {})", from, to),
            Type::Tuple(types) => match types.as_slice() {
                [] => write!(f, "()"),
                [ty] => write!(f, "({},)", ty),
                types => {
                    write!(f, "(")?;
                    for ty in &types[..types.len() - 1] {
                        write!(f, "{}, ", ty)?;
                    }
                    write!(f, "{})", types.last().unwrap())
                }
            },
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Int, Type::Int) => true,
            (Type::Arr(l1, l2), Type::Arr(r1, r2)) => (l1 == r1) && (l2 == r2),
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(ty1, ty2)| ty1 == ty2)
            }
            _ => false,
        }
    }
}
