use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub(crate) enum Type<'src> {
    Bool,
    Int,
    Arr(Rc<Type<'src>>, Rc<Type<'src>>),
    Tuple(Vec<Rc<Type<'src>>>),
    Record(Vec<(&'src str, Rc<Type<'src>>)>),
}

impl<'src> fmt::Display for Type<'src> {
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
            Type::Record(record) => {
                write!(f, "{{ ")?;
                for (id, ty) in &record[..record.len() - 1] {
                    write!(f, "{}: {}, ", id, ty)?;
                }
                if let Some((id, ty)) = record.last() {
                    write!(f, "{}: {} ", id, ty)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl<'src> PartialEq for Type<'src> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Int, Type::Int) => true,
            (Type::Arr(l1, l2), Type::Arr(r1, r2)) => (l1 == r1) && (l2 == r2),
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(ty1, ty2)| ty1 == ty2)
            }
            (Type::Record(r1), Type::Record(r2)) => {
                r1.len() == r2.len()
                    && r1
                        .iter()
                        .zip(r2.iter())
                        .all(|((id1, ty1), (id2, ty2))| id1 == id2 && ty1 == ty2)
            }
            _ => false,
        }
    }
}
