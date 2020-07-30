use std::fmt;
use std::rc::Rc;

use pest::Span;

use crate::types::Type;
use crate::value::Value;

type Scope<'src, T> = im_rc::HashMap<&'src str, Rc<T>>;
type TScope<'src> = Scope<'src, Type>;
type VScope<'src> = Scope<'src, Value<'src>>;

#[derive(Clone, Debug)]
pub(crate) enum Pattern<'src> {
    Any(Span<'src>),
    Id(Span<'src>, &'src str),
    True(Span<'src>),
    False(Span<'src>),
    Int(Span<'src>, i64),
    Tuple(Span<'src>, Vec<Rc<Pattern<'src>>>),
}

impl<'src> Pattern<'src> {
    pub(crate) fn match_value(
        &self,
        scope: VScope<'src>,
        value: Rc<Value<'src>>,
    ) -> Option<VScope<'src>> {
        match (self, value.as_ref()) {
            (Pattern::Any(_), _) => Some(scope),
            (Pattern::Id(_, id), _) => Some(scope.update(id, value)),
            (Pattern::True(_), Value::Bool(true)) => Some(scope),
            (Pattern::False(_), Value::Bool(false)) => Some(scope),
            (Pattern::Int(_, int1), Value::Int(int2)) if int1 == int2 => Some(scope),
            (Pattern::Tuple(_, pats), Value::Tuple(vals)) => {
                if pats.len() != vals.len() {
                    return None;
                }

                pats.iter()
                    .zip(vals.iter())
                    .try_fold(scope, |scope, (pat, val)| {
                        pat.match_value(scope, val.clone())
                    })
            }
            _ => None,
        }
    }

    pub(crate) fn match_type(&self, scope: TScope<'src>, ty: Rc<Type>) -> Option<TScope<'src>> {
        match (self, ty.as_ref()) {
            (Pattern::Any(_), _) => Some(scope),
            (Pattern::Id(_, id), _) => Some(scope.update(id, ty)),
            (Pattern::True(_), Type::Bool) => Some(scope),
            (Pattern::False(_), Type::Bool) => Some(scope),
            (Pattern::Int(_, _), Type::Int) => Some(scope),
            (Pattern::Tuple(_, pats), Type::Tuple(types)) => {
                if pats.len() != types.len() {
                    return None;
                }

                pats.iter()
                    .zip(types.iter())
                    .try_fold(scope, |scope, (pat, ty)| pat.match_type(scope, ty.clone()))
            }
            _ => None,
        }
    }
}

impl<'src> fmt::Display for Pattern<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Any(_) => write!(f, "_"),
            Pattern::Id(_, id) => write!(f, "{}", id),
            Pattern::True(_) => write!(f, "true"),
            Pattern::False(_) => write!(f, "false"),
            Pattern::Int(_, int) => write!(f, "{}", int),
            Pattern::Tuple(_, pats) => match pats.as_slice() {
                [] => write!(f, "()"),
                [pat] => write!(f, "({},)", pat),
                pats => {
                    write!(f, "(")?;
                    for pat in &pats[..pats.len() - 1] {
                        write!(f, "{}, ", pat)?;
                    }
                    write!(f, "{})", pats.last().unwrap())
                }
            },
        }
    }
}
