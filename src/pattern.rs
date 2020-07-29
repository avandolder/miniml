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
}

impl<'src> Pattern<'src> {
    pub(crate) fn type_check(&self, ty: &Type) -> bool {
        match (self, ty) {
            (Pattern::Any(_), _) => true,
            (Pattern::Id(_, _), _) => true,
            (Pattern::True(_), Type::Bool) => true,
            (Pattern::False(_), Type::Bool) => true,
            _ => false,
        }
    }

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
            _ => None,
        }
    }

    pub(crate) fn match_type(&self, scope: TScope<'src>, ty: Rc<Type>) -> Option<TScope<'src>> {
        match (self, ty.as_ref()) {
            (Pattern::Any(_), _) => Some(scope),
            (Pattern::Id(_, id), _) => Some(scope.update(id, ty)),
            (Pattern::True(_), Type::Bool) => Some(scope),
            (Pattern::False(_), Type::Bool) => Some(scope),
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
        }
    }
}
