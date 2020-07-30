use std::fmt;
use std::rc::Rc;

use crate::pattern::Pattern;
use crate::term::Term;

type Scope<'src, T> = im_rc::HashMap<&'src str, Rc<T>>;
type VScope<'src> = Scope<'src, Value<'src>>;

#[derive(Clone, Debug)]
pub(crate) enum Value<'src> {
    Bool(bool),
    Lambda(VScope<'src>, Rc<Pattern<'src>>, Rc<Term<'src>>),
    Tuple(Vec<Rc<Value<'src>>>),
}

impl<'src> fmt::Display for Value<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(bool) => write!(f, "{}", bool),
            Value::Lambda(_, pat, term) => write!(f, "fn {} => {}", pat, term),
            Value::Tuple(values) => match values.as_slice() {
                [] => write!(f, "()"),
                [value] => write!(f, "({},)", value),
                values => {
                    write!(f, "(")?;
                    for value in &values[..values.len() - 1] {
                        write!(f, "{}, ", value)?;
                    }
                    write!(f, "{})", values.last().unwrap())
                }
            },
        }
    }
}
