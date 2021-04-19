use std::fmt;
use std::rc::Rc;

use pest::Span;

use crate::pattern::Pattern;
use crate::types::Type;

#[derive(Clone, Debug)]
pub(crate) enum Term<'src> {
    Apply(Span<'src>, Rc<Term<'src>>, Rc<Term<'src>>),
    Match(
        Span<'src>,
        Rc<Term<'src>>,
        Vec<(Rc<Pattern<'src>>, Rc<Term<'src>>)>,
    ),
    Id(Span<'src>, &'src str),
    Lambda(
        Span<'src>,
        Rc<Pattern<'src>>,
        Option<Rc<Type<'src>>>,
        Rc<Term<'src>>,
    ),
    Let(
        Span<'src>,
        Rc<Pattern<'src>>,
        Option<Rc<Type<'src>>>,
        Rc<Term<'src>>,
        Rc<Term<'src>>,
    ),
    True(Span<'src>),
    False(Span<'src>),
    Tuple(Span<'src>, Vec<Rc<Term<'src>>>),
    Int(Span<'src>, i64),
    Record(Span<'src>, Vec<(Span<'src>, &'src str, Rc<Term<'src>>)>),
}

impl<'src> fmt::Display for Term<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            Apply(_, t1, t2) => write!(f, "({} {})", t1, t2),
            Match(_, term, pats) => {
                write!(f, "match {} with {} => {}", term, pats[0].0, pats[0].1)?;
                for (pat, term) in &pats[1..] {
                    write!(f, " | {} => {}", pat, term)?;
                }
                write!(f, " end")
            }
            Id(_, id) => write!(f, "{}", id),
            Lambda(_, param, Some(ty), term) => write!(f, "fn {}: {} => {}", param, ty, term),
            Lambda(_, param, None, term) => write!(f, "fn {} => {}", param, term),
            Let(_, pat, Some(ty), let_term, in_term) => {
                write!(f, "let {}: {} = {} in\n{}", pat, ty, let_term, in_term)
            }
            Let(_, pat, _, let_term, in_term) => {
                write!(f, "let {} = {} in\n{}", pat, let_term, in_term)
            }
            True(_) => write!(f, "true"),
            False(_) => write!(f, "false"),
            Tuple(_, terms) => match terms.as_slice() {
                [] => write!(f, "()"),
                [term] => write!(f, "({},)", term),
                terms => {
                    write!(f, "(")?;
                    for term in &terms[..terms.len() - 1] {
                        write!(f, "{}, ", term)?;
                    }
                    write!(f, "{})", terms.last().unwrap())
                }
            },
            Record(_, record) => {
                write!(f, "{{ ")?;
                for (_, id, term) in &record[..record.len() - 1] {
                    write!(f, "{}: {}, ", id, term)?;
                }
                if let Some((_, id, term)) = record.last() {
                    write!(f, "{}: {} ", id, term)?;
                }
                write!(f, "}}")
            }
            Int(_, int) => write!(f, "{}", int),
        }
    }
}
