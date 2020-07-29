use std::borrow::Borrow;
use std::fmt;
use std::rc::Rc;

use boolinator::Boolinator;
use pest::Span;

use crate::pattern::Pattern;
use crate::types::Type;

type TScope<'src> = im_rc::HashMap<&'src str, Rc<Type>>;

#[derive(Clone, Debug)]
pub(crate) enum Term<'src> {
    Apply(Span<'src>, Rc<Term<'src>>, Rc<Term<'src>>),
    Match(
        Span<'src>,
        Rc<Term<'src>>,
        Vec<(Rc<Pattern<'src>>, Rc<Term<'src>>)>,
    ),
    Id(Span<'src>, &'src str),
    Lambda(Span<'src>, Rc<Pattern<'src>>, Rc<Type>, Rc<Term<'src>>),
    Let(
        Span<'src>,
        Rc<Pattern<'src>>,
        Rc<Type>,
        Rc<Term<'src>>,
        Rc<Term<'src>>,
    ),
    True(Span<'src>),
    False(Span<'src>),
}

impl<'src> Term<'src> {
    fn type_of(&self, scope: TScope<'src>) -> Rc<Type> {
        use Term::*;
        match self {
            Apply(_info, t1, _t2) => match t1.type_of(scope).as_ref() {
                Type::Arr(_, ty) => ty.clone(),
                _ => panic!(),
            },
            Match(_info, term, pats) => {
                let term_type = term.type_of(scope.clone());
                let out_type = pats[0].1.type_of(scope.clone());
                Rc::new(Type::Arr(term_type, out_type))
            }
            Id(info, id) => scope
                .get(id)
                .unwrap_or_else(|| panic!("id '{}' unknown at {:?}", id, info))
                .clone(),
            Lambda(_info, _param, ty, term) => {
                Rc::new(Type::Arr(ty.clone(), term.as_ref().type_of(scope)))
            }
            Let(info, pat, ty, _, in_term) => {
                let scope = pat
                    .case_types(scope, ty.clone())
                    .unwrap_or_else(|| panic!("pattern doesn't match type at {:?}", info));
                in_term.type_of(scope)
            }
            True(_) => Rc::new(Type::Bool),
            False(_) => Rc::new(Type::Bool),
        }
    }

    pub(crate) fn type_check(&self, scope: TScope<'src>) -> Result<Rc<Type>, String> {
        use Term::*;
        match self {
            Apply(info, t1, t2) => match t1.type_check(scope.clone())?.as_ref() {
                Type::Arr(in_type, out_type) => (in_type.as_ref()
                    == t2.type_check(scope)?.borrow())
                .as_result(out_type.clone(), String::new()),
                _ => Err(format!("can't apply to a non-function at {:?}", info)),
            },
            Match(info, term, pats) => {
                let term_type = term.type_check(scope.clone())?;

                let (pat, result) = &pats[0];
                if !pat.type_check(&term_type) {
                    return Err(format!(
                        "pattern {:?} can't match type {:?} at {:?}",
                        pat, term_type, info
                    ));
                }
                let result_type = result.type_check(scope.clone())?;

                for (next_pat, next_result) in &pats[1..] {
                    if !next_pat.type_check(&term_type) {
                        return Err(format!(
                            "pattern {:?} can't match type {:?} at {:?}",
                            next_pat, term_type, info
                        ));
                    }
                    if result_type != next_result.type_check(scope.clone())? {
                        return Err(format!("type mismatch in match arms at {:?}", info));
                    }
                }

                Ok(result_type)
            }
            Id(info, id) => scope
                .get(id)
                .cloned()
                .ok_or_else(|| format!("id '{}' unknown at {:?}", id, info)),
            Lambda(info, param, ty, term) => {
                let scope = param.case_types(scope, ty.clone()).ok_or_else(|| {
                    format!(
                        "unable to match pattern {:?} with type {:?} at {:?}",
                        param, ty, info
                    )
                })?;
                Ok(Rc::new(Type::Arr(
                    ty.clone(),
                    term.as_ref().type_check(scope)?,
                )))
            }
            Let(info, pat, ty, let_term, in_term) => {
                let let_type = let_term.type_check(scope.clone())?;
                if ty.as_ref() != let_type.as_ref() {
                    return Err(format!(
                        "expected {:?} got {:?} at {:?}",
                        ty, let_type, info
                    ));
                }
                let scope = pat
                    .case_types(scope, ty.clone())
                    .ok_or_else(|| format!("pattern doesn't match type at {:?}", info))?;
                in_term.type_check(scope)
            }
            True(_) => Ok(Rc::new(Type::Bool)),
            False(_) => Ok(Rc::new(Type::Bool)),
        }
    }
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
            Lambda(_, param, ty, term) => write!(f, "fn {}: {} => {}", param, ty, term),
            Let(_, pat, ty, let_term, in_term) => {
                write!(f, "let {}: {} = {} in {}", pat, ty, let_term, in_term)
            }
            True(_) => write!(f, "true"),
            False(_) => write!(f, "false"),
        }
    }
}
