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
    Tuple(Span<'src>, Vec<Rc<Term<'src>>>),
    Int(Span<'src>, i64),
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
                    .match_type(scope, ty.clone())
                    .unwrap_or_else(|| panic!("pattern doesn't match type at {:?}", info));
                in_term.type_of(scope)
            }
            True(_) => Rc::new(Type::Bool),
            False(_) => Rc::new(Type::Bool),
            Tuple(_, terms) => Rc::new(Type::Tuple(
                terms
                    .iter()
                    .map(|term| term.type_of(scope.clone()))
                    .collect(),
            )),
            Int(_, _) => Rc::new(Type::Int),
        }
    }

    pub(crate) fn type_check(&self, scope: TScope<'src>) -> Result<Rc<Type>, String> {
        use Term::*;
        match self {
            Apply(info, t1, t2) => {
                let ty1 = t1.type_check(scope.clone())?;
                match ty1.as_ref() {
                    Type::Arr(in_type, out_type) => {
                        let ty2 = t2.type_check(scope)?;
                        (in_type.as_ref() == ty2.as_ref()).as_result_from(
                            || out_type.clone(),
                            || format!("can't apply {} to {} at {:?}", ty2, in_type, info),
                        )
                    }
                    _ => Err(format!("can't apply to a non-function at {:?}", info)),
                }
            }
            Match(info, term, pats) => {
                let term_type = term.type_check(scope.clone())?;

                let (pat, result) = &pats[0];
                let result_type =
                    if let Some(scope) = pat.match_type(scope.clone(), term_type.clone()) {
                        result.type_check(scope)?
                    } else {
                        return Err(format!(
                            "pattern {:?} can't match type {:?} at {:?}",
                            pat, term_type, info
                        ));
                    };

                for (next_pat, next_result) in &pats[1..] {
                    if let Some(scope) = next_pat.match_type(scope.clone(), term_type.clone()) {
                        if result_type != next_result.type_check(scope)? {
                            return Err(format!("type mismatch in match arms at {:?}", info));
                        }
                    } else {
                        return Err(format!(
                            "pattern {:?} can't match type {:?} at {:?}",
                            next_pat, term_type, info
                        ));
                    }
                }

                Ok(result_type)
            }
            Id(info, id) => scope
                .get(id)
                .cloned()
                .ok_or_else(|| format!("id '{}' unknown at {:?}", id, info)),
            Lambda(info, param, ty, term) => {
                let scope = param.match_type(scope, ty.clone()).ok_or_else(|| {
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
                    .match_type(scope, ty.clone())
                    .ok_or_else(|| format!("pattern doesn't match type at {:?}", info))?;
                in_term.type_check(scope)
            }
            True(_) => Ok(Rc::new(Type::Bool)),
            False(_) => Ok(Rc::new(Type::Bool)),
            Tuple(_, terms) => Ok(Rc::new(Type::Tuple(
                terms
                    .iter()
                    .map(|term| term.type_check(scope.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            ))),
            Int(_, _) => Ok(Rc::new(Type::Int)),
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
                write!(f, "let {}: {} = {} in\n{}", pat, ty, let_term, in_term)
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
            }
            Int(_, int) => write!(f, "{}", int),
        }
    }
}
