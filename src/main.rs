use std::borrow::Borrow;
use std::rc::Rc;

use boolinator::Boolinator;
use im_rc::{self, hashmap};

#[derive(Clone, Debug)]
struct Info<'src> {
    span: &'src str,
    line: usize,
    start: usize,
    end: usize,
}

#[derive(Clone, Debug)]
enum Type {
    Bool,
    Arr(Rc<Type>, Rc<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Arr(l1, l2), Type::Arr(r1, r2)) => (&l1 == &r1) && (&l2 == &r2),
            _ => false,
        }
    }
}

type Scope<'src, T> = im_rc::HashMap<&'src str, Rc<T>>;
type TScope<'src> = Scope<'src, Type>;
type VScope<'src> = Scope<'src, Value<'src>>;

#[derive(Clone, Debug)]
enum Pat<'src> {
    Any(Info<'src>),
    Id(Info<'src>, &'src str),
    True(Info<'src>),
    False(Info<'src>),
}

impl<'src> Pat<'src> {
    fn type_check(&self, ty: &Type) -> bool {
        match (self, ty) {
            (Pat::Any(_), _) => true,
            (Pat::Id(_, _), _) => true,
            (Pat::True(_), Type::Bool) => true,
            (Pat::False(_), Type::Bool) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
enum Term<'src> {
    Apply(Info<'src>, Rc<Term<'src>>, Rc<Term<'src>>),
    Match(
        Info<'src>,
        Rc<Term<'src>>,
        Vec<(Rc<Pat<'src>>, Rc<Term<'src>>)>,
    ),
    Id(Info<'src>, &'src str),
    Lambda(Info<'src>, Rc<Pat<'src>>, Rc<Type>, Rc<Term<'src>>),
    Let(
        Info<'src>,
        Rc<Pat<'src>>,
        Rc<Type>,
        Rc<Term<'src>>,
        Rc<Term<'src>>,
    ),
    True(Info<'src>),
    False(Info<'src>),
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
                let scope = match_types(scope, ty.clone(), pat)
                    .unwrap_or_else(|| panic!("pattern doesn't match type at {:?}", info));
                in_term.type_of(scope)
            }
            True(_) => Rc::new(Type::Bool),
            False(_) => Rc::new(Type::Bool),
        }
    }

    fn type_check(&self, scope: TScope<'src>) -> Result<Rc<Type>, String> {
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
                let scope = match_types(scope, ty.clone(), param).ok_or_else(|| {
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
                let scope = match_types(scope, ty.clone(), pat)
                    .ok_or_else(|| format!("pattern doesn't match type at {:?}", info))?;
                in_term.type_check(scope)
            }
            True(_) => Ok(Rc::new(Type::Bool)),
            False(_) => Ok(Rc::new(Type::Bool)),
        }
    }
}

#[derive(Clone, Debug)]
enum Value<'src> {
    Bool(bool),
    Lambda(VScope<'src>, Rc<Pat<'src>>, Rc<Term<'src>>),
}

fn eval<'a>(scope: VScope<'a>, t: &Term<'a>) -> Rc<Value<'a>> {
    use Term::*;
    match t {
        Apply(info, t1, t2) => {
            let v1 = eval(scope.clone(), &t1);
            let v2 = eval(scope.clone(), &t2);
            apply(info, v1, v2)
        }
        Match(info, term, pats) => {
            let value = eval(scope.clone(), term);
            pats.iter()
                .filter_map(|(pat, term)| {
                    match_pattern(scope.clone(), value.clone(), pat).map(|scope| eval(scope, term))
                })
                .next()
                .unwrap_or_else(|| panic!("no match at {:?}", info))
        }
        Id(info, id) => scope
            .get(id)
            .unwrap_or_else(|| panic!("id '{}' unknown at {:?}", id, info))
            .clone(),
        Lambda(_info, param, _, term) => Rc::new(Value::Lambda(scope, param.clone(), term.clone())),
        Let(_info, pat, _, let_term, in_term) => {
            let let_value = eval(scope.clone(), &let_term);
            eval(match_pattern(scope, let_value, pat).unwrap(), &in_term)
        }
        True(_) => Rc::new(Value::Bool(true)),
        False(_) => Rc::new(Value::Bool(false)),
    }
}

fn apply<'a>(info: &Info<'a>, fun: Rc<Value<'a>>, arg: Rc<Value<'a>>) -> Rc<Value<'a>> {
    match fun.borrow() {
        Value::Lambda(scope, param, term) => {
            eval(match_pattern(scope.clone(), arg, &param).unwrap(), &term)
        }
        v => panic!("unable to apply to value {:?} at {:?}", v, info),
    }
}

fn match_pattern<'a>(scope: VScope<'a>, value: Rc<Value<'a>>, pat: &Pat<'a>) -> Option<VScope<'a>> {
    match (pat, value.as_ref()) {
        (Pat::Any(_), _) => Some(scope),
        (Pat::Id(_, id), _) => Some(scope.update(id, value)),
        (Pat::True(_), Value::Bool(true)) => Some(scope),
        (Pat::False(_), Value::Bool(false)) => Some(scope),
        _ => None,
    }
}

fn match_types<'a>(scope: TScope<'a>, ty: Rc<Type>, pat: &Pat<'a>) -> Option<TScope<'a>> {
    match (pat, ty.as_ref()) {
        (Pat::Any(_), _) => Some(scope),
        (Pat::Id(_, id), _) => Some(scope.update(id, ty)),
        (Pat::True(_), Type::Bool) => Some(scope),
        (Pat::False(_), Type::Bool) => Some(scope),
        _ => None,
    }
}

fn main() {
    let dummy_info = || Info {
        span: "",
        line: 0,
        start: 0,
        end: 0,
    };

    let tfalse = Rc::new(Term::False(dummy_info()));
    let ttrue = Rc::new(Term::True(dummy_info()));
    let not_term = Term::Let(
        dummy_info(),
        Rc::new(Pat::Id(dummy_info(), "not")),
        Rc::new(Type::Arr(Rc::new(Type::Bool), Rc::new(Type::Bool))),
        Rc::new(Term::Lambda(
            dummy_info(),
            Rc::new(Pat::Id(dummy_info(), "b")),
            Rc::new(Type::Bool),
            Rc::new(Term::Match(
                dummy_info(),
                Rc::new(Term::Id(dummy_info(), "b")),
                vec![
                    (Rc::new(Pat::True(dummy_info())), tfalse.clone()),
                    (Rc::new(Pat::False(dummy_info())), ttrue.clone()),
                ],
            )),
        )),
        Rc::new(Term::Apply(
            dummy_info(),
            Rc::new(Term::Id(dummy_info(), "not")),
            ttrue.clone(),
        )),
    );
    let not_type = not_term.type_check(hashmap![]).unwrap();

    println!(
        "(let not = \\x => (match x with true => false | false => true) in not true) : {:?} => {:?}",
        not_type,
        eval(hashmap![], &not_term),
    );
}
