use std::borrow::Borrow;
use std::rc::Rc;

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

#[derive(Clone, Debug)]
enum Pat<'src> {
    Any(Info<'src>),
    Id(Info<'src>, &'src str),
    True(Info<'src>),
    False(Info<'src>),
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
    Lambda(Info<'src>, Rc<Pat<'src>>, Type, Rc<Term<'src>>),
    Let(
        Info<'src>,
        Rc<Pat<'src>>,
        Type,
        Rc<Term<'src>>,
        Rc<Term<'src>>,
    ),
    True(Info<'src>),
    False(Info<'src>),
}

type Scope<'src> = im_rc::HashMap<&'src str, Rc<Value<'src>>>;

#[derive(Clone, Debug)]
enum Value<'src> {
    Bool(bool),
    Lambda(Scope<'src>, Rc<Pat<'src>>, Rc<Term<'src>>),
}

fn eval<'a>(scope: Scope<'a>, t: &Term<'a>) -> Rc<Value<'a>> {
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

fn match_pattern<'a>(scope: Scope<'a>, value: Rc<Value<'a>>, pat: &Pat<'a>) -> Option<Scope<'a>> {
    match (pat, value.as_ref()) {
        (Pat::Any(_), _) => Some(scope),
        (Pat::Id(_, id), _) => Some(scope.update(id, value)),
        (Pat::True(_), Value::Bool(true)) => Some(scope),
        (Pat::False(_), Value::Bool(false)) => Some(scope),
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

    println!("`true` => {:?}", eval(hashmap![], &ttrue));
    println!(
        "`let not in \\x => (match x with true => false | false => true) in not true` => {:?}",
        eval(
            hashmap![],
            &Term::Let(
                dummy_info(),
                Rc::new(Pat::Id(dummy_info(), "not")),
                Type::Arr(Rc::new(Type::Bool), Rc::new(Type::Bool)),
                Rc::new(Term::Lambda(
                    dummy_info(),
                    Rc::new(Pat::Id(dummy_info(), "b")),
                    Type::Bool,
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
            )
        )
    );
}
