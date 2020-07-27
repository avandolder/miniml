use std::rc::Rc;

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
    Match(Info<'src>, Rc<Term<'src>>, Vec<(Rc<Pat<'src>>, Rc<Term<'src>>)>),
    Id(Info<'src>, &'src str),
    Lambda(Info<'src>, Rc<Pat<'src>>, Type, Rc<Term<'src>>),
    Let(Info<'src>, Rc<Pat<'src>>, Type, Rc<Term<'src>>),
    True(Info<'src>),
    False(Info<'src>),
}

fn main() {}
