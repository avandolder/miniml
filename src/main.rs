use std::borrow::Borrow;
use std::rc::Rc;

use im_rc::{self, hashmap};
use pest::Span;

mod parser;
mod pattern;
mod term;
mod types;
mod value;

use parser::parse;
use term::Term;
use value::Value;

type Scope<'src, T> = im_rc::HashMap<&'src str, Rc<T>>;
type VScope<'src> = Scope<'src, Value<'src>>;

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
                    pat.match_value(scope.clone(), value.clone())
                        .map(|scope| eval(scope, term))
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
            eval(pat.match_value(scope, let_value).unwrap(), &in_term)
        }
        True(_) => Rc::new(Value::Bool(true)),
        False(_) => Rc::new(Value::Bool(false)),
        Int(_, int) => Rc::new(Value::Int(*int)),
        Tuple(_info, terms) => Rc::new(Value::Tuple(
            terms.iter().map(|term| eval(scope.clone(), term)).collect(),
        )),
        Record(_info, record) => Rc::new(Value::Record(
            record
                .iter()
                .map(|(_, id, term)| (*id, eval(scope.clone(), term)))
                .collect(),
        )),
    }
}

fn apply<'a>(info: &Span<'a>, fun: Rc<Value<'a>>, arg: Rc<Value<'a>>) -> Rc<Value<'a>> {
    match fun.borrow() {
        Value::Lambda(scope, param, term) => {
            eval(param.match_value(scope.clone(), arg).unwrap(), &term)
        }
        v => panic!("unable to apply to value {:?} at {:?}", v, info),
    }
}

fn interpret(src: &str) -> Result<Rc<Value>, String> {
    let term = Rc::new(parse(src)?);
    types::type_check(term.clone())?;
    Ok(eval(hashmap![], term.as_ref()))
}

fn main() {
    let src = r#"
        let not = fn b: Bool =>
          match b with
            | true => false
            | false => true in
        let zero = 0 in
        let x = 20 in
        let pt = {x, y: 20} in
        let get_x = fn {x, y}: {x: Int, y: Int} => x in
        if not true then zero else get_x pt
    "#;
    println!("{}", interpret(src).unwrap());
}

#[cfg(test)]
mod test {
    use super::*;
    use pattern::Pattern;
    use types::{type_check, Type};

    #[test]
    fn parser_test() {
        let src = r#"
            let not: (Bool -> Bool) = fn b: Bool =>
              match b with
                | true => false
                | false => true in
            not true
        "#;
        let term1 = parse(src).unwrap();

        let dummy_info = || pest::Span::new("", 0, 0).unwrap();
        let tfalse = Rc::new(Term::False(dummy_info()));
        let ttrue = Rc::new(Term::True(dummy_info()));
        let term2 = Term::Let(
            dummy_info(),
            Rc::new(Pattern::Id(dummy_info(), "not")),
            Some(Rc::new(Type::Arr(Rc::new(Type::Bool), Rc::new(Type::Bool)))),
            Rc::new(Term::Lambda(
                dummy_info(),
                Rc::new(Pattern::Id(dummy_info(), "b")),
                Some(Rc::new(Type::Bool)),
                Rc::new(Term::Match(
                    dummy_info(),
                    Rc::new(Term::Id(dummy_info(), "b")),
                    vec![
                        (Rc::new(Pattern::True(dummy_info())), tfalse.clone()),
                        (Rc::new(Pattern::False(dummy_info())), ttrue.clone()),
                    ],
                )),
            )),
            Rc::new(Term::Apply(
                dummy_info(),
                Rc::new(Term::Id(dummy_info(), "not")),
                ttrue.clone(),
            )),
        );

        assert_eq!(term1.to_string(), term2.to_string());
    }

    #[test]
    fn tuple_test() {
        let src = "((), ( ), (()), ((),), (true, false), (true, false,))";
        let term = Rc::new(parse(src).unwrap());
        assert_eq!(
            term.to_string(),
            "((), (), (), ((),), (true, false), (true, false))"
        );
        let ty = type_check(term.clone()).unwrap();
        assert_eq!(
            ty.to_string(),
            "((), (), (), ((),), (Bool, Bool), (Bool, Bool))"
        );
        let val = eval(hashmap![], term.as_ref());
        assert_eq!(
            val.to_string(),
            "((), (), (), ((),), (true, false), (true, false))"
        );
    }

    #[test]
    fn logical_operations() {
        let src = r#"
            let twice: (Bool -> Bool) -> Bool -> Bool =
              fn f: Bool -> Bool => (fn b: Bool => f (f b)) in
            let not: Bool -> Bool = fn b: Bool =>
              match b with
                | true => false
                | false => true
            in
            let and: Bool -> Bool -> Bool = fn x: Bool => fn y: Bool =>
              match (x, y) with
                | (true, true) => true
                | _ => false
            in
            let or: Bool -> Bool -> Bool = fn x: Bool => fn y: Bool =>
              match (x, y) with
                | (true, _) => true
                | (false, z) => z
            in
            and (twice not true) (or false true)
        "#;
        let val = interpret(src).unwrap();
        assert_eq!(val.to_string(), "true");
    }
}
