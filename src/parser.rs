use std::iter::Peekable;
use std::rc::Rc;

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use crate::pattern::Pattern;
use crate::term::Term;
use crate::types::Type;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct TermParser;

fn parse_term(pair: Pair<Rule>) -> Term {
    match pair.as_rule() {
        Rule::Term => parse_term(pair.into_inner().next().unwrap()),
        Rule::Lambda => parse_lambda(pair),
        Rule::Apply => {
            let mut pairs = pair.into_inner().peekable();
            let lhs = parse_term(pairs.next().unwrap());
            parse_apply(pairs, lhs)
        }
        Rule::Let => {
            let span = pair.as_span();
            let mut pairs = pair.into_inner();
            let pat = parse_pattern(pairs.next().unwrap());
            let ty = parse_type(pairs.next().unwrap());
            let let_term = parse_term(pairs.next().unwrap());
            let in_term = parse_term(pairs.next().unwrap());
            Term::Let(span, Rc::new(pat), Rc::new(ty), Rc::new(let_term), Rc::new(in_term))
        }
        Rule::Match => {
            let span = pair.as_span();
            let mut pairs = pair.into_inner();
            let matched = parse_term(pairs.next().unwrap());

            let arms = pairs.map(|arm| {
                let mut pairs = arm.into_inner();
                (
                    Rc::new(parse_pattern(pairs.next().unwrap())),
                    Rc::new(parse_term(pairs.next().unwrap())),
                )
            }).collect::<Vec<_>>();

            Term::Match(span, Rc::new(matched), arms)
        }
        Rule::Id => Term::Id(pair.as_span(), pair.as_str()),
        Rule::True => Term::True(pair.as_span()),
        Rule::False => Term::False(pair.as_span()),
        _ => panic!("term: {}", pair),
    }
}

fn parse_lambda(pair: Pair<Rule>) -> Term {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    let param = parse_pattern(pairs.next().unwrap());
    let ty = parse_type(pairs.next().unwrap());
    let body = parse_term(pairs.next().unwrap());
    Term::Lambda(span, Rc::new(param), Rc::new(ty), Rc::new(body))
}

fn parse_apply<'a>(
    mut pairs: Peekable<impl Iterator<Item = Pair<'a, Rule>>>,
    lhs: Term<'a>,
) -> Term<'a> {
    let pair = pairs.next().unwrap();
    let span = pair.as_span();
    let next = parse_term(pair);

    if let Some(_) = pairs.peek() {
        parse_apply(pairs, Term::Apply(span, Rc::new(lhs), Rc::new(next)))
    } else {
        Term::Apply(span, Rc::new(lhs), Rc::new(next))
    }
}

fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::Pattern => parse_pattern(pair.into_inner().next().unwrap()),
        Rule::Any => Pattern::Any(pair.as_span()),
        Rule::Id => Pattern::Id(pair.as_span(), pair.as_str()),
        Rule::True => Pattern::True(pair.as_span()),
        Rule::False => Pattern::False(pair.as_span()),
        _ => panic!("pattern: {:?}", pair.as_span()),
    }
}

fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::Type => parse_type(pair.into_inner().next().unwrap()),
        Rule::Bool => Type::Bool,
        Rule::Arrow => {
            let mut pairs = pair.into_inner();
            let lhs = parse_type(pairs.next().unwrap());
            let rhs = parse_type(pairs.next().unwrap());
            Type::Arr(Rc::new(lhs), Rc::new(rhs))
        }
        _ => panic!("type: {}", pair),
    }
}

pub(crate) fn parse(src: &str) -> Term {
    parse_term(TermParser::parse(Rule::Program, src)
        .expect("parse failed")
        .next()
        .unwrap()
        .into_inner()
        .next()
        .unwrap())
}