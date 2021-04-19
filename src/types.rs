use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::{pattern::Pattern, term::Term};

#[derive(Clone, Debug)]
pub(crate) enum Type<'src> {
    Bool,
    Int,
    Arr(Rc<Type<'src>>, Rc<Type<'src>>),
    Tuple(Vec<Rc<Type<'src>>>),
    Record(Vec<(&'src str, Rc<Type<'src>>)>),
    Var(usize),
    PolyVar(usize),
}

#[derive(Clone, Copy)]
enum VarType {
    Mono,
    Poly,
}

impl<'src> fmt::Display for Type<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            Type::Arr(from, to) => write!(f, "({} -> {})", from, to),
            Type::Tuple(types) => match types.as_slice() {
                [] => write!(f, "()"),
                [ty] => write!(f, "({},)", ty),
                types => {
                    write!(f, "(")?;
                    for ty in &types[..types.len() - 1] {
                        write!(f, "{}, ", ty)?;
                    }
                    write!(f, "{})", types.last().unwrap())
                }
            },
            Type::Record(record) => {
                write!(f, "{{ ")?;
                for (id, ty) in &record[..record.len() - 1] {
                    write!(f, "{}: {}, ", id, ty)?;
                }
                if let Some((id, ty)) = record.last() {
                    write!(f, "{}: {} ", id, ty)?;
                }
                write!(f, "}}")
            }
            Type::Var(n) | Type::PolyVar(n) => {
                let primes = n / 26;
                let c = ((n % 26) as u8 + b'a') as char;
                write!(f, "{}", c)?;
                for _ in 0..primes {
                    write!(f, "'")?;
                }
                Ok(())
            }
        }
    }
}

pub(crate) struct TypeChecker<'src> {
    var_count: usize,
    type_ctx: HashMap<usize, Rc<Type<'src>>>,
}

type TypeScope<'src> = im_rc::HashMap<&'src str, Rc<Type<'src>>>;

impl<'src> TypeChecker<'src> {
    fn fresh(&mut self) -> Rc<Type<'static>> {
        let fresh_var = Rc::new(Type::Var(self.var_count));
        self.var_count += 1;
        fresh_var
    }

    fn fresh_poly(&mut self) -> Rc<Type<'static>> {
        let fresh_var = Rc::new(Type::PolyVar(self.var_count));
        self.var_count += 1;
        fresh_var
    }

    fn check_term(
        &mut self,
        term: Rc<Term<'src>>,
        vars: TypeScope<'src>,
    ) -> Result<Rc<Type<'src>>, String> {
        match term.as_ref() {
            Term::Apply(_, fun, arg) => {
                let fun_type = self.check_term(fun.clone(), vars.clone())?;
                let arg_type = self.check_term(arg.clone(), vars.clone())?;

                let return_type = self.fresh();
                self.unify(
                    fun_type,
                    Rc::new(Type::Arr(arg_type.clone(), return_type.clone())),
                )?;

                Ok(return_type)
            }

            Term::Id(loc, id) => vars
                .get(id)
                .cloned()
                .ok_or_else(|| format!("Id {} unbound at {:?}", id, loc)),

            Term::Match(_, case, arms) => {
                let case_type = self.check_term(case.clone(), vars.clone())?;
                let result_type = self.fresh();

                for (pat, arm) in arms {
                    let (pat_ty, new_vars) = self.check_pattern(pat.clone(), VarType::Mono)?;
                    self.unify(case_type.clone(), pat_ty)?;
                    let arm_type = self.check_term(arm.clone(), new_vars.union(vars.clone()))?;
                    self.unify(result_type.clone(), arm_type)?;
                }

                Ok(result_type)
            }

            Term::Lambda(_, pat, param_ty, body) => {
                let (pat_ty, new_vars) = self.check_pattern(pat.clone(), VarType::Mono)?;
                if let Some(ty) = param_ty {
                    self.unify(pat_ty.clone(), ty.clone())?;
                }
                let return_type = self.check_term(body.clone(), new_vars.union(vars.clone()))?;
                Ok(Rc::new(Type::Arr(pat_ty, return_type)))
            }

            Term::Let(_, pat, ty, t1, t2) => {
                let (pat_ty, new_vars) = self.check_pattern(pat.clone(), VarType::Poly)?;
                if let Some(ty) = ty {
                    self.unify(pat_ty.clone(), ty.clone())?;
                }
                let ty1 = self.check_term(t1.clone(), vars.clone())?;
                self.unify(pat_ty, ty1.clone())?;
                let ty2 = self.check_term(t2.clone(), new_vars.union(vars.clone()))?;
                Ok(ty2)
            }

            Term::Int(_, _) => Ok(Rc::new(Type::Int)),
            Term::True(_) | Term::False(_) => Ok(Rc::new(Type::Bool)),

            Term::Tuple(_, tuple) => Ok(Rc::new(Type::Tuple(
                tuple
                    .iter()
                    .map(|term| self.check_term(term.clone(), vars.clone()))
                    .collect::<Result<_, _>>()?,
            ))),

            Term::Record(_, record) => Ok(Rc::new(Type::Record(
                record
                    .iter()
                    .map(|(_, id, term)| Ok((*id, self.check_term(term.clone(), vars.clone())?)))
                    .collect::<Result<_, String>>()?,
            ))),
        }
    }

    fn check_pattern(
        &mut self,
        pat: Rc<Pattern<'src>>,
        var_type: VarType,
    ) -> Result<(Rc<Type<'src>>, TypeScope<'src>), String> {
        match pat.as_ref() {
            Pattern::Any(_) => Ok((self.fresh(), im_rc::hashmap![])),
            Pattern::Id(_, id) => {
                let type_var = match var_type {
                    VarType::Mono => self.fresh(),
                    VarType::Poly => self.fresh_poly(),
                };
                let new_vars = im_rc::hashmap![*id => type_var.clone()];
                Ok((type_var, new_vars))
            }
            Pattern::True(_) | Pattern::False(_) => Ok((Rc::new(Type::Bool), im_rc::hashmap![])),
            Pattern::Int(_, _) => Ok((Rc::new(Type::Int), im_rc::hashmap![])),
            Pattern::Tuple(_, tuple) => {
                let mut new_vars = im_rc::hashmap![];
                let ty = Rc::new(Type::Tuple(
                    tuple
                        .iter()
                        .map(|pat| {
                            let (ty, vars) = self.check_pattern(pat.clone(), var_type)?;
                            // TODO: Check for intersection between new IDs being introduced
                            new_vars = new_vars.clone().union(vars);
                            Ok(ty)
                        })
                        .collect::<Result<_, String>>()?,
                ));
                Ok((ty, new_vars))
            }
            Pattern::Record(_, record) => {
                let mut new_vars = im_rc::hashmap![];
                let ty = Rc::new(Type::Record(
                    record
                        .iter()
                        .map(|(_, id, pat)| {
                            let (ty, vars) = self.check_pattern(pat.clone(), var_type)?;
                            // TODO: Check for intersection between new IDs being introduced
                            new_vars = new_vars.clone().union(vars);
                            Ok((*id, ty))
                        })
                        .collect::<Result<_, String>>()?,
                ));
                Ok((ty, new_vars))
            }
        }
    }

    fn unify(&mut self, t1: Rc<Type<'src>>, t2: Rc<Type<'src>>) -> Result<(), String> {
        match (t1.as_ref(), t2.as_ref()) {
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Int, Type::Int) => Ok(()),
            (Type::Arr(l1, l2), Type::Arr(r1, r2)) => {
                self.unify(l1.clone(), r1.clone())?;
                self.unify(l2.clone(), r2.clone())?;
                Ok(())
            }
            (Type::Tuple(tp1), Type::Tuple(tp2)) => {
                if tp1.len() != tp2.len() {
                    return Err(String::from(
                        "unification failure: tuples of differing length",
                    ));
                }
                for (ty1, ty2) in tp1.iter().zip(tp2.iter()) {
                    self.unify(ty1.clone(), ty2.clone())?;
                }
                Ok(())
            }
            (Type::Record(r1), Type::Record(r2)) => {
                if r1.len() != r2.len() {
                    return Err(format!(
                        "unification failure: records with differing numbers of fields",
                    ));
                }
                for ((id1, ty1), (id2, ty2)) in r1.iter().zip(r2.iter()) {
                    if id1 != id2 {
                        return Err(format!(
                            "unification failure: fields {} and {} don't match",
                            id1, id2
                        ));
                    }
                    self.unify(ty1.clone(), ty2.clone())?;
                }
                Ok(())
            }

            // Match polymorphic type variables that are introduced via let-polymorphism.
            (Type::PolyVar(v1), Type::PolyVar(v2)) if v1 == v2 => Ok(()),
            (Type::PolyVar(_v1), Type::PolyVar(_v2)) => todo!("Unify two polyvars"),
            (Type::PolyVar(var), _) => {
                let ty = match self.type_ctx.get(var) {
                    None => {
                        self.type_ctx.insert(*var, t2.clone());
                        return Ok(());
                    }
                    Some(ty) => ty.clone(),
                };
                let ty = self.monomorphize(ty, &mut HashMap::new());
                self.unify(ty, t2)
            }
            (_, Type::PolyVar(var)) => {
                let ty = match self.type_ctx.get(var) {
                    None => {
                        self.type_ctx.insert(*var, t1.clone());
                        return Ok(());
                    }
                    Some(ty) => ty.clone(),
                };
                let ty = self.monomorphize(ty, &mut HashMap::new());
                self.unify(ty, t1)
            }

            (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),
            (Type::Var(v1), Type::Var(v2)) => {
                let ty2 = match self.type_ctx.get(v2) {
                    None => {
                        self.type_ctx.insert(*v2, t1);
                        return Ok(());
                    }
                    Some(ty2) => ty2.clone(),
                };
                let ty1 = match self.type_ctx.get(v1) {
                    None => {
                        self.type_ctx.insert(*v1, t2);
                        return Ok(());
                    }
                    Some(ty1) => ty1.clone(),
                };
                self.unify(ty1, ty2)
            }
            (Type::Var(var), _) => {
                let ty = match self.type_ctx.get(var) {
                    None => {
                        self.type_ctx.insert(*var, t2.clone());
                        return Ok(());
                    }
                    Some(ty) => ty.clone(),
                };
                self.unify(ty, t2)
            }
            (_, Type::Var(var)) => {
                let ty = match self.type_ctx.get(var) {
                    None => {
                        self.type_ctx.insert(*var, t1.clone());
                        return Ok(());
                    }
                    Some(ty) => ty.clone(),
                };
                self.unify(ty, t1)
            }

            (ty1, ty2) => Err(format!(
                "unification failure: types {} and {} can't be unified",
                ty1, ty2
            )),
        }
    }

    fn expand_type(&mut self, ty: Rc<Type<'src>>) -> Rc<Type<'src>> {
        match ty.as_ref() {
            Type::Bool => ty,
            Type::Int => ty,
            Type::Arr(ty1, ty2) => Rc::new(Type::Arr(
                self.expand_type(ty1.clone()),
                self.expand_type(ty2.clone()),
            )),
            Type::Tuple(tuple) => Rc::new(Type::Tuple(
                tuple
                    .iter()
                    .map(|ty| self.expand_type(ty.clone()))
                    .collect(),
            )),
            Type::Record(record) => Rc::new(Type::Record(
                record
                    .iter()
                    .map(|(id, ty)| (*id, self.expand_type(ty.clone())))
                    .collect(),
            )),
            Type::Var(v) => {
                let ty = match self.type_ctx.get(v) {
                    None => return ty,
                    Some(ty) => ty.clone(),
                };
                self.expand_type(ty)
            },
            Type::PolyVar(v) => {
                let ty = match self.type_ctx.get(v) {
                    None => return ty,
                    Some(ty) => ty.clone(),
                };
                // let ty = self.monomorphize(ty, &mut HashMap::new());
                self.expand_type(ty)
            }
        }
    }

    fn monomorphize(
        &mut self,
        ty: Rc<Type<'src>>,
        map: &mut HashMap<usize, Rc<Type<'src>>>,
    ) -> Rc<Type<'src>> {
        match ty.as_ref() {
            Type::Bool | Type::Int => ty,
            Type::Arr(ty1, ty2) => {
                let ty1 = self.monomorphize(ty1.clone(), map);
                let ty2 = self.monomorphize(ty2.clone(), map);
                Rc::new(Type::Arr(ty1, ty2))
            }
            Type::Tuple(tuple) => Rc::new(Type::Tuple(
                tuple
                    .iter()
                    .map(|ty| self.monomorphize(ty.clone(), map))
                    .collect(),
            )),
            Type::Record(record) => Rc::new(Type::Record(
                record
                    .iter()
                    .map(|(id, ty)| (*id, self.monomorphize(ty.clone(), map)))
                    .collect(),
            )),
            Type::Var(v) => match map.get(v) {
                None => {
                    let new_var = self.fresh();
                    let new_var_idx = self.var_count - 1;
                    map.insert(*v, new_var.clone());

                    let ty = match self.type_ctx.get(v) {
                        None => return new_var,
                        Some(ty) => ty.clone(),
                    };
                    let ty = self.monomorphize(ty.clone(), map);
                    self.type_ctx.insert(new_var_idx, ty);
                    new_var
                }
                Some(ty) => ty.clone(),
            },
            Type::PolyVar(v) => match map.get(v) {
                None => {
                    let new_var = self.fresh_poly();
                    let new_var_idx = self.var_count - 1;
                    map.insert(*v, new_var.clone());

                    let ty = match self.type_ctx.get(v) {
                        None => return new_var,
                        Some(ty) => ty.clone(),
                    };
                    let ty = self.monomorphize(ty.clone(), map);
                    self.type_ctx.insert(new_var_idx, ty);
                    new_var
                }
                Some(ty) => self.monomorphize(ty.clone(), map),
            },
        }
    }
}

fn simplify_type_vars<'src>(ty: Rc<Type<'src>>, map: &mut HashMap<usize, usize>) -> Rc<Type<'src>> {
    match ty.as_ref() {
        Type::Bool | Type::Int => ty,
        Type::Arr(ty1, ty2) => {
            let ty1 = simplify_type_vars(ty1.clone(), map);
            let ty2 = simplify_type_vars(ty2.clone(), map);
            Rc::new(Type::Arr(ty1, ty2))
        }
        Type::Tuple(tuple) => Rc::new(Type::Tuple(
            tuple
                .iter()
                .map(|ty| simplify_type_vars(ty.clone(), map))
                .collect(),
        )),
        Type::Record(record) => Rc::new(Type::Record(
            record
                .iter()
                .map(|(id, ty)| (*id, simplify_type_vars(ty.clone(), map)))
                .collect(),
        )),
        Type::Var(v) => match map.get(v) {
            None => {
                let new_var = map.len();
                map.insert(*v, new_var);
                Rc::new(Type::Var(new_var))
            }
            Some(v) => Rc::new(Type::Var(*v)),
        },
        Type::PolyVar(v) => match map.get(v) {
            None => {
                let new_var = map.len();
                map.insert(*v, new_var);
                Rc::new(Type::PolyVar(new_var))
            }
            Some(v) => Rc::new(Type::PolyVar(*v)),
        },
    }
}

pub(crate) fn type_check(term: Rc<Term>) -> Result<Rc<Type>, String> {
    let mut checker = TypeChecker {
        var_count: 0,
        type_ctx: HashMap::new(),
    };
    let ty = checker.check_term(term, im_rc::hashmap![])?;
    let ty = checker.expand_type(ty);
    let ty = simplify_type_vars(ty, &mut HashMap::new());
    Ok(ty)
}

#[cfg(test)]
mod test {
    use super::type_check;
    use crate::parser::parse;
    use std::rc::Rc;

    #[test]
    fn test_type_checker() -> Result<(), String> {
        let src = r#"
            let not = fn b =>
            match b with
                | true => false
                | false => true in
            let zero = 0 in
            let x = 20 in
            let pt = {x, y: 20} in
            let get_x = fn {x, y} => x in
            if not true then zero else get_x pt
        "#;
        let term = parse(src)?;
        assert_eq!(type_check(Rc::new(term))?.to_string(), "Int");

        let src = r#"
            let id2 = fn x => fn y =>
                let id = fn x => x in
                (id x, id y) in
            let _ = id2 true false in
            id2 10 true
        "#;
        let term = parse(src)?;
        assert_eq!(type_check(Rc::new(term))?.to_string(), "(Int, Bool)");

        let src = r#"
            let things = {
                zero: (0,),
                apply: fn x => fn y => x y,
            } in
            let do_something =
                fn things =>
                    match things with
                    | { zero: (zero,), apply } =>
                        fn l =>
                            apply l zero
            in
            do_something things
        "#;
        let term = parse(src)?;
        assert_eq!(type_check(Rc::new(term))?.to_string(), "((Int -> a) -> a)");

        Ok(())
    }
}
