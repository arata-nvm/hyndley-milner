use core::fmt;
use std::{cell::RefCell, collections::HashMap};

#[derive(Debug)]
pub enum Expr {
    Int(isize),
    Bool(bool),
    Variable(String),
    Lambda(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Lambda(Box<Type>, Box<Type>),
    Variable(String),
}

fn eint(val: isize) -> Expr {
    Expr::Int(val)
}

fn ebool(val: bool) -> Expr {
    Expr::Bool(val)
}

fn evar(name: &str) -> Expr {
    Expr::Variable(name.to_string())
}

fn elambda(name: &str, body: Expr) -> Expr {
    Expr::Lambda(name.to_string(), Box::new(body))
}

fn elet(name: &str, expr: Expr, body: Expr) -> Expr {
    Expr::Let(name.to_string(), Box::new(expr), Box::new(body))
}

fn eapply(func: Expr, arg: Expr) -> Expr {
    Expr::Apply(Box::new(func), Box::new(arg))
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            Int(val) => write!(f, "{val}"),
            Bool(val) => write!(f, "{val}"),
            Variable(name) => write!(f, "{name}"),
            Lambda(name, body) => write!(f, "λ{name}.{body}"),
            Let(name, expr, body) => write!(f, "let {name} = {expr} in {body}"),
            Apply(func, arg) => write!(f, "({func} {arg})"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            Lambda(typ1, typ2) => write!(f, "{typ1} → {typ2}"),
            Variable(name) => write!(f, "{name}"),
        }
    }
}

fn dump_env(env: &Env) {
    print!("\x1b[31m{{");
    for (key, val) in env {
        print!("{key}: {val}, ");
    }
    print!("}}\x1b[0m");
}

fn next_type_variable() -> Type {
    thread_local! {
        pub static INDEX: RefCell<u8> = RefCell::new(0);
    }

    let name = INDEX.with(|index| {
        let next_name = (b'a' + *index.borrow()) as char;
        *index.borrow_mut() += 1;
        next_name
    });
    Type::Variable(format!("'{name}"))
}

impl Type {
    fn var_name(&self) -> String {
        match self {
            Type::Variable(name) => name.clone(),
            _ => panic!(),
        }
    }
}

pub type Env = HashMap<String, Type>;

fn infer(expr: &Expr, env: &mut Env) -> Option<Type> {
    print!("infer({expr}, ");
    dump_env(env);
    println!(")");

    match expr {
        Expr::Int(_) => Some(Type::Int),
        Expr::Bool(_) => Some(Type::Bool),
        Expr::Variable(name) => env.get(name).cloned().map(|typ| prune(typ, env)),
        Expr::Lambda(arg, body) => {
            let arg_typ = next_type_variable();
            env.insert(arg.clone(), arg_typ.clone());
            let return_typ = infer(body, env)?;
            let arg_typ = prune(arg_typ, env);
            Some(Type::Lambda(Box::new(arg_typ), Box::new(return_typ)))
        }
        Expr::Let(name, expr, body) => {
            let expr_typ = infer(expr, env)?;
            env.insert(name.clone(), expr_typ);
            infer(body, env)
        }
        Expr::Apply(func, arg) => {
            let func_typ = infer(func, env)?;
            let arg_typ = infer(arg, env)?;
            let ret_typ = next_type_variable();
            unify(
                Type::Lambda(Box::new(arg_typ), Box::new(ret_typ.clone())),
                func_typ,
                env,
            );

            if let Some(resolved_ret_typ) = env.get(&ret_typ.var_name()) {
                Some(resolved_ret_typ.clone())
            } else {
                Some(ret_typ)
            }
        }
    }
}

fn unify(t1: Type, t2: Type, env: &mut Env) {
    print!("unify({t1}, {t2}, ");
    dump_env(env);
    println!(")");

    let t1 = prune(t1, env);
    let t2 = prune(t2, env);
    match (&t1, &t2) {
        (Type::Variable(t1_name), _) => {
            if t1 != t2 {
                env.insert(t1_name.clone(), t2);
            }
        }
        (Type::Lambda(_, _), Type::Variable(_)) => unify(t1, t2, env),
        (Type::Lambda(t1_typ1, t1_typ2), Type::Lambda(t2_typ1, t2_typ2)) => {
            unify(*t1_typ1.clone(), *t2_typ1.clone(), env);
            unify(*t1_typ2.clone(), *t2_typ2.clone(), env);
        }
        _ => {}
    }
}

fn prune(typ: Type, env: &mut Env) -> Type {
    print!("prune({typ}, ");
    dump_env(env);
    println!(")");

    if let Type::Variable(ref name) = typ {
        if let Some(typ_instance) = env.get(name) {
            let typ = prune(typ_instance.clone(), env);
            env.insert(name.clone(), typ.clone());
            return typ;
        }
    }

    typ
}
pub fn main() {
    let exprs = vec![
        eint(1),
        evar("true"),
        elambda("x", eint(1)),
        elambda("x", evar("x")),
        elambda("x", elambda("y", eint(1))),
        elet("x", eint(1), evar("x")),
        evar("add"),
        eapply(evar("add"), eint(1)),
        eapply(eapply(evar("add"), eint(1)), eint(2)),
        elambda("x", eapply(eapply(evar("add"), evar("x")), eint(1))),
        elambda(
            "x",
            elambda("y", eapply(eapply(evar("add"), evar("x")), evar("y"))),
        ),
    ];

    let mut env = Env::new();
    env.insert(String::from("true"), Type::Bool);
    env.insert(
        String::from("add"),
        Type::Lambda(
            Box::new(Type::Int),
            Box::new(Type::Lambda(Box::new(Type::Int), Box::new(Type::Int))),
        ),
    );

    for expr in exprs {
        let typ = infer(&expr, &mut env);
        print!("{expr}: ");
        match typ {
            None => println!("err"),
            Some(typ) => println!("{typ}"),
        }
        println!("------------------------------------------------------");
    }
}
