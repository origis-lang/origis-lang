use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};

use compact_str::CompactStr;
use fnv::FnvHashMap;
use phf::Set;

use crate::interpreter::eval::{eval_expr, eval_stmt, Context};
use crate::interpreter::value::Value;
use crate::interpreter::{
    InterpretationError, Interpreter, ModuleCtx, Symbols,
};
use crate::parser::ast;

pub type BuiltinFn = fn(
    interp: &Interpreter,
    mod_ctx: &ModuleCtx,
    args: FuncArgs,
) -> Result<Value, InterpretationError>;

#[derive(Clone)]
pub enum Func {
    Ast {
        params: Params,
        body: ast::FuncDefBody,
    },
    Builtin {
        params: Params,
        fn_ptr: BuiltinFn,
    },
}

#[derive(Clone)]
pub enum Params {
    Fixed(ast::FuncDefParams),
    Variable,
}

pub type FuncArgs = FnvHashMap<CompactStr, Value>;

pub struct StackFrame {
    pub symbols: Symbols,
}

impl From<&phf::Set<&'static str>> for Params {
    fn from(set: &Set<&'static str>) -> Self {
        Params::Fixed(ast::FuncDefParams(
            set.iter()
                .map(|s| ast::FuncDefParam {
                    name: ast::Ident(CompactStr::from(*s)),
                    ty: ast::Type::Unit(ast::UnitType),
                })
                .collect(),
        ))
    }
}

impl StackFrame {
    pub fn store(&mut self, name: CompactStr, val: Value) {
        self.symbols.0.insert(name, val);
    }
}

impl Func {
    pub fn call(
        &self,
        interp: &Interpreter,
        mod_ctx: &ModuleCtx,
        args: FuncArgs,
    ) -> Result<Value, InterpretationError> {
        match self {
            Func::Ast { body, .. } => {
                let mut frame = StackFrame {
                    symbols: Symbols(args),
                };
                let mut ctx = Context {
                    interp,
                    module_ctx: mod_ctx,
                    stack_frame: &mut frame,
                };

                body.stmts
                    .iter()
                    .try_for_each(|stmt| eval_stmt(&mut ctx, stmt))?;

                if let Some(ret_expr) = &body.ret {
                    eval_expr(&mut ctx, ret_expr)
                } else {
                    Ok(Value::unit())
                }
            }
            Func::Builtin { fn_ptr, .. } => {
                fn_ptr(interp, mod_ctx, args)
            }
        }
    }

    pub fn params(&self) -> &Params {
        match self {
            Func::Ast { params, .. } => params,
            Func::Builtin { params, .. } => params,
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "function@")?;
        match self {
            Func::Ast { params, .. } => match params {
                Params::Fixed(params) => f
                    .debug_list()
                    .entries(params.0.iter().map(|param| {
                        format!("{}({})", param.name, param.ty)
                    }))
                    .finish(),
                Params::Variable => write!(f, "variable"),
            },
            Func::Builtin { .. } => write!(f, "function@builtin"),
        }
    }
}

impl PartialEq for Func {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl PartialOrd for Func {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}
