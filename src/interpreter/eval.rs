use compact_str::CompactStr;
use itertools::Itertools;
use smallvec::SmallVec;

use crate::interpreter::func::{Func, StackFrame};
use crate::interpreter::value::{GcValue, Value};
use crate::interpreter::{
    InterpretationError, Interpreter, ModuleCtx, Params, Symbols,
};
use crate::parser::ast;

pub struct Context<'a> {
    pub interp: &'a Interpreter,
    pub module_ctx: &'a ModuleCtx,
    pub stack_frame: &'a mut StackFrame,
}

pub fn eval_stmt(
    ctx: &mut Context,
    stmt: &ast::Stmt,
) -> Result<(), InterpretationError> {
    match stmt {
        ast::Stmt::VarDef(var_def) => {
            let val = eval_expr(ctx, &var_def.val)?;
            ctx.stack_frame.store(var_def.name.0.clone(), val);
        }
        ast::Stmt::Expr(expr) => {
            eval_expr(ctx, expr)?;
        }
        ast::Stmt::Definition(def) => define(
            ctx.interp,
            &mut ctx.stack_frame.symbols,
            def.clone(),
        )?,
    }

    Ok(())
}

#[inline]
fn eval_exprs<'a, I>(
    ctx: &mut Context,
    exprs: I,
) -> Result<SmallVec<[Value; 10]>, InterpretationError>
where
    I: Iterator<Item = &'a ast::Expr>,
{
    exprs.map(|expr| eval_expr(ctx, expr)).collect()
}

#[inline]
fn eval_stmts<'a, I>(
    ctx: &mut Context,
    mut stmts: I,
) -> Result<(), InterpretationError>
where
    I: Iterator<Item = &'a ast::Stmt>,
{
    stmts.try_for_each(|stmt| eval_stmt(ctx, stmt))
}

pub fn eval_expr(
    ctx: &mut Context,
    expr: &ast::Expr,
) -> Result<Value, InterpretationError> {
    Ok(
        match expr {
            ast::Expr::Value(val_ast) => {
                match val_ast {
                    ast::Value::Primitive(primitive_value) => {
                        match primitive_value {
                            ast::PrimitiveValue::Char(ch) => {
                                Value::char(ch.val)
                            }
                            ast::PrimitiveValue::Integer(int) => {
                                Value::int(int.as_i64())
                            }
                            ast::PrimitiveValue::Float(float) => {
                                Value::float(float.val)
                            }
                            ast::PrimitiveValue::Bool(bool) => {
                                Value::bool(bool.0)
                            }
                        }
                    }
                    ast::Value::String(s) => {
                        Value::str(s.val.clone())
                    }
                    ast::Value::Array(arr) => {
                        Value::arr(
                            arr.params
                                .0
                                .iter()
                                .map(|expr| eval_expr(ctx, expr))
                                .collect::<Result<
                                    Vec<Value>,
                                    InterpretationError,
                                >>(
                                )?,
                        )
                    }
                    ast::Value::Tuple(_) => {
                        todo!()
                    }
                }
            }
            ast::Expr::Ident(id) => ctx
                .stack_frame
                .symbols
                .get(&id.0)
                .or_else(|_| {
                    ctx.module_ctx.global_symbols.get(&id.0)
                })?
                .clone(),
            ast::Expr::Call(func_call) => {
                let val = ctx
                    .stack_frame
                    .symbols
                    .get(&func_call.name.0)
                    .or_else(|_| {
                        ctx.module_ctx
                            .global_symbols
                            .get(&func_call.name.0)
                    })?
                    .clone();

                macro_rules! err {
                    () => {
                        return Err(InterpretationError::TypeNotMatch(
                            "func",
                            val.type_name(),
                        ))
                    };
                }

                match &val {
                    Value::Gc(s) => match &*s.borrow() {
                        GcValue::Func(f) => {
                            let arg_values = eval_exprs(
                                ctx,
                                func_call.args.0.iter(),
                            )?
                            .into_iter();
                            let args = match f.params() {
                            Params::Fixed(fdp) => arg_values
                                .zip(fdp.0.iter())
                                .map(|(arg, param)| (param.name.0.clone(), arg))
                                .collect(),
                            Params::Variable => arg_values
                                .zip(0..)
                                .map(|(arg, i)| {
                                    (
                                        {
                                            use std::io::Write;
                                            assert!(i <= 999);
                                            let mut data = [0u8; 3];
                                            write!(&mut data[..], "{}", i).unwrap();
                                            CompactStr::new_inline(unsafe {
                                                std::str::from_utf8_unchecked(&data)
                                            })
                                        },
                                        arg,
                                    )
                                })
                                .collect(),
                        };

                            f.call(ctx.interp, ctx.module_ctx, args)?
                        }
                        _ => err!(),
                    },
                    _ => err!(),
                }
            }
            ast::Expr::If(if_expr) => {
                let condition = eval_expr(ctx, &if_expr.condition)?
                    .expect_bool()?;
                let ret_expr = if condition {
                    eval_stmts(ctx, if_expr.body.stmts.iter())?;
                    &if_expr.body.ret
                } else {
                    let elif =
                        if_expr.else_ifs.iter().try_find(|elif| {
                            eval_expr(ctx, &elif.condition)?
                                .expect_bool()
                        })?;
                    if let Some(elif) = elif {
                        eval_stmts(ctx, elif.body.stmts.iter())?;
                        &elif.body.ret
                    } else {
                        if let Some(el) = &if_expr.r#else {
                            eval_stmts(ctx, el.0.stmts.iter())?;
                            &el.0.ret
                        } else {
                            &None
                        }
                    }
                };

                ret_expr
                    .as_ref()
                    .map(|expr| eval_expr(ctx, expr))
                    .unwrap_or_else(|| Ok(Value::unit()))?
            }
            ast::Expr::OpExpr(expr) => {
                let mut lhs = eval_expr(ctx, &expr.lhs)?;
                let rhs = eval_expr(ctx, &expr.rhs)?;
                match expr.op {
                    ast::Operator::Add(_) => (lhs + rhs)?,
                    ast::Operator::Sub(_) => (lhs - rhs)?,
                    ast::Operator::Mul(_) => (lhs * rhs)?,
                    ast::Operator::Div(_) => (lhs / rhs)?,
                    ast::Operator::Rem(_) => (lhs % rhs)?,
                    ast::Operator::AddAssign(_) => {
                        lhs.add_assign(rhs)?;
                        lhs
                    }
                    ast::Operator::Eq(_) => Value::bool(lhs == rhs),
                    ast::Operator::NotEq(_) => {
                        Value::bool(lhs != rhs)
                    }
                    ast::Operator::Less(_) => Value::bool(lhs < rhs),
                    ast::Operator::Greater(_) => {
                        Value::bool(lhs > rhs)
                    }
                    ast::Operator::LessOrEq(_) => {
                        Value::bool(lhs <= rhs)
                    }
                    ast::Operator::GreaterOrEq(_) => {
                        Value::bool(lhs >= rhs)
                    }
                    _ => todo!(),
                }
            }
        },
    )
}

pub fn define(
    interp: &Interpreter,
    symbols: &mut Symbols,
    def: ast::Definition,
) -> Result<(), InterpretationError> {
    match def {
        ast::Definition::FuncDef(func_def) => {
            symbols.0.insert(
                func_def.name.0,
                Value::func(Func::Ast {
                    body: func_def.body,
                    params: Params::Fixed(func_def.params),
                }),
            );
            Ok(())
        }
        ast::Definition::UseDef(mut use_def) => {
            let symbol_name = use_def.0 .0.pop().unwrap();
            if !use_def.0 .0.is_empty() {
                let module_path = use_def
                    .0
                     .0
                    .iter()
                    .map(|s| s.0.as_str())
                    .join("::");
                let val = interp
                    .get_module(&module_path)?
                    .global_symbols
                    .get(&symbol_name.0)?
                    .clone();
                symbols.0.insert(symbol_name.0, val);
            }
            Ok(())
        }
    }
}
