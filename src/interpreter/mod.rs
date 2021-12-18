mod value;

use compact_str::CompactStr;
use fnv::FnvHashMap;
use crate::interpreter::value::Value;

use crate::parser::ast;
use crate::parser::ast::{Definition, Expr, Operator, PrimitiveValue, Stmt};

pub struct Interpreter {
    modules: FnvHashMap<CompactStr, ModuleCtx>,
}

pub type Symbols = FnvHashMap<CompactStr, Symbol>;

pub struct ModuleCtx {
    mod_name: CompactStr,
    global_symbols: Symbols,
}

pub struct StackFrame {
    symbols: Symbols,
}

#[derive(Clone)]
pub struct Func {
    body: ast::FuncDefBody,
    params: ast::FuncDefParams,
}

pub type FuncArgs = FnvHashMap<CompactStr, Value>;

pub enum Symbol {
    Value(Value),
    Func(Func),
}

impl StackFrame {
    pub fn store(&mut self, name: CompactStr, symbol: Symbol) {
        self.symbols.insert(name, symbol);
    }
}

impl Func {
    pub fn call(&self, interp: &mut Interpreter, mut args: FuncArgs) -> Value {
        let mut frame = StackFrame {
            symbols: FnvHashMap::default(),
        };
        self.params.0.iter().for_each(|param| {
            let arg = args.remove(&param.name.0).unwrap();
            frame.store(param.name.0.clone(), Symbol::Value(arg));
        });
        self.body.stmts.iter().for_each(|stmt| {
            interp.eval_stmt(&mut frame, stmt);
        });

        if let Some(ret_expr) = &self.body.ret {
            interp.eval_expr(&mut frame, ret_expr)
        } else {
            Value::Unit
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            modules: FnvHashMap::default(),
        }
    }

    pub fn add_module(&mut self, name: &str, module_ast: ast::Module) {
        let mut ctx = ModuleCtx {
            mod_name: CompactStr::from(name),
            global_symbols: FnvHashMap::default(),
        };
        module_ast.defs.into_iter().for_each(|def| define(&mut ctx.global_symbols, def));
        self.modules.insert(CompactStr::from(name), ctx);
    }

    pub fn call(&mut self, module_name: &str, func_name: &str, args: FuncArgs) -> Value {
        let f = self
            .modules
            .get(module_name)
            .map(|ctx| ctx.global_symbols.get(func_name))
            .flatten()
            .map(|symbol| match symbol {
                Symbol::Func(f) => Some(f),
                _ => None,
            })
            .flatten()
            .expect("not found")
            .clone();
        f.call(self, args)
    }

    pub fn eval_stmt(&mut self, ctx: &mut StackFrame, stmt: &ast::Stmt) {
        match stmt {
            Stmt::VarDef(var_def) => {
                let symbol = Symbol::Value(self.eval_expr(ctx, &var_def.val));
                ctx.store(var_def.name.0.clone(), symbol);
            }
            Stmt::Expr(expr) => {
                self.eval_expr(ctx, expr);
            }
            Stmt::Definition(def) => {
                define(&mut ctx.symbols, def.clone())
            }
        }
    }

    pub fn eval_expr(&mut self, ctx: &mut StackFrame, expr: &ast::Expr) -> Value {
        match expr {
            Expr::Value(val_ast) => {
                match val_ast {
                    ast::Value::Primitive(primitive_value) => {
                        match primitive_value {
                            PrimitiveValue::Char(_) => {
                                todo!()
                            }
                            PrimitiveValue::Integer(int) => {
                                Value::Integer(int.as_i64())
                            }
                            PrimitiveValue::Float(_) => {todo!()}
                        }
                    }
                    ast::Value::String(_) => {
                        todo!()
                    }
                    ast::Value::Array(_) => {
                        todo!()
                    }
                    ast::Value::Tuple(_) => {
                        todo!()
                    }
                }
            }
            Expr::Ident(id) => {
                todo!()
            }
            Expr::OpExpr(expr) => {
                let lhs = self.eval_expr(ctx, &expr.lhs);
                let rhs = self.eval_expr(ctx, &expr.rhs);
                match expr.op {
                    Operator::Add(_) => {
                        lhs + rhs
                    }
                    Operator::Sub(_) => {todo!()}
                    Operator::Mul(_) => {todo!()}
                    Operator::Div(_) => {todo!()}
                    Operator::Pow(_) => {todo!()}
                }
            }
        }
    }
}

pub fn define(symbols: &mut Symbols, def: ast::Definition) {
    match def {
        Definition::FuncDef(func_def) => {
            symbols.insert(
                func_def.name.0,
                Symbol::Func(Func {
                    body: func_def.body,
                    params: func_def.params,
                }),
            );
        }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::Interpreter;

    #[test]
    fn test_add() {
        let ast = crate::parser::parse_module(r"fn test_add() () { 1 + 1 }");
        let mut interp = Interpreter::new();
        interp.add_module("Main", ast);
        let ret = interp.call("Main", "test_add", crate::map! {});
        dbg!(ret);
    }
}
