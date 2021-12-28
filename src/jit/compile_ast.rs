use inkwell::module::Module;

use crate::jit::JitCompiler;
use crate::parser::ast;
use crate::parser::ast::{Expr, PrimitiveValue, Value};

pub struct Context<'ctx> {
    pub(crate) jit_compiler: &'ctx JitCompiler<'ctx>,
    pub(crate) module: Module<'ctx>,
}

pub fn compile_stmt(ctx: &Context, stmt: &ast::Stmt) {
    match stmt {
        ast::Stmt::VarDef(ast) => compile_var_def(ctx, ast),
        ast::Stmt::Expr(ast) => compile_expr(ctx, ast),
    }
}

pub fn compile_var_def(ctx: &Context, var_def: &ast::VarDef) {
    match &var_def.val {
        Expr::Value(val) => match val {
            Value::Primitive(val) => match val {
                PrimitiveValue::Char(char) => {
                    let int_ty = ctx.jit_compiler.ctx.i16_type();
                    let ptr = ctx
                        .jit_compiler
                        .builder
                        .build_alloca(int_ty, var_def.name.0);
                    ctx.jit_compiler.builder.build_store(
                        ptr,
                        int_ty
                            .const_int(char.val as u16 as u64, false),
                    );
                }
                PrimitiveValue::Integer(int) => {
                    let int_ty = ctx.jit_compiler.ctx.i64_type();
                    let ptr = ctx
                        .jit_compiler
                        .builder
                        .build_alloca(int_ty, var_def.name.0);
                    ctx.jit_compiler.builder.build_store(
                        ptr,
                        int_ty.const_int(int.as_i64() as u64, true),
                    );
                }
                PrimitiveValue::Float(float) => {
                    let float_ty = ctx.jit_compiler.ctx.f64_type();
                    let ptr = ctx
                        .jit_compiler
                        .builder
                        .build_alloca(float_ty, var_def.name.0);
                    ctx.jit_compiler.builder.build_store(
                        ptr,
                        float_ty.const_float(float.val),
                    );
                }
            },
            Value::String(_) => {}
            Value::Array(_) => {}
            Value::Tuple(_) => {}
        },
        Expr::Ident(_) => {}
        Expr::OpExpr(_) => {}
    }
}

pub fn compile_expr(ctx: &Context, expr: &ast::Expr) {}
