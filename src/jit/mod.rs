use inkwell::builder::Builder;
use inkwell::context::Context as LLContext;
use inkwell::OptimizationLevel;

use crate::parser::ast;
use crate::parser::ast::Expr;

mod compile_ast;

#[derive(Debug, Clone)]
pub struct Module<'p> {
    name: String,
    ast: ast::Module<'p>,
}

pub struct JitCompiler<'ctx> {
    ctx: &'ctx LLContext,
    builder: Builder<'ctx>,
}

impl<'ctx> JitCompiler<'ctx> {
    pub fn compile(module: Module) {
        let mut context = LLContext::create();
        let compiler = JitCompiler {
            builder: context.create_builder(),
            ctx: &context,
        };

        let ll_module = compiler.ctx.create_module(&module.name);
        let context = compile_ast::Context {
            jit_compiler: &compiler,
            module: ll_module,
        };
        module.ast.stmts.into_iter().for_each(|stmt| {
            compile_ast::compile_stmt(&context, &stmt)
        })
    }
}
