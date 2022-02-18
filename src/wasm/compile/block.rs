use crate::analyze::typed_ast::{Block, FlowControl, Stmt};
use crate::wasm::compile::expr::compile_expr;
use crate::wasm::compiler::Compiler;
use crate::wasm::scope::Scope;
use wasm_encoder::{Function, Instruction};

pub fn compile_block(
    compiler: &mut Compiler,
    func: &mut Function,
    block: &Block,
) -> anyhow::Result<()> {
    for stmt in &block.stmts {
        compile_stmt(compiler, func, stmt)?
    }
    Ok(())
}

pub fn compile_stmt(
    compiler: &mut Compiler,
    func: &mut Function,
    stmt: &Stmt,
) -> anyhow::Result<()> {
    match stmt {
        Stmt::Expr(expr) => {
            compile_expr(compiler, func, expr)?;
        }
        Stmt::FlowControl(fc) => match fc {
            FlowControl::Return(expr) => {
                compile_expr(compiler, func, expr)?;
                func.instruction(&Instruction::Return);
            }
            FlowControl::Break(i) => {
                let depth = match compiler.scope.current_mut() {
                    Scope::Global { .. } => unreachable!(),
                    Scope::Local { if_depth, .. } => *if_depth,
                };
                func.instruction(&Instruction::Br(
                    *i as u32 * 2 + 1 + depth,
                ));
            }
            FlowControl::Continue(i) => {
                let depth = match compiler.scope.current_mut() {
                    Scope::Global { .. } => unreachable!(),
                    Scope::Local { if_depth, .. } => *if_depth,
                };
                func.instruction(&Instruction::Br(
                    *i as u32 * 2 + depth,
                ));
            }
        },
        Stmt::Assign(id, ty, expr) => {
            let idx = compiler
                .scope
                .current_mut()
                .set_symbol(id.0.clone(), ty.clone());
            compile_expr(compiler, func, expr)?;
            func.instruction(&Instruction::LocalSet(idx));
        }
        Stmt::ImplicitReturn(expr) => {
            compile_expr(compiler, func, expr)?;
        }
        Stmt::Unreachable => {
            func.instruction(&Instruction::Unreachable);
        }
    }
    Ok(())
}
