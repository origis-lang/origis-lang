use wasm_encoder::{BlockType, Function, Instruction};

use crate::analyze::typed_ast::{Expr, LiteralExpr, Op, Type};
use crate::analyze::Typed;
use crate::wasm::compile::block::compile_block;
use crate::wasm::compiler::Compiler;

pub fn compile_expr(
    compiler: &mut Compiler,
    func: &mut Function,
    expr: &Expr,
) -> anyhow::Result<()> {
    match expr {
        Expr::Literal(lit) => {
            compile_literal_expr(compiler, func, lit)?;
        }
        Expr::Ident(id) => match &id.ty {
            Type::Int | Type::Float | Type::Bool | Type::Char => {
                func.instruction(&Instruction::LocalGet(
                    compiler.scope.get_symbol(&id.name)?.idx(),
                ));
            }
            Type::Ref(_) => {}
            Type::Func { .. } => {}
            Type::Void => {}
            Type::Struct { .. } => {}
        },
        Expr::CallFunc { name, args, .. } => {
            let idx = compiler.scope.get_symbol(&name)?.idx();
            for expr in args {
                compile_expr(compiler, func, expr)?;
            }
            func.instruction(&Instruction::Call(idx));
        }
        Expr::Block(block) => compile_block(compiler, func, block)?,
        Expr::If {
            cond,
            body,
            else_if,
            _else,
        } => {
            compiler.scope.current_mut().entry_if();

            compile_expr(compiler, func, cond)?;
            func.instruction(&Instruction::If(
                body.return_type()
                    .to_wasm_type()
                    .map(|ty| BlockType::Result(ty))
                    .unwrap_or_else(|| BlockType::Empty),
            ));
            compile_block(compiler, func, body)?;

            if !else_if.is_empty() {
                func.instruction(&Instruction::Else);

                for (br_cond, br_block) in else_if {
                    compile_expr(compiler, func, br_cond)?;
                    compiler.scope.current_mut().entry_if();
                    func.instruction(&Instruction::If(
                        br_block.return_type()
                            .to_wasm_type()
                            .map(|ty| BlockType::Result(ty))
                            .unwrap_or_else(|| BlockType::Empty),
                    ));
                    compile_block(compiler, func, br_block)?;
                    func.instruction(&Instruction::Else);
                }
                if let Some(el) = _else {
                    compile_block(compiler, func, el)?;
                } else {
                    func.instruction(&Instruction::Nop);
                }
                for _ in else_if {
                    func.instruction(&Instruction::End);
                    compiler.scope.current_mut().exit_if();
                }
            } else {
                if let Some(el) = _else {
                    func.instruction(&Instruction::Else);
                    compile_block(compiler, func, el)?;
                }
            }
            func.instruction(&Instruction::End);
            compiler.scope.current_mut().exit_if();
        }
        Expr::Loop(block) => {
            let ret_ty = block
                .return_type()
                .to_wasm_type()
                .map(|ty| BlockType::Result(ty))
                .unwrap_or_else(|| BlockType::Empty);
            func.instruction(&Instruction::Block(ret_ty));
            func.instruction(&Instruction::Loop(ret_ty));

            compile_block(compiler, func, block)?;

            func.instruction(&Instruction::Br(0));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::End);
        }
        Expr::Unary(op, rhs) => {
            compile_expr(compiler, func, rhs)?;
            match (op, rhs.return_type()) {
                (Op::Minus, Type::Int) => {
                    func.instruction(&Instruction::I64Ne);
                }
                (Op::Minus, Type::Float) => {
                    func.instruction(&Instruction::F64Ne);
                }
                (Op::Not, Type::Bool) => {
                    func.instruction(&Instruction::I32Eqz);
                }
                _ => anyhow::bail!("Illegal operator: {:?}", op),
            };
        }
        Expr::Binary(lhs, op, rhs) => {
            compile_expr(compiler, func, lhs)?;
            compile_expr(compiler, func, rhs)?;
            compile_op(
                compiler,
                func,
                op,
                lhs.return_type(),
                rhs.return_type(),
            )?
        }
    }
    Ok(())
}

pub fn compile_literal_expr(
    _compiler: &mut Compiler,
    func: &mut Function,
    expr: &LiteralExpr,
) -> anyhow::Result<()> {
    match expr {
        LiteralExpr::Integer(i) => {
            func.instruction(&Instruction::I64Const(*i));
        }
        LiteralExpr::String(_) => {
            func.instruction(&Instruction::Nop);
            todo!()
        }
        LiteralExpr::Float(f) => {
            func.instruction(&Instruction::F64Const(*f));
        }
        LiteralExpr::Bool(b) => {
            func.instruction(&Instruction::I32Const(*b as i32));
        }
        LiteralExpr::Char(ch) => {
            func.instruction(&Instruction::I32Const(*ch as i32));
        }
    }
    Ok(())
}

pub fn compile_op(
    _compiler: &mut Compiler,
    func: &mut Function,
    op: &Op,
    lhs_ty: Type,
    rhs_ty: Type,
) -> anyhow::Result<()> {
    match (lhs_ty, op, rhs_ty) {
        (Type::Int, Op::Plus, Type::Int) => {
            func.instruction(&Instruction::I64Add);
        }
        (Type::Float, Op::Plus, Type::Float) => {
            func.instruction(&Instruction::F64Add);
        }
        (Type::Int, Op::Minus, Type::Int) => {
            func.instruction(&Instruction::I64Sub);
        }
        (Type::Float, Op::Minus, Type::Float) => {
            func.instruction(&Instruction::F64Sub);
        }
        (Type::Int, Op::Eq, Type::Int) => {
            func.instruction(&Instruction::I64Eq);
        }
        (Type::Float, Op::Eq, Type::Float) => {
            func.instruction(&Instruction::F64Eq);
        }
        (Type::Bool, Op::Eq, Type::Bool) => {
            func.instruction(&Instruction::I32Eq);
        }
        (Type::Char, Op::Eq, Type::Char) => {
            func.instruction(&Instruction::I32Eq);
        }
        (Type::Int, Op::Ne, Type::Int) => {
            func.instruction(&Instruction::I64Ne);
        }
        (Type::Float, Op::Ne, Type::Float) => {
            func.instruction(&Instruction::F64Ne);
        }
        (Type::Bool, Op::Ne, Type::Bool) => {
            func.instruction(&Instruction::I32Ne);
        }
        (Type::Char, Op::Ne, Type::Char) => {
            func.instruction(&Instruction::I32Ne);
        }
        (Type::Int, Op::Le, Type::Int) => {
            func.instruction(&Instruction::I64LeS);
        }
        (Type::Float, Op::Le, Type::Float) => {
            func.instruction(&Instruction::F64Le);
        }
        (Type::Int, Op::Ge, Type::Int) => {
            func.instruction(&Instruction::I64GeS);
        }
        (Type::Float, Op::Ge, Type::Float) => {
            func.instruction(&Instruction::F64Ge);
        }
        (Type::Int, Op::Lt, Type::Int) => {
            func.instruction(&Instruction::I64LtS);
        }
        (Type::Float, Op::Lt, Type::Float) => {
            func.instruction(&Instruction::F64Lt);
        }
        (Type::Int, Op::Gt, Type::Int) => {
            func.instruction(&Instruction::I64GtS);
        }
        (Type::Float, Op::Gt, Type::Float) => {
            func.instruction(&Instruction::F64Gt);
        }

        _ => anyhow::bail!("Illegal operator: {:?}", op),
    }
    Ok(())
}
