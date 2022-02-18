use crate::analyze::analyzer::Typed;
use crate::analyze::typed_ast::Op;
use crate::analyze::typed_ast::{Block, Ident, Type, Value};
use compact_str::CompactStr;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralExpr),
    Convert(Box<Expr>, Type),
    Ident(Value),
    CallFunc {
        name: Ident,
        args: Vec<Expr>,
        ret_ty: Type,
    },
    If {
        cond: Box<Expr>,
        body: Box<Block>,
        else_if: Vec<(Expr, Block)>,
        _else: Option<Box<Block>>,
    },
    Block(Box<Block>),
    Loop(Box<Block>),

    Unary(Op, Box<Expr>),
    Binary(Box<Expr>, Op, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Integer(i64),
    String(CompactStr),
    Float(f64),
    Bool(bool),
    Char(char),
}

impl Typed for Expr {
    fn return_type(&self) -> Type {
        match self {
            Expr::Literal(lit) => lit.return_type(),
            Expr::Ident(val) => val.ty.clone(),
            Expr::CallFunc { ret_ty, .. } => ret_ty.clone(),
            Expr::Convert(_, ty) => ty.clone(),
            Expr::Block(block) => block.return_type(),
            Expr::Loop(block) => block.return_type(),
            Expr::If { body, .. } => body.return_type(),
            Expr::Unary(op, expr) => match op {
                Op::Not => Type::Bool,
                Op::Minus => expr.return_type(),
                _ => unreachable!(),
            },
            Expr::Binary(l_expr, op, ..) => match op {
                Op::Eq
                | Op::Ne
                | Op::Lt
                | Op::Gt
                | Op::Le
                | Op::Ge
                | Op::Or
                | Op::And => Type::Bool,
                _ => l_expr.return_type(),
            },
        }
    }
}

impl Typed for LiteralExpr {
    fn return_type(&self) -> Type {
        match self {
            LiteralExpr::Integer(_) => Type::Int,
            LiteralExpr::String(_) => todo!(),
            LiteralExpr::Float(_) => Type::Float,
            LiteralExpr::Bool(_) => Type::Bool,
            LiteralExpr::Char(_) => Type::Char,
        }
    }
}
