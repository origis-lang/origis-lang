use crate::analyze::typed_ast::{Expr, Ident, Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    ImplicitReturn(Expr),
    FlowControl(FlowControl),
    Assign(Ident, Type, Expr),
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum FlowControl {
    Return(Expr),
    Break(i64),
    Continue(i64),
}
