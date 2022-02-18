use crate::analyze::analyzer::Typed;
use crate::analyze::typed_ast::{Ident, Stmt, Type, Value};
use ahash::AHashMap;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: SmallVec<[Stmt; 5]>,
    pub local_symbols: AHashMap<Ident, Value>,
}

impl Typed for Block {
    fn return_type(&self) -> Type {
        match self.stmts.last() {
            Some(Stmt::ImplicitReturn(expr)) => expr.return_type(),
            _ => Type::Void,
        }
    }
}
