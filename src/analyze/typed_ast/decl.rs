use crate::analyze::typed_ast::{Block, Ident, Type};
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub enum Decl {
    FnDecl(FnDecl),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub vis: bool,
    pub name: Ident,
    pub params: SmallVec<[(Ident, Type); 3]>,
    pub ret_type: Type,
    pub body: Block,
}
