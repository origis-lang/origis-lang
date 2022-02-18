use compact_str::CompactStr;
use smallvec::SmallVec;

use crate::analyze::typed_ast::{Block, FuncType, Ident, Type};

#[derive(Debug, Clone)]
pub enum Decl {
    FnDecl(FnDecl),
    Import(CompactStr, Box<Decl>),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub vis: bool,
    pub name: Ident,
    pub params: SmallVec<[(Ident, Type); 3]>,
    pub ret_type: Type,
    pub body: Block,
}

impl Decl {
    pub fn is_pub(&self) -> bool {
        match self {
            Decl::FnDecl(fn_decl) => fn_decl.vis,
            Decl::Import(_, _) => false,
        }
    }
}

impl FnDecl {
    pub fn to_type(&self) -> Type {
        Type::Func(FuncType {
            params: self
                .params
                .iter()
                .map(|(_, ty)| ty.clone())
                .collect(),
            ret: box self.ret_type.clone(),
        })
    }
}
