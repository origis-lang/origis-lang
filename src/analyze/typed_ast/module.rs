use crate::analyze::typed_ast::decl::Decl;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Module {
    pub decls: SmallVec<[Decl; 10]>,
}
