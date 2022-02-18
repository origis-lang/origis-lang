use crate::analyze::typed_ast::decl::Decl;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Module {
    pub decls: SmallVec<[Decl; 10]>,
}

impl Module {
    pub fn get_export(&self, name: &str) -> Option<&Decl> {
        self.decls.iter().find(|decl| {
            if !decl.is_pub() {
                return false;
            }
            match decl {
                Decl::FnDecl(fn_decl) => &*fn_decl.name.0 == name,
                Decl::Import(_, _) => {
                    todo!()
                }
            }
        })
    }
}
