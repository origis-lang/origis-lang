use crate::analyze::typed_ast::{Ident, Type};

#[derive(Debug, Clone)]
pub struct Value {
    pub name: Ident,
    pub ty: Type,
}
