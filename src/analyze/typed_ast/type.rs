use crate::analyze::typed_ast::Ident;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Float,
    Char,
    Bool,
    Ref(Box<Type>),
    Func(FuncType),
    Struct {
        name: Ident,
        fields: Vec<(Ident, Type)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}
