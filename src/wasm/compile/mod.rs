use wasm_encoder::ValType;

pub use module::compile_module;

use crate::analyze::typed_ast::Type;

mod block;
mod expr;
mod func;
mod module;

cfg_if::cfg_if! {
    if #[cfg(feature = "wasm64")] {
        const PTR_SIZE: u32 = 8;
        const PTR_TYPE: ValType = ValType::I64;
    } else {
        const PTR_SIZE: u32 = 4;
        const PTR_TYPE: ValType = ValType::I32;
    }
}

impl Type {
    pub fn to_wasm_type(&self) -> Option<ValType> {
        Some(match self {
            Type::Void => return None,
            Type::Int => ValType::I64,
            Type::Float => ValType::F64,
            Type::Char => ValType::I32,
            Type::Bool => ValType::I32,
            Type::Ref(_) => PTR_TYPE,
            Type::Func(_) => ValType::FuncRef,
            Type::Struct { .. } => {
                todo!()
            }
        })
    }
}
