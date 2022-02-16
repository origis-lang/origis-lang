use crate::analyze::typed_ast::{Decl, Module};
use crate::wasm::compile::func::compile_function;
use crate::wasm::compiler::Compiler;

pub fn compile_module(
    compiler: &mut Compiler,
    module: &Module,
) -> anyhow::Result<()> {
    for decl in &module.decls {
        match decl {
            Decl::FnDecl(fn_decl) => {
                compile_function(compiler, fn_decl)?
            }
        }
    }
    Ok(())
}
