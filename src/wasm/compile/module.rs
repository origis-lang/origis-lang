use crate::analyze::typed_ast::{Decl, Module};
use crate::wasm::compile::func::compile_function;
use crate::wasm::compiler::Compiler;
use smallvec::SmallVec;
use wasm_encoder::EntityType;

pub fn compile_module(
    compiler: &mut Compiler,
    module: &Module,
) -> anyhow::Result<()> {
    for decl in &module.decls {
        match decl {
            Decl::FnDecl(fn_decl) => {
                compile_function(compiler, fn_decl)?
            }
            Decl::Import(path, decl) => match &**decl {
                Decl::FnDecl(fn_decl) => {
                    let idx =
                        compiler.scope.current_mut().set_symbol(
                            fn_decl.name.0.clone(),
                            fn_decl.to_type(),
                        );
                    let wasm_type_params = fn_decl
                        .params
                        .iter()
                        .filter_map(|(id, ty)| {
                            let val = ty.to_wasm_type()?;
                            compiler
                                .scope
                                .current_mut()
                                .set_symbol(id.0.clone(), ty.clone());
                            Some(val)
                        })
                        .collect::<SmallVec<[_; 5]>>();
                    let results = fn_decl
                        .ret_type
                        .to_wasm_type()
                        .map(|ty| SmallVec::from([ty]))
                        .unwrap_or(SmallVec::new_const());
                    compiler
                        .wasm_module
                        ._type
                        .function(wasm_type_params, results);
                    compiler.wasm_module.import.import(
                        path,
                        Some(fn_decl.name.as_str()),
                        EntityType::Function(idx),
                    );
                }
                Decl::Import(_, _) => {}
            },
        }
    }
    Ok(())
}
