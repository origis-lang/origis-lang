use smallvec::SmallVec;
use wasm_encoder::{Export, Function, Instruction};

use crate::analyze::typed_ast::{FnDecl, Type};
use crate::wasm::compile::block::compile_block;
use crate::wasm::compiler::Compiler;

pub fn compile_function(
    compiler: &mut Compiler,
    fn_decl: &FnDecl,
) -> anyhow::Result<()> {
    let idx = compiler
        .scope
        .current_mut()
        .set_symbol(fn_decl.name.0.clone(), fn_decl.to_type());

    if fn_decl.vis {
        compiler
            .wasm_module
            .export
            .export(&fn_decl.name.0, Export::Function(idx));
    }

    compiler.scope.entry();

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
    compiler.wasm_module.fun.function(idx);

    let local_idx = compiler.scope.current().current_idx();

    let locals = fn_decl
        .body
        .local_symbols
        .iter()
        .filter(|(_, val)| !matches!(val.ty, Type::Func(_)))
        .filter(|(id, _)| {
            fn_decl
                .params
                .iter()
                .find(|(id2, _)| *id == id2)
                .is_none()
        })
        .enumerate()
        .filter_map(|(idx, (_, val))| {
            Some((local_idx + idx as u32, val.ty.to_wasm_type()?))
        })
        .collect::<SmallVec<[_; 10]>>();

    let mut func = Function::new(locals);

    compile_block(compiler, &mut func, &fn_decl.body)?;
    func.instruction(&Instruction::End);

    compiler.wasm_module.code.function(&func);
    compiler.scope.exit();
    Ok(())
}
