use std::rc::Rc;

use once_cell::unsync::OnceCell;

use crate::analyze::{Analyzer, Imports};
use crate::wasm::compile::compile_module;
use crate::wasm::scope::ScopeStack;
use crate::wasm::wasm_module::WasmModule;

include!(concat!(env!("OUT_DIR"), "/builtin_std.rs"));

pub struct Compiler {
    pub scope: ScopeStack,
    pub wasm_module: WasmModule,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            scope: ScopeStack::new(),
            wasm_module: WasmModule::default(),
        }
    }

    pub fn compile<S>(&mut self, source: S) -> anyhow::Result<Imports>
    where
        S: Into<String>,
    {
        let mut analyzer = Analyzer::from_source(
            source.into(),
            Compiler::import_std()?,
        );
        let module = analyzer.analyze()?;
        compile_module(self, &module)?;
        Ok(analyzer.imports.clone())
    }

    pub fn import_std() -> anyhow::Result<Imports> {
        thread_local! {
            static STD_CACHE: OnceCell<Imports> = OnceCell::new();
        }

        STD_CACHE.with(|cell| {
            cell.get_or_try_init(|| {
                let imports = Default::default();
                for name in STD.file_names() {
                    let data = STD.get(name)?;
                    let data = std::str::from_utf8(&data)?;
                    let mut analyzer = Analyzer::from_source(
                        data.to_owned(),
                        Rc::clone(&imports),
                    );
                    let module = analyzer.analyze()?;
                    imports.borrow_mut().insert(
                        name.trim_start_matches("src/")
                            .trim_end_matches(".gs")
                            .replace("/", "::")
                            .into(),
                        module,
                    );
                }
                Ok(imports)
            })
            .map(Rc::clone)
        })
    }

    pub fn finish(&self) -> Vec<u8> {
        self.wasm_module.finish()
    }

    fn debug_print(&self) {
        println!(
            "{}",
            wasmprinter::print_bytes(self.wasm_module.finish())
                .unwrap()
        );
    }
}

#[cfg(test)]
mod tests {
    use wasmparser::{Validator, WasmFeatures};

    use crate::wasm::compiler::Compiler;

    #[test]
    fn example_fib() {
        let mut compiler = Compiler::new();

        compiler
            .compile(include_str!("../../examples/fib.gs").to_owned())
            .unwrap();
        compiler.debug_print();

        Validator::new()
            .wasm_features({
                let mut features = WasmFeatures::default();
                features.module_linking = true;
                features
            })
            .validate_all(&compiler.finish())
            .unwrap();
    }
}
