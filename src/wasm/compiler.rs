use crate::analyze::Analyzer;
use crate::wasm::compile::compile_module;
use crate::wasm::scope::ScopeStack;
use crate::wasm::wasm_module::WasmModule;
use std::path::PathBuf;

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

    pub fn compile<S>(&mut self, source: S) -> anyhow::Result<()>
    where
        S: Into<String>,
    {
        let mut analyzer =
            Analyzer::from_source(source.into(), PathBuf::new());
        let module = analyzer.analyze()?;
        compile_module(self, &module)
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
    use crate::wasm::compiler::Compiler;

    #[test]
    fn example_fib() {
        let mut compiler = Compiler::new();

        compiler
            .compile(include_str!("../../examples/fib.gs").to_owned())
            .unwrap();
        compiler.debug_print();
        wasmparser::validate(&compiler.finish()).unwrap();
    }
}
