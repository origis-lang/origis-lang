use wasm_encoder::{
    CodeSection, DataSection, ElementSection, ExportSection,
    FunctionSection, GlobalSection, ImportSection, MemorySection,
    Module, TableSection, TypeSection,
};

#[derive(Default)]
pub struct WasmModule {
    pub _type: TypeSection,
    pub import: ImportSection,
    pub fun: FunctionSection,
    pub table: TableSection,
    pub memory: MemorySection,
    pub global: GlobalSection,
    pub export: ExportSection,
    pub element: ElementSection,
    pub code: CodeSection,
    pub data: DataSection,
}

impl WasmModule {
    pub fn finish(&self) -> Vec<u8> {
        let mut module = Module::new();
        module.section(&self._type);
        module.section(&self.import);
        module.section(&self.fun);
        module.section(&self.table);
        module.section(&self.memory);
        module.section(&self.global);
        module.section(&self.export);
        module.section(&self.element);
        module.section(&self.code);
        module.section(&self.data);
        module.finish()
    }
}
