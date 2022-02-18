use wasmtime::Memory;

pub struct Context {
    pub memory: Option<Memory>,
}

impl Context {
    pub fn new() -> Context {
        Context { memory: None }
    }

    pub fn memory(&self) -> Memory {
        unsafe { self.memory.unwrap_unchecked() }
    }
}
