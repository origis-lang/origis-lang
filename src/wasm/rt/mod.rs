use std::io::{Read, Write};

use ahash::AHashMap;
use cfg_if::cfg_if;
use compact_str::CompactString;
use wasmtime::{
    AsContext, AsContextMut, Caller, Config, Engine, Extern,
    Instance, Linker, Memory, MemoryType, Module, OptLevel, Store,
    StoreContext, StoreContextMut,
};

use crate::wasm::rt::context::Context;

mod context;

pub struct Runtime {
    engine: Engine,
    store: Store<Context>,
    modules: AHashMap<CompactString, Module>,
    linker: Linker<Context>,
}

impl Runtime {
    pub fn new() -> anyhow::Result<Runtime> {
        let engine = Engine::new(
            &Config::default()
                .async_support(true)
                .wasm_module_linking(true)
                .cranelift_opt_level({
                    cfg_if::cfg_if! {
                        if #[cfg(debug_assertions)] {
                            OptLevel::None
                        } else {
                            OptLevel::Speed
                        }
                    }
                }),
        )?;
        Ok(Runtime {
            store: Store::new(&engine, Context::new()),
            linker: Linker::new(&engine),
            engine,
            modules: Default::default(),
        })
    }

    pub fn compile(
        &mut self,
        name: CompactString,
        binary: &[u8],
    ) -> anyhow::Result<()> {
        self.modules
            .insert(name, Module::new(&self.engine, binary)?);
        Ok(())
    }

    pub async fn instance(
        &mut self,
        name: &str,
    ) -> Option<anyhow::Result<Instance>> {
        let module = self.modules.get(name)?;
        Some(
            self.linker
                .instantiate_async(&mut self.store, module)
                .await
                .map_err(Into::into),
        )
    }

    async fn link_modules(&mut self) -> anyhow::Result<()> {
        for (name, module) in &self.modules {
            let i = self
                .linker
                .instantiate_async(&mut self.store, module)
                .await?;
            for export in i.exports(&mut self.store).into_iter() {
                self.linker.define(
                    name,
                    export.name(),
                    export.into_extern(),
                )?;
            }
        }
        Ok(())
    }

    async fn link_std(&mut self) -> anyhow::Result<()> {
        self.linker.allow_shadowing(true);
        cfg_if! {
            if #[cfg(feature = "wasm64")] {
                let mem = Memory::new_async(&mut self.store, MemoryType::new(1, None)).await?;
            } else {
                let mem = Memory::new_async(&mut self.store, MemoryType::new64(1, None)).await?;
            }
        }
        self.store.data_mut().memory = Some(mem);
        self.linker.define(
            "_internal",
            "memory",
            Extern::Memory(mem),
        )?;

        self.linker.func_wrap("std::io", "put_char", |ch: u32| {
            let mut buf = [0u8; 4];
            std::io::stdout()
                .write_all(
                    char::from_u32(ch)
                        .unwrap()
                        .encode_utf8(&mut buf)
                        .as_bytes(),
                )
                .unwrap()
        })?;
        self.linker.func_wrap("std::io", "get_char", || {
            let mut buf = [0u8; 4];
            std::io::stdin().read(&mut buf).unwrap();
            std::str::from_utf8(&buf).unwrap().chars().next().unwrap()
                as i32
        })?;
        self.linker.func_wrap(
            "std::mem",
            "page_size",
            |caller: Caller<'_, Context>| {
                caller.data().memory().size(&caller)
            },
        )?;
        self.linker.func_wrap(
            "std::mem",
            "size",
            |caller: Caller<'_, Context>| {
                caller.data().memory().data_size(&caller) as u64
            },
        )?;
        self.linker.func_wrap(
            "std::mem",
            "store_int",
            |mut caller: Caller<'_, Context>,
             offset: i64,
             val: i64| {
                caller
                    .data()
                    .memory()
                    .write(
                        &mut caller,
                        offset as usize,
                        &val.to_le_bytes(),
                    )
                    .unwrap();
            },
        )?;
        self.linker.func_wrap(
            "std::mem",
            "load_int",
            |mut caller: Caller<'_, Context>, offset: i64| {
                let mut buf = [0u8; 8];
                caller
                    .data()
                    .memory()
                    .read(&mut caller, offset as usize, &mut buf)
                    .unwrap();
                i64::from_le_bytes(buf)
            },
        )?;
        Ok(())
    }
}

impl AsContext for Runtime {
    type Data = Context;

    #[inline]
    fn as_context(&self) -> StoreContext<'_, Self::Data> {
        self.store.as_context()
    }
}

impl AsContextMut for Runtime {
    #[inline]
    fn as_context_mut(&mut self) -> StoreContextMut<'_, Self::Data> {
        self.store.as_context_mut()
    }
}

#[cfg(test)]
mod tests {
    use wasmtime::{Instance, Trap};

    use crate::wasm::compile::compile_module;
    use crate::wasm::compiler::Compiler;
    use crate::wasm::rt::Runtime;

    async fn instantiate(
        source: &str,
    ) -> anyhow::Result<(Runtime, Instance)> {
        let mut compiler = Compiler::new();

        let imports = compiler.compile(source)?;
        let bin = compiler.finish();

        let mut rt = Runtime::new()?;

        for (name, module) in imports.borrow().iter() {
            let mut compiler = Compiler::new();
            compile_module(&mut compiler, &module)?;
            let bin = compiler.finish();
            rt.compile(name.clone(), &bin)?;
        }
        rt.link_modules().await?;
        rt.link_std().await?;

        rt.compile("test".into(), &bin)?;

        let i = rt.instance("test").await.unwrap()?;
        Ok((rt, i))
    }

    async fn run_fn<Params, Results>(
        source: &str,
        func: &str,
        params: Params,
    ) -> Result<Results, Trap>
    where
        Params: wasmtime::WasmParams,
        Results: wasmtime::WasmResults,
    {
        let (mut rt, instance) = instantiate(source).await.unwrap();
        instance
            .get_typed_func::<Params, Results, _>(&mut rt, func)
            .unwrap()
            .call_async(&mut rt, params)
            .await
    }

    #[tokio::test]
    async fn example_fib() {
        let fib_recursion = run_fn::<i64, i64>(
            include_str!("../../../examples/fib.gs"),
            "fib_recursion",
            20,
        )
        .await
        .unwrap();
        let fib_loop = run_fn::<i64, i64>(
            include_str!("../../../examples/fib.gs"),
            "fib_loop",
            20,
        )
        .await
        .unwrap();
        assert_eq!(fib_recursion, fib_loop);
        assert_eq!(fib_recursion, 10946);
    }
}
