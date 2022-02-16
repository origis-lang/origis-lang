use ahash::AHashMap;
use compact_str::CompactStr;
use wasmtime::{
    AsContext, AsContextMut, Config, Engine, Instance, Module,
    OptLevel, Store, StoreContext, StoreContextMut,
};

use crate::wasm::rt::context::Context;

mod context;

pub struct Runtime {
    engine: Engine,
    store: Store<Context>,
    modules: AHashMap<CompactStr, Module>,
}

impl Runtime {
    pub fn new() -> anyhow::Result<Runtime> {
        let engine = Engine::new(
            &Config::default()
                .async_support(true)
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
            engine,
            modules: Default::default(),
        })
    }

    pub fn compile(
        &mut self,
        name: CompactStr,
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
            Instance::new_async(&mut self.store, module, &[])
                .await
                .map_err(Into::into),
        )
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

    use crate::wasm::compiler::Compiler;
    use crate::wasm::rt::Runtime;

    async fn instantiate(source: &str) -> anyhow::Result<(Runtime, Instance)> {
        let mut compiler = Compiler::new();

        compiler.compile(source).unwrap();
        let bin = compiler.finish();

        let mut rt = Runtime::new().unwrap();
        rt.compile("test".into(), &bin).unwrap();
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
