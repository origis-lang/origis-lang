mod compile;
pub mod compiler;
mod scope;
mod wasm_module;

#[cfg(feature = "wasm_rt")]
mod rt;
