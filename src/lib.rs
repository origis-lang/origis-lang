#![allow(dead_code)]
#![feature(decl_macro)]
#![feature(box_syntax)]
#![feature(assert_matches)]

mod parser;

mod analyze;
#[cfg(feature = "wasm_compile")]
pub mod wasm;

#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_export]
macro_rules! fn_args {
    ($($k:expr => $v:expr),*) => {
        {
            let mut _map = fnv::FnvHashMap::default();
            $(
                _map.insert(compact_str::CompactStr::from($k), $crate::interpreter::Value::from($v));
            )*
            _map
        }
    };
}
