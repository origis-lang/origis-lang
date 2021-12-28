#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(try_find)]
#![allow(dead_code)]

pub mod interpreter;
#[cfg(feature = "jit")]
pub mod jit;
pub mod parser;

mod stdlib;

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
