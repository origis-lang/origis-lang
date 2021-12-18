#![feature(box_syntax)]
#![feature(box_patterns)]
#![allow(dead_code)]

pub mod parser;
#[cfg(feature = "jit")]
pub mod jit;
mod interpreter;

#[macro_export]
macro_rules! map {
    ($($k:expr => $v:expr),*) => {
        {
            let mut _map = fnv::FnvHashMap::default();
            $(
                _map.insert($k, $v);
            )*
            _map
        }
    };
}

fn main() {
    println!("Hello, world!");
}
