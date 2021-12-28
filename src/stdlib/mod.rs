use crate::interpreter::{BuiltinFn, Params};

pub mod io;

pub static BUILTIN: phf::Map<
    &'static str,
    (BuiltinFn, Params),
> = include!(concat!(env!("OUT_DIR"), "/builtin_std.rs"));
