use compact_str::CompactStr;

#[derive(thiserror::Error, Debug, PartialEq, Clone)]
pub enum InterpretationError {
    #[error("`{2}` cannot do `{1}` operation on `{0}`.")]
    MismatchedOperation(&'static str, &'static str, &'static str),
    #[error("Cannot find module `{0}`")]
    ModuleNotFound(CompactStr),
    #[error("Cannot find function `{0}::{1}`")]
    FuncNotFound(CompactStr, CompactStr),
    #[error("Cannot find variable `{0}`")]
    VarNotFound(CompactStr),
    #[error("Expect `{0}` but found `{1}`")]
    TypeNotMatch(&'static str, &'static str),
}
