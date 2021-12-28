use std::path::Path;

use compact_str::CompactStr;
use fnv::FnvHashMap;
use itertools::Itertools;
use smallvec::SmallVec;

pub use error::InterpretationError;
pub use func::{BuiltinFn, Func, FuncArgs, Params};
pub use value::Value;

use crate::interpreter::value::GcValue;
use crate::parser::ast::Definition;
use crate::parser::{ast, parse_module};

pub use self::gc::Gc;

mod error;
mod eval;
mod func;
mod gc;
mod value;

pub struct Interpreter {
    modules: FnvHashMap<CompactStr, ModuleCtx>,
}

#[derive(Debug)]
pub struct Symbols(FnvHashMap<CompactStr, Value>);

pub struct ModuleCtx {
    mod_name: CompactStr,
    global_symbols: Symbols,
    last: Option<CompactStr>,
}

impl Symbols {
    pub fn get(
        &self,
        key: &str,
    ) -> Result<&Value, InterpretationError> {
        self.0.get(key).ok_or_else(|| {
            InterpretationError::VarNotFound(CompactStr::from(key))
        })
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            modules: FnvHashMap::default(),
        }
    }

    pub fn get_module(
        &self,
        module_path: &str,
    ) -> Result<&ModuleCtx, InterpretationError> {
        //let mut names = module_name.split("::").peekable();
        self.modules.get(module_path).ok_or_else(|| {
            InterpretationError::ModuleNotFound(CompactStr::from(
                module_path,
            ))
        })
    }

    pub fn get_module_mut(
        &mut self,
        module_path: &str,
    ) -> Result<&mut ModuleCtx, InterpretationError> {
        self.modules.get_mut(module_path).ok_or_else(|| {
            InterpretationError::ModuleNotFound(CompactStr::from(
                module_path,
            ))
        })
    }

    pub fn add_module(
        &mut self,
        module_path: &str,
        module_ast: ast::Module,
    ) -> Result<(), InterpretationError> {
        let mut paths = module_path
            .split("::")
            .collect::<SmallVec<[_; 10]>>()
            .into_iter()
            .rev()
            .peekable();

        let name = CompactStr::from(paths.next().unwrap());
        let mut ctx = ModuleCtx {
            mod_name: name,
            global_symbols: Symbols(FnvHashMap::default()),
            last: paths.peek().map(|s| CompactStr::from(*s)),
        };
        module_ast.defs.into_iter().try_for_each(|def| {
            eval::define(&*self, &mut ctx.global_symbols, def)
        })?;
        self.modules.insert(CompactStr::from(module_path), ctx);

        while let Some(path) = paths.next() {
            let last = Some(CompactStr::from(
                paths
                    .clone()
                    .rev()
                    .collect::<SmallVec<[_; 10]>>()
                    .join("::"),
            ));
            if let Some(ctx) = self.modules.get_mut(path) {
                ctx.last = last;
            } else {
                let ctx = ModuleCtx {
                    mod_name: CompactStr::from(path),
                    global_symbols: Symbols(FnvHashMap::default()),
                    last,
                };
                self.modules.insert(CompactStr::from(path), ctx);
            }
        }
        Ok(())
    }

    pub fn builtin_fn(
        &mut self,
        module_path: &str,
        name: &str,
        params: Params,
        f: BuiltinFn,
    ) -> Result<(), InterpretationError> {
        let module = self.get_module_mut(module_path)?;
        module.global_symbols.0.insert(
            CompactStr::from(name),
            Value::func(Func::Builtin { params, fn_ptr: f }),
        );
        Ok(())
    }

    pub fn call_fn(
        &self,
        module_name: &str,
        func_name: &str,
        args: FuncArgs,
    ) -> Result<Value, InterpretationError> {
        let mod_ctx = self.get_module(module_name)?;
        let val =
            mod_ctx.global_symbols.get(func_name).map_err(|_| {
                InterpretationError::FuncNotFound(
                    CompactStr::from(module_name),
                    CompactStr::from(func_name),
                )
            })?;
        macro_rules! err {
            () => {
                return Err(InterpretationError::TypeNotMatch(
                    "func",
                    val.type_name(),
                ))
            };
        }
        match val {
            Value::Gc(s) => match &*s.borrow() {
                GcValue::Func(f) => Ok(f.call(self, mod_ctx, args)?),
                _ => err!(),
            },
            _ => err!(),
        }
    }

    pub fn builtin_std(&mut self) -> Result<(), InterpretationError> {
        #[derive(rust_embed::RustEmbed)]
        #[folder = "src/stdlib/"]
        #[include = "*.gs"]
        struct StdLib;

        StdLib::iter().try_for_each(|s| {
            let mut path =
                Path::new(s.as_ref().trim_end_matches(".gs"));
            if path.ends_with("mod") {
                path =
                    path.parent().unwrap_or_else(|| Path::new("std"));
            }
            let path =
                path.iter().map(|s| s.to_str().unwrap()).join("::");
            let module_path = ["std::", &path].concat();

            let ast = parse_module(unsafe {
                std::str::from_utf8_unchecked(
                    StdLib::get(s.as_ref()).unwrap().data.as_ref(),
                )
            });
            self.add_module(&module_path, ast.clone())?;

            ast.defs.into_iter().try_for_each(|def| match def {
                Definition::FuncDef(f) => {
                    let (builtin_fn, params) = crate::stdlib::BUILTIN
                        .get(&[&path, &*f.name.0].join("::"))
                        .unwrap();
                    self.builtin_fn(
                        &module_path,
                        &f.name.0,
                        params.clone(),
                        *builtin_fn,
                    )
                }
                Definition::UseDef(_) => Ok(()),
            })
        })?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::Interpreter;

    #[test]
    fn test_add() {
        let ast = crate::parser::parse_module(
            r#"fn test_add(a, b: int) int { let tmp: int = a; tmp + b }"#,
        );
        let mut interp = Interpreter::new();
        interp.builtin_std().unwrap();
        interp.add_module("main", ast).unwrap();
        let ret = interp
            .call_fn(
                "main",
                "test_add",
                crate::fn_args! {
                    "a" => 1,
                    "b" => 2
                },
            )
            .unwrap();
        assert_eq!(ret.expect_int(), Ok(3));
    }

    #[test]
    fn test_fn_call() {
        let ast = crate::parser::parse_module(
            r#"fn add(a, b: int) int { a + b }
                        fn test() int { add(1, 1) }"#,
        );
        let mut interp = Interpreter::new();
        interp.add_module("main", ast).unwrap();
        let ret = interp
            .call_fn("main", "test", crate::fn_args! {})
            .unwrap();
        assert_eq!(ret.expect_int(), Ok(2));
    }

    #[test]
    fn test_module_use() {
        let main = crate::parser::parse_module(
            r#"use test::add; fn main() int { add(1, 2) }"#,
        );
        let test = crate::parser::parse_module(
            r#"fn add(a, b: int) int { a + b }"#,
        );
        let mut interp = Interpreter::new();
        interp.add_module("test", test).unwrap();
        interp.add_module("main", main).unwrap();
        let ret = interp
            .call_fn("main", "main", crate::fn_args! {})
            .unwrap();
        assert_eq!(ret.expect_int(), Ok(3));
    }

    #[test]
    fn test_if() {
        let ast = crate::parser::parse_module(
            r#"fn test(a: int) () { if a == 1 { "one" } else if a == 2 { "two" } else { "other" } }"#,
        );
        let mut interp = Interpreter::new();
        interp.add_module("main", ast).unwrap();
        let ret = interp
            .call_fn(
                "main",
                "test",
                crate::fn_args! {
                    "a" => 1
                },
            )
            .unwrap();
        assert_eq!(ret.expect_str().unwrap().as_str(), "one");
        let ret = interp
            .call_fn(
                "main",
                "test",
                crate::fn_args! {
                    "a" => 2
                },
            )
            .unwrap();
        assert_eq!(ret.expect_str().unwrap().as_str(), "two");
        let ret = interp
            .call_fn(
                "main",
                "test",
                crate::fn_args! {
                    "a" => 3
                },
            )
            .unwrap();
        assert_eq!(ret.expect_str().unwrap().as_str(), "other");
    }

    #[test]
    fn test_fib() {
        let ast = crate::parser::parse_module(include_str!(
            "../../examples/fib.gs"
        ));
        let mut interp = Interpreter::new();
        interp.add_module("main", ast).unwrap();
        let ret = interp
            .call_fn(
                "main",
                "fib",
                crate::fn_args! {
                    "n" => 3
                },
            )
            .unwrap();
        dbg!(ret);
    }

    #[test]
    fn test_std_builtin() {
        let ast = crate::parser::parse_module(
            r#"use std::io::print;
                        fn main() () { print(1, 2) }"#,
        );
        let mut interp = Interpreter::new();
        interp.builtin_std().unwrap();
        interp.add_module("main", ast).unwrap();
        let ret = interp
            .call_fn("main", "main", crate::fn_args! {})
            .unwrap();
        //assert_eq!(ret.expect_int(), Ok(2));
    }
}
