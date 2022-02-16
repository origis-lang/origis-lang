use crate::analyze::typed_ast::{FuncType, Type};
use ahash::AHashMap;
use compact_str::CompactStr;

#[derive(Debug, Clone)]
pub enum Symbol {
    Function { idx: u32, func: FuncType },
    Variable { idx: u32, ty: Type },
}

#[derive(Debug, Clone)]
pub enum Scope {
    Global {
        symbols: AHashMap<CompactStr, Symbol>,
        func_idx: u32,
        local_idx: u32,
    },
    Local {
        symbols: AHashMap<CompactStr, Symbol>,
        local_idx: u32,
        if_depth: u32,
    },
}

#[derive(Debug, Clone)]
pub struct ScopeStack {
    stack: Vec<Scope>,
}

impl Scope {
    pub fn get_symbol(&self, name: &str) -> anyhow::Result<&Symbol> {
        match self {
            Scope::Global { symbols, .. } => symbols,
            Scope::Local { symbols, .. } => symbols,
        }
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Symbol {} not found", name))
    }

    pub fn set_symbol(&mut self, name: CompactStr, ty: Type) -> u32 {
        match self {
            Scope::Global {
                symbols,
                func_idx,
                local_idx,
                ..
            } => match ty {
                Type::Func(func) => {
                    let idx = *func_idx;
                    *func_idx += 1;
                    symbols
                        .insert(name, Symbol::Function { idx, func });
                    idx
                }
                Type::Ref(_) => {
                    todo!()
                }
                Type::Struct { .. } => {
                    todo!()
                }
                Type::Void => {
                    unimplemented!()
                }
                ty => {
                    if !symbols.contains_key(&name) {
                        let idx = *local_idx;
                        *local_idx += 1;
                        symbols
                            .insert(name, Symbol::Variable { idx, ty });
                        idx
                    } else {
                        symbols.get(&name).unwrap().idx()
                    }
                }
            },
            Scope::Local { symbols, local_idx, .. } => match ty {
                Type::Func(func) => {
                    todo!()
                }
                Type::Ref(_) => {
                    todo!()
                }
                Type::Struct { .. } => {
                    todo!()
                }
                Type::Void => {
                    unimplemented!()
                }
                ty => {
                    if !symbols.contains_key(&name) {
                        let idx = *local_idx;
                        *local_idx += 1;
                        symbols
                            .insert(name, Symbol::Variable { idx, ty });
                        idx
                    } else {
                        symbols.get(&name).unwrap().idx()
                    }
                }
            },
        }
    }

    pub fn current_idx(&self) -> u32 {
        match self {
            Scope::Global { local_idx, .. } => *local_idx,
            Scope::Local { local_idx, .. } => *local_idx,
        }
    }

    pub fn entry_if(&mut self) {
        match self {
            Scope::Global { .. } => unreachable!(),
            Scope::Local { if_depth, .. } => {
                *if_depth += 1
            }
        }
    }

    pub fn exit_if(&mut self) {
        match self {
            Scope::Global { .. } => unreachable!(),
            Scope::Local { if_depth, .. } => {
                *if_depth -= 1
            }
        }
    }
}

impl ScopeStack {
    pub fn new() -> Self {
        ScopeStack {
            stack: vec![Default::default()],
        }
    }

    pub fn entry(&mut self) {
        let scope = Scope::Local {
            symbols: Default::default(),
            local_idx: 0,
            if_depth: 0,
        };

        self.stack.push(scope);
    }

    pub fn exit(&mut self) {
        if self.stack.len() > 0 {
            self.stack.pop();
        }
    }

    pub fn current(&self) -> &Scope {
        self.stack.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.stack.last_mut().unwrap()
    }

    pub fn get_symbol(&self, name: &str) -> anyhow::Result<&Symbol> {
        for scope in self.stack.iter().rev() {
            if let Ok(sym) = scope.get_symbol(name) {
                return Ok(sym);
            }
        }
        anyhow::bail!("Symbol {} not found", name)
    }
}

impl Symbol {
    pub fn idx(&self) -> u32 {
        match self {
            Symbol::Function { idx, .. } => *idx,
            Symbol::Variable { idx, .. } => *idx,
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::Global {
            symbols: Default::default(),
            func_idx: 0,
            local_idx: 0,
        }
    }
}
