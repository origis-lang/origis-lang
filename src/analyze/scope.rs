use crate::analyze::typed_ast::{Ident, Type, Value};
use ahash::AHashMap;

#[derive(Debug, Clone)]
pub enum Scope {
    Global { symbols: AHashMap<Ident, Value> },
    Local { symbols: AHashMap<Ident, Value> },
}

#[derive(Debug, Clone)]
pub struct ScopeStack {
    stack: Vec<Scope>,
}

impl Scope {
    pub fn get_symbol(&self, name: &str) -> anyhow::Result<&Value> {
        match self {
            Scope::Global { symbols, .. } => symbols,
            Scope::Local { symbols, .. } => symbols,
        }
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Symbol {} not found", name))
    }

    pub fn set_symbol(&mut self, name: Ident, ty: Type) {
        match self {
            Scope::Global { symbols } => {
                symbols.insert(name.clone(), Value { name, ty });
            }
            Scope::Local { symbols } => {
                symbols.insert(name.clone(), Value { name, ty });
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
        self.stack.push(Scope::Local {
            symbols: Default::default(),
        });
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

    pub fn get_symbol(
        &self,
        name: &str,
    ) -> anyhow::Result<(usize, &Value)> {
        for (idx, scope) in self.stack.iter().rev().enumerate() {
            if let Ok(val) = scope.get_symbol(name) {
                return Ok((idx, val));
            }
        }
        anyhow::bail!("Symbol {} not found", name)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::Global {
            symbols: Default::default(),
        }
    }
}
