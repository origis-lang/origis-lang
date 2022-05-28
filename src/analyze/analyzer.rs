use std::cell::RefCell;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use ahash::AHashMap;
use compact_str::CompactString;
use smallvec::SmallVec;

use crate::analyze::scope::{Scope, ScopeStack};
use crate::analyze::typed_ast::{
    Block, Decl, Expr, FlowControl, FnDecl, FuncType, Ident,
    LiteralExpr, Module, Op, Stmt, Type,
};
use crate::parser::{parse, Parser};

pub type Imports = Rc<RefCell<AHashMap<CompactString, Module>>>;

pub struct Analyzer {
    source: String,
    scope: ScopeStack,

    pub imports: Imports,
}

impl Analyzer {
    pub fn from_source(source: String, imports: Imports) -> Self {
        Analyzer {
            source,
            imports,
            scope: ScopeStack::new(),
        }
    }

    pub fn from_file<P>(
        file: P,
        imports: Imports,
    ) -> anyhow::Result<Self>
    where
        P: AsRef<Path>,
    {
        let source = read_to_string(file.as_ref())?;
        Ok(Analyzer::from_source(source, imports))
    }

    pub fn analyze(&mut self) -> anyhow::Result<Module> {
        let source = self.source.clone();
        let parser = Parser::from_source(&source);
        let m = parser
            .parse_module()
            .map_err(|err| anyhow::Error::msg(err.to_string()))?;
        Ok(self.analyze_module(m)?)
    }

    pub fn analyze_module(
        &mut self,
        module: parse::module::Module,
    ) -> anyhow::Result<Module> {
        Ok(Module {
            decls: module
                .decls
                .into_iter()
                .filter_map(|decl| match self.analyze_decl(decl) {
                    Err(err) => Some(Err(err)),
                    Ok(None) => None,
                    Ok(decl) => decl.map(Ok),
                })
                .collect::<anyhow::Result<_>>()?,
        })
    }

    pub fn analyze_decl(
        &mut self,
        decl: parse::decl::Decl,
    ) -> anyhow::Result<Option<Decl>> {
        match decl {
            parse::decl::Decl::FnDecl(decl) => {
                let name: Ident = decl.name.to_str().into();
                let params =
                    decl.params
                        .into_iter()
                        .map(|id| {
                            Ok((
                                id.to_str().into(),
                                self.analyze_type(id.ty)?,
                            ))
                        })
                        .collect::<anyhow::Result<
                            SmallVec<[(Ident, Type); 3]>,
                        >>()?;
                let ret_type = self.analyze_type(decl.ret_type)?;

                self.scope.current_mut().set_symbol(
                    name.clone(),
                    Type::Func(FuncType {
                        params: params
                            .iter()
                            .map(|(_, ty)| ty.clone())
                            .collect(),
                        ret: box ret_type.clone(),
                    }),
                );
                self.scope.entry();

                for (id, ty) in &params {
                    self.scope
                        .current_mut()
                        .set_symbol(id.clone(), ty.clone())
                }

                let body = self.analyze_block(decl.body)?;
                self.scope.exit();

                Ok(Some(Decl::FnDecl(FnDecl {
                    vis: decl.vis,
                    name,
                    params,
                    ret_type,
                    body,
                })))
            }
            parse::decl::Decl::StructDecl(_) => {
                todo!()
            }
            parse::decl::Decl::Import(mut path) => {
                let name: CompactString = path
                    .segment
                    .pop()
                    .ok_or_else(|| {
                        anyhow::anyhow!("empty module path")
                    })?
                    .to_str()
                    .into();
                let path = path
                    .segment
                    .into_iter()
                    .map(|id| id.to_str().into())
                    .collect::<SmallVec<[CompactString; 3]>>();
                let path = if path.is_empty() {
                    name.clone()
                } else {
                    path.join("::").into()
                };
                let module = if let Some(m) =
                    self.imports.borrow().get(&path)
                {
                    m.clone()
                } else {
                    anyhow::bail!("module not found: {}", path)
                };

                let export =
                    module.get_export(&name).ok_or_else(|| {
                        anyhow::anyhow!(
                            "{} not found in {}",
                            &name,
                            &path
                        )
                    })?;

                match export {
                    Decl::FnDecl(fn_decl) => {
                        self.scope.current_mut().set_symbol(
                            Ident(name.clone()),
                            fn_decl.to_type(),
                        )
                    }
                    Decl::Import(_, _) => {}
                }

                Ok(Some(Decl::Import(path, box export.clone())))
            }
            parse::decl::Decl::Attribute(mut attr) => {
                match attr.name.to_str() {
                    "import" => {
                        let path = attr.values.pop().expect_str()?;
                        let name = attr.values.pop().expect_str()?;
                        if !self.imports.borrow().contains_key(&name)
                        {
                            let mut analyzer = Analyzer::from_file(
                                &*path,
                                self.imports.clone(),
                            )?;
                            self.imports
                                .borrow_mut()
                                .insert(name, analyzer.analyze()?);
                        }
                        Ok(None)
                    }
                    other => {
                        anyhow::bail!("unknown attribute: {}", other)
                    }
                }
            }
        }
    }

    pub fn analyze_stmt(
        &mut self,
        stmt: parse::stmt::Stmt,
    ) -> anyhow::Result<Stmt> {
        Ok(match stmt {
            parse::stmt::Stmt::Expr(expr) => {
                Stmt::Expr(self.analyze_expr(expr)?)
            }
            parse::stmt::Stmt::ImplicitReturn(expr) => {
                Stmt::ImplicitReturn(self.analyze_expr(expr)?)
            }
            parse::stmt::Stmt::FlowControl(fc) => {
                Stmt::FlowControl(match fc {
                    parse::stmt::FlowControl::Return(expr) => {
                        FlowControl::Return(self.analyze_expr(expr)?)
                    }
                    parse::stmt::FlowControl::Break(n) => {
                        FlowControl::Break(n)
                    }
                    parse::stmt::FlowControl::Continue(n) => {
                        FlowControl::Continue(n)
                    }
                })
            }
            parse::stmt::Stmt::Assign(id, expr) => match id {
                parse::ident::MaybeTypedIdent::Typed(id) => {
                    let name = id.to_str();
                    let expr = self.analyze_expr(expr)?;
                    let ty = self.analyze_type(id.ty)?;
                    let actual = expr.return_type();
                    if ty != actual {
                        anyhow::bail!(
                            "an attempt was made to assign an expression of return type {:?} to {} of type {:?}.",
                            actual,
                            name,
                            ty)
                    }
                    self.scope
                        .current_mut()
                        .set_symbol(name.into(), ty.clone());
                    Stmt::Assign(name.into(), ty, expr)
                }
                parse::ident::MaybeTypedIdent::Untyped(id) => {
                    let expr = self.analyze_expr(expr)?;
                    let ty = expr.return_type();
                    self.scope
                        .current_mut()
                        .set_symbol(id.to_str().into(), ty.clone());
                    Stmt::Assign(id.to_str().into(), ty, expr)
                }
            },
            parse::stmt::Stmt::Unreachable => Stmt::Unreachable,
        })
    }

    pub fn analyze_block(
        &mut self,
        block: parse::block::Block,
    ) -> anyhow::Result<Block> {
        Ok(Block {
            stmts: block
                .0
                .into_iter()
                .map(|stmt| self.analyze_stmt(stmt))
                .collect::<anyhow::Result<_>>()?,
            local_symbols: match self.scope.current() {
                Scope::Global { .. } => unreachable!(),
                Scope::Local { symbols } => symbols.clone(),
            },
        })
    }

    pub fn analyze_type(
        &mut self,
        ty: parse::type_literal::TypeLiteral,
    ) -> anyhow::Result<Type> {
        Ok(match ty {
            parse::type_literal::TypeLiteral::Void => Type::Void,
            parse::type_literal::TypeLiteral::Int => Type::Int,
            parse::type_literal::TypeLiteral::Float => Type::Float,
            parse::type_literal::TypeLiteral::Char => Type::Char,
            parse::type_literal::TypeLiteral::Bool => Type::Bool,
            parse::type_literal::TypeLiteral::Ref(ty) => {
                Type::Ref(box self.analyze_type(*ty)?)
            }
            parse::type_literal::TypeLiteral::Func {
                params,
                ret,
            } => Type::Func(FuncType {
                params: params
                    .into_iter()
                    .map(|ty| self.analyze_type(ty))
                    .collect::<anyhow::Result<_>>()?,
                ret: box self.analyze_type(*ret)?,
            }),
            parse::type_literal::TypeLiteral::Custom(name) => {
                let sym = self.scope.get_symbol(name)?.1;
                sym.ty.clone()
            }
        })
    }

    pub fn analyze_expr(
        &mut self,
        expr: parse::expr::Expr,
    ) -> anyhow::Result<Expr> {
        Ok(match expr {
            parse::expr::Expr::Literal(lit) => {
                Expr::Literal(match lit {
                    parse::expr::LiteralExpr::Integer(i) => {
                        LiteralExpr::Integer(i)
                    }
                    parse::expr::LiteralExpr::String(s) => {
                        LiteralExpr::String(s.into())
                    }
                    parse::expr::LiteralExpr::Float(f) => {
                        LiteralExpr::Float(f)
                    }
                    parse::expr::LiteralExpr::Bool(b) => {
                        LiteralExpr::Bool(b)
                    }
                    parse::expr::LiteralExpr::Char(c) => {
                        LiteralExpr::Char(c)
                    }
                })
            }
            parse::expr::Expr::Convert(expr, ty) => Expr::Convert(
                box self.analyze_expr(*expr)?,
                self.analyze_type(ty)?,
            ),
            parse::expr::Expr::Ident(id) => Expr::Ident(
                self.scope.get_symbol(id.to_str())?.1.clone(),
            ),
            parse::expr::Expr::CallFunc { name, args } => {
                let ret_ty =
                    match &self.scope.get_symbol(name.to_str())?.1.ty
                    {
                        Type::Func(FuncType { ret, .. }) => {
                            ret.as_ref().clone()
                        }
                        _ => anyhow::bail!(
                            "Symbol {} is not a function",
                            name.to_str()
                        ),
                    };
                Expr::CallFunc {
                    name: name.to_str().into(),
                    args: args
                        .into_iter()
                        .map(|expr| self.analyze_expr(*expr))
                        .collect::<anyhow::Result<_>>()?,
                    ret_ty,
                }
            }
            parse::expr::Expr::Block(block) => {
                Expr::Block(box self.analyze_block(*block)?)
            }
            parse::expr::Expr::If {
                cond,
                body,
                else_if,
                _else,
            } => {
                let cond = box self.analyze_expr(*cond)?;
                let body = box self.analyze_block(*body)?;
                let else_if = else_if
                    .into_iter()
                    .map(|(expr, block)| {
                        Ok((
                            self.analyze_expr(*expr)?,
                            self.analyze_block(*block)?,
                        ))
                    })
                    .collect::<anyhow::Result<Vec<(Expr, Block)>>>(
                    )?;
                let _else = if let Some(block) = _else {
                    Some(box self.analyze_block(*block)?)
                } else {
                    None
                };

                if cond.return_type() != Type::Bool {
                    anyhow::bail!("the condition of the if expression must be of type bool")
                }

                let body_ty = body.return_type();
                if let Some(block) = &_else {
                    if block.return_type() != body_ty {
                        anyhow::bail!("the return type of all branches of the if expression must be the same")
                    }
                } else if !matches!(&body_ty, Type::Void) {
                    anyhow::bail!("if expression expected to return a value but no else branch")
                }
                for (br_cond, br_block) in &else_if {
                    if br_cond.return_type() != Type::Bool {
                        anyhow::bail!("the condition of the if expression must be of type bool")
                    }
                    if br_block.return_type() != body_ty {
                        anyhow::bail!("the return type of all branches of the if expression must be the same")
                    }
                }
                Expr::If {
                    cond,
                    body,
                    else_if,
                    _else,
                }
            }
            parse::expr::Expr::Loop(block) => {
                Expr::Loop(box self.analyze_block(*block)?)
            }
            parse::expr::Expr::Unary(op, expr) => {
                let op: Op = op.try_into()?;
                let expr = box self.analyze_expr(*expr)?;
                match op {
                    Op::Not => {
                        if expr.return_type() != Type::Bool {
                            anyhow::bail!("the NOT operator must be used on type bool")
                        }
                    }
                    Op::Minus => {
                        if matches!(
                            expr.return_type(),
                            Type::Int | Type::Float
                        ) {
                            anyhow::bail!("the negation operator must be used on numeric types")
                        }
                    }
                    _ => anyhow::bail!(
                        "illegal unary operator {:?}",
                        op
                    ),
                }
                Expr::Unary(op, expr)
            }
            parse::expr::Expr::Binary(l_expr, op, r_expr) => {
                let l_expr = box self.analyze_expr(*l_expr)?;
                let r_expr = box self.analyze_expr(*r_expr)?;
                if l_expr.return_type() != r_expr.return_type() {
                    anyhow::bail!("the types of both sides of a binary operation must be equal")
                }
                Expr::Binary(l_expr, op.try_into()?, r_expr)
            }
        })
    }
}

pub trait Typed {
    fn return_type(&self) -> Type;
}

impl<T> Typed for &T
where
    T: Typed,
{
    fn return_type(&self) -> Type {
        T::return_type(self)
    }
}

trait ExpectLiteral {
    fn expect_str(self) -> anyhow::Result<CompactString>;
}

impl ExpectLiteral for Option<parse::expr::LiteralExpr<'_>> {
    fn expect_str(self) -> anyhow::Result<CompactString> {
        match self.ok_or_else(|| {
            anyhow::anyhow!("expected a literal str but None")
        })? {
            parse::expr::LiteralExpr::String(str) => Ok(str.into()),
            other => {
                anyhow::bail!("expected a literal str but {other:?}")
            }
        }
    }
}
