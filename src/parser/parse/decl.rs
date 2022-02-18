use crate::parser::parse::attr::Attribute;
use smallvec::SmallVec;

use crate::parser::parse::block::Block;
use crate::parser::parse::error::Error;
use crate::parser::parse::ident::{TypedIdent, UntypedIdent};
use crate::parser::parse::module::ModulePath;
use crate::parser::parse::type_literal::TypeLiteral;
use crate::parser::parser::{expect_one_token, expect_token, Parser};
use crate::parser::token::TokenInner;

#[derive(Debug, Clone)]
pub enum Decl<'s> {
    FnDecl(FnDecl<'s>),
    StructDecl(StructDecl<'s>),
    Import(ModulePath<'s>),
    Attribute(Attribute<'s>),
}

#[derive(Debug, Clone)]
pub struct StructDecl<'s> {
    pub vis: bool,
    pub name: UntypedIdent<'s>,
    pub fields: SmallVec<[TypedIdent<'s>; 5]>,
}

#[derive(Debug, Clone)]
pub struct FnDecl<'s> {
    pub vis: bool,
    pub name: UntypedIdent<'s>,
    pub params: SmallVec<[TypedIdent<'s>; 3]>,
    pub ret_type: TypeLiteral<'s>,
    pub body: Block<'s>,
}

impl<'s> Parser<'s> {
    pub fn parse_decl(&self) -> Result<Decl<'s>, Error<'s>> {
        let vis =
            expect_one_token!(self, TokenInner::KeywordPub).is_ok();
        expect_token!(self; {
            TokenInner::KeywordFn => {
                let point = self.current_point();
                self.parse_fn_decl(vis).map(Decl::FnDecl).map_err(|err| {
                    self.back_to(point);
                    err
                })
            },
            TokenInner::KeywordStruct => {
                let point = self.current_point();
                self.parse_struct_decl(vis).map(Decl::StructDecl).map_err(|err| {
                    self.back_to(point);
                    err
                })
            },
            TokenInner::KeywordUse => {
                let path = self.parse_module_path()?;
                expect_one_token!(self, TokenInner::SymbolSemicolon)?;
                Ok(Decl::Import(path))
            },
            TokenInner::SymbolHash => {
                let point = self.current_point();
                self.parse_attr().map(Decl::Attribute).map_err(|err| {
                    self.back_to(point);
                    err
                })
            }
        })
    }

    pub fn parse_struct_decl(
        &self,
        vis: bool,
    ) -> Result<StructDecl<'s>, Error<'s>> {
        let name = self.parse_untyped_ident()?;
        let mut fields = SmallVec::new_const();
        expect_one_token!(self, TokenInner::SymbolLBrace)?;
        while let Ok(id) = self.parse_typed_ident() {
            fields.push(id);
        }
        expect_one_token!(self, TokenInner::SymbolRBrace)?;
        Ok(StructDecl { vis, name, fields })
    }

    pub fn parse_fn_decl(
        &self,
        vis: bool,
    ) -> Result<FnDecl<'s>, Error<'s>> {
        let name = self.parse_untyped_ident()?;
        let mut params = SmallVec::new_const();

        expect_one_token!(self, TokenInner::SymbolLParen)?;

        while let Err(_) =
            expect_one_token!(self, TokenInner::SymbolRParen)
        {
            params.push(self.parse_typed_ident()?);

            expect_one_token!(self, TokenInner::SymbolComma).ok();
        }

        let ret_type = self.parse_type()?;

        expect_one_token!(self, TokenInner::SymbolLBrace)?;

        let body =
            if expect_one_token!(self, TokenInner::SymbolRBrace)
                .is_err()
            {
                self.parse_block()?
            } else {
                Block(SmallVec::new_const())
            };

        Ok(FnDecl {
            vis,
            name,
            params,
            body,
            ret_type,
        })
    }
}
