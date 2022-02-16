use crate::parser::parse::error::Error;
use crate::parser::parse::type_literal::TypeLiteral;
use crate::parser::parser::{expect_one_token, Parser};
use crate::parser::token::{Token, TokenInner};

#[derive(Debug, Copy, Clone)]
pub struct UntypedIdent<'s>(pub Token<'s>);

#[derive(Debug, Clone)]
pub struct TypedIdent<'s> {
    pub tok: Token<'s>,
    pub ty: TypeLiteral<'s>,
}

#[derive(Debug, Clone)]
pub enum MaybeTypedIdent<'s> {
    Typed(TypedIdent<'s>),
    Untyped(UntypedIdent<'s>),
}

impl<'s> Parser<'s> {
    pub fn parse_maybe_typed_ident(
        &self,
    ) -> Result<MaybeTypedIdent<'s>, Error<'s>> {
        let id = expect_one_token!(self, TokenInner::Ident(_))?;
        if expect_one_token!(self, TokenInner::SymbolColon).is_err() {
            Ok(MaybeTypedIdent::Untyped(UntypedIdent(id)))
        } else {
            Ok(MaybeTypedIdent::Typed(TypedIdent {
                tok: id,
                ty: self.parse_type()?,
            }))
        }
    }

    pub fn parse_untyped_ident(
        &self,
    ) -> Result<UntypedIdent<'s>, Error<'s>> {
        Ok(UntypedIdent(expect_one_token!(
            self,
            TokenInner::Ident(_)
        )?))
    }

    pub fn parse_typed_ident(
        &self,
    ) -> Result<TypedIdent<'s>, Error<'s>> {
        let id = expect_one_token!(self, TokenInner::Ident(_))?;
        expect_one_token!(self, TokenInner::SymbolColon)?;
        Ok(TypedIdent {
            tok: id,
            ty: self.parse_type()?,
        })
    }
}

impl<'s> UntypedIdent<'s> {
    pub fn to_str(self) -> &'s str {
        match self.0.inner {
            TokenInner::Ident(s) => s,
            _ => unreachable!(),
        }
    }
}

impl<'s> TypedIdent<'s> {
    pub fn to_str(&self) -> &'s str {
        match self.tok.inner {
            TokenInner::Ident(s) => s,
            _ => unreachable!(),
        }
    }
}
