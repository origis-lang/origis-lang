use crate::parser::parse::error::Error;
use crate::parser::parse::expr::Expr;
use crate::parser::parse::ident::MaybeTypedIdent;
use crate::parser::parser::{expect_one_token, expect_token, Parser};
use crate::parser::token::TokenInner;

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Expr(Expr<'s>),
    ImplicitReturn(Expr<'s>),
    FlowControl(FlowControl<'s>),
    Assign(MaybeTypedIdent<'s>, Expr<'s>),
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum FlowControl<'s> {
    Return(Expr<'s>),
    Break(i64),
    Continue(i64),
}

impl<'s> Parser<'s> {
    pub fn parse_stmt(&self) -> Result<Stmt<'s>, Error<'s>> {
        if expect_one_token!(self, TokenInner::KeywordReturn).is_ok()
        {
            return Ok(Stmt::FlowControl(FlowControl::Return(
                self.parse_expr()?,
            )));
        } else if expect_one_token!(self, TokenInner::KeywordBreak)
            .is_ok()
        {
            return Ok(Stmt::FlowControl(FlowControl::Break(
                expect_token!(self; {
                    TokenInner::LitInteger(i) => Ok(i)
                })
                .unwrap_or(0),
            )));
        } else if expect_one_token!(self, TokenInner::KeywordContinue)
            .is_ok()
        {
            return Ok(Stmt::FlowControl(FlowControl::Continue(
                expect_token!(self; {
                    TokenInner::LitInteger(i) => Ok(i)
                })
                .unwrap_or(0),
            )));
        } else if expect_one_token!(
            self,
            TokenInner::KeywordUnreachable
        )
        .is_ok()
        {
            return Ok(Stmt::Unreachable);
        }

        let point = self.current_point();
        if let (Ok(id), TokenInner::SymbolEq) =
            (self.parse_maybe_typed_ident(), self.next_token().inner)
        {
            let stmt = Stmt::Assign(id, self.parse_expr()?);
            expect_one_token!(self, TokenInner::SymbolSemicolon)?;
            return Ok(stmt);
        } else {
            self.back_to(point)
        }

        if let Ok(expr) = self.parse_expr() {
            return if expect_one_token!(
                self,
                TokenInner::SymbolSemicolon
            )
            .is_ok()
            {
                Ok(Stmt::Expr(expr))
            } else {
                Ok(Stmt::ImplicitReturn(expr))
            };
        }

        Err(Error::ExpectedButFound(
            &["<decl>", "<expr>"],
            self.peek_token(),
        ))
    }
}
