use crate::parser::parse::block::Block;
use smallvec::SmallVec;

use crate::parser::parse::error::Error;
use crate::parser::parse::ident::UntypedIdent;
use crate::parser::parser::{expect_one_token, expect_token, Parser};
use crate::parser::token::{Token, TokenInner};

#[derive(Debug, Clone)]
pub enum Expr<'s> {
    Literal(LiteralExpr<'s>),
    Ident(UntypedIdent<'s>),
    CallFunc {
        name: UntypedIdent<'s>,
        args: SmallVec<[Box<Expr<'s>>; 3]>,
    },
    Block(Box<Block<'s>>),
    If {
        cond: Box<Expr<'s>>,
        body: Box<Block<'s>>,
        else_if: SmallVec<[(Box<Expr<'s>>, Box<Block<'s>>); 3]>,
        _else: Option<Box<Block<'s>>>,
    },
    Loop(Box<Block<'s>>),

    Unary(Token<'s>, Box<Expr<'s>>),
    Binary(Box<Expr<'s>>, Token<'s>, Box<Expr<'s>>),
}

#[derive(Debug, Clone, Copy)]
pub enum LiteralExpr<'s> {
    Integer(i64),
    String(&'s str),
    Float(f64),
    Bool(bool),
    Char(char),
}

pub fn op_precedence(op: TokenInner) -> Option<u8> {
    Some(match op {
        TokenInner::SymbolAster
        | TokenInner::SymbolSlash
        | TokenInner::SymbolPercent => 9,
        TokenInner::SymbolMinus | TokenInner::SymbolPlus => 8,

        TokenInner::SymbolDoubleLt | TokenInner::SymbolDoubleGt => 7,
        TokenInner::SymbolAmp => 6,
        TokenInner::SymbolCaret => 5,
        TokenInner::SymbolPipe => 4,
        TokenInner::SymbolDoubleEq
        | TokenInner::SymbolNotEq
        | TokenInner::SymbolGt
        | TokenInner::SymbolGtEq
        | TokenInner::SymbolLt
        | TokenInner::SymbolLtEq => 3,
        TokenInner::SymbolAnd | TokenInner::SymbolOr => 2,
        TokenInner::SymbolAsterEq
        | TokenInner::SymbolSlashEq
        | TokenInner::SymbolPercentEq
        | TokenInner::SymbolMinusEq
        | TokenInner::SymbolPlusEq
        | TokenInner::SymbolCaretEq => 1,

        _ => return None,
    })
}

impl<'s> Parser<'s> {
    pub fn parse_expr(&self) -> Result<Expr<'s>, Error<'s>> {
        self.parse_binary_expr(1)
    }

    pub fn parse_unary_expr(&self) -> Result<Expr<'s>, Error<'s>> {
        let token = self.peek_token();
        match token.inner {
            TokenInner::SymbolMinus | TokenInner::SymbolBang => {
                self.next_token();
                Ok(Expr::Unary(token, box self.parse_unary_expr()?))
            }
            _ => self.parse_other_expr(),
        }
    }

    pub fn parse_binary_expr(
        &self,
        prev_prec: u8,
    ) -> Result<Expr<'s>, Error<'s>> {
        let mut expr = self.parse_unary_expr()?;

        loop {
            let token = self.next_token();
            if let Some(prec) = op_precedence(token.inner) {
                if prec < prev_prec {
                    self.back();
                    return Ok(expr);
                }

                let rhs = self.parse_binary_expr(prec + 10)?;
                expr = Expr::Binary(box expr, token, box rhs)
            } else {
                self.back();
                break;
            }
        }

        Ok(expr)
    }

    pub fn parse_other_expr(&self) -> Result<Expr<'s>, Error<'s>> {
        if let Ok(expr) = self.parse_literal() {
            Ok(Expr::Literal(expr))
        } else if let Ok(id) = self.parse_untyped_ident() {
            if let TokenInner::SymbolLParen = self.peek_token().inner
            {
                self.parse_call_fn_args(id)
            } else {
                Ok(Expr::Ident(id))
            }
        } else if let TokenInner::SymbolLParen =
            self.peek_token().inner
        {
            self.next_token();
            let expr = self.parse_expr()?;
            expect_one_token!(self, TokenInner::SymbolRParen)?;
            Ok(expr)
        } else if let TokenInner::SymbolLBrace =
            self.peek_token().inner
        {
            self.next_token();
            self.parse_block().map(|b| Expr::Block(box b))
        } else if let TokenInner::KeywordLoop =
            self.peek_token().inner
        {
            self.next_token();
            expect_one_token!(self, TokenInner::SymbolLBrace)?;
            self.parse_block().map(|b| Expr::Loop(box b))
        } else if let TokenInner::KeywordIf = self.peek_token().inner
        {
            self.next_token();
            let cond = self.parse_expr()?;
            expect_one_token!(self, TokenInner::SymbolLBrace)?;
            let body = self.parse_block()?;

            let mut else_if = SmallVec::new_const();
            let mut _else = None;

            loop {
                match (
                    expect_one_token!(self, TokenInner::KeywordElse),
                    expect_one_token!(self, TokenInner::KeywordIf),
                ) {
                    (Ok(_), Ok(_)) => {
                        let cond = self.parse_expr()?;
                        expect_one_token!(
                            self,
                            TokenInner::SymbolLBrace
                        )?;
                        let body = self.parse_block()?;
                        else_if.push((box cond, box body));
                    }
                    (Ok(_), Err(_)) => {
                        expect_one_token!(
                            self,
                            TokenInner::SymbolLBrace
                        )?;
                        _else = Some(box self.parse_block()?);
                        break;
                    }
                    (Err(_), _) => break,
                }
            }

            Ok(Expr::If {
                cond: box cond,
                body: box body,
                else_if,
                _else,
            })
        } else {
            Err(Error::ExpectedButFound(
                &["<literal>", "<ident>"],
                self.peek_token(),
            ))
        }
    }

    pub fn parse_call_fn_args(
        &self,
        name: UntypedIdent<'s>,
    ) -> Result<Expr<'s>, Error<'s>> {
        expect_one_token!(self, TokenInner::SymbolLParen)?;
        let mut args = SmallVec::new_const();
        if expect_one_token!(self, TokenInner::SymbolRParen).is_err()
        {
            loop {
                args.push(box self.parse_expr()?);

                expect_token!(self; {
                    TokenInner::SymbolComma => {
                        Ok(())
                    },
                    TokenInner::SymbolRParen => {
                        break
                    },
                })?;
                if expect_one_token!(self, TokenInner::SymbolRParen)
                    .is_ok()
                {
                    break;
                }
            }
        }

        Ok(Expr::CallFunc { name, args })
    }

    pub fn parse_literal(
        &self,
    ) -> Result<LiteralExpr<'s>, Error<'s>> {
        expect_token!(self; {
            TokenInner::LitInteger(i) => {
                Ok(LiteralExpr::Integer(i))
            },
            TokenInner::LitStr(s) => {
                Ok(LiteralExpr::String(s))
            },
            TokenInner::LitFloat(f) => {
                Ok(LiteralExpr::Float(f))
            },
            TokenInner::LitBoolean(b) => {
                Ok(LiteralExpr::Bool(b))
            },
            TokenInner::LitChar(c) => {
                Ok(LiteralExpr::Char(c))
            },
        })
    }
}
