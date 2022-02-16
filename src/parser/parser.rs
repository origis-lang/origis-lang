use std::cell::UnsafeCell;

use crate::parser::lexer::Lexer;
use crate::parser::rollbackable::{rollbackable, Rollbackable};
use crate::parser::token::{Token, TokenStream};

/// SAFETY: sequential execution, no parallelism
pub struct Parser<'s> {
    pub token_stream: UnsafeCell<Rollbackable<TokenStream<'s>>>,
    pub current: UnsafeCell<Token<'s>>,
    source: &'s str,
}

impl<'s> Parser<'s> {
    pub fn from_source(src: &'s str) -> Self {
        Parser {
            token_stream: UnsafeCell::new(rollbackable(
                TokenStream {
                    lexer: Lexer::from_source(src),
                },
            )),
            current: UnsafeCell::new(Token::eof((0..0).into())),
            source: src,
        }
    }

    #[inline]
    #[allow(clippy::mut_from_ref)]
    fn fuck(&self) -> &mut Rollbackable<TokenStream<'s>> {
        unsafe { &mut *self.token_stream.get() }
    }

    pub fn next_token(&self) -> Token<'s> {
        unsafe { *self.fuck().next().unwrap_unchecked() }
    }

    pub fn peek_token(&self) -> Token<'s> {
        unsafe { *self.fuck().peek().unwrap_unchecked() }
    }

    pub fn last_token(&self) -> Token<'s> {
        unsafe { *self.fuck().last().unwrap_unchecked() }
    }

    pub fn current_point(&self) -> usize {
        self.fuck().current()
    }

    pub fn back_to(&self, point: usize) {
        self.fuck().to(point).unwrap()
    }

    pub fn back(&self) {
        self.fuck().back().unwrap()
    }
}

pub macro expect_token($parser:expr; { $($(@ $a:ident)? $token_inner:pat_param => $e:expr),* $(,)? }) {
    match $parser.next_token() {
        $($($a @)? crate::parser::token::Token {
            inner: $token_inner,
            ..
        } => $e),*,
        token => Err(crate::parser::parse::error::Error::expected_but_found(&[
            $(stringify!($token_inner)),*
        ], token))
    }.map_err(|x| {
        $parser.back();
        x
    })
}

pub macro expect_one_token($parser: expr, $token_inner: pat_param) {
    match $parser.peek_token() {
        token @ crate::parser::token::Token {
            inner: $token_inner,
            ..
        } => Ok(token),
        token => Err(
            crate::parser::parse::error::Error::expected_but_found(
                &[stringify!($token_inner)],
                token,
            ),
        ),
    }
    .map(|x| {
        $parser.next_token();
        x
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::parse::decl::Decl;
    use std::assert_matches::assert_matches;

    use crate::parser::parse::expr::{Expr, LiteralExpr};
    use crate::parser::parse::ident::{
        MaybeTypedIdent, TypedIdent, UntypedIdent,
    };
    use crate::parser::parse::stmt::Stmt;
    use crate::parser::parser::Parser;
    use crate::parser::token::TokenInner::Ident;
    use crate::parser::token::{Token, TokenInner};

    #[test]
    fn fn_decl() {
        let parser = Parser::from_source(
            "pub fn sum(a: int, b: int,) int { 123 }",
        );
        assert!(parser.parse_module().is_ok());
    }

    #[test]
    fn struct_decl() {
        let parser =
            Parser::from_source("struct Test { x: int b: float }");
        assert!(parser.parse_module().is_ok());
    }

    #[test]
    fn use_decl() {
        let parser = Parser::from_source("use a::b::c;");
        assert_matches!(parser.parse_decl(), Ok(Decl::Import(_)));
    }

    #[test]
    fn assign_stmt() {
        let parser = Parser::from_source("x: int = 1;");
        assert_matches!(
            parser.parse_stmt(),
            Ok(Stmt::Assign(
                MaybeTypedIdent::Typed(TypedIdent {
                    tok: Token {
                        inner: Ident("x"),
                        ..
                    },
                    ..
                }),
                Expr::Literal(LiteralExpr::Integer(1,),)
            ))
        );
    }

    #[test]
    fn expr_ident() {
        let parser = Parser::from_source("id");
        assert_matches!(
            parser.parse_expr(),
            Ok(Expr::Ident(UntypedIdent(Token {
                inner: TokenInner::Ident("id"),
                ..
            })))
        )
    }

    #[test]
    fn expr_literal() {
        let parser = Parser::from_source(r#""abc""#);
        assert_matches!(
            parser.parse_expr(),
            Ok(Expr::Literal(LiteralExpr::String("abc")))
        )
    }

    #[test]
    fn expr_call_fn() {
        let parser = Parser::from_source(r#"print("abc",)"#);
        assert_matches!(
            parser.parse_expr(),
            Ok(Expr::CallFunc {
                name: UntypedIdent(Token {
                    inner: TokenInner::Ident("print"),
                    ..
                }),
                args: _,
            })
        )
    }

    #[test]
    fn op_prec() {
        let parser = Parser::from_source("1 + 2 * (3 - 4) / 5");
        assert!(parser.parse_expr().is_ok());
    }

    #[test]
    fn example_fib() {
        let parser = Parser::from_source(include_str!(
            "../../examples/fib.gs"
        ));
        assert!(parser.parse_module().is_ok());
        //dbg!(parser.parse_module());
    }
}
