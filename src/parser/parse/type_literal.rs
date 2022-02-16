use crate::parser::parse::error::Error;
use crate::parser::parser::{expect_one_token, expect_token, Parser};
use crate::parser::token::TokenInner;

#[derive(Debug, Clone)]
pub enum TypeLiteral<'s> {
    Void,
    Int,
    Float,
    Char,
    Bool,
    Ref(Box<TypeLiteral<'s>>),
    Func {
        params: Vec<TypeLiteral<'s>>,
        ret: Box<TypeLiteral<'s>>,
    },
    Custom(&'s str),
}

impl<'s> Parser<'s> {
    pub fn parse_type(&self) -> Result<TypeLiteral<'s>, Error<'s>> {
        expect_token!(self; {
            TokenInner::KeywordInt => Ok(TypeLiteral::Int),
            TokenInner::KeywordFloat => Ok(TypeLiteral::Float),
            TokenInner::KeywordChar => Ok(TypeLiteral::Char),
            TokenInner::KeywordBool => Ok(TypeLiteral::Bool),
            TokenInner::KeywordVoid => Ok(TypeLiteral::Void),
            TokenInner::KeywordRef => {
                Ok(TypeLiteral::Ref(box self.parse_type()?))
            },
            TokenInner::KeywordFunc => {
                let mut params = Vec::new();
                expect_one_token!(self, TokenInner::SymbolLParen)?;
                while let Err(_) = expect_one_token!(self, TokenInner::SymbolRParen) {
                    params.push(self.parse_type()?);

                    expect_one_token!(self, TokenInner::SymbolComma).ok();
                }
                let ret = box self.parse_type().unwrap_or_else(|_| TypeLiteral::Void);

                Ok(TypeLiteral::Func {
                    params,
                    ret
                })
            },
            TokenInner::Ident(custom) => Ok(TypeLiteral::Custom(custom)),
        })
    }
}
