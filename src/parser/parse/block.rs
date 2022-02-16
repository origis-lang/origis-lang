use crate::parser::parse::error::Error;
use crate::parser::parse::stmt::Stmt;
use crate::parser::parser::{expect_one_token, Parser};
use crate::parser::token::TokenInner;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Block<'s>(pub SmallVec<[Stmt<'s>; 5]>);

impl<'s> Parser<'s> {
    pub fn parse_block(&self) -> Result<Block<'s>, Error<'s>> {
        let mut block = SmallVec::new_const();
        while let Ok(stmt) = self.parse_stmt() {
            block.push(stmt)
        }

        expect_one_token!(self, TokenInner::SymbolRBrace)?;

        Ok(Block(block))
    }
}
