use smallvec::SmallVec;

use crate::parser::parse::decl::Decl;
use crate::parser::parse::error::Error;
use crate::parser::parse::ident::UntypedIdent;
use crate::parser::parser::{expect_one_token, Parser};
use crate::parser::token::{Token, TokenInner};

#[derive(Debug, Clone)]
pub struct Module<'s> {
    pub decls: SmallVec<[Decl<'s>; 10]>,
}

#[derive(Debug, Clone)]
pub struct ModulePath<'s> {
    pub segment: SmallVec<[UntypedIdent<'s>; 5]>,
}

impl<'s> Parser<'s> {
    pub fn parse_module(&self) -> Result<Module<'s>, Error<'s>> {
        let mut decls = SmallVec::new_const();
        loop {
            match self.parse_decl() {
                Ok(decl) => {
                    decls.push(decl);
                }
                Err(Error::ExpectedButFound(
                    _,
                    Token {
                        inner: TokenInner::EOF,
                        ..
                    },
                )) => break,
                Err(err) => return Err(err),
            }
        }
        Ok(Module { decls })
    }

    pub fn parse_module_path(
        &self,
    ) -> Result<ModulePath<'s>, Error<'s>> {
        let mut seg = SmallVec::new_const();

        while let Ok(id) = self.parse_untyped_ident() {
            seg.push(id);
            expect_one_token!(self, TokenInner::SymbolDoubleColon)
                .ok();
        }
        Ok(ModulePath { segment: seg })
    }
}
