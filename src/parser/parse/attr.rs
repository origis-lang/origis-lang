use crate::parser::parse::error::Error;
use crate::parser::parse::expr::LiteralExpr;
use crate::parser::parse::ident::UntypedIdent;
use crate::parser::Parser;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Attribute<'s> {
    pub name: UntypedIdent<'s>,
    pub values: SmallVec<[LiteralExpr<'s>; 3]>,
}

impl<'s> Parser<'s> {
    pub fn parse_attr(&self) -> Result<Attribute<'s>, Error<'s>> {
        let name = self.parse_untyped_ident()?;
        let mut values = SmallVec::new_const();
        while let Ok(lit) = self.parse_literal() {
            values.push(lit)
        }
        Ok(Attribute { name, values })
    }
}
