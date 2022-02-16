use crate::parser::token::Token;

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error<'s> {
    #[error("expected and of {0:?} but found {1:?}")]
    ExpectedButFound(&'static [&'static str], Token<'s>),
}

impl<'s> Error<'s> {
    pub fn expected_but_found(
        expected: &'static [&'static str],
        found: Token<'s>,
    ) -> Self {
        Error::ExpectedButFound(expected, found)
    }
}
