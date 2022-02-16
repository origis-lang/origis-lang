use compact_str::CompactStr;
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(pub CompactStr);

impl<T> From<T> for Ident
where
    T: Into<CompactStr>,
{
    fn from(s: T) -> Self {
        Ident(s.into())
    }
}

impl Deref for Ident {
    type Target = CompactStr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for Ident {
    fn borrow(&self) -> &str {
        &*self.0
    }
}
