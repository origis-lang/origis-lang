use crate::parser::token::{Token, TokenInner};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Not,
    Plus,
    Minus,
    Div,
    Mul,
    Rem,
    Xor,
    Lt,
    Gt,
    BitOr,
    BitAnd,
    And,
    Or,
    RemAssign,
    Eq,
    Ne,
    DivAssign,
    MulAssign,
    XorAssign,
    BitAndAssign,
    BitOrAssign,
    MinusAssign,
    PlusAssign,
    Le,
    Ge,
    Shl,
    Shr,
}

impl TryFrom<Token<'_>> for Op {
    type Error = anyhow::Error;

    fn try_from(tok: Token<'_>) -> Result<Self, Self::Error> {
        Ok(match tok.inner {
            TokenInner::SymbolSlash => Op::Div,
            TokenInner::SymbolBang => Op::Not,
            TokenInner::SymbolPercent => Op::Rem,
            TokenInner::SymbolCaret => Op::Xor,
            TokenInner::SymbolAster => Op::Mul,
            TokenInner::SymbolPlus => Op::Plus,
            TokenInner::SymbolMinus => Op::Minus,
            TokenInner::SymbolLt => Op::Lt,
            TokenInner::SymbolGt => Op::Gt,
            TokenInner::SymbolPipe => Op::BitOr,
            TokenInner::SymbolAmp => Op::BitAnd,
            TokenInner::SymbolAnd => Op::And,
            TokenInner::SymbolOr => Op::Or,
            TokenInner::SymbolPercentEq => Op::RemAssign,
            TokenInner::SymbolNotEq => Op::Ne,
            TokenInner::SymbolDoubleEq => Op::Eq,
            TokenInner::SymbolSlashEq => Op::DivAssign,
            TokenInner::SymbolCaretEq => Op::XorAssign,
            TokenInner::SymbolAmpEq => Op::BitAndAssign,
            TokenInner::SymbolPipeEq => Op::BitOrAssign,
            TokenInner::SymbolAsterEq => Op::MulAssign,
            TokenInner::SymbolMinusEq => Op::MinusAssign,
            TokenInner::SymbolPlusEq => Op::PlusAssign,
            TokenInner::SymbolLtEq => Op::Le,
            TokenInner::SymbolGtEq => Op::Ge,
            TokenInner::SymbolDoubleLt => Op::Shl,
            TokenInner::SymbolDoubleGt => Op::Shr,

            _ => anyhow::bail!(
                "token {:?} is not a valid operator",
                tok
            ),
        })
    }
}
