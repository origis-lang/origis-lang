use std::ops::Range;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenInner<'s> {
    EOF,
    UnexpectedEnd(&'static str),

    Comment(&'s str),
    Ident(&'s str),

    // literal
    LitInteger(i64),
    LitFloat(f64),
    LitBoolean(bool),
    LitChar(char),
    LitStr(&'s str),

    // symbol
    /// `/`
    SymbolSlash,
    /// `\`
    SymbolBackslash,
    /// `~`
    SymbolTilde,
    /// ```
    SymbolBackquote,
    /// `!`
    SymbolBang,
    /// `@`
    SymbolAt,
    /// `#`
    SymbolHash,
    /// `$`
    SymbolDollar,
    /// `%`
    SymbolPercent,
    /// `^`
    SymbolCaret,
    /// `&`
    SymbolAmp,
    /// `*`
    SymbolAster,
    /// `-`
    SymbolMinus,
    /// `+`
    SymbolPlus,
    /// `(`
    SymbolLParen,
    /// `)`
    SymbolRParen,
    /// `{`
    SymbolLBrace,
    /// `}`
    SymbolRBrace,
    /// `[`
    SymbolLBracket,
    /// `]`
    SymbolRBracket,
    /// `<`
    SymbolLt,
    /// `>`
    SymbolGt,
    /// `=`
    SymbolEq,
    /// `|`
    SymbolPipe,
    /// `:`
    SymbolColon,
    /// `;`
    SymbolSemicolon,
    /// `,`
    SymbolComma,
    /// `.`
    SymbolDot,
    /// `?`
    SymbolQues,

    /// `&&`
    SymbolAnd,
    /// `||`
    SymbolOr,
    /// `%=`
    SymbolPercentEq,
    /// `!=`
    SymbolNotEq,
    /// `/=`
    SymbolSlashEq,
    /// `^=`
    SymbolCaretEq,
    /// `&=`
    SymbolAmpEq,
    /// `*=`
    SymbolAsterEq,
    /// `-=`
    SymbolMinusEq,
    /// `+=`
    SymbolPlusEq,
    /// `==`
    SymbolDoubleEq,
    /// `|=`
    SymbolPipeEq,
    /// `<=`
    SymbolLtEq,
    /// `>=`
    SymbolGtEq,
    /// `<-`
    SymbolLArrow,
    /// `->`
    SymbolRArrow,
    /// `::`
    SymbolDoubleColon,

    // keyword
    KeywordBool,
    KeywordInt,
    KeywordFloat,
    KeywordStr,
    KeywordChar,
    KeywordArray,

    KeywordAsync,
    KeywordAwait,
    KeywordProc,

    KeywordFn,
    KeywordStruct,
    KeywordEnum,
    KeywordPub,

    KeywordFor,
    KeywordLoop,
    KeywordWhile,
    KeywordBreak,
    KeywordReturn,
    KeywordContinue,
    KeywordIf,
    KeywordElse,

    KeywordSelf,
    KeywordSuper,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'s> {
    pub inner: TokenInner<'s>,
    pub range: TokenRange,
}

impl<'s> Token<'s> {
    pub fn new(
        token: TokenInner<'s>,
        start: usize,
        end: usize,
    ) -> Self {
        Token {
            inner: token,
            range: TokenRange { start, end },
        }
    }

    pub fn eof(range: TokenRange) -> Self {
        Token {
            inner: TokenInner::EOF,
            range,
        }
    }

    pub fn unexpected_end(
        range: TokenRange,
        expect: &'static str,
    ) -> Self {
        Token {
            inner: TokenInner::UnexpectedEnd(expect),
            range,
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.inner, TokenInner::EOF)
    }

    pub fn inner(&self) -> TokenInner {
        self.inner
    }

    pub fn range(&self) -> TokenRange {
        self.range
    }
}

/// like rust: start..=end
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TokenRange {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for TokenRange {
    fn from(range: Range<usize>) -> Self {
        TokenRange {
            start: range.start,
            end: range.end,
        }
    }
}

pub const KEYWORDS: phf::Map<&'static str, TokenInner<'static>> = phf::phf_map! {
    "true" => TokenInner::LitBoolean(true),
    "false" => TokenInner::LitBoolean(false),

    "bool" => TokenInner::KeywordBool,
    "string" => TokenInner::KeywordStr,
    "int" => TokenInner::KeywordInt,
    "float" => TokenInner::KeywordFloat,
    "char" => TokenInner::KeywordChar,
    "array" => TokenInner::KeywordArray,

    "async" => TokenInner::KeywordAsync,
    "await" => TokenInner::KeywordAwait,
    "proc" => TokenInner::KeywordProc,

    "fn" => TokenInner::KeywordFn,
    "struct" => TokenInner::KeywordStruct,
    "enum" => TokenInner::KeywordEnum,
    "pub" => TokenInner::KeywordPub,

    "for" => TokenInner::KeywordFor,
    "loop" => TokenInner::KeywordLoop,
    "while" => TokenInner::KeywordWhile,
    "break" => TokenInner::KeywordBreak,
    "return" => TokenInner::KeywordReturn,
    "continue" => TokenInner::KeywordContinue,
    "if" => TokenInner::KeywordIf,
    "else" => TokenInner::KeywordElse,

    "self" => TokenInner::KeywordSelf,
    "super" => TokenInner::KeywordSuper,
};
