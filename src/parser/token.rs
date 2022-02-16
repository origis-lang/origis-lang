use crate::parser::lexer::Lexer;
use std::ops::Range;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenInner<'s> {
    EOF,

    Comment(&'s str),
    Ident(&'s str),
    Mark(&'s str),

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
    /// `'`
    SymbolSigQuote,

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
    /// `<<`
    SymbolDoubleLt,
    /// `>>`
    SymbolDoubleGt,
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
    KeywordChar,
    KeywordRef,
    KeywordFunc,
    KeywordVoid,
    KeywordArray,

    KeywordAsync,
    KeywordAwait,
    KeywordProc,

    KeywordUnreachable,

    KeywordFn,
    KeywordStruct,
    KeywordEnum,

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
    KeywordPub,
    KeywordUse,
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

pub struct TokenStream<'s> {
    pub lexer: Lexer<'s>,
}

impl<'s> Iterator for TokenStream<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.lexer.next_token();
        // Block comments
        if let TokenInner::Comment(_) = tok.inner {
            self.next()
        } else {
            Some(tok)
        }
    }
}

pub const KEYWORDS: phf::Map<&'static str, TokenInner<'static>> = phf::phf_map! {
    "true" => TokenInner::LitBoolean(true),
    "false" => TokenInner::LitBoolean(false),

    "bool" => TokenInner::KeywordBool,
    "int" => TokenInner::KeywordInt,
    "float" => TokenInner::KeywordFloat,
    "char" => TokenInner::KeywordChar,
    "ref" => TokenInner::KeywordRef,
    "func" => TokenInner::KeywordFunc,
    "void" => TokenInner::KeywordVoid,
    "array" => TokenInner::KeywordArray,

    "async" => TokenInner::KeywordAsync,
    "await" => TokenInner::KeywordAwait,
    "proc" => TokenInner::KeywordProc,

    "unreachable" => TokenInner::KeywordUnreachable,

    "fn" => TokenInner::KeywordFn,
    "struct" => TokenInner::KeywordStruct,
    "enum" => TokenInner::KeywordEnum,

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
    "pub" => TokenInner::KeywordPub,
    "use" => TokenInner::KeywordUse,
};
