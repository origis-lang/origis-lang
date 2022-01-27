use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::CharIndices;

use crate::parser::token::{Token, TokenInner, TokenRange, KEYWORDS};

#[derive(Debug, Copy, Clone)]
pub struct UnexpectedEndError(TokenRange, &'static str);

pub struct Lexer<'s> {
    source: &'s str,
    char_indices: Peekable<CharIndices<'s>>,
    current: (usize, char),
}

impl<'s> Lexer<'s> {
    pub fn from_source(s: &'s str) -> Self {
        Lexer {
            char_indices: s.char_indices().peekable(),
            source: s,
            current: (0, '\x00'),
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let ch = self.char_indices.next()?;
        self.current = ch;
        Some(ch.1)
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.char_indices.peek().map(|(_, c)| *c)
    }

    pub fn current_pos(&self) -> usize {
        self.current.0
    }

    pub fn current_char(&self) -> char {
        self.current.1
    }

    pub fn next_token(
        &mut self,
    ) -> Result<Token<'s>, UnexpectedEndError> {
        match self.next_token_unchecked() {
            Token {
                inner: TokenInner::UnexpectedEnd(expect),
                range,
            } => Err(UnexpectedEndError(range, expect)),
            token @ _ => Ok(token),
        }
    }

    pub fn next_token_unchecked(&mut self) -> Token<'s> {
        if let Some(ch) = self.next_char() {
            match ch {
                '/' => self.lex_comment_or_slash(),
                '"' => self.lex_str(),
                '\'' => self.lex_char(),

                ch if ch.is_ascii_lowercase() => {
                    self.lex_ident_or_keyword()
                }
                ch if ch.is_ascii_digit() => self.lex_digit(),

                ch if is_special_symbols(ch) => self.lex_symbol(),
                ch if ch.is_whitespace() => {
                    self.skip_whitespace();
                    self.next_token_unchecked()
                }
                _ => self.lex_ident(),
            }
        } else {
            Token::eof((self.source.len()..self.source.len()).into())
        }
    }
}

impl<'s> Lexer<'s> {
    pub fn lex_symbol(&mut self) -> Token<'s> {
        match self.current_char() {
            '~' => self.lex_single_symbol(TokenInner::SymbolTilde),
            '`' => {
                self.lex_single_symbol(TokenInner::SymbolBackquote)
            }
            '!' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolNotEq)],
                TokenInner::SymbolBang,
            ),
            '@' => self.lex_single_symbol(TokenInner::SymbolAt),
            '#' => self.lex_single_symbol(TokenInner::SymbolHash),
            '$' => self.lex_single_symbol(TokenInner::SymbolDollar),
            '%' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolPercentEq)],
                TokenInner::SymbolPercent,
            ),
            '^' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolCaretEq)],
                TokenInner::SymbolCaret,
            ),
            '&' => self.lex_consecutive_symbol(
                &[
                    ('=', TokenInner::SymbolAmpEq),
                    ('&', TokenInner::SymbolAnd),
                ],
                TokenInner::SymbolAmp,
            ),
            '*' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolAsterEq)],
                TokenInner::SymbolAster,
            ),
            '(' => self.lex_single_symbol(TokenInner::SymbolLParen),
            ')' => self.lex_single_symbol(TokenInner::SymbolRParen),
            '-' => self.lex_consecutive_symbol(
                &[
                    ('=', TokenInner::SymbolMinusEq),
                    ('>', TokenInner::SymbolRArrow),
                ],
                TokenInner::SymbolMinus,
            ),
            '+' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolPlusEq)],
                TokenInner::SymbolPlus,
            ),
            '=' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolDoubleEq)],
                TokenInner::SymbolEq,
            ),
            '{' => self.lex_single_symbol(TokenInner::SymbolLBrace),
            '}' => self.lex_single_symbol(TokenInner::SymbolRBrace),
            '[' => self.lex_single_symbol(TokenInner::SymbolLBracket),
            ']' => self.lex_single_symbol(TokenInner::SymbolRBrace),
            '|' => self.lex_consecutive_symbol(
                &[
                    ('=', TokenInner::SymbolPipeEq),
                    ('|', TokenInner::SymbolOr),
                ],
                TokenInner::SymbolPipe,
            ),
            '\\' => {
                self.lex_single_symbol(TokenInner::SymbolBackslash)
            }
            ':' => self.lex_consecutive_symbol(
                &[(':', TokenInner::SymbolDoubleColon)],
                TokenInner::SymbolColon,
            ),
            ';' => {
                self.lex_single_symbol(TokenInner::SymbolSemicolon)
            }
            '<' => self.lex_consecutive_symbol(
                &[
                    ('=', TokenInner::SymbolLtEq),
                    ('-', TokenInner::SymbolLArrow),
                ],
                TokenInner::SymbolLt,
            ),
            '>' => self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolGtEq)],
                TokenInner::SymbolGt,
            ),
            ',' => self.lex_single_symbol(TokenInner::SymbolComma),
            '.' => self.lex_single_symbol(TokenInner::SymbolDot),
            '?' => self.lex_single_symbol(TokenInner::SymbolQues),
            _ => unreachable!(),
        }
    }

    pub fn lex_consecutive_symbol(
        &mut self,
        continuously: &[(char, TokenInner<'s>)],
        otherwise: TokenInner<'s>,
    ) -> Token<'s> {
        let start = self.current_pos();

        let mut ret = None;
        for (symbol, token) in continuously {
            if self.peek_char().filter(|ch| ch == symbol).is_some() {
                self.next_char();
                ret = Some(*token);
                break;
            }
        }
        Token::new(
            ret.unwrap_or(otherwise),
            start,
            self.current_pos(),
        )
    }

    pub fn lex_single_symbol(
        &mut self,
        token: TokenInner<'s>,
    ) -> Token<'s> {
        Token::new(token, self.current_pos(), self.current_pos())
    }

    pub fn lex_char(&mut self) -> Token<'s> {
        let start = self.current_pos();

        if let Some(ch) = self.next_char() {
            let c = if ch == '\'' { '\x00' } else { ch };

            if let Some('\'') = self.next_char() {
                let end = self.current_pos();
                return Token::new(
                    TokenInner::LitChar(c),
                    start,
                    end,
                );
            }
        }

        Token::unexpected_end(
            (self.current_pos()..self.current_pos()).into(),
            "'",
        )
    }

    pub fn lex_str(&mut self) -> Token<'s> {
        let start = self.current_pos();

        loop {
            if let Some(ch) = self.next_char() {
                if ch == '"' {
                    break;
                } else if ch == '\\' {
                    if let Some('"') = self.peek_char() {
                        self.next_char();
                    }
                }
            } else {
                return Token::unexpected_end(
                    (self.current_pos()..self.current_pos()).into(),
                    "\"",
                );
            }
        }

        let end = self.current_pos();
        Token::new(
            TokenInner::LitStr(&self.source[start + 1..end]),
            start,
            end,
        )
    }

    pub fn lex_digit(&mut self) -> Token<'s> {
        let int_token = self.lex_integer();
        if let Some('.') = self.peek_char() {
            self.next_char();
            self.lex_float((
                int_token.range().start,
                match int_token.inner() {
                    TokenInner::LitInteger(i) => i,
                    _ => unreachable!(),
                },
            ))
        } else {
            int_token
        }
    }

    pub fn lex_float(
        &mut self,
        (start, int_part): (usize, i64),
    ) -> Token<'s> {
        let mut str = String::new();
        str.push_str(&int_part.to_string());
        str.push('.');

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                str.push(ch);
                self.next_char();
            } else {
                break;
            }
        }

        if let Some('e' | 'E') = self.peek_char() {
            self.next_char();
            if let Some('+' | '-') = self.peek_char() {
                let symbol = self.next_char().unwrap();
                if self
                    .peek_char()
                    .filter(char::is_ascii_digit)
                    .is_some()
                {
                    str.push('e');
                    str.push(symbol);
                    while let Some(ch) = self.peek_char() {
                        if ch.is_ascii_digit() {
                            str.push(ch);
                            self.next_char();
                        } else {
                            break;
                        }
                    }
                }
            } else if self
                .peek_char()
                .filter(char::is_ascii_digit)
                .is_some()
            {
                str.push('e');
                while let Some(ch) = self.peek_char() {
                    if ch.is_ascii_digit() {
                        str.push(ch);
                        self.next_char();
                    } else {
                        break;
                    }
                }
            }
        }

        Token::new(
            TokenInner::LitFloat(str.parse::<f64>().unwrap()),
            start,
            self.current_pos(),
        )
    }

    pub fn lex_integer(&mut self) -> Token<'s> {
        if self.current_char() == '0' {
            if let Some(ch) = self.peek_char() {
                match ch {
                    'x' => return self.lex_integer_radix(16),
                    'o' => return self.lex_integer_radix(8),
                    'b' => return self.lex_integer_radix(2),
                    _ => {}
                }
            }
        }

        let start = self.current_pos();

        let mut str = String::new();
        str.push(self.current_char());

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                str.push(ch);
                self.next_char();
            } else {
                break;
            }
        }

        Token::new(
            TokenInner::LitInteger(str.parse::<i64>().unwrap()),
            start,
            self.current_pos(),
        )
    }

    pub fn lex_integer_radix(&mut self, radix: u32) -> Token<'s> {
        let start = self.current_pos();

        self.next_char();

        let mut str = String::new();

        while let Some(ch) = self.peek_char() {
            if ch.is_digit(radix) {
                str.push(ch);
                self.next_char();
            } else {
                break;
            }
        }

        Token::new(
            TokenInner::LitInteger(
                i64::from_str_radix(&str, radix).unwrap(),
            ),
            start,
            self.current_pos(),
        )
    }

    pub fn lex_comment_or_slash(&mut self) -> Token<'s> {
        let start = self.current_pos();
        if let Some(ch) = self.peek_char() {
            match ch {
                '/' => {
                    self.next_char();
                    self.next_char();
                    let mut offset = 0;
                    while let Some(ch) = self.next_char() {
                        if ch == '\n' {
                            offset += 1;
                            break;
                        }
                    }
                    let end = self.current_pos();
                    Token::new(
                        TokenInner::Comment(
                            &self.source[start + 2..=end - offset],
                        ),
                        start,
                        end,
                    )
                }
                '*' => {
                    self.next_char();
                    self.next_char();
                    loop {
                        if let Some(ch) = self.next_char() {
                            if ch == '*'
                                && matches!(
                                    self.peek_char(),
                                    Some('/')
                                )
                            {
                                self.next_char();
                                break;
                            }
                        } else {
                            return Token::unexpected_end(
                                (self.current_pos()
                                    ..self.current_pos())
                                    .into(),
                                "*/",
                            );
                        }
                    }
                    let end = self.current_pos();
                    Token::new(
                        TokenInner::Comment(
                            &self.source[start + 2..=end - 2],
                        ),
                        start,
                        end,
                    )
                }
                _ => self.lex_consecutive_symbol(
                    &[('=', TokenInner::SymbolSlashEq)],
                    TokenInner::SymbolSlash,
                ),
            }
        } else {
            self.lex_consecutive_symbol(
                &[('=', TokenInner::SymbolSlashEq)],
                TokenInner::SymbolSlash,
            )
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn lex_ident_or_keyword(&mut self) -> Token<'s> {
        let start = self.current_pos();
        while let Some(ch) = self.peek_char() {
            if identifier_boundary(ch) {
                break;
            } else {
                self.next_char();
            }
        }
        let end = self.current_pos();

        let str = &self.source[start..=end];
        if let Some(keyword) = KEYWORDS.get(str) {
            Token::new(*keyword, start, end)
        } else {
            Token::new(TokenInner::Ident(str), start, end)
        }
    }

    pub fn lex_ident(&mut self) -> Token<'s> {
        let start = self.current_pos();
        while let Some(ch) = self.peek_char() {
            if identifier_boundary(ch) {
                break;
            } else {
                self.next_char();
            }
        }
        let end = self.current_pos();
        Token::new(
            TokenInner::Ident(&self.source[start..=end]),
            start,
            end,
        )
    }
}

fn identifier_boundary(ch: char) -> bool {
    ch.is_whitespace() || is_special_symbols(ch)
}

fn is_special_symbols(ch: char) -> bool {
    matches!(
        ch,
        '~' | '`'
            | '!'
            | '@'
            | '#'
            | '$'
            | '%'
            | '^'
            | '&'
            | '*'
            | '('
            | ')'
            | '-'
            | '+'
            | '='
            | '{'
            | '}'
            | '['
            | ']'
            | '|'
            | '/'
            | '\\'
            | ':'
            | ';'
            | '<'
            | '>'
            | ','
            | '.'
            | '?'
    )
}

impl std::error::Error for UnexpectedEndError {}

impl Display for UnexpectedEndError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::lexer::Lexer;
    use crate::parser::token::TokenInner;

    #[test]
    fn comments() {
        let mut lexer =
            Lexer::from_source("// abc\n/*def*/ /*unclosed comment*");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::Comment(" abc")
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::Comment("def")
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::UnexpectedEnd("*/")
        );
    }

    #[test]
    fn ident() {
        let mut lexer = Lexer::from_source("foo bar");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::Ident("foo")
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::Ident("bar")
        );
    }

    #[test]
    fn integer() {
        let mut lexer = Lexer::from_source("114514 0xff 0o10 0b1010");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitInteger(114514)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitInteger(255)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitInteger(8)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitInteger(10)
        );
    }

    #[test]
    fn float() {
        let mut lexer = Lexer::from_source("114.514 7.823E5 1.2e-4");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitFloat(114.514)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitFloat(782300f64)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitFloat(0.00012)
        );
    }

    #[test]
    fn string() {
        let mut lexer = Lexer::from_source(
            "\"hello wo\\\"r\\\"ld\" \"unclosed string",
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitStr(r#"hello wo\"r\"ld"#)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::UnexpectedEnd("\"")
        );
    }

    #[test]
    fn char() {
        let mut lexer = Lexer::from_source("'\\' '\n' \'u");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitChar('\\')
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitChar('\n')
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::UnexpectedEnd("'")
        );
    }

    #[test]
    fn boolean() {
        let mut lexer = Lexer::from_source("true false");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitBoolean(true)
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::LitBoolean(false)
        );
    }

    #[test]
    fn keyword() {
        let mut lexer = Lexer::from_source("char async fn");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::KeywordChar
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::KeywordAsync
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::KeywordFn
        );
    }

    #[test]
    fn symbol() {
        let mut lexer = Lexer::from_source("$ -> <=");
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::SymbolDollar
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::SymbolRArrow
        );
        assert_eq!(
            lexer.next_token_unchecked().inner(),
            TokenInner::SymbolLtEq
        );
    }
}
