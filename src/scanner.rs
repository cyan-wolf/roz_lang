use crate::token::{Keyword, Literal, Op, Token, TokenKind};
use crate::error::Error;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    loc: Location,
}

struct Location {
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(content: Vec<char>) -> Self {
        Scanner {
            source: content,
            tokens: vec![],
            loc: Location {
                start: 0,
                current: 0,
                line: 1,
            },
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.loc.start = self.loc.current;
            self.scan_token()?;
        }
        
        self.tokens.push(Token::new(TokenKind::Eof, self.loc.line));
        
        Ok(self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance();

        let token = match c {
            '(' => Token::new(TokenKind::Op(Op::LeftParen), self.loc.line),
            ')' => Token::new(TokenKind::Op(Op::RightParen), self.loc.line),
            '{' => Token::new(TokenKind::Op(Op::LeftBrace), self.loc.line),
            '}' => Token::new(TokenKind::Op(Op::RightBrace), self.loc.line),
            ',' => Token::new(TokenKind::Op(Op::Comma), self.loc.line),
            '.' => Token::new(TokenKind::Op(Op::Dot), self.loc.line),
            '-' => Token::new(TokenKind::Op(Op::Minus), self.loc.line),
            '+' => Token::new(TokenKind::Op(Op::Plus), self.loc.line),
            ';' => Token::new(TokenKind::Op(Op::Semicolon), self.loc.line),
            '*' => Token::new(TokenKind::Op(Op::Star), self.loc.line),
            '!' => {
                let kind = if self.match_next('=') {
                    TokenKind::Op(Op::BangEq)
                } else {
                    TokenKind::Op(Op::Bang)
                };
                Token::new(kind, self.loc.line)
            },
            '=' => {
                let kind = if self.match_next('=') {
                    TokenKind::Op(Op::Equality)
                } else {
                    TokenKind::Op(Op::Eq)
                };
                Token::new(kind, self.loc.line)
            },
            '<' => {
                let kind = if self.match_next('=') {
                    TokenKind::Op(Op::LessEq)
                } else {
                    TokenKind::Op(Op::Less)
                };
                Token::new(kind, self.loc.line)
            },
            '>' => {
                let kind = if self.match_next('=') {
                    TokenKind::Op(Op::GreaterEq)
                } else {
                    TokenKind::Op(Op::Greater)
                };
                Token::new(kind, self.loc.line)
            },
            '/' => {
                let kind = if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    return Ok(());
                } else {
                    TokenKind::Op(Op::Slash)
                };

                Token::new(kind, self.loc.line)
            },
            '\n' => {
                self.loc.line += 1;
                return Ok(());
            },
            c if c.is_whitespace() => {
                // Ignore whitespace.
                return Ok(());
            },
            c if c.is_ascii_digit() => self.build_number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.build_ident(),
            '"' => self.try_build_string()?,
            _ => return Err(Error::new(self.loc.line, "unexpected character".to_owned(), String::new())),
        };

        self.add_token(token);
        
        Ok(())
    }

    fn advance(&mut self) -> char {
        self.loc.current += 1;
        return self.source[self.loc.current];
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.loc.current] != expected {
            return false;
        }

        self.loc.current += 1;
        return true;
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.loc.current]
        }
    }

    fn peek_next(&self) -> char {
        let curr = self.loc.current;

        if curr + 1 >= self.source.len() {
            return '\0';
        }
        self.source[curr + 1]
    }

    fn try_build_string(&mut self) -> Result<Token, Error> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.loc.line += 1;
            }
            self.advance();
          }
      
        if self.is_at_end() {
            return Err(Error::new(self.loc.line, "unterminated string".to_owned(), String::new()));
        }
      
        // The closing quotes.
        self.advance();
      
        // Trim the surrounding quotes.
        let start = self.loc.start;
        let current = self.loc.current;
        let chars = &self.source[start + 1..current - 1];

        let string = Scanner::chars_to_string(chars);

        Ok(Token::new(TokenKind::Literal(Literal::Str(string)), self.loc.line))
    }

    fn build_number(&mut self) -> Token {
        // Consume the integer part.
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the "."
            self.advance();
        }

        // Cosume the decimal part.
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let chars = &self.source[self.loc.start..self.loc.current];

        let num_string = Scanner::chars_to_string(chars);

        let num = num_string.parse::<f64>()
            .expect("unexpected error while parsing number literal");

        Token::new(TokenKind::Literal(Literal::Num(num)), self.loc.line)
    }

    fn build_ident(&mut self) -> Token {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let chars = &self.source[self.loc.start..self.loc.current];
        let ident = Scanner::chars_to_string(chars);

        let kind = match Scanner::ident_to_keyword(&ident) {
            Some(keyword) => TokenKind::Keyword(keyword),
            None => TokenKind::Literal(Literal::Ident(ident)),
        };

        Token::new(kind, self.loc.line)
    }

    fn ident_to_keyword(ident: &str) -> Option<Keyword> {
        let keyword = match ident {
            "and" => Keyword::And,
            "class" => Keyword::Class,
            "else" => Keyword::Else,
            "false" => Keyword::False,
            "for" => Keyword::For,
            "fun" => Keyword::Fun,
            "if" => Keyword::If,
            "nil" => Keyword::Nil,
            "or" => Keyword::Or,
            "print" => Keyword::Print,
            "return" => Keyword::Return,
            "super" => Keyword::Super,
            "this" => Keyword::This,
            "true" => Keyword::True,
            "var" => Keyword::Var,
            "while" => Keyword::While,
            _ => return None,
        };

        Some(keyword)
    }

    fn is_at_end(&self) -> bool {
        self.loc.current >= self.source.len()
    }

    /// Utility function to turn slices of characters from the source 
    /// into `String`s.
    fn chars_to_string(chars: &[char]) -> String {
        let mut buf = String::new();

        for &c in chars {
            buf.push(c);
        }
        buf
    }
}
