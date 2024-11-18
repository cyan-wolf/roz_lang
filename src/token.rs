
#[derive(Debug, PartialEq)]
pub struct Token {
    line: u32,
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, line: u32) -> Self {
        Self {line, kind}
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Op(Op),
    Literal(Literal),
    Keyword(Keyword),
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum Op {
    LeftParen, 
    RightParen, 
    LeftBrace, 
    RightBrace,
    Comma, 
    Dot, 
    Minus, 
    Plus, 
    Semicolon, 
    Slash, 
    Star,
    Bang,
    BangEq,
    Equality,
    Eq,
    LessEq,
    GreaterEq,
    Less,
    Greater,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Num(f64),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    And, 
    Class, 
    Else, 
    False, 
    Fun, 
    For, 
    If, 
    Nil, 
    Or,
    Print, 
    Return, 
    Super, 
    This, 
    True, 
    Var, 
    While,
}