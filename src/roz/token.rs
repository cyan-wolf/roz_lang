use std::fmt::Display;


#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    line: usize,
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize) -> Self {
        Self {line, kind}
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Op(Op),
    Literal(Literal),
    Keyword(Keyword),
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Str(String),
    Num(f64),
    Ident(String),
}

#[derive(Debug, PartialEq, Clone)]
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Op::LeftParen => "(",
            Op::RightParen => ")",
            Op::LeftBrace => "{",
            Op::RightBrace => "}",
            Op::Comma => ",",
            Op::Dot => ".",
            Op::Minus => "-",
            Op::Plus => "+",
            Op::Semicolon => ";",
            Op::Slash => "/",
            Op::Star => "*",
            Op::Bang => "!",
            Op::BangEq => "!=",
            Op::Equality => "==",
            Op::Eq => "=",
            Op::LessEq => "<=",
            Op::GreaterEq => ">=",
            Op::Less => "<",
            Op::Greater => ">",
        };

        write!(f, "{str}")
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Str(str) => write!(f, "\"{str}\""),
            Literal::Num(num) => write!(f, "{num}"),
            Literal::Ident(ident) => write!(f, "`{ident}`"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Keyword::And => "and",
            Keyword::Class => "class",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fun => "fun",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::Nil => "nil",
            Keyword::Or => "or",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::True => "true",
            Keyword::Var => "var",
            Keyword::While => "while",
        };

        write!(f, "{str}")
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Op(op) => write!(f, "{op}"),
            TokenKind::Literal(literal) => write!(f, "{literal}"),
            TokenKind::Keyword(keyword) => write!(f, "{keyword}"),
            TokenKind::Eof => write!(f, "'end of file'"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{kind}", kind=self.kind)
    }
}