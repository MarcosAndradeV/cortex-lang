use core::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Invalid,
    EOF,
    Identifier,
    Interger,

    Assign,
    Plus,
    Minus,
    Star,
    Bang,
    Slash,
    Lt,
    Gt,
    Eq,
    NEq,

    Comma,
    Semicolon,
    Colon,

    Oparen,
    Cparen,
    OBrace,
    CBrace,
    Fn,
    While,
    If,
    Else,
    True,
    False,
    Nil,
    Return,
}

impl TokenKind {
    fn from_identifier_or_keyword(value: &String) -> Self {
        match value.as_str() {
            "fn" => Self::Fn,
            "if" => Self::If,
            "else" => Self::Else,
            "while" => Self::While,
            "true" => Self::True,
            "false" => Self::False,
            "nil" => Self::Nil,
            "return" => Self::Return,
            _ => Self::Identifier,
        }
    }
    pub fn is_eof(&self) -> bool {
        *self == TokenKind::EOF
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub loc: Loc,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}({})", self.loc, self.kind, self.value)
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Default)]
pub struct Loc {
    filepath: String,
    line: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filepath, self.line, self.col)
    }
}

impl Loc {
    pub fn new(filepath: String, line: usize, col: usize) -> Self {
        Self {
            filepath,
            line,
            col,
        }
    }

    pub fn next_column(&mut self) {
        self.col += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn next(&mut self, c: u8) {
        match c {
            b'\n' => self.next_line(),
            b'\t' => {
                let ts = 8;
                self.col = (self.col / ts) * ts + ts;
            }
            c if (c as char).is_control() => {}
            _ => self.next_column(),
        }
    }
}

#[derive(Default)]
pub struct Lexer {
    input: Vec<u8>,
    pos: usize,
    read_pos: usize,
    ch: u8,
    loc: Loc,
}

impl Lexer {
    pub fn new(filepath: String, input: String) -> Self {
        let mut lex = Self {
            input: input.into_bytes(),
            loc: Loc {
                filepath,
                line: 1,
                col: 1,
            },
            ..Default::default()
        };
        lex.read_char();
        return lex;
    }
    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
        self.loc.next(self.ch);
    }
    fn peek_char(&mut self) -> u8 {
        if self.pos >= self.input.len() {
            0
        } else {
            self.input[self.read_pos]
        }
    }
    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;
        self.skip_whitespace();
        match self.ch {
            b'+' => self.make_token(Plus, "+"),
            b'-' => self.make_token(Minus, "-"),
            b'*' => self.make_token(Star, "*"),
            b'/' => self.make_token(Slash, "/"),
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(NEq, "!=")
                } else {
                    self.make_token(Bang, "!")
                }
            }
            b'<' => self.make_token(Lt, "<"),
            b'>' => self.make_token(Gt, ">"),
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.make_token(Eq, "==")
                } else {
                    self.make_token(Assign, "=")
                }
            }
            b',' => self.make_token(Comma, ","),
            b';' => self.make_token(Semicolon, ";"),
            b':' => self.make_token(Colon, ":"),
            b'(' => self.make_token(Oparen, "("),
            b')' => self.make_token(Cparen, ")"),
            b'{' => self.make_token(OBrace, "{"),
            b'}' => self.make_token(CBrace, "}"),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
            b'0'..=b'9' => self.number(),
            0 => self.make_token(EOF, "\0"),
            c => {
                let loc = self.loc.clone();
                self.read_char();
                Token {
                    kind: Invalid,
                    value: format!("{}", c as char),
                    loc,
                }
            }
        }
    }

    fn make_token(&mut self, kind: TokenKind, value: &str) -> Token {
        let loc = self.loc.clone();
        self.read_char();
        Token {
            kind,
            value: value.into(),
            loc,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !self.ch.is_ascii_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    fn number(&mut self) -> Token {
        let start_pos = self.pos;
        let loc = self.loc.clone();

        loop {
            if !matches!(self.ch, b'0'..=b'9') {
                break;
            }
            self.read_char();
        }
        let value: String = String::from_utf8_lossy(&self.input[start_pos..self.pos]).into();

        Token {
            kind: TokenKind::Interger,
            value,
            loc,
        }
    }
    fn identifier(&mut self) -> Token {
        let start_pos = self.pos;
        let loc = self.loc.clone();

        loop {
            if !matches!(self.ch, b'a'..b'z'|b'A'..b'Z'|b'_') {
                break;
            }
            self.read_char();
        }
        let value: String = String::from_utf8_lossy(&self.input[start_pos..self.pos]).into();

        Token {
            kind: TokenKind::from_identifier_or_keyword(&value),
            value,
            loc,
        }
    }
}
