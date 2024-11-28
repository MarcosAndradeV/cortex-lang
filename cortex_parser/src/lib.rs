use cortex_lexer::{Lexer, Token, TokenKind};

mod ast;

pub struct Parser {
    lex: Lexer,
    peeked: Option<Token>,
}

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        Self { lex, peeked: None }
    }
    fn next_token(&mut self) -> Token {
        let token = self.peeked.take().unwrap_or_else(|| self.lex.next_token());
        return token;
    }

    fn peek_token(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    pub fn parse(&mut self) -> Result<ast::Program, ()> {
        let mut program = ast::Program::new();
        loop {
            let token = self.next_token();
            if token.kind == TokenKind::EOF {
                break;
            }
            let stmt = self.parse_stmt(token)?;
            program.stmts.push(stmt);
        }

        return Ok(program);
    }

    fn parse_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        use TokenKind::*;
        match token.kind {
            Identifier if self.peek_token_is(Assign) => self.parse_assign_stmt(token),
            Return => self.parse_return_stmt(token),
            _ => self.parse_expression_stmt(token),
        }
    }
    fn peek_token_is(&mut self, kind: TokenKind) -> bool {
        self.peek_token().kind == kind
    }
    fn parse_expression_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        let value = self.parse_expression(token, ast::Precedence::Lowest)?;
        self.next_token();
        Ok(ast::Stmt::ExpressionStmt(value))
    }

    fn parse_expression(
        &mut self,
        token: Token,
        precedence: ast::Precedence,
    ) -> Result<ast::Expression, ()> {
        use ast::Precedence::*;
        use TokenKind::*;
        let mut left = match token.kind {
            Bang | Minus => {
                let etoken = self.next_token();
                let expr = self.parse_expression(etoken, Prefix)?;
                ast::Expression::PrefixOperator(ast::PrefixOperator {
                    loc: token.loc,
                    op: ast::Operator::prefix_from_token_kind(&token.kind)?,
                    right: Box::new(expr),
                })
            }
            Identifier => ast::Expression::IdentifierExpr(ast::Identifier {
                loc: token.loc,
                name: token.value,
            }),
            Interger => ast::Expression::from_integer_token(token),
            False | True => ast::Expression::from_boolean_token(token),
            Nil => ast::Expression::from_nil_token(token),
            Oparen => {
                let etoken = self.next_token();
                let expr = self.parse_expression(etoken, Lowest)?;
                let cparen = self.next_token();
                if cparen.kind != Cparen {
                    eprint!("{} Expected \")\".", token.loc);
                    return Err(());
                }
                ast::Expression::Group(ast::Group {
                    loc: token.loc,
                    value: Box::new(expr),
                })
            }
            _ => {
                eprintln!("TODO parse_expression {token}");
                return Err(());
            }
        };

        while !self.peek_token_is(Semicolon) && precedence < self.peek_infix_precedence() {
            let token = self.next_token();
            let infix = self.parse_infix_expression(token, left)?;
            left = infix
        }

        Ok(left)
    }
    fn parse_infix_expression(
        &mut self,
        token: Token,
        left: ast::Expression,
    ) -> Result<ast::Expression, ()> {
        let precedence = self.get_infix_precedence(&token.kind);
        let rtoken = self.next_token();
        let right = self.parse_expression(rtoken, precedence)?;
        Ok(ast::Expression::InfixOperator(ast::InfixOperator {
            loc: token.loc,
            op: ast::Operator::infix_from_token_kind(&token.kind)?,
            right: Box::new(right),
            left: Box::new(left),
        }))
    }
    fn peek_infix_precedence(&mut self) -> ast::Precedence {
        ast::Precedence::infix_from_token_kind(&self.peek_token().kind)
    }
    fn get_infix_precedence(&mut self, kind: &TokenKind) -> ast::Precedence {
        ast::Precedence::infix_from_token_kind(kind)
    }
    fn parse_return_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        let Token {
            kind: _,
            value: _,
            loc,
        } = token;
        let mut value = ast::Expression::default();
        while !self.peek_token_is(TokenKind::Semicolon) {
            let token = self.next_token();
            value = self.parse_expression(token, ast::Precedence::Lowest)?;
        }
        self.next_token();
        Ok(ast::Stmt::ReturnStmt(ast::ReturnStmt { loc, value }))
    }
    fn parse_assign_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        self.next_token();
        let Token {
            kind: _,
            value: name,
            loc,
        } = token;
        let mut value = ast::Expression::default();
        while !self.peek_token_is(TokenKind::Semicolon) {
            let token = self.next_token();
            value = self.parse_expression(token, ast::Precedence::Lowest)?;
        }
        self.next_token();

        Ok(ast::Stmt::AssignStmt(ast::AssignStmt { loc, name, value }))
    }
}
