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
    //TODO: add Error type
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
            Identifier if self.peek_token_is(Colon) => self.parse_assign_stmt(token),
            // _ => self.parse_expression_stmt(token),
            _ => {
                eprintln!("{token} is not allowed on the top level");
                Err(())
            }
        }
    }

    fn peek_token_is(&mut self, kind: TokenKind) -> bool {
        self.peek_token().kind == kind
    }

    fn parse_expression_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        let value = self.parse_expression(token, ast::Precedence::Lowest)?;
        self.skip_token(TokenKind::Semicolon);
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
        let loc = token.loc;
        let mut value = ast::Expression::default();
        while !self.peek_token_is(TokenKind::Semicolon) {
            let token = self.next_token();
            value = self.parse_expression(token, ast::Precedence::Lowest)?;
        }
        self.skip_token(TokenKind::Semicolon);
        Ok(ast::Stmt::ReturnStmt(ast::ReturnStmt { loc, value }))
    }
    fn parse_assign_stmt(&mut self, token: Token) -> Result<ast::Stmt, ()> {
        let name = token.value;
        let loc = token.loc;

        self.skip_token(TokenKind::Colon);

        let typ = self.parse_cortextype()?;

        self.skip_token(TokenKind::Assign);

        let mut value = ast::Expression::default();

        while !self.peek_token_is(TokenKind::Semicolon) {
            let token = self.next_token();
            value = self.parse_expression(token, ast::Precedence::Lowest)?;
        }

        self.skip_token(TokenKind::Semicolon);

        Ok(ast::Stmt::AssignStmt(ast::AssignStmt {
            loc,
            name,
            typ,
            value,
        }))
    }

    fn skip_token(&mut self, _kind: TokenKind) {
        self.next_token();
    }

    fn parse_cortextype(&mut self) -> Result<ast::CortexType, ()> {
        let ntoken = self.next_token();
        match (ntoken.value.as_str(), ntoken.kind) {
            ("int", TokenKind::Identifier) => Ok(ast::CortexType::Int),
            _ => {
                eprintln!("No impl");
                return Err(());
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assing() {
        let mut p = Parser::new(Lexer::new(file!().into(), "x: int = 10;".into()));
        let result = p.parse();
        assert!(result.is_ok());
        if let Ok(program) = result {
            assert_eq!("Program {\n    x : int = 10\n}", &program.to_string());
        }
    }
}
