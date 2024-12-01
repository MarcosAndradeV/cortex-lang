use std::fmt;

use cortex_lexer::{Loc, Token, TokenKind};

#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = write!(f, "Program {{");
        for stmt in &self.stmts {
            let _ = write!(f, "\n    {stmt}");
        }
        write!(f, "\n}}")
    }
}
impl Program {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Debug)]
pub enum Stmt {
    AssignStmt(AssignStmt),
    ReturnStmt(ReturnStmt),
    ExpressionStmt(Expression),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::AssignStmt(assign_stmt) => write!(f, "{assign_stmt}"),
            Stmt::ReturnStmt(return_stmt) => write!(f, "{return_stmt}"),
            Stmt::ExpressionStmt(expression) => write!(f, "{expression}"),
        }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub loc: Loc,
    pub value: Expression,
}

impl fmt::Display for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {}", self.value)
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    pub loc: Loc,
    pub name: String,
    pub typ: CortexType,
    pub value: Expression,
}

impl fmt::Display for AssignStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {} = {}", self.name, self.typ, self.value)
    }
}

#[derive(Debug)]
pub enum CortexType {
    Int
}

impl fmt::Display for CortexType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CortexType::Int => write!(f, "int"),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    IdentifierExpr(Identifier),
    PrefixOperator(PrefixOperator),
    InfixOperator(InfixOperator),
    Group(Group)
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::IdentifierExpr(identifier) => write!(f, "{}", identifier.name),
            Expression::PrefixOperator(prefix_operator) => write!(f, "{prefix_operator}"),
            Expression::InfixOperator(infix_operator) => write!(f, "{infix_operator}"),
            Expression::Group(group) => write!(f, "({})", group.value.as_ref()),
        }
    }
}

#[derive(Debug)]
pub struct Group {
    pub loc: Loc,
    pub value: Box<Expression>,
}

#[derive(Debug)]
pub struct Identifier {
    pub loc: Loc,
    pub name: String,
}

#[derive(Debug)]
pub struct NumberLiteral {
    pub loc: Loc,
    pub value: i64,
}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub loc: Loc,
    pub value: bool,
}

#[derive(Debug)]
pub enum Literal {
    Number(NumberLiteral),
    Boolean(BooleanLiteral),
    Nil(NilLiteral),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(number_literal) => write!(f, "{}", number_literal.value),
            Literal::Boolean(boolean_literal) => write!(f, "{}", boolean_literal.value),
            Literal::Nil(_) => write!(f, "nil",),
        }
    }
}

#[derive(Debug)]
pub struct NilLiteral {
    pub loc: Loc,
}

impl Expression {
    pub fn from_integer_token(value: Token) -> Self {
        let literal = value
            .value
            .parse::<i64>()
            .expect("TODO: Error in lexer probably");
        Expression::Literal(Literal::Number(NumberLiteral {
            loc: value.loc,
            value: literal,
        }))
    }
    pub fn from_boolean_token(value: Token) -> Self {
        let literal = value
            .value
            .parse::<bool>()
            .expect("TODO: Error in lexer probably");
        Expression::Literal(Literal::Boolean(BooleanLiteral {
            loc: value.loc,
            value: literal,
        }))
    }
    pub fn from_nil_token(value: Token) -> Self {
        Expression::Literal(Literal::Nil(NilLiteral { loc: value.loc }))
    }
}
impl Default for Expression {
    fn default() -> Self {
        Expression::Literal(Literal::Nil(NilLiteral {
            loc: Loc::default(),
        }))
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    // Call,
}
impl Precedence {
    pub fn infix_from_token_kind(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Star | TokenKind::Slash => Precedence::Product,
            TokenKind::Lt | TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Eq | TokenKind::NEq => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct PrefixOperator {
    pub loc: Loc,
    pub op: Operator,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.op, self.right.as_ref())
    }
}

#[derive(Debug)]
pub struct InfixOperator {
    pub loc: Loc,
    pub op: Operator,
    pub right: Box<Expression>,
    pub left: Box<Expression>,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.as_ref(),
            self.op,
            self.right.as_ref()
        )
    }
}

#[derive(Debug)]
pub enum Operator {
    LogicalNot,
    Negate,
    Eq,
    NEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Slash,
    Star,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::LogicalNot => write!(f, "!"),
            Operator::Negate => write!(f, "-"),
            Operator::Eq => write!(f, "=="),
            Operator::NEq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Slash => write!(f, "/"),
            Operator::Star => write!(f, "*"),
        }
    }
}

impl Operator {
    pub fn prefix_from_token_kind(kind: &TokenKind) -> Result<Self, ()> {
        use TokenKind::*;
        match kind {
            Bang => Ok(Self::LogicalNot),
            Minus => Ok(Self::Negate),
            _ => {
                eprintln!("TODO kind is not operator");
                Err(())
            }
        }
    }
    pub fn infix_from_token_kind(kind: &TokenKind) -> Result<Self, ()> {
        use TokenKind::*;
        match kind {
            Minus => Ok(Self::Minus),
            Plus => Ok(Self::Plus),
            Star => Ok(Self::Star),
            Slash => Ok(Self::Slash),
            Eq => Ok(Self::Eq),
            NEq => Ok(Self::NEq),
            Lt => Ok(Self::Lt),
            Gt => Ok(Self::Gt),
            _ => {
                eprintln!("TODO kind is not operator");
                Err(())
            }
        }
    }
}
