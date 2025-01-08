use crate::lexer::{Token, WhitespaceKind};

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    Whitespace(WhitespaceKind),
    Comment(String),
}

/// A type to hold positional information, including:
///   - Optional leading whitespace/comments
///   - The token
///   - Optional trailing whitespace/comments
pub type Positioned<T> = (Vec<Spacing>, T, Vec<Spacing>);

pub type PositionedToken = Positioned<Token>;

// pub enum Literal {
//     String(String),
//     Symbol(String),
//     Integer(String),
//     Decimal(String),
//     Boolean(String),
//     Unit,
// }

// Technically, wrapped structures can only be parens, brackets, braces, etc.
// so it's not an _arbitrary_ positioned token
#[derive(Debug, PartialEq)]
pub struct Wrapped<T> {
    pub left: PositionedToken,
    pub middle: T,
    pub right: PositionedToken,
}

#[derive(Debug, PartialEq)]
pub struct IdentifierFields {
    pub identifier: Token,
    pub type_annotation: Option<(Token, Token)>,
}

pub type Identifier = Positioned<IdentifierFields>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct Defun {
    pub defun: PositionedToken,
    pub name: Identifier,
    pub arguments: Wrapped<Vec<Identifier>>,
    pub body: Vec<Expr>,
}
