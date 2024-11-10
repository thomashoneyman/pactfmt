use crate::lexer::Token;

/// A type to hold positional information, including:
///   - Optional leading whitespace/comments
///   - The token
///   - Optional trailing whitespace/comments
pub type Positioned<T> = (Vec<String>, T, Vec<String>);

pub type PositionedToken = Positioned<Token>;

pub enum Literal {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
    Unit,
}

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
pub enum Doc {
    AtDoc(PositionedToken, Positioned<String>),
    Raw(Positioned<String>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct Defun {
    pub defun: PositionedToken,
    pub name: Identifier,
    pub arguments: Wrapped<Vec<Identifier>>,
    pub doc: Option<Doc>,
    pub body: Vec<Expr>,
}
