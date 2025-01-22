use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    NewlineOne,
    NewlineMany,
    Whitespace,
    Comment(String),
}

/// Spacing that comes before any given token.
/// For arbitrary tokens, this is best paired like (PrefixSpacing, Token)
/// But for tokens with known kinds such as '(', the field name usually suffices
pub type PrefixSpacing = Vec<Spacing>;

/// A type to hold positional information, including:
///   - Optional leading whitespace/comments
///   - The token
pub type Positioned<T> = (PrefixSpacing, T);

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
}

pub type Literal = Positioned<LiteralValue>;

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierFields {
    pub identifier: Token,
    pub type_annotation: Option<(Token, Token)>,
}

pub type Identifier = Positioned<IdentifierFields>;

#[derive(Debug, PartialEq, Clone)]
pub struct App {
    pub left_paren: PrefixSpacing,
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal),
    Application(App),
}

#[derive(Debug, PartialEq)]
pub struct Arguments {
    pub left_paren: PrefixSpacing,
    pub args: Vec<Identifier>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub struct Defun {
    pub left_paren: PrefixSpacing,
    pub defun: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Arguments,
    pub body: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}
