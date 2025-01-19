use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    // \n
    NewlineOne,
    // \n\n+
    NewlineMany,
    // \s+
    Whitespace,
    // ;.*
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
    pub left_paren: PrefixSpacing,
    pub defun: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Wrapped<Vec<Identifier>>,
    pub body: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}
