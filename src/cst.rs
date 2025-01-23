#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    NewlineOne,
    NewlineMany,
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

/// A reference like "my-mod.my-name" or "a.b.c.d"
#[derive(Debug, Clone, PartialEq)]
pub struct Reference {
    pub first: String,
    pub second: String,
    pub rest: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Named {
    Ident(String),
    Reference(Reference),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Simple identifier type like :integer or :time
    Ident(String),
    /// List type like [integer] or [object{schema}]
    List(Box<Type>),
    /// Object type like object{schema} or object{}
    Object(Option<String>),
    /// Schema type like {schema}
    Schema(String),
    /// Module type like module{schema,other.schema}
    Module(Vec<Named>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierFields {
    pub identifier: String,
    pub type_annotation: Option<Type>,
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
pub struct List {
    pub left_bracket: PrefixSpacing,
    pub members: Vec<Expr>,
    pub right_bracket: PrefixSpacing,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal),
    Application(App),
    List(List),
}

#[derive(Debug, PartialEq)]
pub struct Arguments {
    pub left_paren: PrefixSpacing,
    pub args: Vec<Identifier>,
    pub right_paren: PrefixSpacing,
}

// FIXME: Missing doc/model annotations and bare docstrings. Will probably
// represent each with their own field and typed where the annotation is
// an Option<PrefixSpacing> for doc or a required PrefixSpacing for model,
// and a string for content for a doc and a ...list expr for model(?)
#[derive(Debug, PartialEq)]
pub struct Defun {
    pub left_paren: PrefixSpacing,
    pub defun: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Arguments,
    // FIXME: Technically you can't have a list of expressions in the body,
    // it's really just an optional doc annotation, optional model, and then
    // body, which is a single expr.
    pub body: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    Defun(Defun),
    Expr(Expr),
}
