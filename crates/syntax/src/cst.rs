use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    Newline(usize),
    Comment(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceToken<T> {
    pub leading: Vec<Spacing>,
    pub value: T,
    pub trailing: Vec<Spacing>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Wrapped<T> {
    pub open: SourceToken<String>,
    pub inner: T,
    pub close: SourceToken<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
}

pub type Literal = SourceToken<LiteralValue>;

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

pub type Identifier = SourceToken<IdentifierFields>;

#[derive(Debug, PartialEq, Clone)]
pub struct App {
    pub left_paren: SourceToken<Token>,
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
    pub right_paren: SourceToken<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub left_bracket: SourceToken<Token>,
    pub members: Vec<(Expr, Option<SourceToken<Token>>)>,
    pub right_bracket: SourceToken<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Object {
    pub left_brace: SourceToken<Token>,
    pub members: Vec<(SourceToken<String>, SourceToken<Token>, Expr)>,
    pub right_brace: SourceToken<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal),
    Application(App),
    List(List),
    Object(Object),
}

#[derive(Debug, PartialEq)]
pub struct Arguments {
    pub left_paren: SourceToken<Token>,
    pub args: Vec<Identifier>,
    pub right_paren: SourceToken<Token>,
}

// FIXME: Missing doc/model annotations and bare docstrings. Will probably
// represent each with their own field and typed where the annotation is
// an Option<PrefixSpacing> for doc or a required PrefixSpacing for model,
// and a string for content for a doc and a ...list expr for model(?)
#[derive(Debug, PartialEq)]
pub struct Defun {
    pub left_paren: SourceToken<Token>,
    pub defun: SourceToken<Token>,
    pub name: Identifier,
    pub arguments: Arguments,
    // FIXME: Technically you can't have a list of expressions in the body,
    // it's really just an optional doc annotation, optional model, and then
    // body, which is a single expr.
    pub body: Vec<Expr>,
    pub right_paren: SourceToken<Token>,
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    Defun(Defun),
    Expr(Expr),
}
