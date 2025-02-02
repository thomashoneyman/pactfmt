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

/// An untyped identifier
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
    pub identifier: Named,
    // Technically, many fields we're treating as 'identifiers' cannot
    // have a type annotation, but it doesn't cause any harm from the
    // perspective of the formatter.
    pub type_annotation: Option<Type>,
}

pub type Identifier = Positioned<IdentifierFields>;

#[derive(Debug, PartialEq, Clone)]
pub struct App {
    pub left_paren: PrefixSpacing,
    pub func: Box<Positioned<Named>>,
    pub args: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub left_bracket: PrefixSpacing,
    pub members: Vec<Expr>,
    pub right_bracket: PrefixSpacing,
}

/// @doc "docstring"
#[derive(Debug, PartialEq, Clone)]
pub struct DocAnn {
    pub ann: PrefixSpacing,
    pub docstr: Positioned<String>,
}

/// @model [ ... ]
#[derive(Debug, PartialEq, Clone)]
pub struct ModelAnn {
    pub ann: PrefixSpacing,
    // Technically this can only be a restricted set of applications
    // or identifiers, but for formatting purposes we don't validate.
    pub exprs: List,
}

/// @managed or @managed amount amount-mgr, where the arguments must
/// be on the same line as @managed
#[derive(Debug, PartialEq, Clone)]
pub struct ManagedAnn {
    pub ann: PrefixSpacing,
    pub args: Option<(Named, Named)>,
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

#[derive(Debug, PartialEq, Clone)]
pub enum DefunBody {
    DocAnn(DocAnn),
    ModelAnn(ModelAnn),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Defun {
    pub left_paren: PrefixSpacing,
    pub defun: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Arguments,
    pub body: Vec<DefunBody>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub enum DefcapBody {
    DocAnn(DocAnn),
    EventAnn(PrefixSpacing),
    ManagedAnn(ManagedAnn),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Defcap {
    pub left_paren: PrefixSpacing,
    pub defcap: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Arguments,
    pub body: Vec<DefcapBody>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub enum ModuleGovernance {
    Keyset(String),
    Cap(String),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub left_paren: PrefixSpacing,
    pub module: PrefixSpacing,
    pub name: Positioned<Named>,
    pub governance: Positioned<ModuleGovernance>,
    // A module technically has a significantly more restricted set
    // of allowed members, but toplevel largely captures the same.
    pub body: Vec<Toplevel>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    Defun(Defun),
    Defcap(Defcap),
    Expr(Expr),
    Module(Module),
}
