#[derive(Debug, PartialEq, Clone)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceRange {
    pub start: SourcePos,
    pub end: SourcePos,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Trivia {
    Comment(String),
    Space(usize),
    Line(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceToken {
    pub kind: TokenKind,
    pub text: String,
    pub range: SourceRange,
    pub leading: Vec<Trivia>,  // Whitespace/comments before this token
    pub trailing: Vec<Trivia>, // Whitespace/comments after this token
}

/// Tokens, enumerated from the Pact lexer:
/// https://github.com/kadena-io/pact-5/blob/master/pact/Pact/Core/Syntax/Lexer.x
/// and the Pact parser:
/// https://github.com/kadena-io/pact-5/blob/master/pact/Pact/Core/Syntax/Parser.y
/// as well as the Pact ungrammar in reference/pact.ungram.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Reserved keywords
    LetKeyword,
    LetStarKeyword,
    LambdaKeyword,
    ModuleKeyword,
    InterfaceKeyword,
    ImportKeyword, // use keyword
    BlessKeyword,
    ImplementsKeyword,
    StepKeyword,
    StepWithRollbackKeyword,
    DefunKeyword,
    DefconstKeyword,
    DefcapKeyword,
    DefpactKeyword,
    DefschemaKeyword,
    DeftableKeyword,
    DocAnnKeyword,     // @doc
    ModelAnnKeyword,   // @model
    EventAnnKeyword,   // @event
    ManagedAnnKeyword, // @managed

    // Syntax
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    Comma,
    Colon,
    Dot,
    BindAssign, // :=
    DynAcc,     // ::

    // Literals
    Ident,
    Number,
    String,
    Symbol,
    Bool,

    // Keywords not reserved in the Pact lexer, but which we expect
    // to provide different formatting.
    WithCapabilityKeyword,  // with-capability
    WithReadKeyword,        // with-read
    WithDefaultReadKeyword, // with-default-read
    EnforceKeyword,         // enforce
    IfKeyword,              // if
    CondKeyword,            // cond
    DoKeyword,              // do

    // Special tokens
    Eof,
    Error,
}

/// Tree kinds for the Pact CST, derived from the Pact ungrammar in
/// reference/pact.ungram.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TreeKind {
    // Top-level structure
    TopLevel,

    // Module and interface structures
    Module,
    Interface,
    Governance,
    Documentation,
    DocAnnotation,
    ModelAnnotation,

    // External declarations
    ExternalDecl,
    Use,
    ImportList,
    Implements,
    Bless,

    // Definitions
    Def,
    ParamList,
    Defun,
    Defconst,
    Defcap,
    Defschema,
    Deftable,
    Defpact,

    // Pact steps
    Step,
    StepWithRollback,

    // Interface definitions (consts, schemas are the same as normal defs)
    IfDef,
    IfDefun,
    IfDefcap,
    IfDefpact,

    // Capability metadata
    CapabilityMeta,

    // Parameters and fields
    Param,
    SchemaField,

    // Type system
    TypeAnn,
    Type,
    PrimType,

    // Expressions
    Let,
    Binder,
    Lambda,
    App,
    Binding,
    List,
    Object,
    FieldPair,
    BindPair,

    // Literals
    IntLiteral,
    DecimalLiteral,

    // Names
    Name,
    ModRef,
    ParsedName,

    // Property expressions
    // PropertyExpr,
    // PropLet,
    // PropBinder,
    // PropLam,
    // PropApp,
    // PropList,
    // PropDefProperty,

    // Special functions we intend to format differently
    // WithCapability,
    // WithRead,
    // WithDefaultRead,
    // Enforce,
    // If,
    // Cond,
    // Do,

    // Error node
    ErrorTree,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tree {
    pub kind: TreeKind,
    pub children: Vec<Child>,
}

impl Tree {
    pub fn has_errors(&self) -> bool {
        if self.kind == TreeKind::ErrorTree {
            return true;
        }

        for child in &self.children {
            if let Child::Tree(subtree) = child {
                if subtree.has_errors() {
                    return true;
                }
            }
        }

        false
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Child {
    Token(SourceToken),
    Tree(Tree),
}
