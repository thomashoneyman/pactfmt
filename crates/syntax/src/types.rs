/// CST types, inspired by
/// https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

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

/// Token kinds for the Pact language lexer
/// https://github.com/kadena-io/pact-5/blob/master/pact/Pact/Core/Syntax/Lexer.x
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Reserved keywords
    LetKeyword,
    LetStarKeyword,
    LambdaKeyword,
    ModuleKeyword,
    InterfaceKeyword,
    ImportKeyword, // 'use' keyword
    BlessKeyword,
    ImplementsKeyword,
    StepKeyword,
    StepWithRollbackKeyword,
    DefunKeyword,
    DefConstKeyword,
    DefCapKeyword,
    DefPactKeyword,
    DefSchemaKeyword,
    DefTableKeyword,
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
    StringLit,
    SingleTick,
    True,
    False,

    // Keywords not reserved in the Pact lexer, but reserved in the parser
    ExpectTypechecksKeyword,       // expect-typechecks
    ExpectTypecheckFailureKeyword, // expect-typecheck-failure
    WithCapabilityKeyword,         // with-capability
    CreateUserGuardKeyword,        // create-user-guard
    EnforceKeyword,                // enforce
    EnforceOneKeyword,             // enforce-one

    // Special tokens
    Eof,
    Error,
}

/// Tree kinds for syntax nodes in the Pact CST; these trees are simplified
/// for formatting, but can be extended to cover more cases later.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TreeKind {
    File,

    // Top-level constructs
    Module,
    Interface,
    Import,
    Expr,

    // Definitions
    Defun,
    Defconst,
    Defcap,
    Defpact,
    Defschema,
    Deftable,

    // Exprs
    Literal,
    ListLiteral,
    ObjectLiteral,
    BindLiteral,
    Let,
    Lambda,

    // Names
    Name,
    TypeAnnotation,
    QualifiedName,

    // Annotations
    DocAnnotation,
    ModelAnnotation,
    EventAnnotation,
    ManagedAnnotation,

    // Error node
    ErrorTree,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tree {
    pub kind: TreeKind,
    pub children: Vec<Child>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Child {
    Token(SourceToken),
    Tree(Tree),
}
