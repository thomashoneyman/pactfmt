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
    // Keywords
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

    // Definition keywords
    DefunKeyword,
    DefConstKeyword,
    DefCapKeyword,
    DefPactKeyword,
    DefSchemaKeyword,
    DefTableKeyword,

    // Annotations
    DocAnnKeyword,     // @doc
    ModelAnnKeyword,   // @model
    EventAnnKeyword,   // @event
    ManagedAnnKeyword, // @managed

    // Boolean literals
    TrueKeyword,
    FalseKeyword,

    // Delimiters
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]

    // Operators and punctuation
    Comma,
    Colon,
    Dot,
    BindAssign, // :=
    DynAcc,     // ::

    // Literals and identifiers
    Ident,
    Number,
    StringLit,
    SingleTick,

    // End of file
    Eof,

    // Special token for errors in the lexer
    Error,
}

/// Tree kinds for syntax nodes in the Pact CST
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TreeKind {
    // Top-level constructs
    Program,
    Module,
    Interface,
    Import,

    // Definitions
    FunctionDef,
    ConstDef,
    CapabilityDef,
    PactDef,
    SchemaDef,
    TableDef,

    // Parameters and type annotations
    ParamList,
    Param,
    TypeAnnotation,

    // Expressions
    Let,
    LetStar,
    Lambda,
    FunctionApp,
    Literal,
    ListLiteral,
    ObjectLiteral,
    Property,
    BinaryOp,

    // Statements
    Block,
    Step,
    StepWithRollback,

    // References
    Name,
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
