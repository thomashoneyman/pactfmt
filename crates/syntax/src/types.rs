
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

/// Token set for the Pact language, aligned with
/// https://github.com/kadena-io/pact-5/blob/master/pact/Pact/Core/Syntax/Lexer.x
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Let,
    LetStar,
    Lambda,
    Module,
    Interface,

    Import, // 'use' keyword
    Bless,
    Implements,

    Step,
    StepWithRollback,

    // Definition keywords
    Defun,
    DefConst,
    DefCap,
    DefPact,
    DefSchema,
    DefTable,

    // Annotations
    DocAnn,     // @doc
    ModelAnn,   // @model
    EventAnn,   // @event
    ManagedAnn, // @managed

    // Boolean literals
    True,
    False,

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

    Ident(String),
    Number(String), // used for integers and decimals
    String(String),
    SingleTick(String),

    // End of file
    Eof,
}
