use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token(":=")]
    ColonEquals,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool),

    #[token("let*")]
    LetRec,

    #[token("let")]
    Let,

    #[token("if")]
    If,

    #[token("defun")]
    Defun,

    #[token("defcap")]
    DefCap,

    #[token("defconst")]
    DefConst,

    #[token("defschema")]
    DefSchema,

    #[token("deftable")]
    DefTable,

    #[token("defpact")]
    DefPact,

    #[token("interface")]
    Interface,

    #[token("module")]
    Module,

    #[token("bless")]
    Bless,

    #[token("implements")]
    Implements,

    #[token("use")]
    Use,

    #[token("lambda")]
    Lambda,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("load")]
    Load,

    #[token("@doc")]
    DocAnn,

    #[token("@model")]
    ModelAnn,

    #[token("@event")]
    EventAnn,

    #[token("@managed")]
    ManagedAnn,

    #[token("step-with-rollback")]
    StepWithRollback,

    #[token("enforce")]
    Enforce,

    #[token("enforce-one")]
    EnforceOne,

    #[token("enforce-guard")]
    EnforceGuard,

    #[token("keyset-ref-guard")]
    KeysetRefGuard,

    #[token("step")]
    Step,

    #[token("with-capability")]
    WithCapability,

    #[token("create-user-guard")]
    CreateUserGuard,

    #[token("try")]
    Try,

    #[token("do")]
    Do,

    #[token("suspend")]
    Suspend,

    #[regex(r"-?[0-9]+", |lex| lex.slice().to_string())]
    Integer(String),

    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().to_string())]
    Decimal(String),

    // A string which may contain internal escaped quotes like \"
    #[regex(r#""(?:[^"\\]|\\.|\\[\n\r][^"])*""#, |lex| lex.slice().to_string())]
    String(String),

    // A single-quoted string terminated by whitespace, a colon, dot, etc.
    #[regex(r#"'[^\s\t\n\r,:.\"\'\[\]()]+"#, |lex| lex.slice().to_string())]
    Symbol(String),

    // NOTE: This is a vague attempt to capture the many characters allowed in a Pact
    // identifier, though only ASCII, whereas a Pact identifier can also include arbitrary
    // unicode. Will almost certainly have to adjust this in the future.
    #[regex(
        r#"[a-zA-Z%#+\-_&$@<>=?*!|/][a-zA-Z0-9%#+\-_&$@<>=?*!|/]*"#,
        priority = 1,
        callback = |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"[ \t]+", logos::skip)]
    Whitespace,

    #[regex(r"(\n|\r\n)+", |lex| lex.slice().to_string())]
    Newlines(String),

    #[regex(r";[^\n]*", |lex| lex.slice().to_string())]
    Comment(String),
}

impl winnow::stream::ContainsToken<Token> for Token {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        *self == token
    }
}

impl winnow::stream::ContainsToken<Token> for &'_ [Token] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for [Token; LEN] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<Token> for &'_ [Token; LEN] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    fn lex_single(input: &str) -> Token {
        Token::lexer(input).find(Result::is_ok).unwrap().unwrap()
    }

    /// A test macro for single tokens that captures a Rust identifier, Captures a Rust
    /// identifier, expression (input string), and another expression (expected token).
    /// Macro accepts many cases repeated, separated by commas, repeating the indicated
    /// test output for every captured ident/expr set.
    macro_rules! single_token {
        ($($name:ident: $input:expr => $token:expr),*,) => {
            $(
                #[test]
                fn $name() {
                    assert_eq!(lex_single($input), $token);
                }
            )*
        }
    }

    single_token! {
      lex_left_brace: "{" => Token::LeftBrace,
      lex_right_brace: "}" => Token::RightBrace,
      lex_left_paren: "(" => Token::LeftParen,
      lex_right_paren: ")" => Token::RightParen,
      lex_left_bracket: "[" => Token::LeftBracket,
      lex_right_bracket: "]" => Token::RightBracket,
      lex_colon_equals: ":=" => Token::ColonEquals,
      lex_colon: ":" => Token::Colon,
      lex_double_colon: "::" => Token::DoubleColon,
      lex_comma: "," => Token::Comma,
      lex_dot: "." => Token::Dot,
      lex_true: "true" => Token::Boolean(true),
      lex_false: "false" => Token::Boolean(false),
      lex_let_rec: "let*" => Token::LetRec,
      lex_let: "let" => Token::Let,
      lex_if: "if" => Token::If,
      lex_defun: "defun" => Token::Defun,
      lex_defcap: "defcap" => Token::DefCap,
      lex_defconst: "defconst" => Token::DefConst,
      lex_defschema: "defschema" => Token::DefSchema,
      lex_deftable: "deftable" => Token::DefTable,
      lex_defpact: "defpact" => Token::DefPact,
      lex_interface: "interface" => Token::Interface,
      lex_module: "module" => Token::Module,
      lex_bless: "bless" => Token::Bless,
      lex_implements: "implements" => Token::Implements,
      lex_use: "use" => Token::Use,
      lex_lambda: "lambda" => Token::Lambda,
      lex_and: "and" => Token::And,
      lex_or: "or" => Token::Or,
      lex_load: "load" => Token::Load,
      lex_doc_ann: "@doc" => Token::DocAnn,
      lex_model_ann: "@model" => Token::ModelAnn,
      lex_event_ann: "@event" => Token::EventAnn,
      lex_managed_ann: "@managed" => Token::ManagedAnn,
      lex_step_with_rollback: "step-with-rollback" => Token::StepWithRollback,
      lex_enforce: "enforce" => Token::Enforce,
      lex_enforce_one: "enforce-one" => Token::EnforceOne,
      lex_enforce_guard: "enforce-guard" => Token::EnforceGuard,
      lex_keyset_ref_guard: "keyset-ref-guard" => Token::KeysetRefGuard,
      lex_step: "step" => Token::Step,
      lex_with_capability: "with-capability" => Token::WithCapability,
      lex_create_user_guard: "create-user-guard" => Token::CreateUserGuard,
      lex_try: "try" => Token::Try,
      lex_do: "do" => Token::Do,
      lex_suspend: "suspend" => Token::Suspend,

      // These entries use regex instead of full string matches
      lex_integer: "42" => Token::Integer("42".into()),
      lex_neg_integer: "-42" => Token::Integer("-42".into()),
      lex_decimal: "3.14" => Token::Decimal("3.14".into()),
      lex_neg_decimal: "-3.14" => Token::Decimal("-3.14".into()),
      lex_ident: "add" => Token::Ident("add".into()),
      lex_complex_ident: "add-two!" => Token::Ident("add-two!".into()),
      lex_string: "\"hello\"" => Token::String("\"hello\"".into()),
      lex_escaped_string: "\"he\\\"llo\"" => Token::String("\"he\\\"llo\"".into()),
      lex_symbol: "'mysym" => Token::Symbol("'mysym".into()),
      lex_comment: "; comment" => Token::Comment("; comment".into()),
      lex_unix_newline: "\n" => Token::Newlines("\n".into()),
      lex_windows_newline: "\r\n" => Token::Newlines("\r\n".into()),
    }

    #[test]
    fn test_whitespace_sequence() {
        let input = "  \n  \t\t";
        let mut lexer = Token::lexer(input);
        while let Some(token_result) = lexer.next() {
            println!(
                "Token result: {:?} at position {:?}",
                token_result,
                lexer.span()
            );
        }

        // Keep the original assertion for when we fix the issue
        let tokens: Vec<_> = Token::lexer(input)
            .map(|result| result.expect("Failed to lex token"))
            .collect();
        assert_eq!(tokens, vec![Token::Newlines("\n".into())]);
    }
}
