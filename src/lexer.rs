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

    #[token("true")]
    #[token("false")]
    Boolean,

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

    #[regex(r"-?[0-9]+")]
    Integer,

    #[regex(r"-?[0-9]+\.[0-9]+")]
    Decimal,

    // A string which may contain internal escaped quotes like \"
    #[regex(r#""(?:[^"\\]|\\.|\\[\n\r][^"])*""#)]
    String,

    #[regex(r#""{3}[^"]*"{3}"#)]
    MultilineString,

    // A single-quoted string terminated by whitespace, a colon, dot, etc.
    #[regex(r#"'[^\s\t\n\r,:.\"\'\[\]()]+"#)]
    Symbol,

    // NOTE: This is a vague attempt to capture the many characters allowed in a Pact
    // identifier, though only ASCII, whereas a Pact identifier can also include arbitrary
    // unicode. Will almost certainly have to adjust this in the future.
    #[regex(
        r#"[a-zA-Z%#+\-_&$@<>=?*!|/][a-zA-Z0-9%#+\-_&$@<>=?*!|/]*"#,
        priority = 1
    )]
    Ident,

    #[regex(r";[^\n]*")]
    Comment,

    #[regex(r"\s+")]
    Whitespace,
}

#[cfg(test)]
mod tests {
    use super::Token;
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
      lex_true: "true" => Token::Boolean,
      lex_false: "false" => Token::Boolean,
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
      lex_step: "step" => Token::Step,
      lex_with_capability: "with-capability" => Token::WithCapability,
      lex_create_user_guard: "create-user-guard" => Token::CreateUserGuard,
      lex_try: "try" => Token::Try,
      lex_do: "do" => Token::Do,
      lex_suspend: "suspend" => Token::Suspend,

      // These entries use regex instead of full string matches
      lex_integer: "42" => Token::Integer,
      lex_neg_integer: "-42" => Token::Integer,
      lex_decimal: "3.14" => Token::Decimal,
      lex_neg_decimal: "-3.14" => Token::Decimal,
      lex_ident: "add" => Token::Ident,
      lex_complex_ident: "add-two!" => Token::Ident,
      lex_string: "\"hello\"" => Token::String,
      lex_escaped_string: "\"he\\\"llo\"" => Token::String,
      lex_multiline: "\"\"\"multi\nline\"\"\"" => Token::MultilineString,
      lex_symbol: "'mysym" => Token::Symbol,
      lex_comment: "; comment" => Token::Comment,
      lex_whitespace: "   " => Token::Whitespace,
      lex_mixed_whitespace: " \t\n\r " => Token::Whitespace,
    }
}
