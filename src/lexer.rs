use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

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

    #[regex(r"-?[0-9]+")]
    Integer,

    #[regex(r"-?[0-9]+\.[0-9]+")]
    Decimal,

    #[regex(r#""(?:[^"\\]|\\.|\\[\n\r][^"])*""#)]
    String,

    #[regex(r#"'[^\s\t\n\r,:.\"\'\[\]()]+"#)]
    Symbol,

    // NOTE: This includes ASCII characters, but technically it's possible for Pact source
    // code to include arbitrary unicode.
    #[regex(
        r#"[a-zA-Z%#+\-_&$@<>=?*!|/][a-zA-Z0-9%#+\-_&$@<>=?*!|/]*"#,
        priority = 1
    )]
    Ident,

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

    // Comments are a semicolon followed by a sequence of characters terminated by a newline. The
    // newline is consumed and dropped from the token stream.
    #[regex(r";[^\n]*", callback = |lex| {
        if lex.remainder().starts_with('\n') {
            lex.bump(1);
        }
        Some(())
    })]
    Comment,

    #[token("\n")]
    Newline,

    #[regex(r"[ \t\r]+", logos::skip)]
    Whitespace,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Result<Token, ()>> {
        Token::lexer(input).collect()
    }

    fn lex_ok(input: &str) -> Vec<Token> {
        lex(input).into_iter().filter_map(Result::ok).collect()
    }

    #[test]
    fn test_punctuation() {
        assert_eq!(
            lex_ok("( ) { } [ ]"),
            vec![
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
                Token::LeftBracket,
                Token::RightBracket
            ]
        );
        assert_eq!(
            lex_ok(":= : :: , ."),
            vec![
                Token::ColonEquals,
                Token::Colon,
                Token::DoubleColon,
                Token::Comma,
                Token::Dot
            ]
        );
    }

    #[test]
    fn test_boolean() {
        assert_eq!(lex_ok("true false"), vec![Token::Boolean, Token::Boolean]);
        assert_eq!(lex_ok("TRUE FALSE"), vec![Token::Ident, Token::Ident]); // Should be identifiers, not booleans
    }

    #[test]
    fn test_numbers() {
        assert_eq!(
            lex_ok("0 42 -17"),
            vec![Token::Integer, Token::Integer, Token::Integer]
        );
        assert_eq!(
            lex_ok("3.14 -0.5 42.0"),
            vec![Token::Decimal, Token::Decimal, Token::Decimal]
        );
        assert_eq!(
            lex_ok("3. .14"),
            vec![Token::Integer, Token::Dot, Token::Dot, Token::Integer]
        ); // Not valid decimals
    }

    #[test]
    fn test_string() {
        assert_eq!(lex_ok(r#""Hello, world!""#), vec![Token::String]);
        assert_eq!(
            lex_ok(r#""Escaped \"quotes\" and \n newlines""#),
            vec![Token::String]
        );
        assert_eq!(lex_ok("\"Multi-line \\\nstring\""), vec![Token::String]);
        assert!(lex("\"Unterminated string").iter().any(|r| r.is_err()));
    }

    #[test]
    fn test_symbol() {
        assert_eq!(lex_ok("'symbol"), vec![Token::Symbol]);
        assert_eq!(lex_ok("'complex-symbol123"), vec![Token::Symbol]);
        assert_eq!(lex_ok("'invalid symbol"), vec![Token::Symbol, Token::Ident]);
        // Space terminates the symbol
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            lex_ok("variable _underscore $dollar"),
            vec![Token::Ident, Token::Ident, Token::Ident]
        );
        assert_eq!(
            lex_ok("!important? ++increment"),
            vec![Token::Ident, Token::Ident]
        );
        assert_eq!(
            lex_ok("@at-sign %percent"),
            vec![Token::Ident, Token::Ident]
        );
    }

    #[test]
    fn test_comment() {
        assert_eq!(
            lex_ok("; This is a comment\n; With a trailing newline\n\n"),
            vec![Token::Comment, Token::Comment, Token::Newline]
        );
        assert_eq!(lex_ok("; Comment without newline"), vec![Token::Comment]);
    }

    #[test]
    fn test_mixed_tokens() {
        let input = r#"
        (module example GOVERNANCE
          (defcap GOVERNANCE ()
            (enforce-keyset 'admin-keyset))

          (defun square (x:integer)
            (* x x))

          (defun sum-squares (a:integer b:integer)
            (let ((a-squared (square a))
                  (b-squared (square b)))
              (+ a-squared b-squared)))

          ; This is a comment
          (defun process-data (amount:decimal)
            @doc "Process data with a given amount"
            @model [(property (> amount 0.0))]
            (enforce (> amount 0.0) "Amount must be positive")
            (with-read accounts 'user-account { "balance" := bal }
              (enforce (<= amount bal) "Insufficient funds")
              (update accounts 'user-account
                { "balance": (- bal amount) })))
        )
        "#;

        let tokens = lex_ok(input);

        // Tokens that should be present
        assert!(tokens.contains(&Token::LeftBracket));
        assert!(tokens.contains(&Token::RightBracket));
        assert!(tokens.contains(&Token::LeftParen));
        assert!(tokens.contains(&Token::RightParen));
        assert!(tokens.contains(&Token::Ident)); // e.g., module, defcap, defun, enforce-keyset, etc.
        assert!(tokens.contains(&Token::Symbol)); // e.g., 'admin-keyset, 'user-account
        assert!(tokens.contains(&Token::String)); // e.g., "Process data with a given amount"
        assert!(tokens.contains(&Token::Decimal)); // e.g., 0.0
        assert!(tokens.contains(&Token::Colon)); // Used in type annotations like x:integer
        assert!(tokens.contains(&Token::Comment)); // The line with ; This is a comment
        assert!(tokens.contains(&Token::LeftBrace)); // Used in { "balance" := bal }
        assert!(tokens.contains(&Token::RightBrace));
        assert!(tokens.contains(&Token::ColonEquals)); // Used in := for binding

        // Tokens that should not be present
        assert!(!tokens.contains(&Token::DoubleColon));
        assert!(!tokens.contains(&Token::Comma));
        assert!(!tokens.contains(&Token::Dot));
        assert!(!tokens.contains(&Token::Boolean));
    }
}
