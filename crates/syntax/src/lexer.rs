use crate::types::{SourcePos, SourceRange, Token, Trivia};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    /// Source input
    input: Peekable<Chars<'a>>,
    /// Current position in the source
    pos: SourcePos,
    /// The current character being processed
    current: Option<char>,
    /// Whether we've reached the end of the file
    at_eof: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from a string
    pub fn new(source: &'a str) -> Self {
        let mut chars = source.chars().peekable();
        let current = chars.next();

        Self {
            input: chars,
            pos: SourcePos { line: 1, column: 1 },
            current,
            at_eof: false,
        }
    }

    /// Advance to the next character in the input
    fn advance(&mut self) {
        if let Some(ch) = self.current {
            // Update position
            if ch == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
        }

        // Get the next character
        self.current = self.input.next();

        // Check if we're at the end of the file
        if self.current.is_none() {
            self.at_eof = true;
        }
    }

    /// Peek at the next character without advancing
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    /// Check if the current character matches the expected one and advance if it does
    fn match_char(&mut self, expected: char) -> bool {
        if let Some(ch) = self.current {
            if ch == expected {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Collect trivia (whitespace and comments) before the next token
    fn collect_trivia(&mut self) -> Vec<Trivia> {
        let mut collected = Vec::new();

        loop {
            match self.current {
                Some(' ') | Some('\t') => {
                    let mut count = 0;

                    // Collect consecutive whitespace characters
                    while let Some(ch) = self.current {
                        if ch == ' ' || ch == '\t' || ch == '\r' {
                            count += 1;
                            self.advance();
                        } else {
                            break;
                        }
                    }

                    collected.push(Trivia::Space(count));
                }
                Some('\n') => {
                    let mut count = 0;

                    // Collect consecutive newlines
                    while let Some('\n') = self.current {
                        count += 1;
                        self.advance();
                    }

                    collected.push(Trivia::Line(count));
                }
                Some(';') => {
                    let mut comment = String::new();

                    // Collect until end of line
                    while let Some(ch) = self.current {
                        if ch == '\n' {
                            break;
                        }
                        comment.push(ch);
                        self.advance();
                    }

                    collected.push(Trivia::Comment(comment));
                }
                _ => break,
            }
        }

        collected
    }

    /// Scan a single token from the input
    pub fn scan_token(&mut self) -> Option<(Token, SourceRange, Vec<Trivia>)> {
        // Collect leading trivia (whitespace and comments)
        let leading = self.collect_trivia();

        if self.at_eof {
            return Some((Token::Eof, self.make_range(self.pos.clone()), leading));
        }

        let start_pos = self.pos.clone();
        let current = self.current?;

        // Process the current character
        let token = match current {
            '(' => {
                self.advance();
                Token::OpenParen
            }
            ')' => {
                self.advance();
                Token::CloseParen
            }
            '{' => {
                self.advance();
                Token::OpenBrace
            }
            '}' => {
                self.advance();
                Token::CloseBrace
            }
            '[' => {
                self.advance();
                Token::OpenBracket
            }
            ']' => {
                self.advance();
                Token::CloseBracket
            }
            ',' => {
                self.advance();
                Token::Comma
            }
            '.' => {
                self.advance();
                Token::Dot
            }
            ':' => {
                self.advance();
                if self.match_char('=') {
                    Token::BindAssign // :=
                } else if self.match_char(':') {
                    Token::DynAcc // ::
                } else {
                    Token::Colon // :
                }
            }
            '@' => {
                self.advance();
                // Check for annotations
                if self.scan_identifier_if("doc") {
                    Token::DocAnn
                } else if self.scan_identifier_if("model") {
                    Token::ModelAnn
                } else if self.scan_identifier_if("event") {
                    Token::EventAnn
                } else if self.scan_identifier_if("managed") {
                    Token::ManagedAnn
                } else {
                    // If it's not a known annotation, treat it as part of an identifier
                    self.scan_identifier_from('@')
                }
            }
            '"' => {
                self.advance();
                self.scan_string()
            }
            '\'' => {
                self.advance();
                self.scan_single_tick()
            }
            ch if Self::is_digit(ch) || (ch == '-' && self.peek().is_some_and(Self::is_digit)) => {
                self.scan_number()
            }
            ch if Self::is_ident_start(ch) => self.scan_identifier(),
            _ => {
                // Unknown character
                self.advance();
                Token::Ident(current.to_string())
            }
        };

        let range = self.make_range(start_pos);
        Some((token, range, leading))
    }

    /// Check if the character is a valid start for an identifier
    fn is_ident_start(ch: char) -> bool {
        ch.is_alphabetic()
            || [
                '%', '#', '+', '-', '_', '&', '$', '@', '<', '>', '=', '^', '?', '*', '!', '|',
                '/', '~',
            ]
            .contains(&ch)
    }

    /// Check if the character can be part of an identifier
    fn is_ident_part(ch: char) -> bool {
        Self::is_ident_start(ch) || ch.is_ascii_digit()
    }

    /// Check if the character is a digit
    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit()
    }

    /// Scan a string literal. Strings can be multi-line using a backslash as a
    /// line continuation. NOTE: We currently remove the continuation.
    fn scan_string(&mut self) -> Token {
        let mut result = String::new();

        while let Some(ch) = self.current {
            if ch == '"' {
                self.advance();
                break;
            } else if ch == '\\' {
                self.advance();

                match self.current {
                    Some('\n') => {
                        // This is a line continuation - skip both the backslash and newline
                        self.advance();
                    }
                    Some(' ') => {
                        // Check if this is a line continuation with space between backslashes
                        self.advance(); // consume the space
                        if let Some('\\') = self.current {
                            self.advance(); // consume the second backslash
                            if let Some('\n') = self.current {
                                self.advance(); // consume the newline
                            }
                        } else {
                            // Not a line continuation, preserve the backslash and space
                            result.push('\\');
                            result.push(' ');
                        }
                    }
                    Some('n') => {
                        result.push('\n');
                        self.advance();
                    }
                    Some('t') => {
                        result.push('\t');
                        self.advance();
                    }
                    Some('"') => {
                        result.push('"');
                        self.advance();
                    }
                    Some('\'') => {
                        result.push('\'');
                        self.advance();
                    }
                    Some('\\') => {
                        // Check if this is a double backslash followed by newline
                        self.advance();
                        if let Some('\n') = self.current {
                            // This is a line continuation - skip both backslashes and newline
                            self.advance();
                        } else {
                            // Just a regular escaped backslash
                            result.push('\\');
                        }
                    }
                    Some(ch) => {
                        // For any other escaped character, preserve both the backslash and the character
                        result.push('\\');
                        result.push(ch);
                        self.advance();
                    }
                    None => {
                        // Unterminated string
                        result.push('\\');
                    }
                }
            } else {
                result.push(ch);
                self.advance();
            }
        }

        Token::String(result)
    }

    /// Scan a single-quoted identifier (symbol)
    fn scan_single_tick(&mut self) -> Token {
        let mut result = String::new();

        // First character should be alphabetic
        if let Some(ch) = self.current {
            if ch.is_alphabetic() {
                result.push(ch);
                self.advance();
            } else {
                return Token::SingleTick(result);
            }
        }

        // Rest of the characters can be alphanumeric or '-' or '_'
        while let Some(ch) = self.current {
            if ch.is_alphanumeric() || ch == '-' || ch == '_' {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Token::SingleTick(result)
    }

    /// Scan a number (may be negative)
    fn scan_number(&mut self) -> Token {
        let mut result = String::new();
        let negative = matches!(self.current, Some('-'));

        // Handle negative sign
        if negative {
            result.push('-');
            self.advance();
        }

        while let Some(ch) = self.current {
            if Self::is_digit(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Token::Number(result)
    }

    /// Scan an identifier
    fn scan_identifier(&mut self) -> Token {
        let first_char = self.current.unwrap();
        self.advance();
        self.scan_identifier_rest(first_char)
    }

    /// Scan an identifier from a specific starting character
    fn scan_identifier_from(&mut self, first_char: char) -> Token {
        self.scan_identifier_rest(first_char)
    }

    /// Continue scanning an identifier after the first character
    fn scan_identifier_rest(&mut self, first_char: char) -> Token {
        let mut ident = String::new();
        ident.push(first_char);

        while let Some(ch) = self.current {
            if Self::is_ident_part(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords
        match ident.as_str() {
            "let" => Token::Let,
            "let*" => Token::LetStar,
            "lambda" => Token::Lambda,
            "module" => Token::Module,
            "interface" => Token::Interface,
            "use" => Token::Import,
            "bless" => Token::Bless,
            "implements" => Token::Implements,
            "step" => Token::Step,
            "step-with-rollback" => Token::StepWithRollback,
            "defun" => Token::Defun,
            "defconst" => Token::DefConst,
            "defcap" => Token::DefCap,
            "defpact" => Token::DefPact,
            "defschema" => Token::DefSchema,
            "deftable" => Token::DefTable,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident),
        }
    }

    /// Check if the following characters match the expected identifier
    fn scan_identifier_if(&mut self, expected: &str) -> bool {
        let mut i = 0;
        let mut iter = self.input.clone();

        for expected_char in expected.chars() {
            match if i == 0 { self.current } else { iter.next() } {
                Some(ch) if ch == expected_char => {
                    i += 1;
                }
                _ => return false,
            }
        }

        // Ensure we're at the end of an identifier
        if let Some(next) = if i == 0 {
            self.peek()
        } else {
            iter.peek().copied()
        } {
            if Self::is_ident_part(next) {
                return false;
            }
        }

        // If we made it here, we have a match
        // Advance the lexer past the matched identifier
        for _ in 0..i {
            self.advance();
        }

        true
    }

    /// Create a source range from a start position to the current position
    fn make_range(&self, start: SourcePos) -> SourceRange {
        SourceRange {
            start,
            end: self.pos.clone(),
        }
    }

    /// Tokenize the entire input, keeping track of trivia
    pub fn tokenize_with_trivia(&mut self) -> Vec<(Token, SourceRange, Vec<Trivia>, Vec<Trivia>)> {
        let mut tokens: Vec<(Token, SourceRange, Vec<Trivia>, Vec<Trivia>)> = Vec::new();
        let mut pending_trivia: Vec<Trivia> = Vec::new();

        while let Some((token, range, leading)) = self.scan_token() {
            let is_eof = matches!(token, Token::Eof);

            // Collect next trivia
            let next_trivia: Vec<Trivia> = self.collect_trivia();

            if is_eof {
                // At EOF, all remaining trivia is trailing for the last real token
                if !tokens.is_empty() {
                    let last_idx = tokens.len() - 1;
                    // Add any pending trivia first
                    tokens[last_idx].3.extend(pending_trivia);
                    // Then add the final trivia
                    tokens[last_idx].3.extend(next_trivia);
                }
                tokens.push((token, range, leading, Vec::new()));
                break;
            }

            // Split trivia at first newline - everything before is trailing, after is leading
            let mut trailing: Vec<Trivia> = Vec::new();
            let mut next_leading: Vec<Trivia> = Vec::new();
            let mut saw_newline = false;

            for t in next_trivia {
                if !saw_newline {
                    if matches!(t, Trivia::Line(_)) {
                        saw_newline = true;
                        trailing.push(t);
                    } else {
                        trailing.push(t);
                    }
                } else {
                    next_leading.push(t);
                }
            }

            // Add any pending trivia to this token's leading trivia
            let mut final_leading = leading;
            final_leading.splice(0..0, pending_trivia);

            tokens.push((token, range, final_leading, trailing));
            pending_trivia = next_leading;
        }

        tokens
    }

    /// Tokenize the entire input (simplified version without trivia)
    pub fn tokenize(&mut self) -> Vec<(Token, SourceRange)> {
        let mut tokens = Vec::new();

        while let Some((token, range, _)) = self.scan_token() {
            let is_eof = matches!(token, Token::Eof);
            tokens.push((token, range));

            if is_eof {
                break;
            }
        }

        tokens
    }
}

/// Tokenize a string of Pact code
pub fn tokenize(source: &str) -> Vec<(Token, SourceRange)> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

/// A token with associated trivia (whitespace, comments)
#[derive(Debug, Clone)]
pub struct TokenWithTrivia {
    /// The token value
    pub token: Token,
    /// Source range information
    pub range: SourceRange,
    /// Leading trivia (whitespace, comments before the token)
    pub leading: Vec<Trivia>,
    /// Trailing trivia (whitespace, comments after the token)
    pub trailing: Vec<Trivia>,
}

/// Tokenize a string of Pact code and include whitespace/newline information
pub fn tokenize_with_trivia(source: &str) -> Vec<TokenWithTrivia> {
    let mut lexer = Lexer::new(source);
    let tokens_with_trivia = lexer.tokenize_with_trivia();

    tokens_with_trivia
        .into_iter()
        .map(|(token, range, leading, trailing)| TokenWithTrivia {
            token,
            range,
            leading,
            trailing,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "(defun add (x y) (+ x y))";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        // Check token types (ignoring source ranges for simplicity)
        assert_eq!(tokens[0].0, Token::OpenParen);
        assert_eq!(tokens[1].0, Token::Defun);
        assert_eq!(tokens[2].0, Token::Ident("add".to_string()));
        assert_eq!(tokens[3].0, Token::OpenParen);
        assert_eq!(tokens[4].0, Token::Ident("x".to_string()));
        assert_eq!(tokens[5].0, Token::Ident("y".to_string()));
        assert_eq!(tokens[6].0, Token::CloseParen);
        assert_eq!(tokens[7].0, Token::OpenParen);
        assert_eq!(tokens[8].0, Token::Ident("+".to_string()));
        assert_eq!(tokens[9].0, Token::Ident("x".to_string()));
        assert_eq!(tokens[10].0, Token::Ident("y".to_string()));
        assert_eq!(tokens[11].0, Token::CloseParen);
        assert_eq!(tokens[12].0, Token::CloseParen);
        assert_eq!(tokens[13].0, Token::Eof);
    }

    #[test]
    fn test_keywords() {
        let source =
            "let let* lambda module interface use bless implements step step-with-rollback";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::Let);
        assert_eq!(tokens[1].0, Token::LetStar);
        assert_eq!(tokens[2].0, Token::Lambda);
        assert_eq!(tokens[3].0, Token::Module);
        assert_eq!(tokens[4].0, Token::Interface);
        assert_eq!(tokens[5].0, Token::Import); // 'use' is mapped to Import
        assert_eq!(tokens[6].0, Token::Bless);
        assert_eq!(tokens[7].0, Token::Implements);
        assert_eq!(tokens[8].0, Token::Step);
        assert_eq!(tokens[9].0, Token::StepWithRollback);
    }

    #[test]
    fn test_annotations() {
        let source = "@doc @model @event @managed";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::DocAnn);
        assert_eq!(tokens[1].0, Token::ModelAnn);
        assert_eq!(tokens[2].0, Token::EventAnn);
        assert_eq!(tokens[3].0, Token::ManagedAnn);
    }

    #[test]
    fn test_punctuation() {
        let source = "( ) { } [ ] , . : := ::";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::OpenParen);
        assert_eq!(tokens[1].0, Token::CloseParen);
        assert_eq!(tokens[2].0, Token::OpenBrace);
        assert_eq!(tokens[3].0, Token::CloseBrace);
        assert_eq!(tokens[4].0, Token::OpenBracket);
        assert_eq!(tokens[5].0, Token::CloseBracket);
        assert_eq!(tokens[6].0, Token::Comma);
        assert_eq!(tokens[7].0, Token::Dot);
        assert_eq!(tokens[8].0, Token::Colon);
        assert_eq!(tokens[9].0, Token::BindAssign);
        assert_eq!(tokens[10].0, Token::DynAcc);
    }

    #[test]
    fn test_identifiers() {
        let source = "var x123 $special _under-dash";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::Ident("var".to_string()));
        assert_eq!(tokens[1].0, Token::Ident("x123".to_string()));
        assert_eq!(tokens[2].0, Token::Ident("$special".to_string()));
        assert_eq!(tokens[3].0, Token::Ident("_under-dash".to_string()));
    }

    #[test]
    fn test_numbers() {
        let source = "123 -456 3.14";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::Number("123".to_string()));
        assert_eq!(tokens[1].0, Token::Number("-456".to_string()));
        assert_eq!(tokens[2].0, Token::Number("3".to_string()));
        assert_eq!(tokens[3].0, Token::Dot);
        assert_eq!(tokens[4].0, Token::Number("14".to_string()));
    }

    #[test]
    fn test_strings() {
        let source = r#"" " "hello" "escape\"quote" "new\nline""#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::String(" ".to_string()));
        assert_eq!(tokens[1].0, Token::String("hello".to_string()));
        assert_eq!(tokens[2].0, Token::String("escape\"quote".to_string()));
        assert_eq!(tokens[3].0, Token::String("new\nline".to_string()));
    }

    #[test]
    fn test_single_tick() {
        let source = "'symbol 'kebab-case 'with_underscore";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::SingleTick("symbol".to_string()));
        assert_eq!(tokens[1].0, Token::SingleTick("kebab-case".to_string()));
        assert_eq!(
            tokens[2].0,
            Token::SingleTick("with_underscore".to_string())
        );
    }

    #[test]
    fn test_comments() {
        let source = "token ; comment\n next_token";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].0, Token::Ident("token".to_string()));
        assert_eq!(tokens[1].0, Token::Ident("next_token".to_string()));
    }

    #[test]
    fn test_mixed_code() {
        let source = r#"(defun transfer (from:string to:string amount:decimal)
            ; Transfer money between accounts
            @doc "Transfer function"
            (enforce (> amount 0.0) "Amount must be positive")
            (enforce (!= from to) "Cannot transfer to yourself"))
        "#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        // Just test a few key tokens
        assert_eq!(tokens[0].0, Token::OpenParen);
        assert_eq!(tokens[1].0, Token::Defun);
        assert_eq!(tokens[2].0, Token::Ident("transfer".to_string()));
    }

    #[test]
    fn test_trivia() {
        // Test 1: Inline comments are trailing
        let tokens = tokenize_with_trivia("foo ; comment\nbar");
        assert_eq!(tokens[0].trailing.len(), 3); // space + comment + newline
        assert!(matches!(tokens[0].trailing[0], Trivia::Space(1)));
        assert!(matches!(tokens[0].trailing[1], Trivia::Comment(ref s) if s == "; comment"));
        assert!(matches!(tokens[0].trailing[2], Trivia::Line(1)));

        // Test 2: After newline, comments become leading
        let tokens = tokenize_with_trivia("foo\n  ; comment\nbar");
        assert_eq!(tokens[1].leading.len(), 3); // space + comment + newline
        assert!(matches!(tokens[1].leading[0], Trivia::Space(2)));
        assert!(matches!(tokens[1].leading[1], Trivia::Comment(ref s) if s == "; comment"));
        assert!(matches!(tokens[1].leading[2], Trivia::Line(1)));

        // Test 3: EOF comments attach to last token
        let tokens = tokenize_with_trivia("foo\n  ; comment");
        let last = &tokens[tokens.len() - 2]; // -2 to skip EOF
        assert!(last
            .trailing
            .iter()
            .any(|t| matches!(t, Trivia::Comment(ref s) if s == "; comment")));
    }
}
