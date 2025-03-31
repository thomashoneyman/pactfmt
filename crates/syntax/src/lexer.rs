use crate::types::{SourcePos, SourceRange, SourceToken, TokenKind, Trivia};
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
            if ch == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
        }

        self.current = self.input.next();

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

                    while let Some('\n') = self.current {
                        count += 1;
                        self.advance();
                    }

                    collected.push(Trivia::Line(count));
                }
                Some(';') => {
                    let mut comment = String::new();

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
    pub fn scan_token(&mut self) -> Option<(TokenKind, String, SourceRange, Vec<Trivia>)> {
        // Collect leading trivia (whitespace and comments)
        let leading = self.collect_trivia();

        if self.at_eof {
            let range = self.make_range(self.pos.clone());
            return Some((TokenKind::Eof, "".to_string(), range, leading));
        }

        let start_pos = self.pos.clone();
        let current = self.current?;
        let mut text = String::new();

        // Process the current character
        let token_kind = match current {
            '(' => {
                text.push(current);
                self.advance();
                TokenKind::OpenParen
            }
            ')' => {
                text.push(current);
                self.advance();
                TokenKind::CloseParen
            }
            '{' => {
                text.push(current);
                self.advance();
                TokenKind::OpenBrace
            }
            '}' => {
                text.push(current);
                self.advance();
                TokenKind::CloseBrace
            }
            '[' => {
                text.push(current);
                self.advance();
                TokenKind::OpenBracket
            }
            ']' => {
                text.push(current);
                self.advance();
                TokenKind::CloseBracket
            }
            ',' => {
                text.push(current);
                self.advance();
                TokenKind::Comma
            }
            '.' => {
                text.push(current);
                self.advance();
                TokenKind::Dot
            }
            ':' => {
                text.push(current);
                self.advance();
                if self.match_char('=') {
                    text.push('=');
                    TokenKind::BindAssign // :=
                } else if self.match_char(':') {
                    text.push(':');
                    TokenKind::DynAcc // ::
                } else {
                    TokenKind::Colon // :
                }
            }
            '@' => {
                text.push(current);
                self.advance();
                // Check for annotations
                let annotation_text = self.scan_annotation_text();
                text.push_str(&annotation_text);

                match annotation_text.as_str() {
                    "doc" => TokenKind::DocAnnKeyword,
                    "model" => TokenKind::ModelAnnKeyword,
                    "event" => TokenKind::EventAnnKeyword,
                    "managed" => TokenKind::ManagedAnnKeyword,
                    _ => {
                        // Unknown annotation is treated as an identifier
                        TokenKind::Ident
                    }
                }
            }
            '"' => {
                text.push(current);
                self.advance();
                let string_content = self.scan_string_content();
                text.push_str(&string_content);
                text.push('"'); // Add the closing quote to the text
                TokenKind::String
            }
            '\'' => {
                text.push(current);
                self.advance();
                let tick_content = self.scan_symbol_content();
                text.push_str(&tick_content);
                TokenKind::Symbol
            }
            ch if Self::is_digit(ch) || (ch == '-' && self.peek().is_some_and(Self::is_digit)) => {
                text.push(ch);
                self.advance();
                let num_content = self.scan_number_content();
                text.push_str(&num_content);
                TokenKind::Number
            }
            ch if Self::is_ident_start(ch) => {
                text.push(ch);
                self.advance();
                let ident_content = self.scan_identifier_content();
                text.push_str(&ident_content);

                // Check for keywords
                match text.as_str() {
                    "let" => TokenKind::LetKeyword,
                    "let*" => TokenKind::LetStarKeyword,
                    "lambda" => TokenKind::LambdaKeyword,
                    "module" => TokenKind::ModuleKeyword,
                    "interface" => TokenKind::InterfaceKeyword,
                    "use" => TokenKind::ImportKeyword,
                    "bless" => TokenKind::BlessKeyword,
                    "implements" => TokenKind::ImplementsKeyword,
                    "step" => TokenKind::StepKeyword,
                    "step-with-rollback" => TokenKind::StepWithRollbackKeyword,
                    "defun" => TokenKind::DefunKeyword,
                    "defconst" => TokenKind::DefconstKeyword,
                    "defcap" => TokenKind::DefcapKeyword,
                    "defpact" => TokenKind::DefpactKeyword,
                    "defschema" => TokenKind::DefschemaKeyword,
                    "deftable" => TokenKind::DeftableKeyword,
                    "true" | "false" => TokenKind::Bool,
                    "lam" => TokenKind::LamKeyword,
                    "defproperty" => TokenKind::DefpropertyKeyword,
                    // Reserved in the parser, but not in the lexer
                    // "with-capability" => TokenKind::WithCapabilityKeyword,
                    // "with-read" => TokenKind::WithReadKeyword,
                    // "with-default-read" => TokenKind::WithDefaultReadKeyword,
                    // "enforce" => TokenKind::EnforceKeyword,
                    // "if" => TokenKind::IfKeyword,
                    // "cond" => TokenKind::CondKeyword,
                    // "do" => TokenKind::DoKeyword,
                    _ => TokenKind::Ident,
                }
            }
            _ => {
                // Unknown character - create an error token but don't crash
                text.push(current);
                self.advance();
                TokenKind::Error
            }
        };

        let range = self.make_range(start_pos);
        Some((token_kind, text, range, leading))
    }

    /// Scan annotation text after the @ symbol
    fn scan_annotation_text(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.current {
            if Self::is_ident_part(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
    }

    /// Scan a string literal. Strings can be multi-line using a backslash as a
    /// line continuation. NOTE: We currently remove the continuation, in keeping
    /// with the Pact lexer, but we may need to preserve it if it is important
    /// for formatting.
    fn scan_string_content(&mut self) -> String {
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

        result
    }

    /// Scan single tick content
    fn scan_symbol_content(&mut self) -> String {
        let mut result = String::new();

        // First character should be alphabetic
        if let Some(ch) = self.current {
            if ch.is_alphabetic() {
                result.push(ch);
                self.advance();
            } else {
                return result;
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

        result
    }

    /// Scan number content
    fn scan_number_content(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.current {
            if Self::is_digit(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
    }

    /// Scan identifier content
    fn scan_identifier_content(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.current {
            if Self::is_ident_part(ch) {
                result.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        result
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

    /// Create a source range from a start position to the current position
    fn make_range(&self, start: SourcePos) -> SourceRange {
        SourceRange {
            start,
            end: self.pos.clone(),
        }
    }

    /// Tokenize the entire input and produce SourceToken objects
    pub fn tokenize(&mut self) -> Vec<SourceToken> {
        let mut tokens = Vec::new();
        let mut pending_trivia: Vec<Trivia> = Vec::new();

        while let Some((kind, text, range, leading)) = self.scan_token() {
            let is_eof = matches!(kind, TokenKind::Eof);

            // Collect trivia after this token
            let next_trivia = self.collect_trivia();

            // Split trivia appropriately:
            // - Comments go in trailing trivia of the current token
            // - Newlines and whitespace after the first newline go in leading trivia of the next token
            let mut trailing: Vec<Trivia> = Vec::new();
            let mut next_leading: Vec<Trivia> = Vec::new();
            let mut saw_newline = false;

            for t in next_trivia {
                if !saw_newline {
                    match t {
                        // Only comments should go in trailing trivia when not at EOF
                        Trivia::Comment(_) => {
                            trailing.push(t);
                        }
                        // Newlines always go to leading trivia of next token
                        Trivia::Line(_) => {
                            saw_newline = true;
                            next_leading.push(t);
                        }
                        // Spaces before newline go to trailing
                        Trivia::Space(_) => {
                            trailing.push(t);
                        }
                    }
                } else {
                    // After newline, everything goes to leading trivia of next token
                    next_leading.push(t);
                }
            }

            // Add any pending trivia to this token's leading trivia
            let mut final_leading = leading;
            final_leading.splice(0..0, pending_trivia);

            tokens.push(SourceToken {
                kind,
                text,
                range,
                leading: final_leading,
                trailing,
            });

            pending_trivia = next_leading;

            if is_eof {
                break;
            }
        }

        tokens
    }
}

/// Tokenize a string of Pact code
pub fn tokenize(source: &str) -> Vec<SourceToken> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "(defun add (x y) (+ x y)) ; comment";
        let tokens = tokenize(source);

        // Check token types
        assert_eq!(tokens[0].kind, TokenKind::OpenParen);
        assert_eq!(tokens[1].kind, TokenKind::DefunKeyword);
        assert_eq!(tokens[2].kind, TokenKind::Ident);
        assert_eq!(tokens[3].kind, TokenKind::OpenParen);
        assert_eq!(tokens[4].kind, TokenKind::Ident);
        assert_eq!(tokens[5].kind, TokenKind::Ident);
        assert_eq!(tokens[6].kind, TokenKind::CloseParen);
        assert_eq!(tokens[7].kind, TokenKind::OpenParen);
        assert_eq!(tokens[8].kind, TokenKind::Ident);
        assert_eq!(tokens[9].kind, TokenKind::Ident);
        assert_eq!(tokens[10].kind, TokenKind::Ident);
        assert_eq!(tokens[11].kind, TokenKind::CloseParen);
        assert_eq!(tokens[12].kind, TokenKind::CloseParen);
        assert_eq!(
            tokens[12].trailing,
            vec![Trivia::Space(1), Trivia::Comment("; comment".to_string())]
        );
        assert_eq!(tokens[13].kind, TokenKind::Eof);

        // Check text
        assert_eq!(tokens[1].text, "defun");
        assert_eq!(tokens[2].text, "add");
        assert_eq!(tokens[4].text, "x");
        assert_eq!(tokens[5].text, "y");
        assert_eq!(tokens[8].text, "+");
    }

    #[test]
    fn test_keywords() {
        let source =
            "let let* lambda lam defproperty module interface use bless implements step step-with-rollback";
        let tokens = tokenize(source);

        assert_eq!(tokens[0].kind, TokenKind::LetKeyword);
        assert_eq!(tokens[1].kind, TokenKind::LetStarKeyword);
        assert_eq!(tokens[2].kind, TokenKind::LambdaKeyword);
        assert_eq!(tokens[3].kind, TokenKind::LamKeyword);
        assert_eq!(tokens[4].kind, TokenKind::DefpropertyKeyword);
        assert_eq!(tokens[5].kind, TokenKind::ModuleKeyword);
        assert_eq!(tokens[6].kind, TokenKind::InterfaceKeyword);
        assert_eq!(tokens[7].kind, TokenKind::ImportKeyword);
        assert_eq!(tokens[8].kind, TokenKind::BlessKeyword);
        assert_eq!(tokens[9].kind, TokenKind::ImplementsKeyword);
        assert_eq!(tokens[10].kind, TokenKind::StepKeyword);
        assert_eq!(tokens[11].kind, TokenKind::StepWithRollbackKeyword);
    }

    #[test]
    fn test_error_recovery() {
        // Using characters not in our is_ident_start allowed set
        let source = "¢ let ♥ x";
        let tokens = tokenize(source);

        // We should get 4 tokens plus EOF
        assert_eq!(tokens.len(), 5);

        // First token should be an error
        assert_eq!(tokens[0].kind, TokenKind::Error);
        assert_eq!(tokens[0].text, "¢");

        // Second token should be a keyword
        assert_eq!(tokens[1].kind, TokenKind::LetKeyword);

        // Third token should be an error
        assert_eq!(tokens[2].kind, TokenKind::Error);
        assert_eq!(tokens[2].text, "♥");

        // Fourth token should be an identifier
        assert_eq!(tokens[3].kind, TokenKind::Ident);
        assert_eq!(tokens[3].text, "x");
    }

    #[test]
    fn test_resilient_lexing() {
        // Use characters that are definitely invalid in Pact
        let source = "(defun £ name € 42)";
        let tokens = tokenize(source);

        // Check that we got all tokens including errors and EOF
        assert_eq!(tokens.len(), 8); // 7 tokens + EOF

        // Verify token sequence with errors
        assert_eq!(tokens[0].kind, TokenKind::OpenParen);
        assert_eq!(tokens[1].kind, TokenKind::DefunKeyword);
        assert_eq!(tokens[2].kind, TokenKind::Error); // £ is invalid
        assert_eq!(tokens[3].kind, TokenKind::Ident); // name
        assert_eq!(tokens[4].kind, TokenKind::Error); // € is invalid
        assert_eq!(tokens[5].kind, TokenKind::Number); // 42
        assert_eq!(tokens[6].kind, TokenKind::CloseParen);

        // Confirm we captured the actual invalid characters
        assert_eq!(tokens[2].text, "£");
        assert_eq!(tokens[4].text, "€");
    }
}
