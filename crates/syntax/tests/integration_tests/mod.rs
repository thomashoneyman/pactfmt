use pretty_assertions::assert_eq;
use syntax::lexer::tokenize;
use syntax::types::{SourceToken, TokenKind};
use test_utils::{assert_pact_5, find_pact_files, flatten_pact_file, run_pact_command};

#[test]
fn test_pact_lexer_agreement() {
    assert_pact_5();

    let pact_files = find_pact_files("tests/integration_tests/fixtures");
    println!("Testing {} .pact files", pact_files.len());

    for file in pact_files {
        println!("Testing file: {}", file.display());

        // The Pact lexer only accepts a single line of input
        let flattened_content = flatten_pact_file(&file);
        let mut our_tokens = tokenize(&flattened_content);

        // Remove the trailing EOF token from our lexer, which Pact doesn't include
        if matches!(our_tokens.last().map(|t| t.kind), Some(TokenKind::Eof)) {
            our_tokens.pop();
        };

        // Pact uses a specific format in its lexer for tokens, and does not include
        // any commas in its output.
        let our_formatted = format_tokens_for_comparison(&our_tokens);

        let pact_output = match run_pact_command(&flattened_content, Some("lexer")) {
            Ok(output) => output,
            Err(err) => panic!("Failed to run Pact command: {}", err),
        };

        let pact_tokens = match extract_first_lexer_output(&pact_output) {
            Some(tokens) => tokens,
            None => panic!("Could not extract tokens from Pact output"),
        };

        let diff_index = find_first_difference(&our_formatted, &pact_tokens);

        match diff_index {
            None => {
                println!("✅ SUCCESS - Tokens match\n");
                continue;
            }
            Some(diff_index) => {
                let start = diff_index.saturating_sub(10);
                let our_string = our_formatted[start..].join(" ");
                let pact_string = pact_tokens[start..].join(" ");

                println!(
                    "First difference at token index {} of {} tokens",
                    diff_index,
                    our_formatted.len().max(pact_tokens.len())
                );

                // We rely on pretty_assertions to print a diff
                assert_eq!(
                    our_string,
                    pact_string,
                    "Token mismatch in file: {}",
                    file.display()
                );
            }
        }
    }
}

/// Find the index of the first difference between two token lists
/// Returns None if the token lists are identical (considering length)
fn find_first_difference(our_tokens: &[String], pact_tokens: &[String]) -> Option<usize> {
    let min_len = our_tokens.len().min(pact_tokens.len());
    for i in 0..min_len {
        if our_tokens[i] != pact_tokens[i] {
            return Some(i);
        }
    }
    if our_tokens.len() != pact_tokens.len() {
        Some(min_len)
    } else {
        None
    }
}

/// Format tokens for comparison with the Pact lexer's output
fn format_tokens_for_comparison(tokens: &[SourceToken]) -> Vec<String> {
    tokens
        .iter()
        .filter(|token| token.kind != TokenKind::Comma)
        .map(|token| match token.kind {
            TokenKind::Comma => unreachable!(),
            TokenKind::Ident => format!("ident<{}>", token.text),
            TokenKind::StringLit => format!("\"{}\"", token.text.trim_matches('"')),
            TokenKind::Number => format!("number<{}>", token.text),
            TokenKind::OpenParen => "(".to_string(),
            TokenKind::CloseParen => ")".to_string(),
            TokenKind::OpenBrace => "{".to_string(),
            TokenKind::CloseBrace => "}".to_string(),
            TokenKind::OpenBracket => "[".to_string(),
            TokenKind::CloseBracket => "]".to_string(),
            TokenKind::Colon => ":".to_string(),
            TokenKind::BindAssign => ":=".to_string(),
            TokenKind::Dot => ".".to_string(),
            TokenKind::DocAnnKeyword => "@doc".to_string(),
            TokenKind::ModelAnnKeyword => "@model".to_string(),
            TokenKind::EventAnnKeyword => "@event".to_string(),
            TokenKind::ManagedAnnKeyword => "@managed".to_string(),
            TokenKind::TrueKeyword => "true".to_string(),
            TokenKind::FalseKeyword => "false".to_string(),
            TokenKind::ModuleKeyword => "module".to_string(),
            TokenKind::DefCapKeyword => "defcap".to_string(),
            TokenKind::DefunKeyword => "defun".to_string(),
            TokenKind::DefSchemaKeyword => "defschema".to_string(),
            TokenKind::DefTableKeyword => "deftable".to_string(),
            TokenKind::DefConstKeyword => "defconst".to_string(),
            TokenKind::LambdaKeyword => "lambda".to_string(),
            TokenKind::LetKeyword => "let".to_string(),
            TokenKind::LetStarKeyword => "let*".to_string(),
            TokenKind::SingleTick => format!("'{}", token.text.trim_start_matches('\'')),
            TokenKind::Eof => "eof".to_string(),
            TokenKind::BlessKeyword => "bless".to_string(),
            TokenKind::ImplementsKeyword => "implements".to_string(),
            TokenKind::ImportKeyword => "use".to_string(),
            TokenKind::InterfaceKeyword => "interface".to_string(),
            TokenKind::DefPactKeyword => "defpact".to_string(),
            TokenKind::StepKeyword => "step".to_string(),
            TokenKind::StepWithRollbackKeyword => "step-with-rollback".to_string(),
            TokenKind::DynAcc => "::".to_string(),
            TokenKind::Error => format!("error<{}>", token.text),
        })
        .collect()
}

/// Extract the first output section from the Pact lexer debug output
fn extract_first_lexer_output(output: &str) -> Option<Vec<String>> {
    let marker = "----------- Lexer output -----------------";

    let mut sections = Vec::new();
    let mut start = 0;
    while let Some(pos) = output[start..].find(marker) {
        let abs_pos = start + pos;
        sections.push(abs_pos);
        start = abs_pos + 1;
    }

    if sections.is_empty() {
        return None;
    }

    let start_idx = sections[0];
    let section_start = start_idx + marker.len();

    let section_end = if let Some(load_idx) = output[section_start..].find("Loaded module") {
        section_start + load_idx
    } else if sections.len() > 1 {
        sections[1]
    } else {
        output.len()
    };

    let section = &output[section_start..section_end];

    // Extract the token array by finding the outer brackets
    if let Some(array_start) = section.find('[') {
        let array_section = &section[array_start..];

        // Find the matching closing bracket by counting brackets
        let mut depth = 0;
        let mut array_end = 0;

        for (i, c) in array_section.chars().enumerate() {
            match c {
                '[' => depth += 1,
                ']' => {
                    depth -= 1;
                    if depth == 0 {
                        array_end = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        if array_end > 0 {
            let array_content = &array_section[1..array_end];
            return Some(parse_pact_token_array(array_content));
        }
    }

    None
}

/// Parse the output tokens of the Pact lexer
fn parse_pact_token_array(array_content: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut in_string = false;
    let mut in_ident = false;

    // Process character by character to handle multiline format
    for c in array_content.chars() {
        match c {
            '"' => {
                current_token.push(c);
                in_string = !in_string;
                if !in_string && !current_token.is_empty() {
                    tokens.push(current_token.trim().to_string());
                    current_token = String::new();
                }
            }
            '<' => {
                in_ident = true;
                current_token.push(c);
            }
            '>' => {
                current_token.push(c);
                if in_ident {
                    in_ident = false;
                    // Don't add the token yet, as there might be more characters
                    // like in "ident<>>"
                }
            }
            ',' => {
                if in_string || in_ident {
                    current_token.push(c);
                } else {
                    // This is a separator between tokens
                    if !current_token.is_empty() {
                        tokens.push(current_token.trim().to_string());
                        current_token = String::new();
                    }
                }
            }
            '\n' => {
                // Ignore newlines unless in a string
                if in_string {
                    current_token.push(c);
                }
            }
            ' ' | '\t' => {
                // Ignore whitespace at the beginning of a token or between tokens
                if in_string || in_ident || !current_token.is_empty() {
                    current_token.push(c);
                }
            }
            // Individual tokens that should be parsed separately
            '(' | ')' | '[' | ']' | '{' | '}' => {
                if in_string || in_ident {
                    current_token.push(c);
                } else {
                    // If we're building another token, finish it first
                    if !current_token.is_empty() {
                        tokens.push(current_token.trim().to_string());
                        current_token = String::new();
                    }
                    // Add the special character as its own token
                    tokens.push(c.to_string());
                }
            }
            ':' | '=' | '.' => {
                // These characters could be part of a larger token
                // like :=, >=, etc.
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Add final token if we have one
    if !current_token.is_empty() {
        tokens.push(current_token.trim().to_string());
    }

    // Process tokens to properly handle whitespace
    tokens
        .iter()
        .map(|token| {
            if token.starts_with("''") {
                // Convert ''gov to 'gov
                token.replace("''", "'")
            } else {
                // Remove any trailing/leading whitespace
                token.trim().to_string()
            }
        })
        .filter(|token| !token.is_empty())
        .collect()
}
