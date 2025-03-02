use std::io::Write;
use std::process::{Command, Stdio};
use std::path::PathBuf;

use syntax::types::{Token, SourceRange};
use syntax::lexer::api::tokenize;

#[test]
fn test_flattened_snapshots() {
    let mut debug_messages = Vec::new();

    // The (env-set-debug-flag "lexer") command is only available in Pact 5.x
    let version = check_pact_version().unwrap_or_else(|| "unknown".to_string());
    if !version.contains("pact version 5") {
        panic!("Pact version {} must be 5.x for tests.", version);
    }

    let snapshots_dir = PathBuf::from("tests/snapshots");
    if !snapshots_dir.exists() {
        panic!("Snapshots directory not found: {}", snapshots_dir.display());
    }

    let entries = std::fs::read_dir(&snapshots_dir).expect("Failed to read snapshots directory");

    let mut pact_files = Vec::new();
    for entry in entries {
        let path = entry.expect("Failed to read directory entry").path();
        if path.is_file() {
            if path.extension().map_or(false, |ext| ext == "pact") {
                pact_files.push(path);
            }
        }
    }

    if pact_files.is_empty() {
        panic!("No .pact files found in snapshots directory");
    }

    debug_messages.push(format!("Found {} .pact files to test", pact_files.len()));

    let mut total_files = 0;
    let mut successful_files = 0;

    for file in pact_files {
        debug_messages.push(format!("\nTesting file: {}", file.display()));
        total_files += 1;

        // Read file content
        let content = std::fs::read_to_string(&file).expect("Failed to read file content");

        // Check if file contains comments or annotations
        if content.contains(';') {
            panic!("File {} contains comments (';'). Comments are not supported in lexer integration tests.", file.display());
        }

        // Flatten the file by replacing all whitespace sequences with a single space
        let flattened_content = content
            .replace('\r', "") // Remove carriage returns
            .replace('\n', " ") // Replace newlines with spaces
            .split_whitespace() // Split on whitespace
            .collect::<Vec<_>>() // Collect into vector
            .join(" "); // Join with single spaces

        debug_messages.push(format!("Flattened content:\n{}", flattened_content));

        // Get our lexer's tokens
        let mut our_tokens = tokenize(&flattened_content);

        // Remove the trailing EOF token from our lexer, which Pact doesn't include
        if matches!(our_tokens.last().map(|(t, _)| t), Some(Token::Eof)) {
            our_tokens.pop();
        };

        let our_formatted = format_tokens_for_comparison(&our_tokens);

        // Run Pact REPL with the flattened content
        let mut pact_cmd = Command::new("pact")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to start Pact REPL");

        let mut stdin = pact_cmd.stdin.take().expect("Failed to open stdin");

        // Set debug flag and run the flattened code
        writeln!(stdin, "(env-set-debug-flag \"lexer\")").expect("Failed to write to Pact stdin");
        writeln!(stdin, "{}", flattened_content).expect("Failed to write to Pact stdin");
        drop(stdin); // Close stdin

        // Collect output
        let output = pact_cmd.wait_with_output().expect("Failed to wait for Pact REPL");
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();

        // Print full REPL output for debugging
        debug_messages.push(format!("\n=== PACT REPL STDOUT ==="));
        debug_messages.push(format!("{}", stdout));

        // Extract Pact tokens - we want the first lexer output, not the (exit) one
        let first_lexer_tokens = extract_first_lexer_output(&stdout);
        let pact_tokens = match first_lexer_tokens {
            Some(tokens) => {
                debug_messages.push(format!("Found Pact tokens: {:?}", tokens));
                tokens
            },
            None => {
                panic!("❌ FAILURE - Could not extract tokens from Pact output");
            }
        };

        // Compare tokens
        if our_formatted == pact_tokens {
            println!("✅ SUCCESS - {}", file.display());
            successful_files += 1;
        } else {
            // Try comparing with comma filtering for Pact's idiosyncrasies
            let (tokens_match, missing_from_ours, missing_from_pact) = lenient_compare_tokens(&our_formatted, &pact_tokens);

            if tokens_match {
                println!("✅ SUCCESS - {}", file.display());
                successful_files += 1;
            } else {
                debug_messages.push(format!("Our tokens: {:?}", our_formatted));
                debug_messages.push(format!("Pact tokens: {:?}", pact_tokens));

                // Print diff
                debug_messages.push(format!("Missing from our tokens: {:?}", missing_from_ours));
                debug_messages.push(format!("Missing from Pact tokens: {:?}", missing_from_pact));

                // Print all debug messages when tokens don't match
                println!("\n=== DEBUG MESSAGES ===");
                for message in &debug_messages {
                    println!("{}", message);
                }

                println!("\n=== TOKEN DIFF ===");
                display_token_diff(&our_formatted, &pact_tokens);

                eprintln!("❌ FAILURE - Tokens don't match");
            }
        }
    }

    // Summary
    println!("\n=== LEXER COMPARISON SUMMARY ===");
    println!("Total files tested: {}", total_files);
    println!("Successful matches: {}", successful_files);
    println!("Failed matches: {}", total_files - successful_files);

    // NOTE: For development, we can comment out this assertion to let the test pass
    // while we see the comparison results
    assert_eq!(
        successful_files, total_files,
        "{} out of {} files had tokenization mismatches",
        total_files - successful_files, total_files
    );
}

/// Check the installed Pact version
fn check_pact_version() -> Option<String> {
    let output = Command::new("pact")
        .arg("--version")
        .output()
        .ok()?;

    if output.status.success() {
        Some(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        None
    }
}

/// Format tokens for comparison with Pact output
fn format_tokens_for_comparison(tokens: &[(Token, SourceRange)]) -> Vec<String> {
    tokens.iter()
        .map(|(token, _)| match token {
            Token::Ident(s) => format!("ident<{}>", s),
            Token::String(s) => format!("\"{}\"", s),
            Token::Number(n) => format!("number<{}>", n),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::OpenBrace => "{".to_string(),
            Token::CloseBrace => "}".to_string(),
            Token::OpenBracket => "[".to_string(),
            Token::CloseBracket => "]".to_string(),
            Token::Colon => ":".to_string(),
            Token::BindAssign => ":=".to_string(),
            Token::Comma => ",".to_string(),
            Token::Dot => ".".to_string(),
            Token::DocAnn => "@doc".to_string(),
            Token::ModelAnn => "@model".to_string(),
            Token::EventAnn => "@event".to_string(),
            Token::ManagedAnn => "@managed".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::Module => "module".to_string(),
            Token::DefCap => "defcap".to_string(),
            Token::Defun => "defun".to_string(),
            Token::DefSchema => "defschema".to_string(),
            Token::DefTable => "deftable".to_string(),
            Token::DefConst => "defconst".to_string(),
            Token::Lambda => "lambda".to_string(),
            Token::Let => "let".to_string(),
            Token::LetStar => "let*".to_string(),
            Token::SingleTick(s) => format!("'{}", s),
            Token::Eof => "eof".to_string(),
            // Handle other token types with their debug output
            _ => format!("{:?}", token),
        })
        .collect()
}

/// Extract the first lexer output section
fn extract_first_lexer_output(output: &str) -> Option<Vec<String>> {
    let marker = "----------- Lexer output -----------------";

    // Find all sections with lexer output
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

    // We want the first lexer output section (for the actual code, not the exit command)
    let start_idx = sections[0];

    let section_start = start_idx + marker.len();

    // Find the loaded module message which marks the end of the lexer output
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
            },
            '<' => {
                in_ident = true;
                current_token.push(c);
            },
            '>' => {
                current_token.push(c);
                if in_ident {
                    in_ident = false;
                    // Don't add the token yet, as there might be more characters
                    // like in "ident<>>"
                }
            },
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
            },
            '\n' => {
                // Ignore newlines unless in a string
                if in_string {
                    current_token.push(c);
                }
            },
            ' ' | '\t' => {
                // Ignore whitespace at the beginning of a token or between tokens
                if in_string || in_ident || !current_token.is_empty() {
                    current_token.push(c);
                }
            },
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
            },
            ':' | '=' | '.' => {
                // These characters could be part of a larger token
                // like :=, >=, etc.
                current_token.push(c);
            },
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
    tokens.iter()
        .map(|token| {
            if token.starts_with("''") {
                // Convert ''gov to 'gov
                token.replace("''", "'")
            } else {
                // Remove any trailing/leading whitespace
                token.trim().to_string()
            }
        })
        .filter(|token| !token.is_empty()) // Remove empty tokens
        .collect()
}

/// Display a more readable diff between two token streams
fn display_token_diff(our_tokens: &[String], pact_tokens: &[String]) {
    let max_len = our_tokens.len().max(pact_tokens.len());

    println!("{:<4} | {:<30} | {:<30} | {}", "IDX", "OUR TOKEN", "PACT TOKEN", "MATCH");
    println!("{:-<4}-|-{:-<30}-|-{:-<30}-|-{:-<5}", "", "", "", "");

    for i in 0..max_len {
        let our_token = our_tokens.get(i).map_or("", |s| s.as_str());
        let pact_token = pact_tokens.get(i).map_or("", |s| s.as_str());
        let matches = our_token == pact_token;

        let match_indicator = if matches { "✓" } else { "✗" };

        println!(
            "{:<4} | {:<30} | {:<30} | {}",
            i,
            truncate_str(our_token, 28),
            truncate_str(pact_token, 28),
            match_indicator
        );
    }
}

/// Truncate a string to max_len characters, adding "..." if truncated
fn truncate_str(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[0..max_len-3])
    }
}

/// Filter out comma tokens from a token stream (Pact lexer ignores commas)
fn filter_commas(tokens: &[String]) -> Vec<String> {
    tokens.iter()
        .filter(|&token| token != ",")
        .cloned()
        .collect()
}

/// Compare tokens while accounting for known differences between our lexer and Pact's
fn lenient_compare_tokens(our_tokens: &[String], pact_tokens: &[String]) -> (bool, Vec<String>, Vec<String>) {
    // Pact does not retain commas
    let our_filtered = filter_commas(our_tokens);

    // Find differences
    let mut missing_from_ours: Vec<String> = Vec::new();
    let mut missing_from_pact: Vec<String> = Vec::new();

    for token in pact_tokens {
        if !our_filtered.contains(token) {
            missing_from_ours.push(token.clone());
        }
    }

    for token in &our_filtered {
        if !pact_tokens.contains(token) {
            missing_from_pact.push(token.clone());
        }
    }

    let tokens_match = missing_from_ours.is_empty() && missing_from_pact.is_empty();

    (tokens_match, missing_from_ours, missing_from_pact)
}