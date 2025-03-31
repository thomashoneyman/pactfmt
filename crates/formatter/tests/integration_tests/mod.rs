use formatter::format_source;
use pretty_assertions::assert_eq;
use test_utils::{find_pact_files, flatten_pact_file, flatten_pact_string, run_pact_command};

#[test]
fn test_pact_parser_agreement() {
    let test_files = find_pact_files("tests/integration_tests/fixtures");
    let mut failures = Vec::new();

    for file in test_files {
        let content = flatten_pact_file(&file);
        let formatted = match format_source(&content, 80) {
            Ok(formatted) => flatten_pact_string(&formatted),
            Err(err) => panic!("Failed to format content: {}", err),
        };

        let original_output = match run_pact_command(&content, Some("parser")) {
            Ok(output) => output,
            Err(err) => panic!("Failed to run Pact parser on original content: {}", err),
        };

        let formatted_output = match run_pact_command(&formatted, Some("parser")) {
            Ok(output) => output,
            Err(err) => panic!("Failed to run Pact parser on formatted content: {}", err),
        };

        if original_output == formatted_output {
            println!("✅ {}", file.display());
            continue;
        } else {
            println!("❌ {}", file.display());

            let original_lines: Vec<&str> = original_output.lines().collect();
            let formatted_lines: Vec<&str> = formatted_output.lines().collect();

            let min_len = original_lines.len().min(formatted_lines.len());
            let mut diff_line = min_len;

            for i in 0..min_len {
                if original_lines[i] != formatted_lines[i] {
                    diff_line = i;
                    break;
                }
            }

            let context_start = diff_line.saturating_sub(3);

            let original_context: String = original_lines[context_start..].join("\n");
            let formatted_context: String = formatted_lines[context_start..].join("\n");

            failures.push((file.display().to_string(), original_context, formatted_context));
        }
    }

    // After processing all files, assert on any failures
    if !failures.is_empty() {
        for (file, original, formatted) in failures {
            println!("\nFailure in file: {}", file);
            assert_eq!(original, formatted, "Parser output mismatch in {}", file);
        }
    }
}
