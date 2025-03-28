use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Check the installed Pact version
pub fn check_pact_version() -> Option<String> {
    let output = Command::new("pact").arg("--version").output().ok()?;

    if output.status.success() {
        Some(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        None
    }
}

/// Assert that the installed Pact version is 5.x
pub fn assert_pact_5() {
    let version = check_pact_version().unwrap_or_else(|| "unknown".to_string());
    if !version.contains("pact version 5") {
        panic!("Pact version {} must be 5.x for tests.", version);
    }
}

/// Find and read all .pact files in a specified directory
pub fn find_pact_files(dir: &str) -> Vec<PathBuf> {
    let dir = PathBuf::from(dir);
    if !dir.exists() {
        panic!("Directory not found: {}", dir.display());
    }

    let entries = std::fs::read_dir(&dir).expect("Failed to read directory");

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
        panic!("No .pact files found in {}", dir.display());
    }

    pact_files
}

/// Read a .pact file and return its content as a flattened string
pub fn flatten_pact_file(file: &PathBuf) -> String {
    let content = std::fs::read_to_string(&file).expect("Failed to read file content");
    if content.contains(";") {
        panic!(
            "Comments are not allowed in integration test files due to flattening: {}",
            file.display()
        );
    }
    flatten_pact_string(&content)
}

/// Flatten a Pact string by replacing newlines with spaces and joining whitespace
pub fn flatten_pact_string(input: &str) -> String {
    input
        .replace('\n', " ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Run a Pact REPL command and return the stdout output
pub fn run_pact_command(input: &str, debug_flag: Option<&str>) -> Result<String, String> {
    let mut pact_cmd = Command::new("pact")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to start Pact REPL: {}", e))?;

    let mut stdin = pact_cmd.stdin.take().expect("Failed to open stdin");

    // Set debug flag if provided
    if let Some(flag) = debug_flag {
        writeln!(stdin, "(env-set-debug-flag \"{}\")", flag)
            .map_err(|e| format!("Failed to write to Pact stdin: {}", e))?;
    }

    // Write the input command
    writeln!(stdin, "{}", input).map_err(|e| format!("Failed to write to Pact stdin: {}", e))?;
    drop(stdin);

    let output = pact_cmd
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for Pact REPL: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Pact command failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
