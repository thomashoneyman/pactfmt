use std::env;
use std::error;
use std::fs;
use std::path::Path;
use std::process;

type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: pactfmt <check|format> <glob-patterns>");
        process::exit(1);
    }

    let command = &args[1];
    let patterns = &args[2..];

    match command.as_str() {
        "check" => check(patterns)?,
        "format" => format(patterns)?,
        _ => {
            eprintln!("Unknown command: {}", command);
            process::exit(1);
        }
    }

    Ok(())
}

fn check(patterns: &[String]) -> Result<()> {
    let mut all_formatted = true;
    for pattern in patterns {
        for entry in glob::glob(pattern)? {
            match entry {
                Ok(path) => {
                    if !check_file(&path)? {
                        println!("Would reformat {}", path.display());
                        all_formatted = false;
                    }
                }
                Err(e) => eprintln!("Error: {}", e),
            }
        }
    }
    if all_formatted {
        println!("All files formatted.");
    } else {
        process::exit(1);
    }
    Ok(())
}

fn check_file(path: &Path) -> Result<bool> {
    let content = fs::read_to_string(path)?;
    let formatted = format_content(&content);
    Ok(content == formatted)
}

fn format(patterns: &[String]) -> Result<()> {
    for pattern in patterns {
        for entry in glob::glob(pattern)? {
            match entry {
                Ok(path) => {
                    if let Err(e) = format_file(&path) {
                        eprintln!("Error processing {}: {}", path.display(), e);
                    }
                }
                Err(e) => eprintln!("Error: {}", e),
            }
        }
    }
    Ok(())
}

fn format_file(path: &Path) -> Result<()> {
    let content = fs::read_to_string(path)?;
    let formatted = format_content(&content);
    if content != formatted {
        fs::write(path, formatted)?;
        println!("Reformatted {}", path.display());
    }
    Ok(())
}

fn format_content(content: &str) -> String {
    // TODO: Unimplemented
    content.to_string()
}
