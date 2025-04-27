use clap::{Parser, Subcommand};
use formatter::{check_source, format_source};
use std::{
    fs,
    io::{self, Read},
};

#[derive(Debug, Parser)]
#[command(arg_required_else_help(true), version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Check an input
    Check {
        /// The input source
        #[arg(default_value = None)]
        input: Option<String>,
    },
    /// Format an input
    Format {
        /// The input source
        #[arg(default_value = None)]
        input: Option<String>,
    },
}

fn file_or_stdin(input: Option<&str>) -> io::Result<String> {
    if let Some(path) = input {
        fs::read_to_string(path)
    } else {
        let mut stdin = io::stdin();
        let mut buffer = String::new();
        stdin.read_to_string(&mut buffer)?;
        Ok(buffer)
    }
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Check { input }) => {
            let content = file_or_stdin(input.as_deref())?;
            if check_source(&content, 80) {
                println!("Syntax OK");
            } else {
                eprintln!("Syntax Error");
                std::process::exit(1);
            }
            Ok(())
        }
        Some(Commands::Format { input }) => {
            let content = file_or_stdin(input.as_deref())?;
            let formatted = format_source(&content, 80);
            match formatted {
                Ok(formatted) => println!("{}", formatted),
                Err(err) => {
                    eprintln!("Error: {}", err);
                    std::process::exit(1);
                }
            }
            Ok(())
        }
        _ => unreachable!(),
    }
}
