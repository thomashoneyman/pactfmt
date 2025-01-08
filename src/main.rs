mod cst;
mod lexer;
mod parser;
mod pretty;

use clap::{Parser, Subcommand};
use lexer::Token;
use logos::Logos;
use parser::parse;
use std::io::{self, Read};

#[derive(Debug, Parser)]
#[command(version)]
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

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Check { input }) => {
            let content = match input {
                Some(path) => std::fs::read_to_string(path)?,
                None => {
                    let mut buffer = String::new();
                    std::io::stdin().read_to_string(&mut buffer)?;
                    buffer
                }
            };
            println!("Checked!\n{}", content);
            Ok(())
        }
        Some(Commands::Format { input }) => {
            let content = match input {
                Some(path) => std::fs::read_to_string(path)?,
                None => {
                    let mut buffer = String::new();
                    std::io::stdin().read_to_string(&mut buffer)?;
                    buffer
                }
            };
            let lexed: Vec<_> = Token::lexer(&content)
                .filter_map(|token| token.ok())
                .collect();
            println!("Parsed!\n{:?}", parse(&lexed));
            println!("Formatted!\n{}", content);
            Ok(())
        }
        _ => {
            println!("Unrecognized!");
            Ok(())
        }
    }
}
