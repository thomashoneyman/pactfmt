mod cst;
mod format;
mod lexer;
mod parser;

use clap::{Parser, Subcommand};
use lexer::Token;
use logos::Logos;
use parser::parse;
use pretty::RcAllocator;
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

            let lexed = Token::lexer(&content);
            let lexed_ok: Vec<_> = lexed.filter_map(|token| token.ok()).collect();
            let parsed = parse(&mut lexed_ok.as_slice()).expect("failed to parse");
            let lowered = parsed
                .into_iter()
                .map(cst::lower_toplevel)
                .collect::<Vec<_>>();
            let format_doc = lowered
                .iter()
                .fold(format::FormatDoc::nil(&RcAllocator), |acc, fst| {
                    acc.append(fst.format(&RcAllocator))
                });
            let formatted = format_doc.pretty(80);

            println!("Formatted!\n{}", formatted);
            Ok(())
        }
        _ => {
            println!("Unrecognized!");
            Ok(())
        }
    }
}
