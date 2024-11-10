mod cst;
mod lexer;
mod parser;

use clap::{Parser, Subcommand};

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
        input: String,
    },
    /// Format an input
    Format {
        /// The input source
        input: String,
    },
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Check { input }) => {
            println!("Check! {}", input);
        }
        Some(Commands::Format { input }) => {
            println!("{}", input)
        }
        _ => {
            println!("Unrecognized!")
        }
    }
}
