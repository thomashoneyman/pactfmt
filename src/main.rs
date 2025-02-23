use clap::{Parser, Subcommand};
use formatter::{check_source, format_source};
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
            if check_source(&content) {
                println!("Syntax OK");
            } else {
                eprintln!("Syntax Error");
                std::process::exit(1);
            }
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

            let formatted = format_source(&content, 80);
            println!("{}", formatted);
            Ok(())
        }
        _ => {
            println!("Unrecognized command!");
            std::process::exit(1);
        }
    }
}
