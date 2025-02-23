pub mod cst;
pub mod format;
pub mod lexer;
pub mod parser;

use logos::Logos;
use pretty::RcAllocator;

pub use cst::*;
pub use format::*;
pub use lexer::*;
pub use parser::*;

pub fn format_source(input: &str) -> String {
    let lexed = lexer::Token::lexer(input);
    let lexed_ok: Vec<_> = lexed.filter_map(|token| token.ok()).collect();
    let parsed = parser::parse(&mut lexed_ok.as_slice()).expect("failed to parse");
    let lowered = parsed
        .into_iter()
        .map(cst::lower_toplevel)
        .collect::<Vec<_>>();
    let format_doc = lowered
        .iter()
        .fold(format::FormatDoc::nil(&RcAllocator), |acc, fst| {
            acc.append(fst.format(&RcAllocator))
        });
    format_doc.pretty(80)
}

pub fn check_source(input: &str) -> bool {
    let lexed = lexer::Token::lexer(input);
    let lexed_ok: Vec<_> = lexed.filter_map(|token| token.ok()).collect();
    parser::parse(&mut lexed_ok.as_slice()).is_ok()
}
