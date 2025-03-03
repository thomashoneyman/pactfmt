mod format_doc;
mod format_tree;
mod lower_cst;

use logos::Logos;
use pretty::RcAllocator;
use syntax::{lexer_old, parser};

use crate::format_doc::FormatDoc;
use crate::lower_cst::lower_toplevel;

pub fn format_source(input: &str, width: usize) -> String {
    let lexed = lexer_old::Token::lexer(input);
    let lexed_ok: Vec<_> = lexed.filter_map(|token| token.ok()).collect();
    let parsed = parser::parse(&mut lexed_ok.as_slice()).expect("failed to parse");
    let lowered = parsed.into_iter().map(lower_toplevel).collect::<Vec<_>>();
    let format_doc = lowered
        .iter()
        .fold(FormatDoc::nil(&RcAllocator), |acc, fst| {
            acc.append(fst.format(&RcAllocator))
        });
    format_doc.pretty(width)
}

pub fn check_source(input: &str) -> bool {
    let lexed = lexer_old::Token::lexer(input);
    let lexed_ok: Vec<_> = lexed.filter_map(|token| token.ok()).collect();
    parser::parse(&mut lexed_ok.as_slice()).is_ok()
}
