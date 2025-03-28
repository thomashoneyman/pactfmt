mod format_doc;
mod format_tree;
mod lower_tree;

use pretty::RcAllocator;
use syntax::parser::parse;
use syntax::tokenize;

use crate::format_doc::FormatDoc;

pub fn format_source(input: &str, width: usize) -> String {
    let tokens = tokenize(input);
    let parsed = parse(tokens);
    let lowered = lower_tree::lower_file(parsed);
    let format_doc = lowered
        .iter()
        .fold(FormatDoc::nil(&RcAllocator), |acc, fst| {
            acc.append(fst.format(&RcAllocator))
        });
    format_doc.pretty(width)
}

pub fn check_source(input: &str) -> bool {
    let tokens = tokenize(input);
    let parsed = parse(tokens);
    parsed.kind == syntax::types::TreeKind::File
}
