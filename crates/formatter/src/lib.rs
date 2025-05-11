mod format_doc;
mod format_tree;
mod lower_tree;

use pretty::RcAllocator;
use syntax::parser::parse;
use syntax::{report_errors, tokenize};

use crate::{format_doc::FormatDoc, format_tree::FST};

/// Format the input source code, returning an error if the parsed input
/// contains any error trees.
pub fn format_source(input: &str, width: usize) -> Result<String, String> {
    let tokens = tokenize(input);
    let (parsed_trees, parsed_errors) = parse(tokens);
    let significant_errors = report_errors(&parsed_trees, &parsed_errors);
    if parsed_trees.iter().any(|tree| tree.has_errors()) {
        return Err("Parse errors".to_string());
    }
    let fst_trees: Vec<FST> = parsed_trees
        .into_iter()
        .map(lower_tree::lower_tree)
        .collect();
    let allocator = RcAllocator;
    let formatted = fst_trees
        .iter()
        .fold(FormatDoc::nil(&allocator), |acc, fst| {
            acc.append(fst.format(&allocator))
        })
        .pretty(width);
    Ok(formatted)
}

/// True if the input can be formatted (no parse errors) and the formatted
/// output is the same as the input.
pub fn check_source(input: &str, width: usize) -> bool {
    let formatted = format_source(input, width);
    match formatted {
        Ok(formatted) => input == formatted,
        Err(_) => false,
    }
}
