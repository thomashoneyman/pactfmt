mod format_doc;
mod format_tree;
mod lower_tree;

use pretty::RcAllocator;
use syntax::parser::parse;
use syntax::tokenize;
use syntax::types::{Tree, TreeKind};

use crate::format_doc::FormatDoc;
use crate::format_tree::FST;

/// Format the input source code, returning an error if the parsed input
/// contains any error trees.
pub fn format_source(input: &str, width: usize) -> Result<String, String> {
    let tokens = tokenize(input);
    let (parsed_trees, errors) = parse(tokens);

    dbg!(&errors);

    let mut index = 0;
    for tree in &parsed_trees {
        associate_errors(&errors, &tree, &mut index);
    }

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

// Goals:
// Given an error message, recover the position from the Tree
fn associate_errors(errors: &[String], tree: &Tree, index: &mut usize) {
    if tree.kind == TreeKind::ErrorTree {
        dbg!(&errors[*index], tree);
        *index += 1;
    }
    for subtree in &tree.children {
        match subtree {
            syntax::types::Child::Token(_) => (),
            syntax::types::Child::Tree(tree) => {
                associate_errors(errors, tree, index);
            }
        }
    }
}

// #[test]
// fn test_error() {
//     let _ = format_source("(module my-mod)", 80);
// }

#[test]
fn test_error_recovered() {
    let _ = format_source(
        "(module mod 123 (defcap GOV () true)) (module mod GOV (defcap GOV () true) (defun abc () false))",
        80,
    );
}
