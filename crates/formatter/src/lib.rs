mod format_doc;
mod format_tree;
mod lower_tree;

use ariadne::{Label, Report, ReportKind, Source};
use line_index::{LineCol, LineIndex};
use pretty::RcAllocator;
use syntax::parser::{parse, ParseError};
use syntax::{report_errors, tokenize};

use crate::{format_doc::FormatDoc, format_tree::FST};

/// Format the input source code, returning an error if the parsed input
/// contains any error trees.
pub fn format_source(input: &str, width: usize) -> Result<String, Vec<u8>> {
    let tokens = tokenize(input);
    let (parsed_trees, parsed_errors) = parse(tokens);
    let significant_errors = report_errors(&parsed_trees, &parsed_errors);

    if !significant_errors.is_empty() {
        return Err(format_errors(input, significant_errors));
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

fn format_errors(input: &str, errors: Vec<&ParseError>) -> Vec<u8> {
    let line_index = LineIndex::new(input);
    let errors = errors.iter().filter_map(|&error| {
        let line = (error.line - 1) as u32;
        let col = (error.column - 1) as u32;
        let line_col = LineCol { line, col };
        let index: usize = line_index.offset(line_col)?.into();
        Some((&error.message, index))
    });

    let mut report =
        Report::build(ReportKind::Error, ("<stdin>", 0..input.len())).with_message("Syntax error!");

    for (message, index) in errors {
        report = report.with_label(Label::new(("<stdin>", index..index)).with_message(message))
    }

    let mut buffer = vec![];
    report
        .finish()
        .write(("<stdin>", Source::from(input)), &mut buffer)
        .unwrap();
    buffer
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
