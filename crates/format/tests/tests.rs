use format::{lexer, parser, cst, FormatDoc};
use logos::Logos;
use pretty::RcAllocator;

use test_each_file::test_each_file;

fn lex(input: &str) -> Vec<lexer::Token> {
    lexer::Token::lexer(input)
        .filter_map(|token| token.ok())
        .collect()
}

test_each_file! { in "crates/format/tests/fixtures" => |content: &str| {
    let tokens = lex(content);
    let mut input = tokens.as_slice();
    let parsed = parser::parse(&mut input).expect("parsing");
    let fst = parsed.into_iter().map(cst::lower_toplevel).collect::<Vec<_>>();

    let doc = fst.iter().fold(FormatDoc::nil(&RcAllocator), |acc, fst| acc.append(fst.format(&RcAllocator)));

    insta::assert_snapshot!(doc.pretty(40));
}}
