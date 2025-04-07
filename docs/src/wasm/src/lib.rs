use formatter;
use syntax;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn lex(source: &str) -> String {
    let tokens = syntax::tokenize(source);
    let mut result = String::new();
    for token in tokens {
        result.push_str(&format!("{}\n", token.format_condensed()));
    }
    result
}

#[wasm_bindgen]
pub fn parse(source: &str) -> String {
    let tokens = syntax::tokenize(source);
    let ast = syntax::parse(tokens);
    let mut result = String::new();
    for tree in ast {
        result.push_str(&format!("{}", tree.format_condensed()));
    }
    result
}

#[wasm_bindgen]
pub fn format(source: &str) -> String {
    let src = formatter::format_source(source, 80);
    match src {
        Err(err) => err,
        Ok(formatted) => formatted,
    }
}
