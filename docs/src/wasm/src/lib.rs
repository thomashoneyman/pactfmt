use formatter::format_source;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn format(source: &str) -> String {
    let src = format_source(source, 80);
    match src {
        Err(err) => err,
        Ok(formatted) => formatted,
    }
}
