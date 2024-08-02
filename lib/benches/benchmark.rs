use criterion::{black_box, criterion_group, criterion_main, Criterion};
use logos::Logos;
use pactfmt_lib::lexer::Token;
use pactfmt_macros::benchmark_corpus;

pub fn lexer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("lex");

    fn lex(input: &str) -> Vec<Result<Token, ()>> {
        Token::lexer(input).collect()
    }

    benchmark_corpus!(lex);
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);
