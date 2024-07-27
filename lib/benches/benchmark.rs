use criterion::{black_box, criterion_group, criterion_main, Criterion};
use logos::Logos;
use pactfmt_lib::Token;

pub fn criterion_benchmark(c: &mut Criterion) {
    fn lex(input: &str) -> Vec<Result<Token, ()>> {
        Token::lexer(input).collect()
    }

    c.bench_function("lex", |b| b.iter(|| lex(black_box("( )"))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
