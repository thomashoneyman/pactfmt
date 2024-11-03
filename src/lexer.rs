use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[regex("[a-z]+")]
    Identifier,

    #[regex(r"\s+")]
    Whitespace,
}

#[cfg(test)]
mod tests {
    use super::Token;
    use logos::Logos;

    #[test]
    fn lex_abc() {
        let mut lexer = Token::lexer("( abc\n )");
        assert_eq!(lexer.next(), Some(Ok(Token::LeftParen)));
        assert_eq!(lexer.span(), 0..1);
        assert_eq!(lexer.next(), Some(Ok(Token::Whitespace)));
        assert_eq!(lexer.span(), 1..2);
        assert_eq!(lexer.next(), Some(Ok(Token::Identifier)));
        assert_eq!(lexer.span(), 2..5);
        assert_eq!(lexer.next(), Some(Ok(Token::Whitespace)));
        assert_eq!(lexer.span(), 5..7);
        assert_eq!(lexer.next(), Some(Ok(Token::RightParen)));
        assert_eq!(lexer.span(), 7..8);
    }
}
