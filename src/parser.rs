use winnow::combinator::{opt, peek, repeat};
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::token::any;

use crate::cst::*;
use crate::lexer::Token;

pub fn parse(input: &mut Input) -> PResult<Defun> {
    defun.parse_next(input)
}

type Input<'a> = &'a [Token];

/// Consumes consecutive whitespace and comment tokens into vectors,
/// returning an empty vector if no whitespace or comments are found.
pub fn whitespace_or_comment(input: &mut Input) -> PResult<Vec<Spacing>> {
    repeat(
        0..,
        any.verify_map(|tok| match tok {
            Token::Newlines(s) => {
                if s.lines().count() > 1 {
                    Some(Spacing::NewlineMany)
                } else {
                    Some(Spacing::NewlineOne)
                }
            }
            Token::Comment(s) => Some(Spacing::Comment(s)),
            _ => None,
        }),
    )
    .parse_next(input)
}

/// Modifies a parser so it collects leading whitespace/comments
pub fn positioned<'a, P, O>(parser: P) -> impl Parser<Input<'a>, Positioned<O>, ContextError>
where
    P: Parser<Input<'a>, O, ContextError>,
{
    (whitespace_or_comment, parser)
}

/// Modifies a parser to handle content wrapped in paired tokens (like parentheses),
/// collecting leading/trailing whitespace/comments for both wrapper tokens
pub fn wrapped<'a, P, O>(
    left: Token,
    parser: P,
    right: Token,
) -> impl Parser<Input<'a>, Wrapped<O>, ContextError>
where
    P: Parser<Input<'a>, O, ContextError>,
{
    // Not so sure about the `move` here
    (
        positioned(any.verify(move |t| *t == left)),
        parser,
        positioned(any.verify(move |t| *t == right)),
    )
        .map(|(l, m, r)| Wrapped {
            left: l,
            middle: m,
            right: r,
        })
}

/// Parse an identifier with an optional type annotation. We're pretty lenient
/// on type annotations; they can be anything, so long as there is no whitespace
/// around the colon which signifies an annotation.
pub fn identifier(input: &mut Input) -> PResult<Identifier> {
    let ident_with_type = (
        // First get the identifier
        any.verify_map(|tok| match tok {
            Token::Ident(s) => Some(Token::Ident(s)),
            _ => None,
        }),
        // Then handle optional type annotation
        opt((
            // Only succeed if next token is colon (no whitespace)
            peek(any.verify(|tok| matches!(tok, Token::Colon))),
            any.verify_map(|tok| match tok {
                Token::Colon => Some(Token::Colon),
                _ => None,
            }),
            any.verify(|tok| !matches!(tok, Token::Newlines(_) | Token::Comment(_))),
        ))
        .map(|opt| opt.map(|(_, colon, ty)| (colon, ty))),
    )
        .map(|(id, type_ann)| IdentifierFields {
            identifier: id,
            type_annotation: type_ann,
        });

    positioned(ident_with_type).parse_next(input)
}

/// Parse an arguments list
fn arguments(input: &mut Input) -> PResult<Wrapped<Vec<Identifier>>> {
    wrapped(Token::LeftParen, repeat(0.., identifier), Token::RightParen).parse_next(input)
}

// Parse an expression
fn expr(input: &mut Input) -> PResult<Expr> {
    identifier.map(Expr::Identifier).parse_next(input)
}

// Parse a wrapped function definition
pub fn defun(input: &mut Input) -> PResult<Defun> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (defun, _) = positioned(any.verify(|t| *t == Token::Defun)).parse_next(input)?;
    let name = identifier.parse_next(input)?;
    let arguments = arguments.parse_next(input)?;
    let body = repeat(1.., expr).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    Ok(Defun {
        left_paren,
        defun,
        name,
        arguments,
        body,
        right_paren,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cst::Spacing;
    use crate::lexer::Token;
    use logos::Logos;

    fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input).filter_map(|token| token.ok()).collect()
    }

    #[test]
    fn test_whitespace_or_comment() {
        let tokens = lex(" ; hello\n");
        let mut input = tokens.as_slice();
        let result = whitespace_or_comment.parse_next(&mut input);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            vec![
                Spacing::Whitespace,
                Spacing::Comment("; hello".into()),
                Spacing::Whitespace,
            ]
        );
    }

    #[test]
    fn test_identifier() {
        let tokens = lex(" ; hello\nfoo ");
        let mut input = tokens.as_slice();
        let result = identifier.parse_next(&mut input);

        assert!(result.is_ok());
        let (leading, fields) = result.unwrap();
        assert_eq!(
            leading,
            vec![
                Spacing::Whitespace,
                Spacing::Comment("; hello".into()),
                Spacing::Whitespace,
            ]
        );
        assert!(matches!(fields.identifier, Token::Ident(s) if s == "foo"));
        assert_eq!(fields.type_annotation, None);
    }

    #[test]
    fn test_typed_identifier() {
        let valid = [
            "myvar:integer\n",
            "myvar:{schema}\n",
            "myvar:object{schema}\n",
            "myvar:[*]\n",
            "myvar:[integer]\n",
        ];

        for case in valid {
            let tokens = lex(case);
            let mut input = tokens.as_slice();
            let result = identifier.parse_next(&mut input);
            assert!(result.is_ok(), "Failed to parse valid case: {}", case);
        }
    }

    #[test]
    fn test_empty_arguments() {
        let tokens = lex("()");
        let mut input = tokens.as_slice();
        let result = arguments.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().middle.len(), 0);
    }

    #[test]
    fn test_single_argument() {
        let tokens = lex("(arg:integer)");
        let mut input = tokens.as_slice();
        let result = arguments.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().middle.len(), 1);
    }

    #[test]
    fn test_multiple_arguments() {
        let tokens = lex("(arg1:integer arg2 arg3:decimal)");
        let mut input = tokens.as_slice();
        let result = arguments.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().middle.len(), 3);
    }

    #[test]
    fn test_minimal_defun() {
        let tokens = lex("(defun f () x)");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());

        let defun = result.unwrap();
        assert_eq!(defun.body.len(), 1);
    }

    #[test]
    fn test_typed_defun() {
        let tokens = lex("(defun add:integer (x:integer y:integer) x)");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());

        let defun = result.unwrap();
        assert_eq!(defun.arguments.middle.len(), 2);
    }

    #[test]
    fn test_defun_with_whitespace() {
        let tokens = lex("
        (defun add:integer
            (x:integer y:integer)
            x)
    ");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());
        let defun = result.unwrap();
        assert_eq!(defun.arguments.middle.len(), 2);
    }

    #[test]
    fn test_defun_with_comments() {
        let tokens = lex("
        (defun add:integer ; adds two numbers
            (x:integer y:integer) ; the parameters
            x) ; returns first param
    ");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());
    }
}
