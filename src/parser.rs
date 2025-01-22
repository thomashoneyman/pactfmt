use winnow::combinator::{alt, opt, peek, repeat};
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
            Token::Whitespace => Some(Spacing::Whitespace),
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
            any.verify(|tok| {
                !matches!(
                    tok,
                    Token::Whitespace | Token::Newlines(_) | Token::Comment(_)
                )
            }),
        ))
        .map(|opt| opt.map(|(_, colon, ty)| (colon, ty))),
    )
        .map(|(id, type_ann)| IdentifierFields {
            identifier: id,
            type_annotation: type_ann,
        });

    positioned(ident_with_type).parse_next(input)
}

fn literal(input: &mut Input) -> PResult<Literal> {
    positioned(any.verify_map(|tok| match tok {
        Token::String(s) => Some(LiteralValue::String(s)),
        Token::Symbol(s) => Some(LiteralValue::Symbol(s)),
        Token::Integer(s) => Some(LiteralValue::Integer(s)),
        Token::Decimal(s) => Some(LiteralValue::Decimal(s)),
        Token::Boolean(true) => Some(LiteralValue::Boolean("true".into())),
        Token::Boolean(false) => Some(LiteralValue::Boolean("false".into())),
        _ => None,
    }))
    .parse_next(input)
}

/// Parse an arguments list
fn arguments(input: &mut Input) -> PResult<Arguments> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let args: Vec<Identifier> = repeat(0.., identifier).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    // We remove whitespace preemptively because we have strict rules around
    // printing lists of arguments.
    let left_paren = left_paren
        .into_iter()
        .filter(|s| !matches!(s, Spacing::Whitespace))
        .collect();
    let args = args
        .into_iter()
        .map(|(spacing, fields)| {
            (
                spacing
                    .into_iter()
                    .filter(|s| !matches!(s, Spacing::Whitespace))
                    .collect(),
                fields,
            )
        })
        .collect();
    let right_paren = right_paren
        .into_iter()
        .filter(|s| !matches!(s, Spacing::Whitespace))
        .collect();

    Ok(Arguments {
        left_paren,
        args,
        right_paren,
    })
}

// In parser.rs
fn app(input: &mut Input) -> PResult<App> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let func = Box::new(expr.parse_next(input)?);
    let args: Vec<Expr> = repeat(0.., expr).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    // We remove whitespace preemptively because we have strict rules around
    // printing lists of arguments.
    let left_paren = left_paren
        .into_iter()
        .filter(|s| !matches!(s, Spacing::Whitespace))
        .collect();
    let args = args
        .into_iter()
        .map(|expr| match expr {
            Expr::Identifier((spacing, fields)) => Expr::Identifier((
                spacing
                    .into_iter()
                    .filter(|s| !matches!(s, Spacing::Whitespace))
                    .collect(),
                fields,
            )),
            Expr::Literal((spacing, value)) => Expr::Literal((
                spacing
                    .into_iter()
                    .filter(|s| !matches!(s, Spacing::Whitespace))
                    .collect(),
                value,
            )),
            Expr::Application(app) => Expr::Application(app),
        })
        .collect();
    let right_paren = right_paren
        .into_iter()
        .filter(|s| !matches!(s, Spacing::Whitespace))
        .collect();

    Ok(App {
        left_paren,
        func,
        args,
        right_paren,
    })
}

fn expr(input: &mut Input) -> PResult<Expr> {
    alt((
        literal.map(Expr::Literal),
        identifier.map(Expr::Identifier),
        app.map(Expr::Application),
    ))
    .parse_next(input)
}

// Parse a wrapped function definition
pub fn defun(input: &mut Input) -> PResult<Defun> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (defun, _) = positioned(any.verify(|t| *t == Token::Defun)).parse_next(input)?;
    let name = identifier.parse_next(input)?;
    let arguments = arguments.parse_next(input)?;
    let body: Vec<Expr> = repeat(1.., expr).parse_next(input)?;
    let body = body
        .into_iter()
        .map(|expr| match expr {
            Expr::Identifier((spacing, fields)) => Expr::Identifier((
                spacing
                    .into_iter()
                    .filter(|s| !matches!(s, Spacing::Whitespace))
                    .collect(),
                fields,
            )),
            Expr::Literal((spacing, value)) => Expr::Literal((
                spacing
                    .into_iter()
                    .filter(|s| !matches!(s, Spacing::Whitespace))
                    .collect(),
                value,
            )),
            Expr::Application(app) => Expr::Application(app),
        })
        .collect();
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;
    let right_paren = right_paren
        .into_iter()
        .filter(|s| !matches!(s, Spacing::Whitespace))
        .collect();

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
                Spacing::NewlineOne,
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
                Spacing::NewlineOne,
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
        assert_eq!(result.unwrap().args.len(), 0);
    }

    #[test]
    fn test_single_argument() {
        let tokens = lex("(arg:integer)");
        let mut input = tokens.as_slice();
        let result = arguments.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().args.len(), 1);
    }

    #[test]
    fn test_multiple_arguments() {
        let tokens = lex("(arg1:integer arg2 arg3:decimal)");
        let mut input = tokens.as_slice();
        let result = arguments.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().args.len(), 3);
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
        assert_eq!(defun.arguments.args.len(), 2);
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
        assert_eq!(defun.arguments.args.len(), 2);
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

    #[test]
    fn test_literal_basic() {
        let tokens = lex("42");
        let mut input = tokens.as_slice();
        let result = literal.parse_next(&mut input);
        assert!(result.is_ok());
        let (spacing, value) = result.unwrap();
        assert!(spacing.is_empty());
        assert!(matches!(value, LiteralValue::Integer(s) if s == "42"));
    }

    #[test]
    fn test_literal_with_spacing() {
        let tokens = lex("  ; comment\n  42");
        let mut input = tokens.as_slice();
        let result = literal.parse_next(&mut input);
        assert!(result.is_ok());
        let (spacing, value) = result.unwrap();
        assert_eq!(
            spacing,
            vec![
                Spacing::Whitespace,
                Spacing::Comment("; comment".into()),
                Spacing::NewlineOne,
                Spacing::Whitespace,
            ]
        );
        assert!(matches!(value, LiteralValue::Integer(s) if s == "42"));
    }

    #[test]
    fn test_expr_identifier() {
        let tokens = lex("my-var");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Expr::Identifier(_)));
    }

    #[test]
    fn test_expr_literal() {
        let tokens = lex("42");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Expr::Literal(_)));
    }

    #[test]
    fn test_defun_with_literals() {
        let test_cases = [
            "(defun f () 42)",
            "(defun f () \"hello\")",
            "(defun f () true)",
            "(defun f () 3.14)",
            "(defun f () 'symbol)",
        ];

        for case in test_cases {
            let tokens = lex(case);
            let mut input = tokens.as_slice();
            let result = defun.parse_next(&mut input);
            assert!(result.is_ok(), "Failed to parse: {}", case);
            assert_eq!(
                result.unwrap().body.len(),
                1,
                "Wrong body length for: {}",
                case
            );
        }
    }

    #[test]
    fn test_defun_with_mixed_expressions() {
        let tokens = lex("(defun f (x) x 42 \"hello\" my-var)");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());
        let defun = result.unwrap();
        assert_eq!(defun.body.len(), 4);
        assert!(matches!(defun.body[0], Expr::Identifier(_)));
        assert!(matches!(defun.body[1], Expr::Literal(_)));
        assert!(matches!(defun.body[2], Expr::Literal(_)));
        assert!(matches!(defun.body[3], Expr::Identifier(_)));
    }

    #[test]
    fn test_application() {
        let tokens = lex("(f x)");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        match result.unwrap() {
            Expr::Application(App { func, args, .. }) => {
                assert!(matches!(*func, Expr::Identifier(_)));
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Identifier(_)));
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_multiple_application() {
        let tokens = lex("(f x y z)");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        match result.unwrap() {
            Expr::Application(App { func, args, .. }) => {
                assert!(matches!(*func, Expr::Identifier(_)));
                assert_eq!(args.len(), 3);
                for arg in args {
                    assert!(matches!(arg, Expr::Identifier(_)));
                }
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_nested_application() {
        let tokens = lex("(f (g x) y)");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        match result.unwrap() {
            Expr::Application(App { func, args, .. }) => {
                assert!(matches!(*func, Expr::Identifier(_)));
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Application { .. }));
                assert!(matches!(args[1], Expr::Identifier(_)));
            }
            _ => panic!("Expected application"),
        }
    }
}
