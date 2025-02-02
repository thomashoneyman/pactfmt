use winnow::combinator::{alt, not, opt, peek, repeat, separated};
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::token::any;

use crate::cst::*;
use crate::lexer::Token;

pub fn parse(input: &mut Input) -> PResult<Vec<Toplevel>> {
    repeat(0.., top_level).parse_next(input)
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

fn reference(input: &mut Input) -> PResult<Reference> {
    let first = any
        .verify_map(|tok| match tok {
            Token::Ident(s) => Some(s),
            _ => None,
        })
        .parse_next(input)?;

    any.verify(|tok| *tok == Token::Dot).parse_next(input)?;

    let second = any
        .verify_map(|tok| match tok {
            Token::Ident(s) => Some(s),
            _ => None,
        })
        .parse_next(input)?;

    let mut rest = Vec::new();
    while opt(any.verify(|tok| *tok == Token::Dot))
        .parse_next(input)?
        .is_some()
    {
        rest.push(
            any.verify_map(|tok| match tok {
                Token::Ident(s) => Some(s),
                _ => None,
            })
            .parse_next(input)?,
        );
    }

    Ok(Reference {
        first,
        second,
        rest,
    })
}

fn named(input: &mut Input) -> PResult<Named> {
    alt((
        // Try reference first since it's more specific
        reference.map(Named::Reference),
        // Fall back to simple identifier
        any.verify_map(|tok| match tok {
            Token::Ident(s) => Some(Named::Ident(s)),
            // Not actually identifiers, but we can treat them as such
            // for formatter convenience.
            Token::Enforce => Some(Named::Ident("enforce".into())),
            Token::EnforceOne => Some(Named::Ident("enforce-one".into())),
            Token::EnforceGuard => Some(Named::Ident("enforce-guard".into())),
            Token::KeysetRefGuard => Some(Named::Ident("keyset-ref-guard".into())),
            Token::Use => Some(Named::Ident("use".into())),
            Token::Implements => Some(Named::Ident("implements".into())),
            _ => None,
        }),
    ))
    .parse_next(input)
}

/// Parse a type annotation
fn type_annotation(input: &mut Input) -> PResult<Type> {
    alt((
        // Module type: module{mod,name}
        (
            any.verify(|tok| *tok == Token::Module),
            any.verify(|tok| *tok == Token::LeftBrace),
            separated(0.., named, any.verify(|tok| *tok == Token::Comma)),
            any.verify(|tok| *tok == Token::RightBrace),
        )
            .map(|(_, _, refs, _)| Type::Module(refs)),
        // Object type: object{schema}
        (
            any.verify(|tok| *tok == Token::Ident("object".into())),
            any.verify(|tok| *tok == Token::LeftBrace),
            opt(any.verify_map(|tok| match tok {
                Token::Ident(s) => Some(s),
                _ => None,
            })),
            any.verify(|tok| *tok == Token::RightBrace),
        )
            .map(|(_, _, ty, _)| Type::Object(ty)),
        // List type: [integer] or [object{schema}] etc.
        (
            any.verify(|tok| *tok == Token::LeftBracket),
            type_annotation,
            any.verify(|tok| *tok == Token::RightBracket),
        )
            .map(|(_, ty, _)| Type::List(Box::new(ty))),
        // Schema type: {schema}
        (
            any.verify(|tok| *tok == Token::LeftBrace),
            any.verify_map(|tok| match tok {
                Token::Ident(s) => Some(s),
                _ => None,
            }),
            any.verify(|tok| *tok == Token::RightBrace),
        )
            .map(|(_, ty, _)| Type::Schema(ty)),
        // Simple type: integer
        any.verify_map(|tok| match tok {
            Token::Ident(s) => Some(Type::Ident(s)),
            _ => None,
        }),
    ))
    .parse_next(input)
}

/// Parse an identifier with an optional type annotation
pub fn identifier(input: &mut Input) -> PResult<Identifier> {
    let ident_with_type = alt((
        // Just an identifier with no type
        (named, peek(not(any.verify(|tok| *tok == Token::Colon)))).map(|(id, _)| {
            IdentifierFields {
                identifier: id,
                type_annotation: None,
            }
        }),
        // Identifier with required type annotation
        (
            named,
            any.verify(|tok| *tok == Token::Colon),
            type_annotation,
        )
            .map(|(id, _, ty)| IdentifierFields {
                identifier: id,
                type_annotation: Some(ty),
            }),
    ));

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

fn doc_ann(input: &mut Input) -> PResult<DocAnn> {
    let (ann, _) = positioned(any.verify(|t| *t == Token::DocAnn)).parse_next(input)?;
    let docstr = positioned(any.verify_map(|tok| match tok {
        Token::String(s) => Some(s),
        _ => None,
    }))
    .parse_next(input)?;
    Ok(DocAnn { ann, docstr })
}

fn model_ann(input: &mut Input) -> PResult<ModelAnn> {
    let (ann, _) = positioned(any.verify(|t| *t == Token::ModelAnn)).parse_next(input)?;
    let exprs = list(input)?;
    Ok(ModelAnn { ann, exprs })
}

fn event_ann(input: &mut Input) -> PResult<PrefixSpacing> {
    let (ann, _) = positioned(any.verify(|t| *t == Token::EventAnn)).parse_next(input)?;
    Ok(ann)
}

fn managed_ann(input: &mut Input) -> PResult<ManagedAnn> {
    let (ann, _) = positioned(any.verify(|t| *t == Token::ManagedAnn)).parse_next(input)?;
    let args = opt((named, named)).parse_next(input)?;
    Ok(ManagedAnn { ann, args })
}

/// Parse an arguments list
fn arguments(input: &mut Input) -> PResult<Arguments> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let args: Vec<Identifier> = repeat(0.., identifier).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    Ok(Arguments {
        left_paren,
        args,
        right_paren,
    })
}

fn list(input: &mut Input) -> PResult<List> {
    let (left_bracket, _) =
        positioned(any.verify(|t| *t == Token::LeftBracket)).parse_next(input)?;
    let members: Vec<Expr> = repeat(0.., expr).parse_next(input)?;
    let (right_bracket, _) =
        positioned(any.verify(|t| *t == Token::RightBracket)).parse_next(input)?;

    Ok(List {
        left_bracket,
        members,
        right_bracket,
    })
}

fn app(input: &mut Input) -> PResult<App> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let func = positioned(named).parse_next(input)?;
    let args: Vec<Expr> = repeat(0.., expr).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    Ok(App {
        left_paren,
        func: Box::new(func),
        args,
        right_paren,
    })
}

fn expr(input: &mut Input) -> PResult<Expr> {
    alt((
        literal.map(Expr::Literal),
        identifier.map(Expr::Identifier),
        app.map(Expr::Application),
        list.map(Expr::List),
    ))
    .parse_next(input)
}

pub fn defun_body(input: &mut Input) -> PResult<DefunBody> {
    alt((
        doc_ann.map(DefunBody::DocAnn),
        model_ann.map(DefunBody::ModelAnn),
        expr.map(DefunBody::Expr),
    ))
    .parse_next(input)
}

/// Parse a function definition
pub fn defun(input: &mut Input) -> PResult<Defun> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (defun, _) = positioned(any.verify(|t| *t == Token::Defun)).parse_next(input)?;
    let name = identifier.parse_next(input)?;
    let arguments = arguments.parse_next(input)?;
    let body = repeat(1.., defun_body).parse_next(input)?;
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

pub fn defcap_body(input: &mut Input) -> PResult<DefcapBody> {
    alt((
        doc_ann.map(DefcapBody::DocAnn),
        event_ann.map(DefcapBody::EventAnn),
        managed_ann.map(DefcapBody::ManagedAnn),
        expr.map(DefcapBody::Expr),
    ))
    .parse_next(input)
}

/// Parse a function definition
pub fn defcap(input: &mut Input) -> PResult<Defcap> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (defcap, _) = positioned(any.verify(|t| *t == Token::DefCap)).parse_next(input)?;
    let name = identifier.parse_next(input)?;
    let arguments = arguments.parse_next(input)?;
    let body = repeat(1.., defcap_body).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    Ok(Defcap {
        left_paren,
        defcap,
        name,
        arguments,
        body,
        right_paren,
    })
}

pub fn defconst_body(input: &mut Input) -> PResult<DefconstBody> {
    alt((
        doc_ann.map(DefconstBody::DocAnn),
        expr.map(DefconstBody::Expr),
    ))
    .parse_next(input)
}

pub fn defconst(input: &mut Input) -> PResult<Defconst> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (defconst, _) = positioned(any.verify(|t| *t == Token::DefConst)).parse_next(input)?;
    let name = identifier.parse_next(input)?;
    let body = repeat(1.., defconst_body).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;

    Ok(Defconst {
        left_paren,
        defconst,
        name,
        body,
        right_paren,
    })
}

fn governance(input: &mut Input) -> PResult<ModuleGovernance> {
    any.verify_map(|tok| match tok {
        Token::Ident(s) => Some(ModuleGovernance::Cap(s)),
        Token::String(s) => Some(ModuleGovernance::Keyset(s)),
        Token::Symbol(s) => Some(ModuleGovernance::Keyset(s)),
        _ => None,
    })
    .parse_next(input)
}

fn module(input: &mut Input) -> PResult<Module> {
    let (left_paren, _) = positioned(any.verify(|t| *t == Token::LeftParen)).parse_next(input)?;
    let (module, _) = positioned(any.verify(|t| *t == Token::Module)).parse_next(input)?;
    let name = positioned(named).parse_next(input)?;
    let governance = positioned(governance).parse_next(input)?;
    let body = repeat(1.., top_level).parse_next(input)?;
    let (right_paren, _) = positioned(any.verify(|t| *t == Token::RightParen)).parse_next(input)?;
    Ok(Module {
        left_paren,
        module,
        name,
        governance,
        body,
        right_paren,
    })
}

fn top_level(input: &mut Input) -> PResult<Toplevel> {
    alt((
        defun.map(Toplevel::Defun),
        defcap.map(Toplevel::Defcap),
        defconst.map(Toplevel::Defconst),
        expr.map(Toplevel::Expr),
        module.map(|val| Toplevel::Module(Box::new(val))),
    ))
    .parse_next(input)
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
            vec![Spacing::Comment("; hello".into()), Spacing::NewlineOne]
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
            vec![Spacing::Comment("; hello".into()), Spacing::NewlineOne,]
        );
        assert!(matches!(fields.identifier, s if s == Named::Ident("foo".into())));
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
    fn test_empty_list() {
        let tokens = lex("[]");
        let mut input = tokens.as_slice();
        let result = list.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().members.len(), 0);
    }

    #[test]
    fn test_single_list() {
        let tokens = lex("[arg]");
        let mut input = tokens.as_slice();
        let result = list.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().members.len(), 1);
    }

    #[test]
    fn test_multiple_list() {
        let tokens = lex("[1 [2] (x 3) 4]");
        let mut input = tokens.as_slice();
        let result = list.parse_next(&mut input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().members.len(), 4);
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
            vec![Spacing::Comment("; comment".into()), Spacing::NewlineOne]
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
    fn test_type_annotations() {
        let test_cases = [
            ("wot:integer", Type::Ident("integer".into())),
            (
                "wot:[integer]",
                Type::List(Box::new(Type::Ident("integer".into()))),
            ),
            (
                "wot:[[integer]]",
                Type::List(Box::new(Type::List(Box::new(Type::Ident(
                    "integer".into(),
                ))))),
            ),
            ("wot:{schema}", Type::Schema("schema".into())),
            ("wot:object{schema}", Type::Object(Some("schema".into()))),
            ("wot:object{}", Type::Object(None)),
            (
                "wot:module{schema}",
                Type::Module(vec![Named::Ident("schema".into())]),
            ),
            (
                "wot:module{one,two}",
                Type::Module(vec![Named::Ident("one".into()), Named::Ident("two".into())]),
            ),
            (
                "wot:module{a.b,c.d.e}",
                Type::Module(vec![
                    Named::Reference(Reference {
                        first: "a".into(),
                        second: "b".into(),
                        rest: vec![],
                    }),
                    Named::Reference(Reference {
                        first: "c".into(),
                        second: "d".into(),
                        rest: vec!["e".into()],
                    }),
                ]),
            ),
            ("wot:module{}", Type::Module(vec![])),
        ];

        for (input, expected_type) in test_cases {
            let tokens = lex(input);
            let mut input = tokens.as_slice();
            let result = identifier.parse_next(&mut input);
            assert!(result.is_ok(), "Failed to parse: {:?}", input);
            let (_, fields) = result.unwrap();
            assert_eq!(fields.type_annotation.unwrap(), expected_type);
        }
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
        let tokens = lex("(defun f (x) @model [] x 42 \"hello\" my-var)");
        let mut input = tokens.as_slice();
        let result = defun.parse_next(&mut input);
        assert!(result.is_ok());
        let defun = result.unwrap();
        assert_eq!(defun.body.len(), 5);
        assert!(matches!(defun.body[0], DefunBody::ModelAnn(_)));
        assert!(matches!(
            defun.body[1],
            DefunBody::Expr(Expr::Identifier(_))
        ));
        assert!(matches!(defun.body[2], DefunBody::Expr(Expr::Literal(_))));
        assert!(matches!(defun.body[3], DefunBody::Expr(Expr::Literal(_))));
        assert!(matches!(
            defun.body[4],
            DefunBody::Expr(Expr::Identifier(_))
        ));
    }

    #[test]
    fn test_application() {
        let tokens = lex("(f x)");
        let mut input = tokens.as_slice();
        let result = expr.parse_next(&mut input);
        assert!(result.is_ok());
        match result.unwrap() {
            Expr::Application(App { args, .. }) => {
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
            Expr::Application(App { args, .. }) => {
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
            Expr::Application(App { args, .. }) => {
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Application { .. }));
                assert!(matches!(args[1], Expr::Identifier(_)));
            }
            _ => panic!("Expected application"),
        }
    }

    #[test]
    fn test_module_basic() {
        let tokens = lex("
        ; This is a Pact module
        (module basic GOV
            (defcap GOV () true)
            (defun add:integer (x:integer y:integer) (+ x y))
        )
    ");
        let mut input = tokens.as_slice();
        let result = module.parse_next(&mut input);
        assert!(result.is_ok(), "{:?}", result);
    }
}
