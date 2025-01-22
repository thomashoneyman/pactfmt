use pretty::RcDoc;

use crate::cst::*;
use crate::lexer::Token;

pub trait Pretty {
    fn pretty(&self) -> RcDoc<()>;
}

fn format_spacing(spacing: &[Spacing]) -> RcDoc<()> {
    if spacing.is_empty() {
        return RcDoc::nil();
    }

    let docs: Vec<RcDoc<()>> = spacing
        .iter()
        .map(|s| match s {
            Spacing::NewlineOne => RcDoc::hardline(),
            Spacing::NewlineMany => RcDoc::hardline().append(RcDoc::hardline()),
            Spacing::Comment(text) => RcDoc::text(text),
        })
        .collect();

    RcDoc::concat(docs)
}

fn has_newline(spacing: &[Spacing]) -> bool {
    spacing
        .iter()
        .any(|s| matches!(s, Spacing::NewlineOne | Spacing::NewlineMany))
}

impl Pretty for Token {
    fn pretty(&self) -> RcDoc<()> {
        match *self {
            Token::LeftBrace => RcDoc::text("{"),
            Token::RightBrace => RcDoc::text("}"),
            Token::LeftParen => RcDoc::text("("),
            Token::RightParen => RcDoc::text(")"),
            Token::LeftBracket => RcDoc::text("["),
            Token::RightBracket => RcDoc::text("]"),
            Token::ColonEquals => RcDoc::text(":="),
            Token::Colon => RcDoc::text(":"),
            Token::DoubleColon => RcDoc::text("::"),
            Token::Comma => RcDoc::text(","),
            Token::Dot => RcDoc::text("."),
            Token::Boolean(bool) => match bool {
                true => RcDoc::text("true"),
                false => RcDoc::text("false"),
            },
            Token::LetRec => RcDoc::text("let*"),
            Token::Let => RcDoc::text("let"),
            Token::If => RcDoc::text("if"),
            Token::Defun => RcDoc::text("defun"),
            Token::DefCap => RcDoc::text("defcap"),
            Token::DefConst => RcDoc::text("defconst"),
            Token::DefSchema => RcDoc::text("defschema"),
            Token::DefTable => RcDoc::text("deftable"),
            Token::DefPact => RcDoc::text("defpact"),
            Token::Interface => RcDoc::text("interface"),
            Token::Module => RcDoc::text("module"),
            Token::Bless => RcDoc::text("bless"),
            Token::Implements => RcDoc::text("implements"),
            Token::Use => RcDoc::text("use"),
            Token::Lambda => RcDoc::text("lambda"),
            Token::And => RcDoc::text("and"),
            Token::Or => RcDoc::text("or"),
            Token::Load => RcDoc::text("load"),
            Token::DocAnn => RcDoc::text("@doc"),
            Token::ModelAnn => RcDoc::text("@model"),
            Token::EventAnn => RcDoc::text("@event"),
            Token::ManagedAnn => RcDoc::text("@managed"),
            Token::Step => RcDoc::text("step"),
            Token::StepWithRollback => RcDoc::text("step-with-rollback"),
            Token::Enforce => RcDoc::text("enforce"),
            Token::EnforceOne => RcDoc::text("enforce-one"),
            Token::WithCapability => RcDoc::text("with-capability"),
            Token::CreateUserGuard => RcDoc::text("create-user-guard"),
            Token::Try => RcDoc::text("try"),
            Token::Do => RcDoc::text("do"),
            Token::Suspend => RcDoc::text("suspend"),
            Token::Integer(ref str) => RcDoc::text(str),
            Token::Decimal(ref str) => RcDoc::text(str),
            Token::String(ref str) => RcDoc::text(str),
            Token::Symbol(ref str) => RcDoc::text(str),
            Token::Ident(ref str) => RcDoc::text(str),
            Token::Whitespace => {
                unreachable!("invariant violated: whitespace was consumed as arbitrary token")
            }
            Token::Newlines(_) => {
                unreachable!("invariant violated: newline was consumed as arbitrary token")
            }
            Token::Comment(_) => {
                unreachable!("invariant violated: comment was consumed as arbitrary token")
            }
        }
    }
}

impl<T> Pretty for Positioned<T>
where
    T: Pretty,
{
    fn pretty(&self) -> RcDoc<()> {
        let leading: RcDoc<()> = format_spacing(&self.0);
        leading.append(self.1.pretty())
    }
}

impl Pretty for Type {
    fn pretty(&self) -> RcDoc<()> {
        match &self {
            Type::Ident(s) => RcDoc::text(s),
            Type::List(ty) => RcDoc::text("[")
                .append(ty.pretty())
                .append(RcDoc::text("]")),
            Type::Object(ty) => RcDoc::text("object{")
                .append(RcDoc::text(ty.as_deref().unwrap_or("")))
                .append(RcDoc::text("}")),
            Type::Schema(ty) => RcDoc::text("{")
                .append(RcDoc::text(ty))
                .append(RcDoc::text("}")),
            Type::Module(parts) => {
                let mut doc = RcDoc::text("module{");
                let mut first = true;
                for part in parts {
                    if !first {
                        doc = doc.append(RcDoc::text(","));
                    }
                    doc = doc.append(part.pretty());
                    first = false;
                }
                doc.append(RcDoc::text("}"))
            }
        }
    }
}

impl Pretty for IdentifierFields {
    fn pretty(&self) -> RcDoc<()> {
        match self.type_annotation {
            None => RcDoc::text(&self.identifier),
            Some(ref ty) => RcDoc::text(&self.identifier)
                .append(RcDoc::text(":"))
                .append(ty.pretty()),
        }
    }
}

impl Pretty for LiteralValue {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            LiteralValue::String(str) => RcDoc::text(str),
            LiteralValue::Symbol(str) => RcDoc::text(str),
            LiteralValue::Integer(str) => RcDoc::text(str),
            LiteralValue::Decimal(str) => RcDoc::text(str),
            LiteralValue::Boolean(str) => RcDoc::text(str),
        }
    }
}

impl Pretty for App {
    fn pretty(&self) -> RcDoc<()> {
        let l_paren = format_spacing(&self.left_paren).append("(");
        let r_paren = format_spacing(&self.right_paren).append(")");

        // If any argument has a newline, we'll use multi-line format
        let multiline = self.args.iter().any(|arg| match arg {
            Expr::Identifier((spacing, _))
            | Expr::Literal((spacing, _))
            | Expr::Application(App {
                left_paren: spacing,
                ..
            }) => has_newline(spacing),
        });

        if multiline {
            // Each argument gets a newline before it, unless it already has one
            let args_doc = RcDoc::intersperse(
                self.args.iter().map(|arg| match arg {
                    Expr::Identifier((spacing, _))
                    | Expr::Literal((spacing, _))
                    | Expr::Application(App {
                        left_paren: spacing,
                        ..
                    }) if !has_newline(spacing) => RcDoc::hardline().append(arg.pretty()),
                    _ => arg.pretty(),
                }),
                RcDoc::nil(),
            )
            .nest(2);

            RcDoc::hardline()
                .append(l_paren)
                .append(self.func.pretty())
                .append(args_doc)
                .append(r_paren)
        } else {
            let args = match self.args.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc = RcDoc::space().append(first.pretty());
                    for arg in rest {
                        doc = doc.append(RcDoc::space()).append(arg.pretty());
                    }
                    doc
                }
            };

            l_paren
                .append(self.func.pretty())
                .append(args)
                .append(r_paren)
        }
    }
}

impl Pretty for Expr {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            Expr::Identifier(id) => id.pretty(),
            Expr::Literal(lit) => lit.pretty(),
            Expr::Application(app) => app.pretty(),
        }
    }
}

impl Pretty for Arguments {
    fn pretty(&self) -> RcDoc<()> {
        let l_paren = format_spacing(&self.left_paren).append("(");
        let r_paren = format_spacing(&self.right_paren).append(")");

        // Check if any argument has a newline
        let multiline = self.args.iter().any(|arg| match arg {
            (spacing, _) => has_newline(spacing),
        });

        if multiline {
            match self.args.split_first() {
                Some((first, rest)) => l_paren
                    .append(RcDoc::space())
                    .append(first.pretty())
                    .append(
                        RcDoc::concat(rest.iter().map(|arg| match arg {
                            (spacing, _) if !has_newline(spacing) => {
                                RcDoc::hardline().append(arg.pretty())
                            }
                            _ => arg.pretty(),
                        }))
                        .nest(2),
                    )
                    .append(RcDoc::hardline())
                    .append(r_paren)
                    .nest(2),
                None => l_paren.append(r_paren),
            }
        } else {
            l_paren
                .append(RcDoc::concat(self.args.iter().map(Pretty::pretty)))
                .append(r_paren)
        }
    }
}

impl Pretty for Defun {
    fn pretty(&self) -> RcDoc<()> {
        let l_paren = format_spacing(&self.left_paren).append("(");
        let defun = format_spacing(&self.defun)
            .append("defun")
            .append(RcDoc::space());
        let name = format_spacing(&self.name.0)
            .append(self.name.1.pretty())
            .append(RcDoc::space());
        let r_paren = format_spacing(&self.right_paren)
            .append(")")
            .append(RcDoc::space());

        let body = RcDoc::concat(self.body.iter().map(|expr| match expr {
            Expr::Identifier((spacing, _))
            | Expr::Literal((spacing, _))
            | Expr::Application(App {
                left_paren: spacing,
                ..
            }) if !has_newline(spacing) => RcDoc::hardline().append(expr.pretty()),
            _ => expr.pretty(),
        }));

        l_paren
            .append(defun)
            .append(name)
            .append(self.arguments.pretty())
            .append(body.nest(2))
            .append(r_paren)
    }
}

impl Pretty for Named {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            Named::Ident(s) => RcDoc::text(s),
            Named::Reference(r) => r.pretty(),
        }
    }
}

impl Pretty for Reference {
    fn pretty(&self) -> RcDoc<()> {
        let mut doc = RcDoc::text(&self.first)
            .append(RcDoc::text("."))
            .append(RcDoc::text(&self.second));

        for part in &self.rest {
            doc = doc.append(RcDoc::text(".")).append(RcDoc::text(part));
        }
        doc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::*;
    use crate::parser;
    use crate::pretty::Pretty;

    use logos::Logos;
    use test_each_file::test_each_file;

    fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input).filter_map(|token| token.ok()).collect()
    }

    test_each_file! { in "./fixtures" => |content: &str| {
        let tokens = lex(content);
        let mut input = tokens.as_slice();
        let parsed = parser::defun(&mut input).expect("parsing");

        let doc = parsed.pretty();
        let mut buffer = String::new();
        doc.render_fmt(80, &mut buffer).expect("pretty printing");

        insta::assert_snapshot!(buffer);
    }}

    #[test]
    fn test_format_spacing() {
        let spacing = vec![
            Spacing::NewlineMany,
            Spacing::Comment("; hello".to_string()),
            Spacing::NewlineOne,
        ];

        let doc = format_spacing(&spacing);
        let mut output = String::new();
        doc.render_fmt(80, &mut output).unwrap();

        assert_eq!(output, "\n\n; hello\n");
    }
}
