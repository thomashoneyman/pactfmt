use pretty::RcDoc;

use crate::cst::*;
use crate::lexer::{Token, WhitespaceKind};

pub trait Pretty {
    fn pretty(&self) -> RcDoc<()>;
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
            Token::Whitespace(ref kind) => match kind {
                WhitespaceKind::Newline(_) => RcDoc::hardline(),
                WhitespaceKind::Spaces(s) => RcDoc::text(s),
            },
            Token::Comment(ref s) => RcDoc::text(s),
        }
    }
}

// Don't love the duplication of Token here, but it's not clear how to share
// the implementations without adding calls to `clone()`.
impl Pretty for Spacing {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            Spacing::Whitespace(kind) => match kind {
                WhitespaceKind::Newline(_) => RcDoc::hardline(),
                WhitespaceKind::Spaces(s) => RcDoc::text(s),
            },
            Spacing::Comment(s) => RcDoc::text(s),
        }
    }
}

impl<T> Pretty for Positioned<T>
where
    T: Pretty,
{
    fn pretty(&self) -> RcDoc<()> {
        let leading: Vec<RcDoc<()>> = self.0.iter().map(Pretty::pretty).collect();
        let trailing: Vec<RcDoc<()>> = self.2.iter().map(Pretty::pretty).collect();
        RcDoc::intersperse(leading, RcDoc::line())
            .append(self.1.pretty())
            .append(RcDoc::intersperse(trailing, RcDoc::line()))
    }
}

impl<T> Pretty for Wrapped<T>
where
    T: Pretty,
{
    fn pretty(&self) -> RcDoc<()> {
        (self.left.pretty())
            .append(self.middle.pretty())
            .append(self.right.pretty())
    }
}

impl Pretty for IdentifierFields {
    fn pretty(&self) -> RcDoc<()> {
        match self.type_annotation {
            None => self.identifier.pretty(),
            Some(ref toks) => (self.identifier.pretty())
                .append(toks.0.pretty())
                .append(toks.1.pretty()),
        }
    }
}

fn pretty_arguments(args: &Wrapped<Vec<Identifier>>) -> RcDoc<()> {
    RcDoc::text("(")
        .append(
            RcDoc::concat(args.middle.iter().map(Pretty::pretty))
                .nest(2)
                .group(),
        )
        .append(RcDoc::text(")"))
}

impl Pretty for Expr {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            Expr::Identifier(id) => id.pretty(),
        }
    }
}

// Not sure how to handle trailing spaces right now — this is a hacky
// temporary workaround, but we need proper handling.
fn strip_trailing_space(doc: RcDoc<()>) -> RcDoc<()> {
    let mut s = String::new();
    doc.render_fmt(80, &mut s).unwrap();
    RcDoc::text(s.trim_end().to_owned())
}

impl Pretty for Defun {
    fn pretty(&self) -> RcDoc<()> {
        let body = RcDoc::intersperse(
            self.body
                .iter()
                .map(|expr| strip_trailing_space(expr.pretty())),
            RcDoc::hardline(),
        );

        (self.defun.pretty())
            .append(self.name.pretty())
            .append(pretty_arguments(&self.arguments))
            .append(RcDoc::hardline())
            .append(body)
            .nest(2)
            .group()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;
    use crate::parser;
    use crate::pretty::Pretty;

    use logos::Logos;

    fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input).filter_map(|token| token.ok()).collect()
    }

    #[test]
    fn test_defun_pretty() {
        let tokens = lex("(defun f (x:integer y:integer) x y)");
        let mut input = tokens.as_slice();
        let Ok(parsed) = parser::defun(&mut input) else {
            return;
        };
        let doc = parsed.pretty();
        let mut buffer = String::new();
        doc.render_fmt(80, &mut buffer).expect("pretty printing");
        let expected = "(defun f (x:integer y:integer)
  x
  y)";
        assert!(expected == buffer)
    }
}
