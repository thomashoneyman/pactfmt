use pretty::RcDoc;

use crate::cst::*;
use crate::lexer::Token;

pub trait Pretty {
    fn pretty(&self) -> RcDoc<()>;
}

pub fn format_spacing(spacing: &[Spacing]) -> RcDoc<()> {
    if spacing.is_empty() {
        return RcDoc::nil();
    }

    let docs: Vec<RcDoc<()>> = spacing
        .iter()
        .map(|s| match s {
            Spacing::NewlineOne => RcDoc::hardline(),
            Spacing::NewlineMany => RcDoc::hardline().append(RcDoc::hardline()),
            Spacing::Whitespace => RcDoc::space(),
            Spacing::Comment(text) => RcDoc::text(text),
        })
        .collect();

    RcDoc::concat(docs)
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

impl Pretty for Expr {
    fn pretty(&self) -> RcDoc<()> {
        match self {
            Expr::Identifier(id) => id.pretty(),
        }
    }
}

impl Pretty for Arguments {
    fn pretty(&self) -> RcDoc<()> {
        let l_paren = format_spacing(&self.left_paren).append("(");
        let r_paren = format_spacing(&self.right_paren).append(")");
        let single = RcDoc::concat(self.args.iter().map(Pretty::pretty));
        let multi =
            RcDoc::intersperse(self.args.iter().map(Pretty::pretty), RcDoc::hardline()).nest(2);
        let args = RcDoc::flat_alt(single, multi);

        l_paren.append(args).append(r_paren)
    }
}

impl Pretty for Defun {
    fn pretty(&self) -> RcDoc<()> {
        let l_paren = format_spacing(&self.left_paren).append("(");
        let defun = format_spacing(&self.defun).append("defun");
        let name = format_spacing(&self.name.0).append(self.name.1.pretty());
        let body = RcDoc::concat(self.body.iter().map(Pretty::pretty));
        let r_paren = format_spacing(&self.right_paren).append(")");

        l_paren
            .append(defun)
            .append(name)
            .append(self.arguments.pretty())
            .append(body.nest(2))
            .append(r_paren)
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
            Spacing::Whitespace,
            Spacing::NewlineMany,
            Spacing::Comment("; hello".to_string()),
            Spacing::NewlineOne,
        ];

        let doc = format_spacing(&spacing);
        let mut output = String::new();
        doc.render_fmt(80, &mut output).unwrap();

        assert_eq!(output, " \n\n; hello\n");
    }
}
