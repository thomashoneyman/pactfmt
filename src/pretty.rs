use pretty::RcDoc;

use crate::cst::*;
use crate::lexer::Token;

/// A utility to determine whether spacing contains a comment
fn has_comment(spacing: &[Spacing]) -> bool {
    spacing
        .iter()
        .any(|space| matches!(space, Spacing::Comment(_)))
}

pub struct FormatContext {
    // FIXME: Rename this, perhaps 'force_line_break'
    multiline: bool,
}

pub trait Pretty {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()>;
}

pub trait HasSpacing {
    /// Whether the node contains spacing before itself
    fn leading_spacing(&self) -> bool;

    /// Whether the node contains spacing within itself
    fn inner_spacing(&self) -> bool;
}

impl HasSpacing for String {
    fn leading_spacing(&self) -> bool {
        false
    }

    fn inner_spacing(&self) -> bool {
        false
    }
}

impl Pretty for Vec<Spacing> {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        // In normalization we ensure there are no repeated newlines and no trailing newline
        // after a comment to end the spacing.
        //
        // We represent the normalize rules with a 'buffer' of newlines to apply of max
        // size 2. If we see a comment, we empty the buffer and append the comment.
        //
        // To ensure we have the right number of newlines in the absence of a comment,
        // we initialize the count with 1 newline if we see the spacing collection does
        // not begin with a newline.
        let init_newlines = match self.first() {
            None if ctx.multiline => 1,
            Some(Spacing::Comment(_)) if ctx.multiline => 1,
            _ => 0,
        };

        let (newlines, comment, doc) = self.iter().fold(
            (init_newlines, false, RcDoc::nil()),
            |(newlines, comment, doc), tok| match tok {
                Spacing::Comment(text) => {
                    let doc = if newlines > 0 {
                        (0..newlines)
                            .fold(doc, |next, _| next.append(RcDoc::hardline()))
                            .append(RcDoc::text(text))
                    } else {
                        RcDoc::text(text)
                    };
                    (0, true, doc)
                }
                Spacing::NewlineOne => {
                    if newlines == 0 {
                        (1, comment, doc)
                    } else {
                        (2, comment, doc)
                    }
                }
                Spacing::NewlineMany => (2, comment, doc),
            },
        );

        // If we formatted the spacing and there was no comment, then we are left with
        // an unused buffer of newlines we need to format.
        if !comment && newlines > 0 {
            (0..newlines).fold(doc, |next, _| next.append(RcDoc::hardline()))
        } else if comment {
            doc.append(RcDoc::hardline())
        } else {
            doc
        }
    }
}

impl Pretty for Token {
    fn pretty(&self, _ctx: &FormatContext) -> RcDoc<()> {
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
            Token::EnforceGuard => RcDoc::text("enforce-guard"),
            Token::KeysetRefGuard => RcDoc::text("keyset-ref-guard"),
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
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let leading: RcDoc<()> = self.0.pretty(ctx);
        leading.append(self.1.pretty(ctx))
    }
}

impl<T> HasSpacing for Positioned<T>
where
    T: HasSpacing,
{
    fn leading_spacing(&self) -> bool {
        !self.0.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        self.1.leading_spacing() || self.1.inner_spacing()
    }
}

impl Pretty for Named {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            Named::Ident(s) => RcDoc::text(s),
            Named::Reference(r) => r.pretty(ctx),
        }
    }
}

impl HasSpacing for Named {
    fn leading_spacing(&self) -> bool {
        false
    }

    fn inner_spacing(&self) -> bool {
        false
    }
}

impl Pretty for Reference {
    fn pretty(&self, _ctx: &FormatContext) -> RcDoc<()> {
        let mut doc = RcDoc::text(&self.first)
            .append(RcDoc::text("."))
            .append(RcDoc::text(&self.second));

        for part in &self.rest {
            doc = doc.append(RcDoc::text(".")).append(RcDoc::text(part));
        }
        doc
    }
}

impl Pretty for Type {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match &self {
            Type::Ident(s) => RcDoc::text(s),
            Type::List(ty) => RcDoc::text("[")
                .append(ty.pretty(ctx))
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
                    doc = doc.append(part.pretty(ctx));
                    first = false;
                }
                doc.append(RcDoc::text("}"))
            }
        }
    }
}

impl Pretty for IdentifierFields {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self.type_annotation {
            None => self.identifier.pretty(ctx),
            Some(ref ty) => (self.identifier.pretty(ctx))
                .append(RcDoc::text(":"))
                .append(ty.pretty(ctx)),
        }
    }
}

impl HasSpacing for IdentifierFields {
    fn leading_spacing(&self) -> bool {
        false
    }

    fn inner_spacing(&self) -> bool {
        false
    }
}

impl Pretty for LiteralValue {
    fn pretty(&self, _ctx: &FormatContext) -> RcDoc<()> {
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
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let l_paren = (self.left_paren.pretty(ctx)).append("(");

        let func_multiline = has_comment(&self.func.0);
        let func = if func_multiline {
            self.func.pretty(ctx)
        } else {
            self.func.1.pretty(ctx).append(RcDoc::space())
        };

        let multiline = self
            .args
            .iter()
            .any(|arg| arg.leading_spacing() || arg.inner_spacing());

        let args_context = FormatContext { multiline };
        let args: RcDoc<()> = if multiline {
            let docs = self.args.iter().map(|arg| arg.pretty(&args_context));
            RcDoc::concat(docs).nest(2)
        } else {
            match self.args.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc: RcDoc<()> = RcDoc::nil();
                    doc = doc.append(first.pretty(&args_context));
                    for arg in rest {
                        doc = doc.append(RcDoc::space()).append(arg.pretty(&args_context));
                    }
                    doc
                }
            }
        };

        let r_paren = (self.right_paren.pretty(&args_context)).append(")");
        l_paren.append(func).append(args).append(r_paren)
    }
}

impl Pretty for List {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let l_bracket = self.left_bracket.pretty(ctx).append("[");
        let r_bracket = self.right_bracket.pretty(ctx).append("]");

        let multiline = self
            .members
            .iter()
            .any(|member| member.leading_spacing() || member.inner_spacing());

        let members: RcDoc<()> = if multiline {
            let inner_context = FormatContext { multiline: true };
            let docs = self
                .members
                .iter()
                .map(|member| member.pretty(&inner_context));
            RcDoc::concat(docs).nest(2)
        } else {
            match self.members.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc: RcDoc<()> = RcDoc::nil();
                    doc = doc.append(first.pretty(ctx));
                    for arg in rest {
                        doc = doc.append(RcDoc::space()).append(arg.pretty(ctx));
                    }
                    doc
                }
            }
        };

        l_bracket.append(members).append(r_bracket)
    }
}

impl HasSpacing for List {
    fn leading_spacing(&self) -> bool {
        !self.left_bracket.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        !self.right_bracket.is_empty()
            || self
                .members
                .iter()
                .any(|expr| expr.leading_spacing() || expr.inner_spacing())
    }
}

impl Pretty for Expr {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            Expr::Identifier(id) => id.pretty(ctx),
            Expr::Literal(lit) => lit.pretty(ctx),
            Expr::Application(app) => app.pretty(ctx),
            Expr::List(list) => list.pretty(ctx),
        }
    }
}

impl HasSpacing for Expr {
    fn leading_spacing(&self) -> bool {
        match self {
            Expr::Identifier((spacing, _))
            | Expr::Literal((spacing, _))
            | Expr::Application(App {
                left_paren: spacing,
                ..
            })
            | Expr::List(List {
                left_bracket: spacing,
                ..
            }) => !spacing.is_empty(),
        }
    }

    fn inner_spacing(&self) -> bool {
        match self {
            Expr::Identifier(_) => false,
            Expr::Literal(_) => false,
            Expr::Application(App {
                left_paren: _,
                func,
                args,
                right_paren,
            }) => {
                !right_paren.is_empty()
                    || func.leading_spacing()
                    || func.inner_spacing()
                    || args
                        .iter()
                        .any(|arg| arg.leading_spacing() || arg.inner_spacing())
            }
            Expr::List(List {
                left_bracket: _,
                members,
                right_bracket,
            }) => {
                !right_bracket.is_empty()
                    || members
                        .iter()
                        .any(|member| member.leading_spacing() || member.inner_spacing())
            }
        }
    }
}

impl Pretty for Arguments {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let l_paren = self.left_paren.pretty(ctx).append("(");
        let r_paren = self.right_paren.pretty(ctx).append(")");

        let multiline = self
            .args
            .iter()
            .any(|arg| arg.leading_spacing() || arg.inner_spacing());

        let args: RcDoc<()> = if multiline {
            let inner_context = FormatContext { multiline: true };
            let docs = self.args.iter().map(|arg| arg.pretty(&inner_context));
            RcDoc::concat(docs).nest(2)
        } else {
            match self.args.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc: RcDoc<()> = RcDoc::nil();
                    doc = doc.append(first.pretty(ctx));
                    for arg in rest {
                        doc = doc.append(RcDoc::space()).append(arg.pretty(ctx));
                    }
                    doc
                }
            }
        };

        l_paren.append(args).append(r_paren)
    }
}

impl HasSpacing for Arguments {
    fn leading_spacing(&self) -> bool {
        !self.left_paren.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        !self.right_paren.is_empty()
            || self
                .args
                .iter()
                .any(|arg| arg.leading_spacing() || arg.inner_spacing())
    }
}

impl Pretty for DocAnn {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        self.ann
            .pretty(ctx)
            .append("@doc")
            .append(RcDoc::space())
            // FIXME: Include? .append(self.docstr.0.pretty(ctx))
            .append(RcDoc::text(&self.docstr.1))
    }
}

impl HasSpacing for DocAnn {
    fn leading_spacing(&self) -> bool {
        !self.ann.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        self.docstr.leading_spacing() || self.docstr.inner_spacing()
    }
}

impl Pretty for ModelAnn {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        self.ann
            .pretty(ctx)
            .append("@model")
            .append(RcDoc::space())
            .append(self.exprs.pretty(ctx))
    }
}

impl HasSpacing for ModelAnn {
    fn leading_spacing(&self) -> bool {
        !self.ann.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        self.exprs.leading_spacing() || self.exprs.inner_spacing()
    }
}

impl Pretty for ManagedAnn {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let ann = self.ann.pretty(ctx).append("@managed");
        match &self.args {
            Some((a, b)) => ann
                .append(RcDoc::space())
                .append(a.pretty(ctx))
                .append(RcDoc::space())
                .append(b.pretty(ctx)),
            None => ann,
        }
    }
}

impl HasSpacing for ManagedAnn {
    fn leading_spacing(&self) -> bool {
        !self.ann.is_empty()
    }

    fn inner_spacing(&self) -> bool {
        false
    }
}

impl Pretty for DefunBody {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            DefunBody::DocAnn(ann) => ann.pretty(ctx),
            DefunBody::ModelAnn(ann) => ann.pretty(ctx),
            DefunBody::Expr(expr) => expr.pretty(ctx),
        }
    }
}

impl HasSpacing for DefunBody {
    fn leading_spacing(&self) -> bool {
        match self {
            DefunBody::DocAnn(ann) => ann.leading_spacing(),
            DefunBody::ModelAnn(ann) => ann.leading_spacing(),
            DefunBody::Expr(expr) => expr.leading_spacing(),
        }
    }

    fn inner_spacing(&self) -> bool {
        match self {
            DefunBody::DocAnn(ann) => ann.inner_spacing(),
            DefunBody::ModelAnn(ann) => ann.inner_spacing(),
            DefunBody::Expr(expr) => expr.inner_spacing(),
        }
    }
}

impl Pretty for Defun {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let m_ctx = FormatContext { multiline: true };
        let s_ctx = FormatContext { multiline: false };

        let defun_multiline = has_comment(&self.defun);
        let name_multiline = has_comment(&self.name.0);
        let args_multiline =
            name_multiline || self.arguments.leading_spacing() || self.arguments.inner_spacing();
        let body_multiline = args_multiline
            || self
                .body
                .iter()
                .any(|body| body.leading_spacing() || body.inner_spacing());

        let l_paren = self.left_paren.pretty(ctx).append("(");

        let defun = if defun_multiline {
            self.defun.pretty(&m_ctx).append("defun")
        } else {
            RcDoc::text("defun").append(RcDoc::space())
        };

        let name = if name_multiline {
            self.name.pretty(&m_ctx).nest(2)
        } else {
            self.name.1.pretty(ctx).append(RcDoc::space())
        };

        let args = if args_multiline {
            self.arguments.pretty(&m_ctx).nest(2)
        } else {
            self.arguments.pretty(&s_ctx).append(RcDoc::space())
        };

        let body = if body_multiline {
            RcDoc::concat(self.body.iter().map(|body| body.pretty(&m_ctx))).nest(2)
        } else {
            match self.body.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc = first.pretty(&s_ctx);
                    for expr in rest {
                        doc = doc.append(RcDoc::space()).append(expr.pretty(&s_ctx));
                    }
                    doc
                }
            }
        };

        let r_paren = if body_multiline {
            self.right_paren.pretty(&m_ctx).append(")")
        } else {
            self.right_paren.pretty(&s_ctx).append(")")
        };

        l_paren
            .append(defun)
            .append(name)
            .append(args)
            .append(body)
            .append(r_paren)
    }
}

impl Pretty for DefcapBody {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            DefcapBody::DocAnn(ann) => ann.pretty(ctx),
            DefcapBody::ManagedAnn(ann) => ann.pretty(ctx),
            DefcapBody::EventAnn(spacing) => spacing.pretty(ctx).append("@event"),
            DefcapBody::Expr(expr) => expr.pretty(ctx),
        }
    }
}

impl HasSpacing for DefcapBody {
    fn leading_spacing(&self) -> bool {
        match self {
            DefcapBody::DocAnn(ann) => ann.leading_spacing(),
            DefcapBody::ManagedAnn(ann) => ann.leading_spacing(),
            DefcapBody::EventAnn(ann) => !ann.is_empty(),
            DefcapBody::Expr(expr) => expr.leading_spacing(),
        }
    }

    fn inner_spacing(&self) -> bool {
        match self {
            DefcapBody::DocAnn(ann) => ann.inner_spacing(),
            DefcapBody::ManagedAnn(ann) => ann.inner_spacing(),
            DefcapBody::EventAnn(_) => false,
            DefcapBody::Expr(expr) => expr.inner_spacing(),
        }
    }
}

impl Pretty for Defcap {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let m_ctx = FormatContext { multiline: true };
        let s_ctx = FormatContext { multiline: false };

        let defcap_multiline = has_comment(&self.defcap);
        let name_multiline = has_comment(&self.name.0);
        let args_multiline =
            name_multiline || self.arguments.leading_spacing() || self.arguments.inner_spacing();
        let body_multiline = args_multiline
            || self
                .body
                .iter()
                .any(|body| body.leading_spacing() || body.inner_spacing());

        let l_paren = self.left_paren.pretty(ctx).append("(");

        let defcap = if defcap_multiline {
            self.defcap.pretty(&m_ctx).append("defcap")
        } else {
            RcDoc::text("defcap").append(RcDoc::space())
        };

        let name = if name_multiline {
            self.name.pretty(&m_ctx).nest(2)
        } else {
            self.name.1.pretty(ctx).append(RcDoc::space())
        };

        let args = if args_multiline {
            self.arguments.pretty(&m_ctx).nest(2)
        } else {
            self.arguments.pretty(&s_ctx).append(RcDoc::space())
        };

        let body = if body_multiline {
            RcDoc::concat(self.body.iter().map(|body| body.pretty(&m_ctx))).nest(2)
        } else {
            match self.body.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc = first.pretty(&s_ctx);
                    for expr in rest {
                        doc = doc.append(RcDoc::space()).append(expr.pretty(&s_ctx));
                    }
                    doc
                }
            }
        };

        let r_paren = if body_multiline {
            self.right_paren.pretty(&m_ctx).append(")")
        } else {
            self.right_paren.pretty(&s_ctx).append(")")
        };

        l_paren
            .append(defcap)
            .append(name)
            .append(args)
            .append(body)
            .append(r_paren)
    }
}

impl Pretty for DefconstBody {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            DefconstBody::DocAnn(ann) => ann.pretty(ctx),
            DefconstBody::Expr(expr) => expr.pretty(ctx),
        }
    }
}

impl HasSpacing for DefconstBody {
    fn leading_spacing(&self) -> bool {
        match self {
            DefconstBody::DocAnn(ann) => ann.leading_spacing(),
            DefconstBody::Expr(expr) => expr.leading_spacing(),
        }
    }

    fn inner_spacing(&self) -> bool {
        match self {
            DefconstBody::DocAnn(ann) => ann.inner_spacing(),
            DefconstBody::Expr(expr) => expr.inner_spacing(),
        }
    }
}

impl Pretty for Defconst {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        let m_ctx = FormatContext { multiline: true };
        let s_ctx = FormatContext { multiline: false };

        let defconst_multiline = has_comment(&self.defconst);
        let name_multiline = has_comment(&self.name.0);
        let body_multiline = self
            .body
            .iter()
            .any(|body| body.leading_spacing() || body.inner_spacing());

        let l_paren = self.left_paren.pretty(ctx).append("(");

        let defconst = if defconst_multiline {
            self.defconst.pretty(&m_ctx).append("defconst")
        } else {
            RcDoc::text("defconst").append(RcDoc::space())
        };

        let name = if name_multiline {
            self.name.pretty(&m_ctx).nest(2)
        } else {
            self.name.1.pretty(ctx).append(RcDoc::space())
        };

        let body = if body_multiline {
            RcDoc::concat(self.body.iter().map(|body| body.pretty(&m_ctx))).nest(2)
        } else {
            match self.body.split_first() {
                None => RcDoc::nil(),
                Some((first, rest)) => {
                    let mut doc = first.pretty(&s_ctx);
                    for expr in rest {
                        doc = doc.append(RcDoc::space()).append(expr.pretty(&s_ctx));
                    }
                    doc
                }
            }
        };

        let r_paren = if body_multiline {
            self.right_paren.pretty(&m_ctx).append(")")
        } else {
            self.right_paren.pretty(&s_ctx).append(")")
        };

        l_paren
            .append(defconst)
            .append(name)
            .append(body)
            .append(r_paren)
    }
}

impl Pretty for ModuleGovernance {
    fn pretty(&self, _: &FormatContext) -> RcDoc<()> {
        match self {
            ModuleGovernance::Keyset(str) => RcDoc::text(str),
            ModuleGovernance::Cap(str) => RcDoc::text(str),
        }
    }
}

impl Pretty for Module {
    fn pretty(&self, _: &FormatContext) -> RcDoc<()> {
        let ctx = FormatContext { multiline: true };

        let l_paren = self.left_paren.pretty(&ctx).append("(");

        let module_multiline = has_comment(&self.module);
        let name_multiline = has_comment(&self.name.0);
        let gov_multiline = has_comment(&self.governance.0);

        let module = if module_multiline && !name_multiline {
            self.module
                .pretty(&ctx)
                .append("module")
                .append(RcDoc::space())
        } else if module_multiline {
            self.module.pretty(&ctx).append("module")
        } else {
            RcDoc::text("module").append(RcDoc::space())
        };

        let name = if name_multiline && !gov_multiline {
            self.name.pretty(&ctx).append(RcDoc::space())
        } else if name_multiline {
            self.name.pretty(&ctx)
        } else {
            self.name.1.pretty(&ctx).append(RcDoc::space())
        };

        let gov = if gov_multiline {
            self.governance.pretty(&ctx)
        } else {
            self.governance.1.pretty(&ctx).append(RcDoc::space())
        };

        let body = RcDoc::concat(self.body.iter().map(|body| body.pretty(&ctx)));
        let r_paren = self.right_paren.pretty(&ctx).append(")");

        l_paren
            .append(module.nest(2))
            .append(name.nest(2))
            .append(gov.nest(2))
            .append(body.nest(2))
            .append(r_paren)
    }
}

impl Pretty for Toplevel {
    fn pretty(&self, ctx: &FormatContext) -> RcDoc<()> {
        match self {
            Toplevel::Defun(defun) => defun.pretty(ctx),
            Toplevel::Defcap(defcap) => defcap.pretty(ctx),
            Toplevel::Defconst(defconst) => defconst.pretty(ctx),
            Toplevel::Expr(expr) => expr.pretty(ctx),
            Toplevel::Module(module) => module.pretty(ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::parser;
    use crate::pretty::{Pretty, RcDoc};

    use logos::Logos;
    use test_each_file::test_each_file;

    fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input).filter_map(|token| token.ok()).collect()
    }

    test_each_file! { in "./fixtures" => |content: &str| {
        let tokens = lex(content);
        let mut input = tokens.as_slice();
        let parsed = parser::parse(&mut input).expect("parsing");
        let ctx = FormatContext { multiline: false };
        let doc = RcDoc::concat(parsed.iter().map(|toplevel| toplevel.pretty(&ctx)));
        let mut buffer = String::new();
        doc.render_fmt(80, &mut buffer).expect("pretty printing");
        insta::assert_snapshot!(buffer);
    }}
}
