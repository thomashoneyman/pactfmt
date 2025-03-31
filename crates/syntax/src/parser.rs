/// TODO:
///   - widen error trees such that an error continues until
///     some recoverable syntax (like an open paren) is found
///   - record errors in the parser, as purescript-analyzer does
use crate::types::{Child, SourceToken, TokenKind, Tree, TreeKind};
use std::cell::Cell;

/// These represent the operations needed to build a syntax tree.
#[derive(Debug, Clone)]
enum Event {
    Open { kind: TreeKind },
    Close,
    Advance,
}

/// Sometimes it is only possible to determine the type of a syntax
/// node after parsing; the mark is used to replace the Open tree
/// kind at closing time.
struct MarkOpened {
    index: usize,
}

struct Parser {
    tokens: Vec<SourceToken>,
    // Current position in the token stream
    pos: usize,
    /// Simplifies debugging by terminating the parser if it loops.
    fuel: Cell<u32>,
    events: Vec<Event>,
}

/// Basic parser methods
impl Parser {
    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: TreeKind::ErrorTree,
        });
        mark
    }

    fn close(&mut self, mark: MarkOpened, kind: TreeKind) {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    fn nth(&self, lookahead: usize) -> TokenKind {
        if self.fuel.get() == 0 {
            panic!("parser is stuck")
        }
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.pos + lookahead)
            .map_or(TokenKind::Eof, |it| it.kind)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.nth(0) == kind
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.eat(kind) {
            return;
        }
        // TODO: Error reporting.
        eprintln!(
            "expected {:?} but received {:?} at line {}, column {}",
            kind,
            self.nth(0),
            self.tokens[self.pos].range.start.line,
            self.tokens[self.pos].range.start.column
        );
    }

    fn advance_with_error(&mut self, error: &str) {
        let mark = self.open();
        eprintln!("{error}");
        self.advance();
        self.close(mark, TreeKind::ErrorTree);
    }
}

impl Parser {
    fn build_trees(self) -> Vec<Tree> {
        let mut tokens = self.tokens.into_iter();
        let events = self.events;
        let mut stack = Vec::new(); // in-progress trees
        let mut result = Vec::new(); // completed trees

        for event in events {
            match event {
                Event::Open { kind } => {
                    stack.push(Tree {
                        kind,
                        children: Vec::new(),
                    });
                }

                Event::Close => {
                    let tree = stack.pop().unwrap();
                    if stack.is_empty() {
                        // An empty stack means this was a root-level tree
                        result.push(tree);
                    } else {
                        // Otherwise, this is a child of the current tree on the stack
                        stack.last_mut().unwrap().children.push(Child::Tree(tree));
                    }
                }

                // This token is part of the current tree
                Event::Advance => {
                    let token = tokens.next().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Token(token));
                }
            }
        }

        assert!(matches!(
            tokens.next(),
            Some(SourceToken {
                kind: TokenKind::Eof,
                ..
            }) | None
        ));

        result
    }
}

pub fn parse(tokens: Vec<SourceToken>) -> Vec<Tree> {
    let mut p = Parser {
        tokens,
        pos: 0,
        fuel: Cell::new(256),
        events: Vec::new(),
    };
    while !p.eof() {
        top_level(&mut p);
    }
    p.build_trees()
}

fn top_level(p: &mut Parser) {
    if p.at(TokenKind::OpenParen) {
        match p.nth(1) {
            TokenKind::ModuleKeyword => module(p),
            TokenKind::InterfaceKeyword => todo!("interface parser"),
            TokenKind::ImportKeyword => todo!("import parser"),
            _ => expr(p),
        }
    } else {
        expr(p);
    }
}

fn module(p: &mut Parser) {
    assert!(p.at(TokenKind::OpenParen));
    let m = p.open();

    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::ModuleKeyword);
    p.expect(TokenKind::Ident);

    governance(p);
    annotations(p);

    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            match p.nth(1) {
                TokenKind::BlessKeyword => bless(p),
                TokenKind::DefunKeyword => defun(p),
                TokenKind::DefcapKeyword => defcap(p),
                TokenKind::DefconstKeyword => defconst(p),
                TokenKind::DefschemaKeyword => defschema(p),
                TokenKind::DeftableKeyword => deftable(p),
                TokenKind::DefpactKeyword => defpact(p),
                _ => p.advance_with_error(&format!(
                    "expected a definition keyword but received {:?}",
                    p.nth(1)
                )),
            }
        } else {
            p.advance_with_error("expected open paren for definition or external decl");
        }
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Module);
}

fn governance(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::String | TokenKind::Symbol | TokenKind::Ident => p.advance(),
        _ => p.advance_with_error(&format!(
            "expected string, symbol, or capability name for governance but received {:?}",
            p.nth(0)
        )),
    }
}

fn bless(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::BlessKeyword);
    if p.at(TokenKind::String) || p.at(TokenKind::Symbol) {
        p.advance();
    } else {
        p.advance_with_error(&format!(
            "expected string or symbol for bless but received {:?}",
            p.nth(0)
        ));
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Bless);
}

fn defun(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefunKeyword);
    name(p);
    param_list(p);
    annotations(p);
    if p.at(TokenKind::CloseParen) {
        p.advance_with_error("function body must have at least one expression");
    } else {
        while !p.at(TokenKind::CloseParen) && !p.eof() {
            expr(p);
        }
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defun);
}

fn defcap(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefcapKeyword);
    name(p);
    param_list(p);
    annotations(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        expr(p);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defcap);
}

fn defconst(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefconstKeyword);
    name(p);
    expr(p);
    // TODO: Only allows doc annotations, really. So we should check for a string
    // or an @doc string.
    if p.at(TokenKind::String) {
        p.advance();
    } else if p.at(TokenKind::DocAnnKeyword) {
        doc_ann(p);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defconst);
}

fn defschema(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefschemaKeyword);
    p.expect(TokenKind::Ident);
    annotations(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        let m = p.open();
        p.expect(TokenKind::Ident);
        p.expect(TokenKind::Colon);
        ty(p);
        p.close(m, TreeKind::SchemaField);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defschema);
}

fn deftable(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DeftableKeyword);

    let table_name = p.open();
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::Colon);
    p.expect(TokenKind::OpenBrace);
    untyped_name(p);
    p.expect(TokenKind::CloseBrace);
    p.close(table_name, TreeKind::TableName);

    annotations(p);
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Deftable);
}

fn defpact(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefpactKeyword);
    name(p);
    param_list(p);
    annotations(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            match p.nth(1) {
                TokenKind::StepKeyword => step(p),
                TokenKind::StepWithRollbackKeyword => step_with_rollback(p),
                TokenKind::ResumeKeyword => resume(p),
                _ => p.advance_with_error(&format!(
                    "expected step, step-with-rollback, or resume but received {:?}",
                    p.nth(1)
                )),
            }
        } else {
            p.advance_with_error("expected open paren for step, step-with-rollback, or resume");
        }
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defpact);
}

fn step(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::StepKeyword);
    if p.at(TokenKind::Ident) {
        name(p);
    }
    expr(p);
    annotations(p);
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Step);
}

fn step_with_rollback(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::StepWithRollbackKeyword);
    if p.at(TokenKind::Ident) {
        name(p);
    }
    expr(p); // body
    expr(p); // rollback
    annotations(p); // TODO: technically only a model annotation allowed here
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::StepWithRollback);
}

fn resume(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::ResumeKeyword);
    expr_binding(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        expr(p);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Resume);
}

fn expr(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::Bool | TokenKind::String | TokenKind::Symbol => p.advance(),
        TokenKind::Ident => parsed_name(p),
        TokenKind::Number => expr_number(p),
        TokenKind::OpenBracket => expr_list(p),
        TokenKind::OpenBrace => match p.nth(2) {
            TokenKind::BindAssign => expr_binding(p),
            TokenKind::Colon => expr_object(p),
            _ => p.advance_with_error(&format!(
                "Expected : or := in object or binding but received {:?}",
                p.nth(2)
            )),
        },
        TokenKind::OpenParen => match p.nth(1) {
            TokenKind::LetKeyword | TokenKind::LetStarKeyword => expr_let(p),
            TokenKind::LambdaKeyword => expr_lambda(p),
            TokenKind::Ident if p.tokens[p.pos + 1].text == "if" => expr_if(p),
            TokenKind::Ident => expr_app(p),
            _ => p.advance_with_error(&format!(
                "Expected if, let, app, or lambda but received {:?}",
                p.nth(1)
            )),
        },
        _ => p.advance_with_error(&format!(
            "Expected literal, '[', or '(' in expr but received {:?}",
            p.nth(0)
        )),
    }
}

fn expr_let(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    match p.nth(0) {
        TokenKind::LetKeyword | TokenKind::LetStarKeyword => {
            p.advance();
        }
        _ => p.advance_with_error(&format!(
            "expected 'let' or 'let*' in let binding but received {:?}",
            p.nth(0)
        )),
    }

    let binding_list = p.open();
    p.expect(TokenKind::OpenParen);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            let m = p.open();
            p.expect(TokenKind::OpenParen);
            name(p);
            expr(p);
            p.expect(TokenKind::CloseParen);
            p.close(m, TreeKind::Binder);
        } else {
            p.advance_with_error("expected a valid binder");
        }
    }
    p.expect(TokenKind::CloseParen);
    p.close(binding_list, TreeKind::BindingList);

    while !p.at(TokenKind::CloseParen) && !p.eof() {
        expr(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Let)
}

fn expr_lambda(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::LambdaKeyword);
    param_list(p);
    expr(p);
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Lambda);
}

fn expr_app(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    parsed_name(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        expr(p);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::App);
}

fn expr_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenBracket);
    while !p.at(TokenKind::CloseBracket) && !p.eof() {
        expr(p);
        if p.at(TokenKind::Comma) {
            p.advance();
        }
    }
    p.expect(TokenKind::CloseBracket);
    p.close(m, TreeKind::List);
}

fn expr_object(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenBrace);
    while !p.at(TokenKind::CloseBrace) && !p.eof() {
        if p.at(TokenKind::String) || p.at(TokenKind::Symbol) {
            p.advance();
            p.expect(TokenKind::Colon);
            expr(p);
            if !p.at(TokenKind::CloseBrace) {
                p.expect(TokenKind::Comma);
            }
        } else {
            p.advance_with_error("Expected string or symbol as object key")
        }
    }
    p.expect(TokenKind::CloseBrace);
    p.close(m, TreeKind::Object);
}

fn expr_binding(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenBrace);
    while !p.at(TokenKind::CloseBrace) && !p.eof() {
        if p.at(TokenKind::String) || p.at(TokenKind::Symbol) {
            p.advance();
            p.expect(TokenKind::BindAssign);
            name(p);
            if !p.at(TokenKind::CloseBrace) {
                p.expect(TokenKind::Comma);
            }
        } else {
            p.advance_with_error("Expected string or symbol as binding key")
        }
    }
    p.expect(TokenKind::CloseBrace);
    p.close(m, TreeKind::Binding);
}

fn expr_number(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::Number);

    let mut decimal_seen = false;
    while p.at(TokenKind::Dot) {
        if decimal_seen {
            p.advance_with_error("Invalid number format: multiple decimal points");
        } else {
            decimal_seen = true;
            p.advance();

            if !p.at(TokenKind::Number) {
                p.advance_with_error("Decimal point must be followed by digits");
                break;
            }

            p.advance();
        }
    }

    let kind = if decimal_seen {
        TreeKind::DecimalLiteral
    } else {
        TreeKind::IntLiteral
    };

    p.close(m, kind);
}

fn param_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);

    // Parse parameters one by one
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if !p.at(TokenKind::Ident) {
            p.advance_with_error("expected identifier in parameter list");
            continue;
        }
        name(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::ParamList);
}

// Parse a simple name or qualified name (a or a.b.c)
fn untyped_name(p: &mut Parser) {
    let name = p.open();
    p.expect(TokenKind::Ident);
    while p.at(TokenKind::Dot) {
        p.advance();
        p.expect(TokenKind::Ident);
    }
    p.close(name, TreeKind::Name);
}

// Parse a simple name or qualified name (a or a.b.c) with optional type annotation
fn name(p: &mut Parser) {
    let name = p.open();
    p.expect(TokenKind::Ident);
    while p.at(TokenKind::Dot) {
        p.advance();
        p.expect(TokenKind::Ident);
    }
    // Include type annotation as a child of the name node if present
    // TODO: We perhaps should provide separate name and typeable_name
    // nodes, since types aren't allowed in all places.
    if p.at(TokenKind::Colon) {
        let m = p.open();
        p.expect(TokenKind::Colon);
        ty(p);
        p.close(m, TreeKind::TypeAnn);
    }
    p.close(name, TreeKind::Name);
}

// Helper function for parsing types as part of a type annotation
fn ty(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::Ident => {
            let m = p.open();

            let token_text = &p.tokens[p.pos].text;

            // Object types: object or object{type}
            if token_text == "object" {
                p.advance();

                // Handle object{type} syntax
                if p.at(TokenKind::OpenBrace) {
                    p.expect(TokenKind::OpenBrace);
                    parsed_name(p);
                    p.expect(TokenKind::CloseBrace);
                }

                p.close(m, TreeKind::Type);
                return;
            }

            // Consume the identifier
            p.advance();

            // Schema type
            if p.at(TokenKind::OpenBrace) {
                p.expect(TokenKind::OpenBrace);
                parsed_name(p);
                p.expect(TokenKind::CloseBrace);
                p.close(m, TreeKind::Type);
            }
            // Otherwise, it's a primitive type. Technically, there is a limitedh
            // set of primitive types, but it's unnecessary to enforce in the
            // formatter.
            else {
                p.close(m, TreeKind::PrimType);
            }
        }

        // List type: [Type] or [*]
        TokenKind::OpenBracket => {
            let m = p.open();
            p.expect(TokenKind::OpenBracket);

            // Check for wildcard "*"
            if p.at(TokenKind::Ident) && &p.tokens[p.pos].text == "*" {
                p.advance();
            } else {
                ty(p);
            }

            p.expect(TokenKind::CloseBracket);
            p.close(m, TreeKind::Type);
        }

        // Module reference type: module { Name, ... }
        TokenKind::ModuleKeyword => {
            let m = p.open();
            p.advance();
            p.expect(TokenKind::OpenBrace);

            while !p.at(TokenKind::CloseBrace) && !p.eof() {
                parsed_name(p);
                if p.at(TokenKind::Comma) {
                    p.advance();
                }
            }

            p.expect(TokenKind::CloseBrace);
            p.close(m, TreeKind::Type);
        }

        _ => p.advance_with_error("expected a valid type"),
    }
}

// Parse a module reference (my.mod::name.key)
fn modref(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::Ident);
    p.expect(TokenKind::DynAcc);
    name(p);
    p.close(m, TreeKind::ModRef);
}

// Parse a name or module reference
fn parsed_name(p: &mut Parser) {
    if p.at(TokenKind::Ident) {
        if p.nth(1) == TokenKind::DynAcc {
            modref(p);
        } else {
            name(p);
        }
    } else {
        p.advance_with_error("expected identifier in name");
    }
}

// TODO: This currently parsers all annotations and allows multiple
// instances of the same annotation, but in practice not all annotations
// can be used together in all contexts and any particular annotation
// can appear at most once. We also don't parse leading strings as a
// docstring and just assume it is part of the body.
fn annotations(p: &mut Parser) {
    // An initial bare string is a docstring
    if p.at(TokenKind::String) && p.nth(1) != TokenKind::CloseParen {
        p.advance();
    }

    while p.at(TokenKind::DocAnnKeyword)
        || p.at(TokenKind::ModelAnnKeyword)
        || p.at(TokenKind::EventAnnKeyword)
        || p.at(TokenKind::ManagedAnnKeyword)
    {
        match p.nth(0) {
            TokenKind::DocAnnKeyword => doc_ann(p),
            TokenKind::ModelAnnKeyword => model_ann(p),
            TokenKind::EventAnnKeyword => event_ann(p),
            TokenKind::ManagedAnnKeyword => managed_ann(p),
            _ => unreachable!(),
        }
    }
}

fn doc_ann(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::DocAnnKeyword);
    p.expect(TokenKind::String);
    p.close(m, TreeKind::DocAnn);
}

fn model_ann(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::ModelAnnKeyword);
    prop_list(p);
    p.close(m, TreeKind::ModelAnn);
}

fn event_ann(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::EventAnnKeyword);
    p.close(m, TreeKind::EventAnn);
}

fn managed_ann(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::ManagedAnnKeyword);
    if p.at(TokenKind::Ident) {
        name(p); // resource
        name(p); // manager fn
    }
    p.close(m, TreeKind::ManagedAnn);
}

fn prop_let(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::LetKeyword);

    p.expect(TokenKind::OpenParen);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            let m = p.open();
            p.expect(TokenKind::OpenParen);
            name(p);
            property_expr(p);
            p.expect(TokenKind::CloseParen);
            p.close(m, TreeKind::PropBinder);
        } else {
            p.advance_with_error("expected a valid property binder");
        }
    }
    p.expect(TokenKind::CloseParen);

    while !p.at(TokenKind::CloseParen) && !p.eof() {
        property_expr(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::PropLet);
}

fn prop_lam(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::LamKeyword);
    param_list(p);

    while !p.at(TokenKind::CloseParen) && !p.eof() {
        property_expr(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::PropLam);
}

fn prop_app(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    parsed_name(p);

    while !p.at(TokenKind::CloseParen) && !p.eof() {
        property_expr(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::PropApp);
}

fn prop_def_property(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefpropertyKeyword);
    p.expect(TokenKind::Ident);

    if p.at(TokenKind::OpenParen) && has_param_list(p) {
        param_list(p);
        property_expr(p);
    } else {
        property_expr(p);
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::PropDefProperty);
}

fn has_param_list(p: &mut Parser) -> bool {
    let mut pos = p.pos;
    let mut depth = 1;  // Start at 1 since we're already inside defproperty

    // Skip the open paren
    pos += 1;
    depth += 1;

    // Look for a close paren followed by another open paren
    // This would indicate two separate expressions in the defproperty body
    while pos < p.tokens.len() && depth > 1 {
        match p.tokens[pos].kind {
            TokenKind::OpenParen => depth += 1,
            TokenKind::CloseParen => {
                depth -= 1;
                // If we closed the first paren and immediately see another open paren
                // before closing the defproperty, then we have a param list
                if depth == 1 && pos + 1 < p.tokens.len() &&
                   p.tokens[pos + 1].kind == TokenKind::OpenParen {
                    return true;
                }
            },
            _ => {}
        }
        pos += 1;
    }

    false
}

fn prop_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenBracket);
    while !p.at(TokenKind::CloseBracket) && !p.eof() {
        property_expr(p);
    }
    p.expect(TokenKind::CloseBracket);
    p.close(m, TreeKind::PropList);
}

fn property_expr(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::Bool | TokenKind::String | TokenKind::Symbol => p.advance(),
        TokenKind::Ident => parsed_name(p),
        TokenKind::Number => expr_number(p),
        TokenKind::OpenBracket => prop_list(p),
        TokenKind::OpenBrace => expr_object(p),
        TokenKind::OpenParen => match p.nth(1) {
            TokenKind::LetKeyword => prop_let(p),
            TokenKind::LamKeyword => prop_lam(p),
            TokenKind::DefpropertyKeyword => prop_def_property(p),
            TokenKind::Ident => prop_app(p),
            _ => p.advance_with_error(&format!(
                "Expected let, lam, defproperty, app, or ident but received {:?}",
                p.nth(1)
            )),
        },
        _ => p.advance_with_error(&format!(
            "Expected literal, identifier, '[', or '(' in property expression but received {:?}",
            p.nth(0)
        )),
    }
}

fn expr_if(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::Ident); // 'if'
    expr(p); // condition
    expr(p); // 'then'
    expr(p); // 'else'

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::If);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_parse_model_annotation() {
        let input = r#"(module my-mod GOV
    @model
    [ (defproperty conserves-mass
        (= (column-delta coin-table 'balance) 0.0))

        (defproperty valid-account (account:string)
        (and
            (>= (length account) 3)
            (<= (length account) 256)))
    ]
)"#;
        let tokens = tokenize(input);
        let parsed = parse(tokens);
        println!("Parse tree: {:#?}", parsed);
    }
}
