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
        eprintln!("expected {kind:?}");
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

// Immediate TODOs:
//   - module body
//   - typed identifiers
//   - annotations (doc, event, etc)
//
// Then move on to lower_tree and iterate?

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
    // TODO: documentation
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            match p.nth(1) {
                TokenKind::DefunKeyword => defun(p),
                TokenKind::DefcapKeyword => defcap(p),
                _ => todo!("other definitions"),
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

fn defun(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::DefunKeyword);
    p.expect(TokenKind::Ident);
    if p.at(TokenKind::Colon) {
        type_annotation(p);
    }

    param_list(p);

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
    p.expect(TokenKind::Ident);
    if p.at(TokenKind::Colon) {
        type_annotation(p);
    }
    param_list(p);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        expr(p);
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Defcap);
}

fn expr(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::Bool | TokenKind::String | TokenKind::Symbol => p.advance(),
        TokenKind::Ident => parsed_name(p),
        TokenKind::Number => expr_number(p),
        TokenKind::OpenBracket => expr_list(p),
        TokenKind::OpenBrace => match p.nth(2) {
            TokenKind::Colon => expr_object(p),
            TokenKind::BindAssign => expr_binding(p),
            _ => p.advance_with_error(&format!(
                "Expected : or := in object or binding but received {:?}",
                p.nth(2)
            )),
        },
        TokenKind::OpenParen => match p.nth(1) {
            TokenKind::LetKeyword | TokenKind::LetStarKeyword => expr_let(p),
            TokenKind::LambdaKeyword => expr_lambda(p),
            TokenKind::Ident => expr_app(p),
            _ => p.advance_with_error(&format!(
                "Expected let, app, or lambda but received {:?}",
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

    p.expect(TokenKind::OpenParen);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            let m = p.open();
            p.expect(TokenKind::OpenParen);
            p.expect(TokenKind::Ident);
            if p.at(TokenKind::Colon) {
                type_annotation(p);
            }
            expr(p);
            p.expect(TokenKind::CloseParen);
            p.close(m, TreeKind::Binder);
        } else {
            p.advance_with_error("expected a valid binder");
        }
    }
    p.expect(TokenKind::CloseParen);

    expr(p);

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
            if p.nth(1) != TokenKind::CloseBrace {
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
            p.expect(TokenKind::Ident);
            if p.at(TokenKind::Colon) {
                type_annotation(p);
            }
            if p.nth(1) != TokenKind::CloseBrace {
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

        p.advance();
        if p.at(TokenKind::Colon) {
            type_annotation(p);
        }
    }

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::ParamList);
}

// TODO: Really, type annotations cannot contain whitespace, comments, or newlines,
// so we may need to enforce this in the parser or lexer.
fn type_annotation(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::Colon);
    parse_type(p);
    p.close(m, TreeKind::TypeAnn);
}

fn parse_type(p: &mut Parser) {
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
                parse_type(p);
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

// Parse a simple name or qualified name (a or a.b.c)
fn name(p: &mut Parser) {
    let name = p.open();

    p.expect(TokenKind::Ident);

    // Handle qualified name with dots (a.b.c)
    while p.at(TokenKind::Dot) {
        p.advance();
        p.expect(TokenKind::Ident);
    }

    p.close(name, TreeKind::Name);
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
