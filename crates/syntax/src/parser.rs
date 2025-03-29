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
    fn build_tree(self) -> Tree {
        let mut tokens = self.tokens.into_iter();
        let mut events = self.events;
        let mut stack = Vec::new();

        // Special case: pop the last Close event to ensure
        // the stack is non-empty within the loop.
        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                // Starting a new node: push an empty tree to the stack.
                Event::Open { kind } => {
                    stack.push(Tree {
                        kind,
                        children: Vec::new(),
                    });
                }

                // A tree is done: pop it off the stack and append to a new current tree
                Event::Close => {
                    let tree = stack.pop().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Tree(tree));
                }

                // Consume a token and append it to the current tree
                Event::Advance => {
                    let token = tokens.next().unwrap();
                    stack.last_mut().unwrap().children.push(Child::Token(token));
                }
            }
        }

        // Parser will guarantee all trees are closed and cover the entirety
        // of the token stream.
        assert!(stack.len() == 1);
        assert!(matches!(
            tokens.next(),
            Some(SourceToken {
                kind: TokenKind::Eof,
                ..
            }) | None
        ));

        stack.pop().unwrap()
    }
}

/// Parse a Pact file
pub fn parse(tokens: Vec<SourceToken>) -> Tree {
    let mut p = Parser {
        tokens,
        pos: 0,
        fuel: Cell::new(256),
        events: Vec::new(),
    };
    file(&mut p);
    p.build_tree()
}

/// File = TopLevel*
fn file(p: &mut Parser) {
    let m = p.open();
    while !p.eof() {
        top_level(p);
    }
    p.close(m, TreeKind::File);
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
    // TODO: documentation
    // TODO: module body
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Module);
}

fn governance(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::String | TokenKind::Symbol | TokenKind::Ident => p.advance(),
        _ => p.advance_with_error("expected string, symbol, or capability name for governance"),
    }
}

fn expr(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::Bool | TokenKind::String | TokenKind::Symbol | TokenKind::Ident => p.advance(),
        TokenKind::Number => expr_number(p),
        TokenKind::OpenBracket => expr_list(p),
        TokenKind::OpenBrace => match p.nth(2) {
            TokenKind::Colon => expr_object(p),
            TokenKind::BindAssign => expr_binding(p),
            _ => p.advance_with_error("Expected object or binding"),
        },
        TokenKind::OpenParen => match p.nth(1) {
            TokenKind::LetKeyword | TokenKind::LetStarKeyword => expr_let(p),
            TokenKind::LambdaKeyword => expr_lambda(p),
            TokenKind::Ident => expr_app(p),
            _ => p.advance_with_error("Expected let, app, or lambda"),
        },
        _ => p.advance_with_error("Expected literal, '[', or '(' in expr"),
    }
}

fn expr_let(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    match p.nth(0) {
        TokenKind::LetKeyword | TokenKind::LetStarKeyword => {
            p.advance();
        }
        _ => p.advance_with_error("expected 'let' or 'let*' in let binding"),
    }

    p.expect(TokenKind::OpenParen);
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        if p.at(TokenKind::OpenParen) {
            let_binder(p);
        } else {
            p.advance_with_error("expected a valid binder");
        }
    }
    p.expect(TokenKind::CloseParen);

    expr(p);

    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Let)
}

fn let_binder(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenParen);
    p.expect(TokenKind::Ident);
    if p.at(TokenKind::Colon) {
        todo!("parse types...");
    }
    expr(p);
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::Binder);
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
    p.expect(TokenKind::Ident);
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
            p.advance_with_error("Expected string or symbol in object")
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
                todo!("parse types...");
            }
            if p.nth(1) != TokenKind::CloseBrace {
                p.expect(TokenKind::Comma);
            }
        } else {
            p.advance_with_error("Expected string or symbol in binding")
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
    while !p.at(TokenKind::CloseParen) && !p.eof() {
        p.expect(TokenKind::Ident);
        if p.at(TokenKind::Colon) {
            todo!("parse types...");
        }
    }
    p.expect(TokenKind::CloseParen);
    p.close(m, TreeKind::ParamList);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn parser_debug() {
        let input = r#"
            (+ -1.23 { 'a: 1.001 })
            (lambda (a b) (+ a b))
        "#;
        let tokens = tokenize(input);
        let parsed = parse(tokens);
        assert!(!&parsed.has_errors(), "Tree has errors {:?}", parsed);
    }
}
