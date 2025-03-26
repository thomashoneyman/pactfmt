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
        self.pos == self.tokens.len()
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

        // Special case: pop the last `Close` event to ensure
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
        assert!(tokens.next().is_none());

        let mut result = stack.pop().unwrap();

        // Remove final EOF error node if present
        if let Some(Child::Tree(tree)) = result.children.last() {
            if tree.kind == TreeKind::ErrorTree
                && tree.children.len() == 1
                && matches!(tree.children.first(), Some(Child::Token(token)) if token.kind == TokenKind::Eof)
            {
                result.children.pop();
            }
        }

        result
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

/// TopLevel = Module | Interface | Use | Expr
fn top_level(p: &mut Parser) {
    if p.at(TokenKind::OpenParen) {
        match p.nth(1) {
            TokenKind::ModuleKeyword => module(p),
            TokenKind::InterfaceKeyword => todo!("interface parser"),
            TokenKind::ImportKeyword => todo!("import parser"),
            _ => todo!("expr parser"),
        }
    } else if p.at(TokenKind::OpenBracket) {
        number_list(p);
    } else {
        p.advance_with_error("expected open paren, number, or top-level construct");
    }
}

/// Module = '(' 'module' name:Ident gov:Governance Documentation? (ExternalDecl | Def)* ')'
fn module(p: &mut Parser) {
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

/// Governance = String | Symbol | Ident
fn governance(p: &mut Parser) {
    match p.nth(0) {
        TokenKind::StringLit | TokenKind::SingleTick | TokenKind::Ident => p.advance(),
        _ => p.advance_with_error("expected string, symbol, or capability name for governance"),
    }
}

// FIXME: Temporary, for verifying parser integration test
fn number_list(p: &mut Parser) {
    let m = p.open();
    p.expect(TokenKind::OpenBracket);
    while !p.at(TokenKind::CloseBracket) {
        if p.at(TokenKind::Number) {
            p.advance();
            if p.at(TokenKind::Comma) {
                p.advance();
            }
        } else {
            p.advance_with_error("expected number or closing bracket");
        }
    }
    p.expect(TokenKind::CloseBracket);
    p.close(m, TreeKind::ListLiteral);
}
