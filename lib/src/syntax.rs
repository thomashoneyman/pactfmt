use crate::lexer::Token;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExp {
    pub value: Literal,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Decimal,
    Boolean,
    String,
    Guard,
    Time,
    Unit,
    ListOf(Box<Type>),
    Polylist,
    ModuleRef(String),
    Keyset,
    Object(String),
    SchemaRef(String),
    TableSchemaRef(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub type_ann: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefConst {
    pub name: Ident,
    pub value: LiteralExp,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PactTree {
    DefConst(DefConst),
    // More variants will be added here as we implement other top-level constructs
}

impl PactTree {
    pub fn span(&self) -> &Span {
        match self {
            PactTree::DefConst(def_const) => &def_const.span,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    line: usize,
    column: usize,
    peek_token: Option<(Token, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Token::lexer(input),
            line: 1,
            column: 1,
            peek_token: None,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<PactTree>, String> {
        let mut results = Vec::new();
        loop {
            match self.peek() {
                Some((Token::LeftParen, _)) => {
                    let def_const = self.parse_defconst()?;
                    results.push(PactTree::DefConst(def_const));
                    continue;
                }
                Some((token, _)) => {
                    return Err(format!("Expected left paren but received {:?}", token))
                }
                None => break,
            }
        }
        return Ok(results);
    }

    // Private helper functions for parsing

    fn peek(&mut self) -> Option<&(Token, Span)> {
        if self.peek_token.is_none() {
            self.peek_token = self.step_token();
        }
        self.peek_token.as_ref()
    }

    fn consume(&mut self) -> Option<(Token, Span)> {
        if let Some(token) = self.peek_token.take() {
            return Some(token);
        }
        self.step_token()
    }

    fn step_token(&mut self) -> Option<(Token, Span)> {
        loop {
            match self.lexer.next() {
                Some(Ok(Token::Whitespace)) => {
                    self.column += self.lexer.span().len();
                    continue;
                }

                Some(Ok(Token::Newline)) => {
                    self.line += 1;
                    self.column = 1;
                    continue;
                }

                Some(Ok(token)) => {
                    let start = Position {
                        line: self.line,
                        column: self.column,
                    };

                    let token_span = self.lexer.span();
                    self.column += token_span.len();

                    let end = Position {
                        line: self.line,
                        column: self.column,
                    };

                    return Some((token, Span { start, end }));
                }

                Some(Err(_)) => {
                    // TODO: Handle lexer errors?
                    continue;
                }

                None => return None,
            }
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Span, String> {
        match self.consume() {
            Some((token, span)) if token == expected => Ok(span),
            Some((token, _)) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    // Parsing individual constructs

    fn parse_literal(&mut self) -> Result<LiteralExp, String> {
        let (token, span) = self.consume().ok_or("Expected literal")?;
        let value = match token {
            Token::Integer => Literal::Integer(self.lexer.slice().to_string()),
            Token::Decimal => Literal::Decimal(self.lexer.slice().to_string()),
            Token::String => Literal::String(self.lexer.slice().to_string()),
            Token::Symbol => Literal::Symbol(self.lexer.slice().to_string()),
            Token::Boolean => Literal::Boolean(self.lexer.slice().to_string()),
            _ => return Err("Expected literal".to_string()),
        };
        Ok(LiteralExp { value, span })
    }

    fn parse_type(&self, type_str: &str) -> Result<Type, String> {
        match type_str {
            "integer" => Ok(Type::Integer),
            "decimal" => Ok(Type::Decimal),
            "bool" => Ok(Type::Boolean),
            "string" => Ok(Type::String),
            "guard" => Ok(Type::Guard),
            "time" => Ok(Type::Time),
            "keyset" => Ok(Type::Keyset),
            // FIXME: Lots more here. Plus, not clear how to handle types
            // using tokens, ie. lists, objects, etc.
            _ => Err(format!("Unknown type: {}", type_str)),
        }
    }

    fn parse_ident(&mut self) -> Result<Ident, String> {
        let (token, span) = self.consume().ok_or("Expected identifier")?;

        let name = match token {
            Token::Ident => self.lexer.slice().to_string(),
            _ => return Err(format!("Expected identifier, found {:?}", token)),
        };

        let start_span = span.clone();

        // Check if there's a type annotation
        let type_ann = if let Some((Token::Colon, _)) = self.peek() {
            self.consume(); // Consume the colon
            match self.peek() {
                Some((Token::Ident, _)) => {
                    self.consume();
                    let type_name = self.lexer.slice().to_string();
                    Some(self.parse_type(&type_name)?)
                }
                Some((unexpected, _)) => {
                    return Err(format!(
                        "Expected type identifier after colon, found {:?}",
                        unexpected
                    ))
                }
                None => return Err("Unexpected end of input after colon".to_string()),
            }
        } else {
            None
        };

        let end_span = span;

        Ok(Ident {
            name,
            type_ann,
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_defconst(&mut self) -> Result<DefConst, String> {
        let start_span = self.expect(Token::LeftParen)?;
        self.expect(Token::DefConst)?;

        let name = self.parse_ident()?;
        let value = self.parse_literal()?;

        let end_span = self.expect(Token::RightParen)?;

        Ok(DefConst {
            name,
            value,
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_defconst() {
        let input = r#"(defconst MY_STRING "Hello, World!")"#;
        let mut parser = Parser::new(input);

        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        match &result.unwrap()[0] {
            PactTree::DefConst(def_const) => {
                assert_eq!(
                    def_const.span,
                    Span {
                        start: Position { line: 1, column: 1 },
                        end: Position {
                            line: 1,
                            column: 37
                        }
                    }
                );
            }
        }
    }

    #[test]
    fn test_parse_defconst_type_ann() {
        let input = "(defconst MY_INT:integer\n  1)\n(defconst MY_BOOL:bool\n  true)";
        let mut parser = Parser::new(input);

        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        match &result.clone().unwrap()[0] {
            PactTree::DefConst(def_const) => {
                assert_eq!(
                    def_const.span,
                    Span {
                        start: Position { line: 1, column: 1 },
                        end: Position { line: 2, column: 5 }
                    }
                );
                assert_eq!(def_const.name.name, "MY_INT");
                assert_eq!(
                    def_const.name.span,
                    Span {
                        start: Position {
                            line: 1,
                            column: 11
                        },
                        end: Position {
                            line: 1,
                            column: 17
                        }
                    }
                );
                assert_eq!(def_const.name.type_ann, Some(Type::Integer));
                match &def_const.value.value {
                    Literal::Integer(val) => assert_eq!(val, "1"),
                    _ => panic!("Expected integer literal"),
                }
            }
        }

        match &result.unwrap()[1] {
            PactTree::DefConst(def_const) => {
                assert_eq!(def_const.name.name, "MY_BOOL");
            }
        }
    }
}
