use crate::lexer::Token;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
    Unit,
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
    // Technically it isn't true that identifiers in all positions can have a
    // type annotation, but that consideration is irrelevant for formatting.
    pub type_ann: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(Literal),
    App(Box<(Expr, Expr)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefConst {
    pub name: Ident,
    pub docs: Option<String>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PactTree {
    DefConst(DefConst),
    // More variants will be added here as we implement other top-level constructs
}

pub struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    line: usize,
    column: usize,
    peek_token: Option<Token>,
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

        while let Some(token) = self.peek() {
            match &token {
                Token::LeftParen => {
                    let def_const = self.parse_defconst()?;
                    results.push(PactTree::DefConst(def_const));
                }
                token => return Err(format!("Expected left paren but received {:?}", token)),
            }
        }

        Ok(results)
    }

    // Private helper functions for parsing

    fn peek(&mut self) -> Option<&Token> {
        if self.peek_token.is_none() {
            self.peek_token = self.step_token();
        }
        self.peek_token.as_ref()
    }

    fn consume(&mut self) -> Option<Token> {
        if let Some(token) = self.peek_token.take() {
            return Some(token);
        }
        self.step_token()
    }

    fn step_token(&mut self) -> Option<Token> {
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
                    let token_span = self.lexer.span();
                    self.column += token_span.len();
                    return Some(token);
                }

                Some(Err(_)) => {
                    // TODO: Handle lexer errors?
                    continue;
                }

                None => return None,
            }
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.consume() {
            Some(token) if token == expected => Ok(()),
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    // Parsing individual constructs

    fn parse_literal(&mut self) -> Result<Literal, String> {
        let token = self.consume().ok_or("Expected literal")?;
        let value = match token {
            Token::Integer => Literal::Integer(self.lexer.slice().to_string()),
            Token::Decimal => Literal::Decimal(self.lexer.slice().to_string()),
            Token::String => Literal::String(self.lexer.slice().to_string()),
            Token::Symbol => Literal::Symbol(self.lexer.slice().to_string()),
            Token::Boolean => Literal::Boolean(self.lexer.slice().to_string()),
            _ => return Err("Expected literal".to_string()),
        };
        Ok(value)
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
        let token = self.consume().ok_or("Expected identifier")?;

        let name = match token {
            Token::Ident => self.lexer.slice().to_string(),
            _ => return Err(format!("Expected identifier, found {:?}", token)),
        };

        // Check if there's a type annotation
        let type_ann = if let Some(Token::Colon) = self.peek() {
            self.consume(); // Consume the colon
            match self.peek() {
                Some(Token::Ident) => {
                    self.consume();
                    let type_name = self.lexer.slice().to_string();
                    Some(self.parse_type(&type_name)?)
                }
                Some(unexpected) => {
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

        Ok(Ident { name, type_ann })
    }

    // FIXME: Add cases for various expressions.
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let literal = self.parse_literal()?;
        Ok(Expr::Constant(literal))
    }

    fn parse_defconst(&mut self) -> Result<DefConst, String> {
        self.expect(Token::LeftParen)?;
        self.expect(Token::DefConst)?;
        let name = self.parse_ident()?;
        let body = self.parse_expr()?;
        self.expect(Token::RightParen)?;

        Ok(DefConst {
            name,
            docs: None,
            body,
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
    }

    #[test]
    fn test_parse_defconst_type_ann() {
        let input = "(defconst MY_INT:integer\n  1)\n(defconst MY_BOOL:bool\n  true)";
        let mut parser = Parser::new(input);

        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        match &result.clone().unwrap()[0] {
            PactTree::DefConst(def_const) => {
                assert_eq!(def_const.name.name, "MY_INT");
                assert_eq!(def_const.name.type_ann, Some(Type::Integer));
                match &def_const.body {
                    Expr::Constant(Literal::Integer(val)) => assert_eq!(val, "1"),
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
