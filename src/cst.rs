use crate::format::{SourceToken, Spacing, SpecialForm, Wrapped, FST};

/// Spacing that comes before any given token.
/// For arbitrary tokens, this is best paired like (PrefixSpacing, Token)
/// But for tokens with known kinds such as '(', the field name usually suffices
pub type PrefixSpacing = Vec<Spacing>;

/// A type to hold positional information, including:
///   - Optional leading whitespace/comments
///   - The token
pub type Positioned<T> = (PrefixSpacing, T);

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    String(String),
    Symbol(String),
    Integer(String),
    Decimal(String),
    Boolean(String),
}

pub type Literal = Positioned<LiteralValue>;

/// A reference like "my-mod.my-name" or "a.b.c.d"
#[derive(Debug, Clone, PartialEq)]
pub struct Reference {
    pub first: String,
    pub second: String,
    pub rest: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Named {
    Ident(String),
    Reference(Reference),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Simple identifier type like :integer or :time
    Ident(String),
    /// List type like [integer] or [object{schema}]
    List(Box<Type>),
    /// Object type like object{schema} or object{}
    Object(Option<String>),
    /// Schema type like {schema}
    Schema(String),
    /// Module type like module{schema,other.schema}
    Module(Vec<Named>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierFields {
    pub identifier: String,
    pub type_annotation: Option<Type>,
}

pub type Identifier = Positioned<IdentifierFields>;

#[derive(Debug, PartialEq, Clone)]
pub struct App {
    pub left_paren: PrefixSpacing,
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub left_bracket: PrefixSpacing,
    pub members: Vec<Expr>,
    pub right_bracket: PrefixSpacing,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal),
    Application(App),
    List(List),
}

#[derive(Debug, PartialEq)]
pub struct Arguments {
    pub left_paren: PrefixSpacing,
    pub args: Vec<Identifier>,
    pub right_paren: PrefixSpacing,
}

// FIXME: Missing doc/model annotations and bare docstrings. Will probably
// represent each with their own field and typed where the annotation is
// an Option<PrefixSpacing> for doc or a required PrefixSpacing for model,
// and a string for content for a doc and a ...list expr for model(?)
#[derive(Debug, PartialEq)]
pub struct Defun {
    pub left_paren: PrefixSpacing,
    pub defun: PrefixSpacing,
    pub name: Identifier,
    pub arguments: Arguments,
    // FIXME: Technically you can't have a list of expressions in the body,
    // it's really just an optional doc annotation, optional model, and then
    // body, which is a single expr.
    pub body: Vec<Expr>,
    pub right_paren: PrefixSpacing,
}

#[derive(Debug, PartialEq)]
pub enum Toplevel {
    Defun(Defun),
    Expr(Expr),
}

pub fn lower_toplevel(toplevel: Toplevel) -> FST {
    match toplevel {
        Toplevel::Expr(expr) => lower_expr(expr),
        Toplevel::Defun(defun) => lower_defun(defun),
    }
}

fn lower_expr(expr: Expr) -> FST {
    match expr {
        Expr::Literal((spacing, lit_value)) => {
            let value = match lit_value {
                LiteralValue::String(s) => s,
                LiteralValue::Symbol(s) => s,
                LiteralValue::Integer(s) => s,
                LiteralValue::Decimal(s) => s,
                LiteralValue::Boolean(s) => s,
            };

            FST::Literal(SourceToken {
                leading: spacing,
                value,
                trailing: vec![],
            })
        }

        Expr::Identifier((spacing, fields)) => {
            let value = if let Some(type_ann) = fields.type_annotation {
                format!("{}:{}", fields.identifier, format_type(type_ann))
            } else {
                fields.identifier
            };

            FST::Literal(SourceToken {
                leading: spacing,
                value,
                trailing: vec![],
            })
        }

        Expr::List(list) => FST::List(Wrapped {
            open: SourceToken {
                leading: list.left_bracket,
                value: "[".to_string(),
                trailing: vec![],
            },
            inner: list.members.into_iter().map(lower_expr).collect(),
            close: SourceToken {
                leading: list.right_bracket,
                value: "]".to_string(),
                trailing: vec![],
            },
        }),

        Expr::Application(app) => FST::SExp(Wrapped {
            open: SourceToken {
                leading: app.left_paren,
                value: "(".to_string(),
                trailing: vec![],
            },
            inner: std::iter::once(*app.func)
                .chain(app.args)
                .map(lower_expr)
                .collect(),
            close: SourceToken {
                leading: app.right_paren,
                value: ")".to_string(),
                trailing: vec![],
            },
        }),
    }
}

fn format_type(type_ann: Type) -> String {
    match type_ann {
        Type::Ident(s) => s,
        Type::List(inner) => format!("[{}]", format_type(*inner)),
        Type::Object(schema) => match schema {
            Some(s) => format!("object{{{}}}", s),
            None => "object{}".to_string(),
        },
        Type::Schema(s) => format!("{{{}}}", s),
        Type::Module(names) => {
            let names = names
                .into_iter()
                .map(|n| match n {
                    Named::Ident(s) => s,
                    Named::Reference(r) => {
                        let mut parts = vec![r.first, r.second];
                        parts.extend(r.rest);
                        parts.join(".")
                    }
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("module{{{}}}", names)
        }
    }
}

fn lower_defun(defun: Defun) -> FST {
    FST::SpecialForm(Wrapped {
        open: SourceToken {
            leading: defun.left_paren,
            value: "(".to_string(),
            trailing: vec![],
        },
        inner: SpecialForm {
            keyword: SourceToken {
                leading: defun.defun,
                value: "defun".to_string(),
                trailing: vec![],
            },
            sections: vec![
                lower_expr(Expr::Identifier(defun.name)),
                FST::List(Wrapped {
                    open: SourceToken {
                        leading: defun.arguments.left_paren,
                        value: "(".to_string(),
                        trailing: vec![],
                    },
                    inner: defun
                        .arguments
                        .args
                        .into_iter()
                        .map(|arg| lower_expr(Expr::Identifier(arg)))
                        .collect(),
                    close: SourceToken {
                        leading: defun.arguments.right_paren,
                        value: ")".to_string(),
                        trailing: vec![],
                    },
                }),
            ],
            body: defun.body.into_iter().map(lower_expr).collect(),
        },
        close: SourceToken {
            leading: defun.right_paren,
            value: ")".to_string(),
            trailing: vec![],
        },
    })
}
