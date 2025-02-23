use crate::format_tree::*;
use syntax::cst::*;

pub fn lower_toplevel(toplevel: Toplevel) -> FST {
    match toplevel {
        Toplevel::Expr(expr) => lower_expr(expr),
        Toplevel::Defun(defun) => lower_defun(defun),
    }
}

fn lower_expr(expr: Expr) -> FST {
    match expr {
        Expr::Literal(lit) => {
            let value = match lit.value {
                LiteralValue::String(s) => s,
                LiteralValue::Symbol(s) => s,
                LiteralValue::Integer(s) => s,
                LiteralValue::Decimal(s) => s,
                LiteralValue::Boolean(s) => s,
            };

            FST::Literal(SourceToken {
                leading: lit.leading,
                value,
                trailing: lit.trailing,
            })
        }

        Expr::Identifier(ident) => {
            let value = if let Some(type_ann) = ident.value.type_annotation {
                format!("{}:{}", ident.value.identifier, format_type(type_ann))
            } else {
                ident.value.identifier
            };

            FST::Literal(SourceToken {
                leading: ident.leading,
                value,
                trailing: ident.trailing,
            })
        }

        Expr::List(list) => FST::List(Wrapped {
            open: SourceToken {
                leading: list.left_bracket.leading,
                value: "[".to_string(),
                trailing: list.left_bracket.trailing,
            },
            // Lists are optionally followed by commas, but the formatter should
            // always strip these while preserving comments.
            inner: list
                .members
                .into_iter()
                .map(|(mut expr, separator)| {
                    if let Some(sep) = separator {
                        if !sep.leading.is_empty() || !sep.trailing.is_empty() {
                            match &mut expr {
                                Expr::Literal(lit) => {
                                    lit.trailing.extend(sep.leading);
                                    lit.trailing.extend(sep.trailing);
                                }
                                Expr::Identifier(ident) => {
                                    ident.trailing.extend(sep.leading);
                                    ident.trailing.extend(sep.trailing);
                                }
                                Expr::Application(app) => {
                                    app.right_paren.trailing.extend(sep.leading);
                                    app.right_paren.trailing.extend(sep.trailing);
                                }
                                Expr::List(list) => {
                                    list.right_bracket.trailing.extend(sep.leading);
                                    list.right_bracket.trailing.extend(sep.trailing);
                                }
                                Expr::Object(obj) => {
                                    obj.right_brace.trailing.extend(sep.leading);
                                    obj.right_brace.trailing.extend(sep.trailing);
                                }
                            }
                        }
                    }
                    lower_expr(expr)
                })
                .collect(),
            close: SourceToken {
                leading: list.right_bracket.leading,
                value: "]".to_string(),
                trailing: list.right_bracket.trailing,
            },
        }),

        Expr::Object(obj) => FST::Object(Wrapped {
            open: SourceToken {
                leading: obj.left_brace.leading,
                value: "{".to_string(),
                trailing: obj.left_brace.trailing,
            },
            //Object(Wrapped<Vec<ObjectItem>>),
            inner: obj
                .members
                .into_iter()
                .map(|(key, colon, value)| ObjectItem {
                    key,
                    sep: SourceToken {
                        leading: colon.leading,
                        value: ":".to_string(),
                        trailing: colon.trailing,
                    },
                    value: lower_expr(value),
                })
                .collect(),
            close: SourceToken {
                leading: obj.right_brace.leading,
                value: "}".to_string(),
                trailing: obj.right_brace.trailing,
            },
        }),

        Expr::Application(app) => FST::SExp(Wrapped {
            open: SourceToken {
                leading: app.left_paren.leading,
                value: "(".to_string(),
                trailing: app.left_paren.trailing,
            },
            inner: std::iter::once(*app.func)
                .chain(app.args)
                .map(lower_expr)
                .collect(),
            close: SourceToken {
                leading: app.right_paren.leading,
                value: ")".to_string(),
                trailing: app.right_paren.trailing,
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
            leading: defun.left_paren.leading,
            value: "(".to_string(),
            trailing: defun.left_paren.trailing,
        },
        inner: SpecialForm {
            keyword: SourceToken {
                leading: defun.defun.leading,
                value: "defun".to_string(),
                trailing: defun.defun.trailing,
            },
            sections: vec![
                lower_expr(Expr::Identifier(defun.name)),
                FST::List(Wrapped {
                    open: SourceToken {
                        leading: defun.arguments.left_paren.leading,
                        value: "(".to_string(),
                        trailing: defun.arguments.left_paren.trailing,
                    },
                    inner: defun
                        .arguments
                        .args
                        .into_iter()
                        .map(|tok| lower_expr(Expr::Identifier(tok)))
                        .collect(),
                    close: SourceToken {
                        leading: defun.arguments.right_paren.leading,
                        value: ")".to_string(),
                        trailing: defun.arguments.right_paren.trailing,
                    },
                }),
            ],
            body: defun.body.into_iter().map(lower_expr).collect(),
        },
        close: SourceToken {
            leading: defun.right_paren.leading,
            value: ")".to_string(),
            trailing: defun.right_paren.trailing,
        },
    })
}
