use syntax::types::{Child, Tree, TreeKind};

use crate::format_tree::{ListItem, SpecialForm, Wrapped, FST};

pub fn lower_file(tree: Tree) -> Vec<FST> {
    tree.children.into_iter().map(lower_child).collect()
}

pub fn lower_child(child: Child) -> FST {
    match child {
        Child::Token(token) => FST::Literal(token),
        Child::Tree(tree) => match tree.kind {
            // Toplevel
            TreeKind::Module => lower_module(&tree),

            // Defs
            TreeKind::Defun => lower_defun(&tree),
            TreeKind::Defcap => lower_defcap(&tree),

            // Exprs
            TreeKind::App => lower_app(&tree),
            TreeKind::List => lower_list(&tree),
            TreeKind::IntLiteral => lower_int(&tree),
            TreeKind::DecimalLiteral => lower_decimal(&tree),

            // Other
            TreeKind::ParamList => lower_param_list(&tree),

            _ => panic!("Unsupported tree kind: {:?}", tree.kind),
        },
    }
}

fn lower_module(tree: &Tree) -> FST {
    let body_items: Vec<FST> = tree.children[4..tree.children.len() - 1]
        .iter()
        .map(|child| lower_child(child.clone()))
        .collect();

    FST::SpecialForm(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: SpecialForm {
            keyword: extract_token(&tree.children[1], "module keyword"),
            sections: vec![
                FST::Literal(extract_token(&tree.children[2], "name")),
                FST::Literal(extract_token(&tree.children[3], "governance")),
            ],
            body: body_items,
        },
        close: extract_token(&tree.children[tree.children.len() - 1], "close paren"),
    })
}

fn extract_token(child: &Child, expected: &str) -> syntax::types::SourceToken {
    match child {
        Child::Token(token) => token.clone(),
        _ => panic!("Expected token for {}", expected),
    }
}

fn lower_list(tree: &Tree) -> FST {
    let mut items = Vec::new();
    let mut current_item: Option<ListItem> = None;

    for i in 1..tree.children.len() - 1 {
        match &tree.children[i] {
            Child::Token(token) if token.text == "," => {
                if let Some(item) = current_item.take() {
                    items.push(ListItem {
                        value: item.value,
                        comma: Some(token.clone()),
                    });
                } else {
                    panic!("Found comma with no preceding list item");
                }
            }
            child => {
                if let Some(item) = current_item.take() {
                    items.push(item);
                }

                current_item = Some(ListItem {
                    value: lower_child(child.clone()),
                    comma: None,
                });
            }
        }
    }

    if let Some(item) = current_item {
        items.push(item);
    }

    FST::List(Wrapped {
        open: extract_token(&tree.children[0], "open bracket"),
        inner: items,
        close: extract_token(&tree.children[tree.children.len() - 1], "close bracket"),
    })
}

fn lower_int(tree: &Tree) -> FST {
    // IntLiteral trees should contain a single Number token
    if tree.children.len() != 1 {
        panic!("IntLiteral should have exactly one child");
    }
    FST::Literal(extract_token(&tree.children[0], "integer"))
}

fn lower_decimal(tree: &Tree) -> FST {
    if tree.children.len() < 3 {
        panic!("DecimalLiteral should have at least 3 children");
    }

    let first_token = extract_token(&tree.children[0], "decimal integer part");
    let last_token = extract_token(
        &tree.children[tree.children.len() - 1],
        "decimal fractional part",
    );

    let mut combined_text = String::new();
    for child in &tree.children {
        if let Child::Token(token) = child {
            combined_text.push_str(&token.text);
        } else {
            panic!("Expected token in DecimalLiteral");
        }
    }

    let mut combined_token = first_token.clone();
    combined_token.text = combined_text;
    combined_token.range.end = last_token.range.end;

    FST::Literal(combined_token)
}

fn lower_defun(tree: &Tree) -> FST {
    FST::SpecialForm(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: SpecialForm {
            keyword: extract_token(&tree.children[1], "defun keyword"),
            sections: vec![
                FST::Literal(extract_token(&tree.children[2], "function name")),
                lower_child(tree.children[3].clone()), // param list
            ],
            body: tree.children[4..tree.children.len() - 1]
                .iter()
                .map(|child| lower_child(child.clone()))
                .collect(),
        },
        close: extract_token(&tree.children[tree.children.len() - 1], "close paren"),
    })
}

fn lower_defcap(tree: &Tree) -> FST {
    FST::SpecialForm(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: SpecialForm {
            keyword: extract_token(&tree.children[1], "defcap keyword"),
            sections: vec![
                FST::Literal(extract_token(&tree.children[2], "capability name")),
                lower_child(tree.children[3].clone()),
            ],
            body: tree.children[4..tree.children.len() - 1]
                .iter()
                .map(|child| lower_child(child.clone()))
                .collect(),
        },
        close: extract_token(&tree.children[tree.children.len() - 1], "close paren"),
    })
}

fn lower_param_list(tree: &Tree) -> FST {
    let mut params = Vec::new();

    for i in 1..tree.children.len() - 1 {
        params.push(lower_child(tree.children[i].clone()));
    }

    FST::List(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: params
            .into_iter()
            .map(|param| ListItem {
                value: param,
                comma: None,
            })
            .collect(),
        close: extract_token(&tree.children[tree.children.len() - 1], "close paren"),
    })
}

fn lower_app(tree: &Tree) -> FST {
    let func_name = extract_token(&tree.children[1], "function name");

    let args: Vec<FST> = tree.children[2..tree.children.len() - 1]
        .iter()
        .map(|child| lower_child(child.clone()))
        .collect();

    FST::SpecialForm(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: SpecialForm {
            keyword: func_name,
            sections: vec![],
            body: args,
        },
        close: extract_token(&tree.children[tree.children.len() - 1], "close paren"),
    })
}
