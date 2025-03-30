use syntax::types::{Child, SourceToken, Tree, TreeKind, TokenKind};

use crate::format_tree::{ListItem, SpecialForm, Wrapped, FST};

/// Lower a parsed tree into a format syntax tree suitable for
/// use with format_source and other formatting functions.
pub fn lower_tree(tree: Tree) -> FST {
    match tree.kind {
        // Toplevel
        TreeKind::Module => special_form(&tree, 4),

        // Defs
        TreeKind::Defun => special_form(&tree, 4),
        TreeKind::Defcap => special_form(&tree, 4),

        // Exprs
        TreeKind::App => sexp(&tree, 1),
        TreeKind::List => bracket_list(&tree),

        // Literals
        TreeKind::IntLiteral => literal(&tree, "integer"),
        TreeKind::DecimalLiteral => literal(&tree, "decimal"),

        // Other
        TreeKind::ParamList => paren_list(&tree),

        _ => panic!("Unsupported tree kind: {:?}", tree.kind),
    }
}

fn bracket_list(tree: &Tree) -> FST {
    list(tree, "bracket", true)
}

fn paren_list(tree: &Tree) -> FST {
    list(tree, "paren", false)
}

fn list(tree: &Tree, delim_desc: &str, allow_commas: bool) -> FST {
    if !allow_commas {
        let items = lower_children(tree, 1, 1);
        FST::List(Wrapped {
            open: open_token(tree, &format!("open {}", delim_desc)),
            inner: items
                .into_iter()
                .map(|item| ListItem {
                    value: item,
                    comma: None,
                })
                .collect(),
            close: close_token(tree, &format!("close {}", delim_desc)),
        })
    } else {
        let mut items = Vec::new();
        let mut current_item: Option<ListItem> = None;

        for i in 1..tree.children.len() - 1 {
            match &tree.children[i] {
                Child::Token(token) if token.kind == TokenKind::Comma => {
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
            open: open_token(tree, &format!("open {}", delim_desc)),
            inner: items,
            close: close_token(tree, &format!("close {}", delim_desc)),
        })
    }
}

// Extract opening token (always first child)
fn open_token(tree: &Tree, desc: &str) -> SourceToken {
    extract_token(&tree.children[0], desc)
}

// Extract closing token (always last child)
fn close_token(tree: &Tree, desc: &str) -> SourceToken {
    extract_token(&tree.children[tree.children.len() - 1], desc)
}

// Process children in a range
fn lower_children(tree: &Tree, start: usize, end: usize) -> Vec<FST> {
    tree.children[start..tree.children.len() - end]
        .iter()
        .map(|child| lower_child(child.clone()))
        .collect()
}

fn extract_children(tree: &Tree, desc: &str) -> Vec<SourceToken> {
    tree.children.iter()
        .map(|child| extract_token(child, desc))
        .collect()
}

// Helper for creating s-expressions
fn sexp(tree: &Tree, start: usize) -> FST {
    FST::SExp(Wrapped {
        open: open_token(tree, "open paren"),
        inner: lower_children(tree, start, 1),
        close: close_token(tree, "close paren"),
    })
}

// Helper function to create special forms with less boilerplate
fn special_form(
    tree: &Tree,
    body_start: usize,
) -> FST {
    let sections = tree.children[2..body_start]
        .iter()
        .map(|child| lower_child(child.clone()))
        .collect();

    FST::SpecialForm(Wrapped {
        open: open_token(tree, "open paren"),
        inner: SpecialForm {
            keyword: extract_token(&tree.children[1], "keyword"),
            sections,
            body: lower_children(tree, body_start, 1),
        },
        close: close_token(tree, "close paren"),
    })
}

// Helper function to lower a child to an FST
pub fn lower_child(child: Child) -> FST {
    match child {
        Child::Tree(tree) => lower_tree(tree),
        Child::Token(token) => FST::Literal(token),
    }
}

// Helper function to extract a token from a child
fn extract_token(child: &Child, expected: &str) -> SourceToken {
    match child {
        Child::Token(token) => token.clone(),
        _ => panic!("Expected token for {}", expected),
    }
}

// Helper function to create a literal from a token
fn literal(tree: &Tree, desc: &str) -> FST {
    if tree.children.len() == 1 {
        FST::Literal(open_token(tree, desc))
    } else {
        literal_tokens(&extract_children(tree, desc))
    }
}

// Helper function to create simple literals from tokens
fn literal_tokens(tokens: &[SourceToken]) -> FST {
    if tokens.is_empty() {
        panic!("Cannot create literal from empty tokens");
    }

    let first = &tokens[0];
    let last = &tokens[tokens.len() - 1];

    let mut combined_text = String::new();
    for token in tokens {
        combined_text.push_str(&token.text);
    }

    let mut combined_token = first.clone();
    combined_token.text = combined_text;
    combined_token.range.end = last.range.end.clone();

    FST::Literal(combined_token)
}
