use syntax::types::{Child, Tree, TreeKind};

use crate::format_tree::{ListItem, SpecialForm, Wrapped, FST};

pub fn lower_file(tree: Tree) -> Vec<FST> {
    tree.children.into_iter().map(lower_child).collect()
}

pub fn lower_child(child: Child) -> FST {
    match child {
        Child::Token(token) => FST::Literal(token),
        Child::Tree(tree) => match tree.kind {
            TreeKind::Module => lower_module(&tree),
            TreeKind::ListLiteral => lower_list(&tree),
            _ => panic!("Unsupported tree kind: {:?}", tree.kind),
        },
    }
}

fn lower_module(tree: &Tree) -> FST {
    FST::SpecialForm(Wrapped {
        open: extract_token(&tree.children[0], "open paren"),
        inner: SpecialForm {
            keyword: extract_token(&tree.children[1], "module keyword"),
            sections: vec![
                FST::Literal(extract_token(&tree.children[2], "name")),
                FST::Literal(extract_token(&tree.children[3], "governance")),
            ],
            body: vec![],
        },
        close: extract_token(&tree.children[4], "close paren"),
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
