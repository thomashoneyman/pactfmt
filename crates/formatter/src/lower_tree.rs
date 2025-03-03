use syntax::types::{Child, Tree, TreeKind};

use crate::format_tree::{SpecialForm, Wrapped, FST};

pub fn lower_program(tree: Tree) -> Vec<FST> {
    let mut fsts = vec![];
    // Naively assumes the tree is a program.
    for child in tree.children {
        fsts.push(lower_child(child));
    }
    fsts
}

pub fn lower_child(child: Child) -> FST {
    match child {
        Child::Token(token) => FST::Literal(token),
        Child::Tree(tree) => match tree.kind {
            TreeKind::Module => FST::SpecialForm(Wrapped {
                open: match tree.children[0].clone() {
                    Child::Token(token) => token,
                    Child::Tree(tree) => panic!("Expected token, got tree {:?}", tree),
                },
                inner: SpecialForm {
                    keyword: match tree.children[1].clone() {
                        Child::Token(token) => token,
                        Child::Tree(tree) => panic!("Expected token, got tree {:?}", tree),
                    },
                    sections: vec![
                        // name
                        match tree.children[2].clone() {
                            Child::Token(token) => FST::Literal(token),
                            Child::Tree(tree) => panic!("Expected token, got tree {:?}", tree),
                        },
                        // governance
                        match tree.children[3].clone() {
                            Child::Token(token) => FST::Literal(token),
                            Child::Tree(tree) => panic!("Expected token, got tree {:?}", tree),
                        },
                    ],
                    // TODO: body not supported yet
                    body: vec![],
                },
                close: match tree.children[4].clone() {
                    Child::Token(token) => token,
                    Child::Tree(tree) => panic!("Expected token, got tree {:?}", tree),
                },
            }),
            _ => panic!("Unsupported tree kind: {:?}", tree.kind),
        },
    }
}
