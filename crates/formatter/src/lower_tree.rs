use syntax::types::{Child, SourceToken, TokenKind, Tree, TreeKind};

use crate::format_tree::{ListItem, ObjectItem, SpecialForm, Wrapped, FST};

/// Lower a parsed tree into a format syntax tree suitable for
/// use with format_source and other formatting functions.
pub fn lower_tree(tree: Tree) -> FST {
    match tree.kind {
        // Toplevel
        TreeKind::Module => special_form(&tree, 4),
        TreeKind::Interface => special_form(&tree, 3),

        // Module body
        TreeKind::Bless => sexp(&tree),
        TreeKind::Import => special_form(&tree, find_import_list_index_with_default(&tree, 2)),
        TreeKind::Implements => sexp(&tree),

        // Defs
        TreeKind::Defun => special_form(&tree, find_param_list_index(&tree) + 1),
        TreeKind::Defcap => special_form(&tree, find_param_list_index(&tree) + 1),
        TreeKind::Defconst => special_form(&tree, tree.children.len() - 2),
        TreeKind::Defschema => special_form(&tree, find_first_schema_field_index(&tree) + 1),
        TreeKind::Deftable => special_form(&tree, find_table_name_index(&tree) + 1),
        TreeKind::Defpact => special_form(&tree, find_param_list_index(&tree) + 1),

        // Interface defs
        TreeKind::IfDefun => special_form(&tree, find_param_list_index(&tree) + 1),
        TreeKind::IfDefcap => special_form(&tree, find_param_list_index(&tree) + 1),
        TreeKind::IfDefpact => special_form(&tree, find_param_list_index(&tree) + 1),

        // Exprs
        TreeKind::App => sexp(&tree),
        TreeKind::List => bracket_list(&tree),
        TreeKind::Let => special_form(&tree, 3),
        TreeKind::Object => object(&tree),
        TreeKind::Binding => object(&tree),

        // Specially-handled exprs
        TreeKind::If => special_form(&tree, 4),
        TreeKind::Cond => sexp(&tree),
        TreeKind::CondBranch => sexp(&tree),
        TreeKind::WithCapability => special_form(&tree, 3),
        TreeKind::WithDefaultRead => special_form(&tree, 4),
        TreeKind::WithRead => special_form(&tree, 4),
        TreeKind::Update => special_form(&tree, 4),
        TreeKind::Enforce => special_form(&tree, 4),
        TreeKind::Write => special_form(&tree, 4),

        // Expr parts
        TreeKind::ImportList => bracket_list(&tree),
        TreeKind::ParamList => paren_list(&tree),
        TreeKind::BindingList => paren_list(&tree),
        TreeKind::SchemaField => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::TableName => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::Binder => sexp(&tree),

        // Pact steps
        TreeKind::Step => sexp(&tree),
        TreeKind::StepWithRollback => sexp(&tree),
        TreeKind::Resume => sexp(&tree),

        // Literals
        TreeKind::IntLiteral => literal_token(&tree, "integer"),
        TreeKind::DecimalLiteral => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::MultilineString => multiline_string(&tree),

        // Names
        TreeKind::Name => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::ModRef => compound_literal(&extract_all_tokens(&tree)),

        // Types
        TreeKind::TypeAnn => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::Type => compound_literal(&extract_all_tokens(&tree)),
        TreeKind::PrimType => literal_token(&tree, "primitive type"),

        // Annotations
        TreeKind::DocAnn => unwrapped_annotation(&tree),
        TreeKind::ModelAnn => unwrapped_annotation(&tree),
        TreeKind::EventAnn => unwrapped_annotation(&tree),
        TreeKind::ManagedAnn => unwrapped_annotation(&tree),

        // Property expressions
        TreeKind::PropList => bracket_list(&tree),
        TreeKind::PropLet => sexp(&tree),
        TreeKind::PropBinder => sexp(&tree),
        TreeKind::PropLam => sexp(&tree),
        TreeKind::PropApp => sexp(&tree),
        TreeKind::PropDefProperty => {
            special_form(&tree, find_param_list_index_with_default(&tree, 2) + 1)
        }

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

// Helper for creating s-expressions
fn sexp(tree: &Tree) -> FST {
    FST::SExp(Wrapped {
        open: open_token(tree, "open paren"),
        inner: lower_children(tree, 1, 1),
        close: close_token(tree, "close paren"),
    })
}

// Helper function to create special forms with less boilerplate
fn special_form(tree: &Tree, body_start: usize) -> FST {
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

fn object(tree: &Tree) -> FST {
    fn process_item(chunk: &[Child], is_final: bool) -> ObjectItem {
        let key = extract_token(&chunk[0], "key");
        let sep = extract_token(&chunk[1], "separator");
        let value = lower_child(chunk[2].clone());

        let comma = if !is_final {
            Some(extract_token(&chunk[3], "comma"))
        } else {
            None
        };

        ObjectItem {
            key,
            sep,
            value,
            comma,
        }
    }

    let inner_children = &tree.children[1..tree.children.len() - 1];

    let mut items = Vec::new();
    let mut i = 0;

    while i + 3 <= inner_children.len() {
        let is_final = i + 3 == inner_children.len() || i + 4 == inner_children.len();
        let chunk_size = if is_final { 3 } else { 4 };
        items.push(process_item(&inner_children[i..i + chunk_size], is_final));
        i += chunk_size;
    }

    FST::Object(Wrapped {
        open: open_token(tree, "open brace"),
        inner: items,
        close: close_token(tree, "close brace"),
    })
}

// Helper function to lower a child to an FST
pub fn lower_child(child: Child) -> FST {
    match child {
        Child::Tree(tree) => lower_tree(tree),
        Child::Token(token) => FST::Literal(vec![token]),
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
fn literal_token(tree: &Tree, desc: &str) -> FST {
    if tree.children.len() == 1 {
        FST::Literal(vec![extract_token(&tree.children[0], desc)])
    } else {
        panic!("Expected single token for {}", desc);
    }
}

// Create a compound literal from a list of tokens (joined without spaces)
fn compound_literal(tokens: &[SourceToken]) -> FST {
    if tokens.is_empty() {
        panic!("Cannot create literal from empty tokens");
    }
    FST::CompoundLiteral(tokens.to_vec())
}

// Helper function to extract all tokens from a tree, recursively, essentially flattening
// them together.
fn extract_all_tokens(tree: &Tree) -> Vec<SourceToken> {
    let mut tokens = Vec::new();
    for child in &tree.children {
        match child {
            Child::Token(token) => tokens.push(token.clone()),
            Child::Tree(subtree) => tokens.extend(extract_all_tokens(subtree)),
        }
    }
    tokens
}

// Find the index of the parameter list, e.g. in a defun or defcap
fn find_param_list_index(tree: &Tree) -> usize {
    tree.children
        .iter()
        .position(|child| matches!(child, Child::Tree(t) if t.kind == TreeKind::ParamList))
        .expect("Expected parameter list but none found.")
}

fn find_param_list_index_with_default(tree: &Tree, default: usize) -> usize {
    tree.children
        .iter()
        .position(|child| matches!(child, Child::Tree(t) if t.kind == TreeKind::ParamList))
        .unwrap_or(default)
}

fn find_import_list_index_with_default(tree: &Tree, default: usize) -> usize {
    tree.children
        .iter()
        .position(|child| matches!(child, Child::Tree(t) if t.kind == TreeKind::ImportList))
        .unwrap_or(default)
}

// Find the index of the first schema field, e.g. in a defschema
fn find_first_schema_field_index(tree: &Tree) -> usize {
    tree.children
        .iter()
        .position(|child| matches!(child, Child::Tree(t) if t.kind == TreeKind::SchemaField))
        .expect("Expected schema field but none found.")
}

fn find_table_name_index(tree: &Tree) -> usize {
    tree.children
        .iter()
        .position(|child| matches!(child, Child::Tree(t) if t.kind == TreeKind::TableName))
        .expect("Expected table name but none found.")
}

fn unwrapped_annotation(tree: &Tree) -> FST {
    let annotation = match &tree.children[0] {
        Child::Token(token) => FST::Literal(vec![token.clone()]),
        _ => panic!("Expected token for annotation name"),
    };

    let content: Vec<FST> = tree.children[1..]
        .iter()
        .map(|child| lower_child(child.clone()))
        .collect();

    let mut items = vec![annotation];
    items.extend(content);

    FST::Unwrapped(items)
}

fn multiline_string(tree: &Tree) -> FST {
    if let Child::Token(token) = &tree.children[0] {
        // Extract the content without quotes
        let content = if token.text.len() >= 2 {
            &token.text[1..token.text.len() - 1]
        } else {
            return FST::Literal(vec![token.clone()]);
        };

        // Split on escaped newlines
        let lines: Vec<&str> = content.split("\\\n").collect();

        // Find minimum common indentation in continuation lines
        let min_indent = lines[1..]
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
            .min()
            .unwrap_or(0);

        // Process lines, removing common indentation
        let processed_lines: Vec<String> = lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                if i == 0 {
                    line.to_string() // Keep first line as is
                } else if line.len() > min_indent {
                    line[min_indent..].to_string() // Trim common indent
                } else {
                    line.to_string() // Line is shorter than min_indent
                }
            })
            .collect();

        return FST::MultilineString {
            leading: token.leading.clone(),
            trailing: token.trailing.clone(),
            lines: processed_lines,
        };
    }

    FST::Literal(extract_all_tokens(tree))
}
