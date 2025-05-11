use crate::{
    parser::ParseError,
    types::{Child, TreeKind},
    Tree,
};

struct ReportErrors<'e> {
    errors: &'e [ParseError],
    index: usize,
    indices: Vec<usize>,
}

impl<'e> ReportErrors<'e> {
    fn new(errors: &'e [ParseError]) -> ReportErrors<'e> {
        let index = 0;
        let indices = vec![];
        ReportErrors {
            errors,
            index,
            indices,
        }
    }

    fn collect(&mut self, children: &[Child]) {
        let start = self.index;
        for child in children {
            if let Child::Tree(tree) = child {
                if matches!(tree.kind, TreeKind::ErrorTree) {
                    if start == self.index {
                        self.indices.push(self.index);
                    }
                    self.index += 1;
                } else {
                    self.collect(&tree.children);
                }
            }
        }
    }

    fn finish(self) -> Vec<&'e ParseError> {
        self.indices
            .iter()
            .filter_map(|index| self.errors.get(*index))
            .collect()
    }
}

/// Traverses the given [`Tree`] to find the first [`ParseError`] to report.
pub fn report_errors<'e>(trees: &[Tree], errors: &'e [ParseError]) -> Vec<&'e ParseError> {
    let mut report_errors = ReportErrors::new(errors);
    for tree in trees {
        report_errors.collect(&tree.children);
    }
    report_errors.finish()
}

#[cfg(test)]
mod tests {
    use crate::{
        error::ReportErrors,
        parser::ParseError,
        types::{Child, TreeKind},
        Tree,
    };

    #[test]
    fn reduce_error_selects_first() {
        let errors = &[
            ParseError {
                message: "1".to_string(),
                line: 1,
                column: 1,
            },
            ParseError {
                message: "2".to_string(),
                line: 2,
                column: 2,
            },
        ];

        let mut report_errors = ReportErrors::new(errors);

        report_errors.collect(&[
            Child::Tree(Tree {
                kind: TreeKind::ErrorTree,
                children: vec![],
            }),
            Child::Tree(Tree {
                kind: TreeKind::ErrorTree,
                children: vec![],
            }),
        ]);

        assert_eq!(&report_errors.indices, &[0]);
    }

    #[test]
    fn reduce_error_selects_deep_first() {
        let errors = &[
            ParseError {
                message: "1".to_string(),
                line: 1,
                column: 1,
            },
            ParseError {
                message: "2".to_string(),
                line: 2,
                column: 2,
            },
            ParseError {
                message: "3".to_string(),
                line: 3,
                column: 3,
            },
            ParseError {
                message: "4".to_string(),
                line: 4,
                column: 4,
            },
        ];

        let mut report_errors = ReportErrors::new(errors);

        report_errors.collect(&[
            Child::Tree(Tree {
                kind: TreeKind::Module,
                children: vec![
                    Child::Tree(Tree {
                        kind: TreeKind::ErrorTree,
                        children: vec![],
                    }),
                    Child::Tree(Tree {
                        kind: TreeKind::ErrorTree,
                        children: vec![],
                    }),
                ],
            }),
            Child::Tree(Tree {
                kind: TreeKind::Module,
                children: vec![
                    Child::Tree(Tree {
                        kind: TreeKind::ErrorTree,
                        children: vec![],
                    }),
                    Child::Tree(Tree {
                        kind: TreeKind::ErrorTree,
                        children: vec![],
                    }),
                ],
            }),
        ]);

        assert_eq!(&report_errors.indices, &[0, 2]);
    }
}
