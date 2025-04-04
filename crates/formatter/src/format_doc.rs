use pretty::{Doc, DocAllocator, DocBuilder};
use std::cmp;
use std::convert::identity;
use syntax::types::Trivia;

/// A hint to the pretty printer to break a line or add a space. For example,
/// a user-provided newline should break the line even if the pretty layout
/// algorithm would otherwise prefer to keep the line unbroken.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Force {
    None,
    Space,
    Break,
}

/// Apply a force to a doc.
fn break_doc<'a, D: DocAllocator<'a, ()>>(
    force: Force,
    doc: DocBuilder<'a, D, ()>,
) -> DocBuilder<'a, D, ()> {
    if doc_is_nil(&doc) {
        doc
    } else {
        let allocator = doc.0;
        match force {
            Force::None => doc,
            Force::Space => allocator.space().append(doc),
            Force::Break => allocator.hardline().append(doc),
        }
    }
}

/// Apply a force break to a document, either breaking the line or applying the given
/// transformation function.
fn apply_force<'a, D: DocAllocator<'a, ()>>(
    k: impl Fn(DocBuilder<'a, D, ()>) -> DocBuilder<'a, D, ()>,
    force: Force,
    doc: DocBuilder<'a, D, ()>,
) -> DocBuilder<'a, D, ()> {
    match force {
        Force::Break => doc.0.hardline().append(doc),
        _ => k(doc),
    }
}

/// Creates a new doc from a force break and number of lines. If lines are specified,
/// then breaks always apply regardless of the Force option. Otherwise we fall back
/// to the force break (break -> break, space -> space, none -> empty doc)
fn breaks<'a, D: DocAllocator<'a, ()>>(
    allocator: &'a D,
    force: Force,
    n: usize,
) -> DocBuilder<'a, D, ()> {
    if n >= 2 {
        allocator.hardline().append(allocator.hardline())
    } else if n == 1 {
        allocator.hardline()
    } else {
        match force {
            Force::Break => allocator.hardline(),
            Force::Space => allocator.space(),
            Force::None => allocator.nil(),
        }
    }
}

/// Check if a DocBuilder is nil.
pub fn doc_is_nil<'a, D: DocAllocator<'a, ()>>(doc: &DocBuilder<'a, D, ()>) -> bool {
    matches!(&**doc, Doc::Nil)
}

/// Leading comments and/or spacing which precede a token.
#[derive(Clone)]
pub struct Leading<'a, D: DocAllocator<'a, ()>>
where
    D::Doc: Clone,
{
    pub doc: DocBuilder<'a, D, ()>,
    pub left: Force,
    pub lines: usize,
    pub right: Force,
}

impl<'a, D: DocAllocator<'a, ()>> Leading<'a, D>
where
    D::Doc: Clone,
{
    pub fn nil(allocator: &'a D) -> Self {
        Self {
            doc: allocator.nil(),
            left: Force::None,
            lines: 0,
            right: Force::None,
        }
    }

    pub fn append(self, other: Self) -> Self {
        // If first doc is empty, return second with combined metadata
        if doc_is_nil(&self.doc) {
            return Self {
                left: cmp::max(self.left, other.left),
                lines: self.lines + other.lines,
                ..other
            };
        }

        let allocator = self.doc.0;

        // If second doc is empty, append line breaks to first
        if doc_is_nil(&other.doc) {
            return Self {
                doc: self.doc.append(breaks(allocator, Force::None, other.lines)),
                right: if other.lines > 0 {
                    Force::None
                } else {
                    cmp::max(self.right, other.right)
                },
                ..self
            };
        }

        // Both docs have content, so join them together.
        let br = cmp::max(self.right, other.left);
        if other.lines > 0 || br == Force::Break {
            Self {
                doc: self
                    .doc
                    .append(breaks(allocator, Force::Break, other.lines))
                    .append(other.doc),
                right: other.right,
                ..self
            }
        } else {
            Self {
                doc: self.doc.append(break_doc(br, other.doc)),
                right: other.right,
                ..self
            }
        }
    }
}

/// Trailing comment which follows a token; unlike leading comments, lines
/// are not recorded, as you can at maximum have a single comment.
#[derive(Clone)]
pub struct Trailing<'a, D: DocAllocator<'a, ()>>
where
    D::Doc: Clone,
{
    pub doc: DocBuilder<'a, D, ()>,
    pub left: Force,
    pub right: Force,
}

impl<'a, D: DocAllocator<'a, ()>> Trailing<'a, D>
where
    D::Doc: Clone,
{
    fn nil(allocator: &'a D) -> Self {
        Self {
            doc: allocator.nil(),
            left: Force::None,
            right: Force::None,
        }
    }

    fn append(self, other: Self) -> Self {
        if doc_is_nil(&self.doc) {
            return Self {
                left: cmp::max(self.left, other.left),
                ..other
            };
        }

        if doc_is_nil(&other.doc) {
            return Self {
                right: cmp::max(self.right, other.right),
                ..self
            };
        }

        Self {
            doc: self
                .doc
                .append(break_doc(cmp::max(self.right, other.left), other.doc)),
            right: other.right,
            ..self
        }
    }
}

fn leading_line_comment<'a, D>(str: String, doc: FormatDoc<'a, D>) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let allocator = doc.doc.0;
    let comm = Leading {
        doc: allocator.text(str),
        left: Force::Break,
        lines: 0,
        right: Force::Break,
    };

    FormatDoc {
        leading: comm.append(doc.leading),
        is_empty: false,
        ..doc
    }
}

fn trailing_line_comment<'a, D>(str: String, doc: FormatDoc<'a, D>) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let allocator = doc.doc.0;
    let comm = Trailing {
        doc: allocator.text(str),
        left: Force::Space,
        right: Force::Break,
    };

    FormatDoc {
        trailing: comm.append(doc.trailing),
        is_empty: false,
        ..doc
    }
}

fn format_comment<'a, D>(
    line_comment: impl Fn(String, FormatDoc<'a, D>) -> FormatDoc<'a, D>,
    com: &Trivia,
    next: FormatDoc<'a, D>,
) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    match com {
        Trivia::Comment(str) => line_comment(str.to_string(), next),
        Trivia::Space(_) => next,
        Trivia::Line(n) => source_break(*n, next),
    }
}

pub fn format_with_comments<'a, D>(
    leading: &[Trivia],
    trailing: &[Trivia],
    doc: FormatDoc<'a, D>,
) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    let mut result = doc;
    for comment in leading.iter().rev() {
        result = format_comment(leading_line_comment, comment, result);
    }
    for comment in trailing.iter().rev() {
        result = format_comment(trailing_line_comment, comment, result);
    }
    result
}

fn source_break<'a, D>(n: usize, doc: FormatDoc<'a, D>) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    FormatDoc {
        is_empty: false,
        leading: Leading {
            lines: doc.leading.lines + n,
            ..doc.leading
        },
        ..doc
    }
}

// An expansion over the `pretty::DocBuilder` that includes leading and trailing
/// comments and spacing. These in turn are used to respect user choices around
/// when to break lines when formatting.
#[derive(Clone)]
pub struct FormatDoc<'a, D: DocAllocator<'a, ()>>
where
    D::Doc: Clone,
{
    pub doc: DocBuilder<'a, D, ()>,
    pub is_empty: bool,
    pub leading: Leading<'a, D>,
    pub trailing: Trailing<'a, D>,
}

impl<'a, D: DocAllocator<'a, ()>> FormatDoc<'a, D>
where
    D::Doc: Clone,
{
    pub fn nil(allocator: &'a D) -> Self {
        Self {
            doc: allocator.nil(),
            is_empty: true,
            leading: Leading::nil(allocator),
            trailing: Trailing::nil(allocator),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.is_empty
    }

    pub fn text(allocator: &'a D, value: impl Into<String>) -> Self {
        Self::from_doc(allocator.text(value.into()))
    }

    pub fn from_doc(doc: DocBuilder<'a, D, ()>) -> Self {
        let allocator = doc.0;
        if matches!(*doc, Doc::Nil) {
            Self::nil(allocator)
        } else {
            Self {
                doc,
                is_empty: false,
                leading: Leading::nil(allocator),
                trailing: Trailing::nil(allocator),
            }
        }
    }

    /// Convert the FormatDoc to a DocBuilder, applying leading and trailing
    /// comments and spacing.
    pub fn docbuilder(self) -> DocBuilder<'a, D, ()> {
        let allocator = self.doc.0;
        if self.is_empty {
            allocator.nil()
        } else {
            let leading = self.leading;
            let trailing = self.trailing;
            (leading.doc)
                .append(break_doc(leading.right, self.doc))
                .append(break_doc(trailing.left, trailing.doc))
        }
    }

    pub fn pretty(self, width: usize) -> String {
        let formatted = self.docbuilder().pretty(width).to_string();

        // We have to manually trim spaces from lines followed by a newline
        // because `pretty` always adds spaces as indentation.
        // https://github.com/Marwes/pretty.rs/issues/85
        let mut result = String::with_capacity(formatted.len());
        let mut last_pos = 0;

        for (i, c) in formatted.char_indices() {
            if c == '\n' {
                let line = &formatted[last_pos..i];
                let trimmed = line.trim_end();
                result.push_str(trimmed);
                result.push('\n');
                last_pos = i + 1;
            }
        }

        // We need special handling for the last line because the loop above
        // only processes lines that end with a newline character.
        // If the formatted string doesn't end with a newline, we need to
        // manually process the remaining text after the last newline.
        if last_pos < formatted.len() {
            let last_line = &formatted[last_pos..].trim_end();
            result.push_str(last_line);
        }

        // Ensure the file ends with exactly one newline
        if result.ends_with('\n') {
            while result.ends_with("\n\n") {
                result.pop();
            }
        } else {
            result.push('\n');
        }

        result
    }

    pub fn append(self, other: Self) -> Self {
        join_docs(self, other, |force, doc| apply_force(identity, force, doc))
    }

    /// joins with space()
    pub fn join_space(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.space().append(d), force, doc)
        })
    }

    /// Joins with line(), which is a space break
    pub fn join_line(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.line().append(d), force, doc)
        })
    }

    /// Joins with line_(), which is a soft break
    pub fn join_line_(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.line_().append(d), force, doc)
        })
    }

    pub fn group(self) -> Self {
        Self {
            doc: self.doc.group(),
            ..self
        }
    }

    pub fn nest(self, n: isize) -> Self {
        Self {
            doc: self.doc.nest(n),
            ..self
        }
    }
}

/// Join two FormatDoc instances together given a spacing function. Primarily used to
/// implement other helpers like space, break, etc.
fn join_docs<'a, D: DocAllocator<'a, ()>>(
    doc1: FormatDoc<'a, D>,
    doc2: FormatDoc<'a, D>,
    space_fn: impl Fn(Force, DocBuilder<'a, D, ()>) -> DocBuilder<'a, D, ()>,
) -> FormatDoc<'a, D>
where
    D::Doc: Clone,
{
    if doc1.is_empty() {
        doc2
    } else if doc2.is_empty() {
        doc1
    } else {
        let allocator = doc1.doc.0;
        let comm1 = doc1.trailing;
        let comm2 = doc2.leading;

        let doc_left = doc1.doc.append(break_doc(comm1.left, comm1.doc));
        let doc_right = comm2.doc.append(break_doc(comm2.right, doc2.doc));

        if comm2.lines > 0 {
            FormatDoc {
                doc: doc_left
                    .append(breaks(allocator, Force::Break, comm2.lines))
                    .append(doc_right),
                trailing: doc2.trailing,
                ..doc1
            }
        } else {
            let doc3 = space_fn(cmp::max(comm1.right, comm2.left), doc_right);
            FormatDoc {
                doc: doc_left.append(doc3),
                trailing: doc2.trailing,
                ..doc1
            }
        }
    }
}
