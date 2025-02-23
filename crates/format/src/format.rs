use pretty::{Doc, DocAllocator, DocBuilder};
use std::{cmp, convert::identity};

#[derive(Debug, PartialEq, Clone)]
pub enum Spacing {
    Newline(usize),
    Comment(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceToken<T> {
    pub leading: Vec<Spacing>,
    pub value: T,
    pub trailing: Vec<Spacing>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Wrapped<T> {
    pub open: SourceToken<String>,
    pub inner: T,
    pub close: SourceToken<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ListItem {
    pub value: FST,
    pub comma: Option<SourceToken<String>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectItem {
    pub key: SourceToken<String>,
    pub sep: SourceToken<String>,
    pub value: FST,
}

/// A special form is a keyword followed by a list of sections and a body.
/// (defun NAME (arg1 arg2) body1 body2)
#[derive(Debug, PartialEq, Clone)]
pub struct SpecialForm {
    pub keyword: SourceToken<String>,
    pub sections: Vec<FST>,
    pub body: Vec<FST>,
}

/// A 'format syntax tree' is an intermediate representation of the CST simplified for formatting.
#[allow(dead_code)] // remove once we implement object
#[allow(clippy::upper_case_acronyms)] // Allow FST instead of Fst
#[derive(Debug, PartialEq, Clone)]
pub enum FST {
    Literal(SourceToken<String>),
    List(Wrapped<Vec<FST>>),
    Object(Wrapped<Vec<ObjectItem>>),
    SExp(Wrapped<Vec<FST>>),
    SpecialForm(Wrapped<SpecialForm>),
}

impl FST {
    pub fn format<'a, D>(&'a self, allocator: &'a D) -> FormatDoc<'a, D>
    where
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            FST::Literal(SourceToken {
                leading,
                value,
                trailing,
            }) => format_with_comments(leading, trailing, FormatDoc::text(allocator, value)),

            FST::List(Wrapped { open, inner, close }) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = inner.iter().map(|item| item.format(allocator));
                if let Some(first) = iter.next() {
                    result = first;
                    for doc in iter {
                        result = result.join_line(doc)
                    }
                }

                format_with_comments(
                    &open.leading,
                    &open.trailing,
                    FormatDoc::text(allocator, &open.value),
                )
                .join_line_(result)
                .nest(2)
                .join_line_(format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.value),
                ))
                .group()
            }

            FST::Object(Wrapped { open, inner, close }) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = inner.iter().map(|ObjectItem { key, sep, value }| {
                    format_with_comments(
                        &key.leading,
                        &key.trailing,
                        FormatDoc::text(allocator, key.value.clone()),
                    )
                    .append(format_with_comments(
                        &sep.leading,
                        &sep.trailing,
                        FormatDoc::text(allocator, sep.value.clone()),
                    ))
                    .join_space(value.format(allocator))
                    .nest(2)
                });
                if let Some(first) = iter.next() {
                    result = first;
                    for doc in iter {
                        result = result.join_line(doc)
                    }
                }

                format_with_comments(
                    &open.leading,
                    &open.trailing,
                    FormatDoc::text(allocator, &open.value),
                )
                .join_line_(result)
                .nest(2)
                .join_line_(format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.value),
                ))
                .group()
            }

            FST::SExp(Wrapped { open, inner, close }) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = inner.iter().map(|item| item.format(allocator));
                if let Some(first) = iter.next() {
                    result = first;
                    for doc in iter {
                        result = result.join_line(doc);
                    }
                }

                format_with_comments(
                    &open.leading,
                    &open.trailing,
                    FormatDoc::text(allocator, &open.value)
                        .append(result.nest(2))
                        .append(format_with_comments(
                            &close.leading,
                            &close.trailing,
                            FormatDoc::text(allocator, &close.value),
                        ))
                        .group(),
                )
            }

            FST::SpecialForm(Wrapped {
                open,
                inner:
                    SpecialForm {
                        keyword,
                        sections,
                        body,
                    },
                close,
            }) => {
                let open = format_with_comments(
                    &open.leading,
                    &open.trailing,
                    FormatDoc::text(allocator, &open.value),
                );

                let keyword = format_with_comments(
                    &keyword.leading,
                    &keyword.trailing,
                    FormatDoc::text(allocator, keyword.value.clone()),
                );

                let close = format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.value),
                );

                let mut sections_doc = FormatDoc::nil(allocator);
                let mut sections_iter = sections.iter().map(|item| item.format(allocator));
                if let Some(first) = sections_iter.next() {
                    sections_doc = first;
                    for doc in sections_iter {
                        sections_doc = sections_doc.join_line(doc);
                    }
                }

                let mut body_doc = FormatDoc::nil(allocator);
                let mut body_iter = body.iter().map(|item| item.format(allocator));
                if let Some(first) = body_iter.next() {
                    body_doc = first;
                    for doc in body_iter {
                        body_doc = body_doc.join_line(doc);
                    }
                }

                let content = if !doc_is_nil(&keyword.leading.doc) {
                    keyword
                        .join_line(sections_doc)
                        .group()
                        .join_line(body_doc)
                        .nest(2)
                } else {
                    keyword.join_space(sections_doc).group().join_line(body_doc)
                };

                open.append(content).nest(2).join_line_(close).group()
            }
        }
    }
}

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
fn doc_is_nil<'a, D: DocAllocator<'a, ()>>(doc: &DocBuilder<'a, D, ()>) -> bool {
    matches!(&**doc, Doc::Nil)
}

/// Leading comments and/or spacing which precede a token.
#[derive(Clone)]
pub struct Leading<'a, D: DocAllocator<'a, ()>>
where
    D::Doc: Clone,
{
    doc: DocBuilder<'a, D, ()>,
    left: Force,
    lines: usize,
    right: Force,
}

impl<'a, D: DocAllocator<'a, ()>> Leading<'a, D>
where
    D::Doc: Clone,
{
    fn nil(allocator: &'a D) -> Self {
        Self {
            doc: allocator.nil(),
            left: Force::None,
            lines: 0,
            right: Force::None,
        }
    }

    fn append(self, other: Self) -> Self {
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
    doc: DocBuilder<'a, D, ()>,
    left: Force,
    right: Force,
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
    com: &Spacing,
    next: FormatDoc<'a, D>,
) -> FormatDoc<'a, D>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    match com {
        Spacing::Comment(str) => line_comment(str.to_string(), next),
        Spacing::Newline(n) => source_break(*n, next),
    }
}

fn format_with_comments<'a, D>(
    leading: &[Spacing],
    trailing: &[Spacing],
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
    doc: DocBuilder<'a, D, ()>,
    is_empty: bool,
    leading: Leading<'a, D>,
    trailing: Trailing<'a, D>,
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

    fn is_empty(&self) -> bool {
        self.is_empty
    }

    fn text(allocator: &'a D, value: impl Into<String>) -> Self {
        Self::from_doc(allocator.text(value.into()))
    }

    fn from_doc(doc: DocBuilder<'a, D, ()>) -> Self {
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
    fn docbuilder(self) -> DocBuilder<'a, D, ()> {
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
        self.docbuilder().pretty(width).to_string()
    }

    pub fn append(self, other: Self) -> Self {
        join_docs(self, other, |force, doc| apply_force(identity, force, doc))
    }

    /// Joins with space()
    fn join_space(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.space().append(d), force, doc)
        })
    }

    /// Joins with line(), which is a space break
    fn join_line(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.line().append(d), force, doc)
        })
    }

    /// Joins with line_(), which is a soft break
    fn join_line_(self, other: Self) -> Self {
        let allocator = self.doc.0;
        join_docs(self, other, |force, doc| {
            apply_force(|d| allocator.line_().append(d), force, doc)
        })
    }

    fn group(self) -> Self {
        Self {
            doc: self.doc.group(),
            ..self
        }
    }

    fn nest(self, n: isize) -> Self {
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
