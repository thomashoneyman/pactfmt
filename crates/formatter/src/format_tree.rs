use crate::format_doc::{doc_is_nil, format_with_comments, FormatDoc};
use pretty::DocAllocator;
use syntax::cst::*;

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
#[allow(clippy::upper_case_acronyms)]
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
