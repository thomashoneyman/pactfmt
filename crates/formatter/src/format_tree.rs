/// TODO:
///   - add 'raw' FST node, which takes a list of tokens and
///     just prints them out using basic RcDoc primitives, including
///     faithfully reproducing trivia.
use crate::format_doc::{doc_is_nil, format_with_comments, FormatDoc};
use pretty::DocAllocator;
use syntax::types::{SourceToken, Trivia, TokenKind};

#[derive(Debug, PartialEq, Clone)]
pub struct ListItem {
    pub value: FST,
    pub comma: Option<SourceToken>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectItem {
    pub key: SourceToken,
    pub sep: SourceToken,
    pub value: FST,
    pub comma: Option<SourceToken>,
}

/// A special form is a keyword followed by a list of sections and a body.
/// (defun NAME (arg1 arg2) body1 body2)
#[derive(Debug, PartialEq, Clone)]
pub struct SpecialForm {
    pub keyword: SourceToken,
    pub sections: Vec<FST>,
    pub body: Vec<FST>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Wrapped<T> {
    pub open: SourceToken,
    pub inner: T,
    pub close: SourceToken,
}

/// A 'format syntax tree' is an intermediate representation of the CST simplified for formatting.
#[allow(clippy::upper_case_acronyms, dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum FST {
    Literal(Vec<SourceToken>),
    CompoundLiteral(Vec<SourceToken>),
    List(Wrapped<Vec<ListItem>>),
    Object(Wrapped<Vec<ObjectItem>>),
    SExp(Wrapped<Vec<FST>>),
    SpecialForm(Wrapped<SpecialForm>),
    Unwrapped(Vec<FST>),
}

impl FST {
    pub fn format<'a, D>(&'a self, allocator: &'a D) -> FormatDoc<'a, D>
    where
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            FST::Literal(tokens) => {
                if tokens.is_empty() {
                    return FormatDoc::nil(allocator);
                }

                let mut result = format_with_comments(
                    &tokens[0].leading,
                    &tokens[0].trailing,
                    FormatDoc::text(allocator, &tokens[0].text),
                );

                for token in &tokens[1..] {
                    let doc = format_with_comments(
                        &token.leading,
                        &token.trailing,
                        FormatDoc::text(allocator, &token.text),
                    );
                    result = result.join_line(doc);
                }

                result.group()
            }

            FST::CompoundLiteral(tokens) => {
                if tokens.is_empty() {
                    return FormatDoc::nil(allocator);
                }

                let mut result = format_with_comments(
                    &tokens[0].leading,
                    &tokens[0].trailing,
                    FormatDoc::text(allocator, &tokens[0].text),
                );

                for token in &tokens[1..] {
                    let doc = format_with_comments(
                        &token.leading,
                        &token.trailing,
                        FormatDoc::text(allocator, &token.text),
                    );
                    result = result.append(doc);
                }

                result.group()
            }

            // TODO: Handle empty lists
            FST::List(Wrapped { open, inner, close }) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = inner.iter().map(|ListItem { value, comma }| {
                    let comma = match comma {
                        Some(comma) => format_with_comments(
                            &comma.leading,
                            &comma.trailing,
                            FormatDoc::text(allocator, &comma.text),
                        ),
                        None => FormatDoc::nil(allocator),
                    };
                    value.format(allocator).append(comma)
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
                    FormatDoc::text(allocator, &open.text),
                )
                .join_line_(result)
                .nest(2)
                .join_line_(format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.text),
                ))
                .group()
            }

            FST::Object(Wrapped { open, inner, close }) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = inner.iter().map(|ObjectItem { key, sep, value, comma }| {
                    let comma = match comma {
                        Some(comma) => {
                            let leading = if comma.leading.iter().any(|t| matches!(t, Trivia::Comment(_))) {
                                &comma.leading
                            } else {
                                &vec![]
                            };
                            format_with_comments(
                                &leading,
                                &comma.trailing,
                                FormatDoc::text(allocator, &comma.text),
                            )
                        },
                        None => FormatDoc::nil(allocator),
                    };

                    let key_doc = format_with_comments(
                        &key.leading,
                        &key.trailing,
                        FormatDoc::text(allocator, key.text.clone()),
                    );

                    let sep_doc = format_with_comments(
                        &sep.leading,
                        &sep.trailing,
                        FormatDoc::text(allocator, sep.text.clone()),
                    );

                    let value_doc = value.format(allocator).append(comma);

                    let key_val_doc = if sep.kind == TokenKind::Colon {
                        key_doc.append(sep_doc)
                    } else {
                        key_doc.join_space(sep_doc)
                    };

                    key_val_doc.join_space(value_doc)
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
                    FormatDoc::text(allocator, &open.text),
                )
                .join_line_(result)
                .nest(2)
                .join_line_(format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.text),
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
                    FormatDoc::text(allocator, &open.text)
                        .append(result.nest(2))
                        .append(format_with_comments(
                            &close.leading,
                            &close.trailing,
                            FormatDoc::text(allocator, &close.text),
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
                    FormatDoc::text(allocator, &open.text),
                );

                let keyword = format_with_comments(
                    &keyword.leading,
                    &keyword.trailing,
                    FormatDoc::text(allocator, keyword.text.clone()),
                );

                let close = format_with_comments(
                    &close.leading,
                    &close.trailing,
                    FormatDoc::text(allocator, &close.text),
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

            // Essentially an s-expression, but without parentheses.
            FST::Unwrapped(items) => {
                let mut result = FormatDoc::nil(allocator);
                let mut iter = items.iter().map(|item| item.format(allocator));

                if let Some(first) = iter.next() {
                    result = first;
                    for doc in iter {
                        result = result.join_line(doc);
                    }
                }

                result.nest(2).group()
            }
        }
    }
}