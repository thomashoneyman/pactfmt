use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use proc_macro2::{self, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, Ident,
};
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Default)]
struct Directory {
    children: HashMap<PathBuf, Directory>,
    files: HashSet<PathBuf>,
}

impl Directory {
    fn new(base: &Path) -> Directory {
        let mut directory = Directory::default();
        for entry in base.read_dir().unwrap() {
            let entry = entry.unwrap().path();
            if entry.is_file() {
                directory.files.insert(entry);
            } else if entry.is_dir() {
                directory
                    .children
                    .insert(entry.clone(), Self::new(entry.as_path()));
            }
        }

        directory
    }
}

struct TestCorpusArgs {
    function: Expr,
}

impl Parse for TestCorpusArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let function = match input.parse::<Expr>() {
            Ok(function) => function,
            Err(e) => return Err(syn::Error::new(e.span(), "invalid function arg")),
        };

        Ok(Self { function })
    }
}

fn sanitize_ident(input: &str) -> Ident {
    let name: String = input
        .chars()
        .map(|c| if is_xid_continue(c) { c } else { '_' })
        .collect();

    if !is_xid_start(name.chars().next().expect("Name is not empty")) {
        format_ident!("_{name}")
    } else {
        format_ident!("{name}")
    }
}

fn generate_directory_benchmarks(
    base: &Directory,
    path: String,
    stream: &mut TokenStream,
    args: &TestCorpusArgs,
) -> () {
    for file in base.files.iter() {
        let name = file.file_name().unwrap().to_str().unwrap();
        let file_path = file.canonicalize().unwrap();
        let file_path = file_path.to_str().unwrap();
        let function = &args.function;
        let arguments = quote!(include_str!(#file_path));
        let test_name = format!("{}/{}", path, name);
        stream.extend(quote! {
          group.bench_function(#test_name, |b| b.iter(|| #function(black_box(#arguments))));
        });
    }

    for (name, dir) in base.children.iter() {
        let name = name.file_name().unwrap().to_str().unwrap();

        let mut next_stream = TokenStream::new();
        let path = format!("{}/{}", path, name);
        generate_directory_benchmarks(dir, path, &mut next_stream, args);
        stream.extend(next_stream);
    }
}

fn generate_directory_tests(
    base: &Directory,
    stream: &mut TokenStream,
    args: &TestCorpusArgs,
) -> () {
    for (name, dir) in base.children.iter() {
        let name = name.file_name().unwrap().to_str().unwrap();
        let name = sanitize_ident(&name);

        let mut next_stream = TokenStream::new();
        generate_directory_tests(dir, &mut next_stream, args);
        stream.extend(quote! {
            mod #name {
                use super::*;
                #next_stream
            }
        });
    }

    for file in base.files.iter() {
        let name = file.file_name().unwrap().to_str().unwrap();
        let name = format_ident!("{}", sanitize_ident(&name));
        let path = file.canonicalize().unwrap();
        let path = path.to_str().unwrap();
        let function = &args.function;
        let arguments = quote!(include_str!(#path));
        stream.extend(quote! {
            #[test]
            fn #name() {
                (#function)(#arguments)
            }
        });
    }
}

#[proc_macro]
pub fn test_corpus(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(input as TestCorpusArgs);

    let mut tokens = TokenStream::new();

    let corpus = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../corpus"));

    let directory = Directory::new(corpus);

    generate_directory_tests(&directory, &mut tokens, &args);

    tokens = quote! {
        #[cfg(test)]
        mod test_corpus {
            use super::*;
            #tokens
        }
    };

    proc_macro::TokenStream::from(tokens)
}

#[proc_macro]
pub fn benchmark_corpus(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(input as TestCorpusArgs);

    let mut tokens = TokenStream::new();

    let corpus = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/../corpus"));

    let directory = Directory::new(corpus);

    generate_directory_benchmarks(&directory, "corpus".to_owned(), &mut tokens, &args);

    proc_macro::TokenStream::from(tokens)
}
