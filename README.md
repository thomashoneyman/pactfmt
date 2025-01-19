# pactfmt

[![CI](https://github.com/thomashoneyman/pactfmt/actions/workflows/ci.yaml/badge.svg)](https://github.com/thomashoneyman/pactfmt/actions/workflows/ci.yaml)

A formatter for Pact source files.

## Installation

Clone the repository, and then execute:

```sh
cargo install --path .
```

## Usage

```sh
# Format a file
pactfmt format input.pact

# Check if a file needs formatting
pactfmt check input.pact

# Format from stdin
cat input.pact | pactfmt format
```

## Contributing

Run all tests with Cargo:

```sh
cargo test
```

### Snapshots

This project uses `insta` for snapshot testing. First install the cargo-insta tool:

```sh
cargo install cargo-insta
```

Then you can:

```sh
# Review snapshot changes
cargo insta review

# Accept all snapshot changes
cargo insta accept

# See all available commands
cargo insta --help
```
