use formatter::format_source;
use test_each_file::test_each_file;

test_each_file! { in "crates/formatter/tests/fixtures" => |content: &str| {
    insta::assert_snapshot!(format_source(content, 40));
}}
