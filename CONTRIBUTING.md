# Contributing to iamb

## Building

You can build `iamb` locally by using `cargo build`.

## Pull Requests

When making changes to `iamb`, please make sure to:

- Add new tests for fixed bugs and new features whenever possible
- Add new documentation with new features

If you're adding a large amount of new code, please make sure to look at a test
coverage report and ensure that your tests sufficiently cover your changes.

You can generate an HTML report with [cargo-tarpaulin] by running:

```
% cargo tarpaulin --avoid-cfg-tarpaulin --out html
```

## Tests

You can run the unit tests and documentation tests using `cargo test`.

[cargo-tarpaulin]: https://github.com/xd009642/tarpaulin
