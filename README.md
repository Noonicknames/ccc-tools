# CCC Tools

<a href="https://noonicknames.github.io/ccc-tools/">Reference</a>

This is a collection of tools used for CCC related work.

# Tools

## extract-cs

Used for extracting cross section results.
## calc-rates
![Static Badge](https://img.shields.io/badge/book-calc--rates-blue?logo=mdbook&link=ttps%3A%2F%2Fnoonicknames.github.io%2Fccc-tools%2Fbook%2Fcalc-rates.html)
![Static Badge](https://img.shields.io/badge/doc-calc--rates-blue?logo=docsdotrs&link=https%3A%2F%2Fnoonicknames.github.io%2Fccc-tools%2Fdoc%2Fcalc_rates%2Findex.html)
Used for calculating collision rates from cross section data.

# Building

These tools are written in Rust and currently requires Rust 1.85 or above to build.

To build a tool, run the following,
`cargo build --release --package <TOOL-NAME>`

For example,
`cargo build --release --package extract-cs`

Alternatively, first navigate into the tool directory then simply run `cargo build --release`.