# CCC Tools

<a href="https://noonicknames.github.io/ccc-tools/book/"><b>Read the book 📖</b></a>

This is a collection of tools used for CCC related work.

# Tools

## extract-cs
<a href="https://noonicknames.github.io/ccc-tools/book/extract-cs/overview.html"><img alt="extract-cs book" src="https://img.shields.io/badge/book-extract--cs-blue?logo=mdbook"></a>
<a href="https://noonicknames.github.io/ccc-tools/doc/extract_cs/index.html"><img alt="extract-cs doc" src="https://img.shields.io/badge/doc-extract--cs-blue?logo=docsdotrs"></a>
<br>
<br>

Used for extracting cross section results.
## calc-rates
<a href="https://noonicknames.github.io/ccc-tools/book/calc-rates/overview.html"><img alt="calc-rates book" src="https://img.shields.io/badge/book-calc--rates-blue?logo=mdbook"></a>
<a href="https://noonicknames.github.io/ccc-tools/doc/calc_rates/index.html"><img alt="calc-rates doc" src="https://img.shields.io/badge/doc-calc--rates-blue?logo=docsdotrs"></a>
<br>
<br>
Used for calculating collision rates from cross section data.

# Building

These tools are written in Rust and currently requires Rust 1.85 or above to build.

To build a tool, run the following,
`cargo build --release --package <TOOL-NAME>`

For example,
`cargo build --release --package extract-cs`

Alternatively, first navigate into the tool directory then simply run `cargo build --release`.