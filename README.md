# CCC Tools

<a href="https://noonicknames.github.io/ccc-tools/">Reference</a>

This is a collection of tools used for CCC related work.

# Tools

- extract-cs
    - Used for extracting cross section results.
- calc-rates
    - Used for calculating collision rates from cross section data.


# Building

These tools are written in Rust and currently requires Rust 1.85 or above to build.

To build a tool, run the following,
`cargo build --release --package <TOOL-NAME>`

For example,
`cargo build --release --package extract-cs`

Alternatively, first navigate into the tool directory then simply run `cargo build --release`.