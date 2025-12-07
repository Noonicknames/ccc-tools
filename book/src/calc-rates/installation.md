# Installation

Currently, `calc-rates` can only be built from source directly or via `cargo install`.
To build, ensure Rust 1.85 or above is installed.
To install Rust, follow instructions [here](https://rust-lang.org/tools/install/).

> [!TIP]
> On certain systems, check if Rust 1.85 or above is available as a module with `module avail rust`.

## Dependencies

Whilst not required for building, `calc-rates` dynamically links to OpenBLAS and LAPACKE or IntelMKL so ensure either are installed on your system and available in the PATH variable.
The paths which calc-rates searches for these libraries is platform specific.
For more details on how these libraries are linked see [la](https://github.com/Noonicknames/la).

## With Cargo Install

Cargo install builds to `${CARGO_HOME}/bin/`, which is typically `~/.cargo/bin/`. 
Ensure this is in your `$PATH` variable, and set `${CARGO_HOME}` to a different directory if `~/.cargo/bin/` is inappropriate.

> [!NOTE]
> It is recommended to not set `${CARGO_HOME}` to a directory with file size or count limits since dependencies will be stored there.

To install, run the following commands,

```
cargo install --git https://github.com/Noonicknames/ccc-tools --package calc-rates
```

## Direct Build From Source

To directly build, run the following commands,

```
git clone https://github.com/Noonicknames/ccc-tools
cd ccc-tools
cargo build --release --package calc-rates
```

After finishing building, the standalone binary is located at `./target/release/ccc-tools` or `./target/release/ccc-tools.exe` on windows.
This can be copied to a different folder e.g. `~/bin/`

## From Pre-compiled Binary

ccc-tools is currently unavailable for installation directly as a binary.