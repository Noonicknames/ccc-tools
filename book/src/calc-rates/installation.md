# Installation

Currently, `calc-rates` can only be built from source directly or via `cargo install`.
Before building, ensure Rust 1.85 or above is installed which comes with its package manager `cargo`.
To install Rust, follow instructions [here](https://rust-lang.org/tools/install/).

> [!TIP]
> On certain systems, check if Rust 1.85 or above is available as a module with `module avail rust`.

## Dependencies

Whilst not required for building, `calc-rates` dynamically links to OpenBLAS and LAPACKE or IntelMKL so ensure either are installed on your system.
The paths which calc-rates searches for these libraries is platform specific.
Optionally, static linking is also supported, see [below](#blas-linking-options).
For more details on how these libraries are linked, see [la](https://github.com/Noonicknames/la).

## With Cargo Install

Cargo install builds to `${CARGO_HOME}/bin/`, which is typically `~/.cargo/bin/`. 
Ensure this is in your `$PATH` variable, and set `${CARGO_HOME}` to a different directory if `~/.cargo/bin/` is inappropriate.

> [!NOTE]
> It is recommended to not set `${CARGO_HOME}` to a directory with file size or count limits since dependencies will be stored there.

To install, run the following commands,

```bash
cargo install --git https://github.com/Noonicknames/ccc-tools --package calc-rates
```

## Direct Build From Source

To directly build, run the following commands,

```bash
git clone https://github.com/Noonicknames/ccc-tools
cd ccc-tools
cargo build --release --package calc-rates
```

After finishing building, the standalone binary is located at `./target/release/calc-rates` or `./target/release/calc-rates.exe` on windows.
This can be copied to a different folder e.g. `~/bin/` with `cp ./target/release/calc-rates ~/bin/`.

## BLAS Linking Options
By default, `calc-rates` searches for instances of either OpenBLAS or IntelMKL then dynamically loads in either library with higher priority for IntelMKL.
If neither library is found, the program would simply fail to run.
However, `calc-rates` may be configured to fall back to a statically linked library via Rust features.

To statically link to openblas, ensure OpenBLAS may be statically linked to i.e. `/usr/lib/libopenblas.a` exists, then add the following flags to the `cargo` commands from before,

```
--features static-openblas
```

and the equivalent for IntelMKL, ensure `/opt/intel/mkl/lib/intel64/libmkl_core.a`, `/opt/intel/mkl/lib/intel64/libmkl_sequential.a` and `/opt/intel/mkl/lib/intel64/libmkl_intel_lp64` exist,

```
--features static-intelmkl
```

To disable dynamic linking, add the following flag,

```
--no-default-features
```
> [!NOTE]
> If the installation path of OpenBLAS or IntelMKL differs from above, include the correct paths in `LD_LIBRARY_PATH` on linux, `DYLD_FALLBACK_LIBRARY_PATH` on macOS or `PATH` on windows.
> OpenBLAS is often installed such that only dynamic linking is possible.

## From Pre-compiled Binary

ccc-tools is currently unavailable for installation directly as a binary.