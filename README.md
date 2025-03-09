# ccrust: A basic C compiler written in Rust

## Installation
You can clone this repository and use `cargo` to build and run the compiler

```shell
git clone https://github.com/i-m-ag-e/rustcc.git
cd rustcc
cargo run
```

Tests can be run using:
```shell
cargo test
```

## Supported subset of C
Currently, the compiler can only work with `int`s. It supports the following subset of C:

- all binary, unary and ternary operators
- ONLY the `main` function (with a necessary `void` as the only argument)
- `int` variable declarations and assignments
- conditional operators
- if-else statements
- for, do-while, while loops
- break, continue
- switch statements 

The compiler does not support functions yet, so it cannot print anything. The only way to check computations is to return the result from `main` and check the exit code of the generated executable.

## Assembly generation
The compiler only generates assembly for x64 systems, in the AT&T syntax. It uses the GNU Assembler to assemble the generated assembly into an executable (for which it requires `gcc`).

For debugging purposes, detailed comments are added to the generated assembly by the compiler. 

## TODO (in order of priority):
I'm following the book [Writing a C compiler by Nora Sandler](https://nostarch.com/writing-c-compiler), so naturally, my next steps would be according to chapters given in the book. But there a few additional things I want to fix or add to the currently existing compiler
- Refine the interface for adding debug info into the assembly (currently it is extremely cluttered and verbose)
- Add an option to turn off the comments in the generated assembly
- create macros to simplify the parsing (again, it is currently much more verbose than needed)
- Add more CLI options
- Add a preprocessor
