# ccrust: A basic C compiler written in Rust

## Dependencies
- The compiler emits GNU Assembly, so it requires `gcc` to compile the generated assembly to executable binaries.

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

(the tests are not at all comprehensive, although I plan to work on them)


## Usage 

```shell
# replace `cargo run --` with executable if not running inside the package
cargo run -- files/hello.c -o hello   # outputs an executable `hello`
cargo run -- files/hello.o -c -o hello.o # outputs an object file `hello.o`
cargo run -- files/bye.c files/hello.c -o combined # links both the files and outputs an executable `combined`

# the program cannot handle object files given as arguments
```

Since GCC automatically links the generated object files with GNU libc, it is valid to use C functions that only work with integers as long as they are declared with the correct signature in the source file that uses them. Here is an example C file that demonstrates mostly what the compiler can handle,

```c
// file hello.c
int putchar(int c);
int print_num(int n);

int fib(int n) {
    if (n == 1)
        return 0;
    if (n == 2)
        return 1;
    return fib(n - 2) + fib(n - 1);
}

int max(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8, int a9) {
    int max = a0 < a1? a1 : a0;
    max = max < a2? a2 : max;
    max = max < a3? a3 : max;
    max = max < a4? a4 : max;
    max = max < a5? a5 : max;
    max = max < a6? a6 : max;
    max = max < a7? a7 : max;
    max = max < a8? a8 : max;
    max = max < a9? a9 : max;
    return max;
}

int hello() {
    putchar(104); putchar(101); putchar(108);
    putchar(108); putchar(111); putchar(10);
    return 0;
}

int print_num(int n) {
    if (n < 9)
        putchar(48 + n);
    else {
        print_num(n / 10);
        putchar(48 + (n % 10));
    }
    return 0;
}

int main(void) {
    int a = 2, b = 3;
    print_num(138094);
    putchar(10);
    print_num(fib(9));
    putchar(10);
    print_num(max(1, a, b, 12, 13, 43, 5, 9, 0, 120));
    putchar(10);
    hello();
    return 0;
}
```

```shell
cargo run -- hello.c -o hello
./hello
# Output
# 138094
# 21
# 120
# hello
```

## Supported subset of C
Currently, the compiler can only work with `int`s. It supports the following subset of C:

- all binary, unary and ternary operators
- `int` variable declarations and assignment expressions
- conditional operators (with short-circuiting)
- if-else statements
- for, do-while, while loops
- break, continue
- switch statements
- basic type checking stage - currently it can tell apart the only types it can handle - functions and integers.
- function declarations and definitions (functions must return `int` and can take any number of or no `int` arguments).

Multiple `.c` files can be compiled together in order to link them (the linking part is handled by GCC).

## Assembly generation
The compiler only generates assembly for x64 systems, in the AT&T syntax. It uses the GNU Assembler to assemble the generated assembly into an executable (for which it requires `gcc`).

For debugging and learning purposes, detailed comments are added to the generated assembly by the compiler. 

## TODO (in order of priority):
I'm following the book [Writing a C compiler by Nora Sandler](https://nostarch.com/writing-c-compiler), so naturally, my next steps would be according to chapters given in the book. But there a few additional things I want to fix or add to the currently existing compiler
- Refine the interface for adding debug info into the assembly (currently it is extremely cluttered and verbose)
- Add an option to turn off the comments in the generated assembly
- create macros to simplify the parsing (again, it is currently much more verbose than needed)
- Add more CLI options
- Add a preprocessor
