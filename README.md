# clocalc

![](https://github.com/sdingcn/clocalc/actions/workflows/run_test.yml/badge.svg)

**Clo**sure **calc**ulus is an interpreted functional programming language.
See [test/](test/) for code examples (`*.clo`).

## syntax

```
<comment>   := #[^\n]*\n
<integer>   := [+-]?[0-9]+  // C++ int
<string>    := "([^"] | \")*"  // see interpreter source for the supported alphabet
<variable>  := [a-zA-Z_][a-zA-Z0-9_]*
<intrinsic> := .void  // generates a Void object
             | .+ | .- | .* | ./ | .% | .< | .<= | .> | .>= | .= | ./=
             | .and | .or | .not  // no short-circuit; use "if" for short-circuit
             | .s+ | .s< | .s<= | .s> | .s>= | .s= | .s/= | .s|| | .s[] | .quote | .unquote
             | .s->i | .i->s
             | .type  // 0 for Void, 1 for Int, 2 for String, 3 for Closure
             | .eval
             | .getchar | .getint | .putstr | .flush  // IO
<vepair>    := <variable> <expr>
<expr>      := <integer>
             | <string>
             | <variable>
             | lambda ( <variable>* ) <expr>
             | letrec ( <vepair>* ) <expr>
             | if <expr> <expr> <expr>
             | { <expr>+ }  // sequenced evaluation
             | ( <intrinsic> <expr>* )
             | ( <expr> <expr>* )
             | @ <variable> <expr>  // accesses a closure's environment variable
```

## dependencies

This project was tested on macOS.

+ `clang++` with C++20 support
+ `make`
+ `python3` (only needed for `run_test.py`)

## build and run

```
make -C src/ release
bin/clocalc <source-path>
```

`python3 run_test.py` (re-)builds the interpreter and runs all tests.
