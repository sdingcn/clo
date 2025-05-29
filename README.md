# clo

![](https://github.com/sdingcn/clo/actions/workflows/run_test.yml/badge.svg)

**Clo** is an interpreted programming language.

The distinguished feature of this language is the ability to save
the current program state as a string.
The built-in function `.forkstate` returns
a string encoding the entire program state,
and when the state is resumed using `.eval` it starts
right after the `.forkstate` call but with a return value of Void type.
```
{
  (.putstr "0\n")
  (.putstr "1\n")
  letrec (state (.forkstate)) {
    (.putstr "2\n")
    if (.= (.type state) 0)  # if it is a void value
       (.putstr "3\n")
       (.eval state)
  }
}
```

The other features of this language is just a normal
dynamically typed function programming language.
```
letrec (
    sum lambda (n acc)
        if (.< n 1)
        acc
        (sum (.- n 1) (.+ acc n))
)
(sum (.getint) 0)
```

See [test/](test/) for more code examples (`*.clo`).

## dependencies

This project was tested on macOS.

+ `clang++` with C++20 support
+ `make`
+ `python3` (only needed for `run_test.py`)

## build and run

```
make -C src/ release
bin/clo <source-path>
```

`python3 run_test.py` (re-)builds the interpreter and runs all tests.
