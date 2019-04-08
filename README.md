# Poly OCaml

Simplify polynomials with OCaml 

Assignment information in the [GettingStarted.txt](GettingStarted.txt) file

## Note to the Grader

To make it easier to check if tests were correct, I've substituted `simplify` function with `check_simplify`, which will run through the simplification and print out if the original input and output are the same (there is a caveat that if the x value is to a high enough power, int math breaks down, so sometimes it will say the values aren't equal even though they are).

Though the simplification works fine, some calculations (such as `(x+1)^100`), run very slowly. I was able to make a handful of minor optimizations, but I think the combination of appending long lists and have many recursive calls are to blame.

## Getting Started

### Installing

Install dependencies (menhir and ocamlbuild) with

```bash
make install
```

### Compile

Compile the program with

```bash
make
```

### Test

Run all of the test in the `tests/` directory (all the `*.in` files)

```bash
make tests
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
