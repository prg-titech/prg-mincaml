# prg-mincaml

A customized MinCaml compiler that includes a number of backends.

## Prerequisites

- OCaml >= 4.10.2

## Build

```
$ opam install -y dune ppx_deriving ppx_inline_test
$ dune build
```

## Run

To emit a bytecode for TLA lang:

```
$ dune exec tlac -- test/[file].ml
```

# Acknowledgements

This work is based on the [MinCaml](https://github.com/esumii/min-caml) compiler.
