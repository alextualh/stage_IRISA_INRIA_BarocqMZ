# VeriMZ

VeriMZ is a formal modelisation of [TinyMZ](https://gitlab.inria.fr/cchavano/tinymz). This model is intended to be verified.

# Project structure

- `theories` contains the Coq formalisation of TinyMZ
- `src` contains OCaml code
- `extraction` is where Coq files are extracted

# Building and running the project

## Dependencies

VeriMZ requires OCaml and Coq. The recommended way to install OCaml and Coq is to use the OCaml Package Manager `opam`. You can install it on your system by following the [opam documentation](https://opam.ocaml.org/).

After getting opam configured, install the following packages with `opam install <pkg>.<version>`:

```text
ocaml           (v4.14.2)
coq             (v8.19.2)
dune            (v3.16.0)
ocamlformat     (v0.26.2)
```
## Buid process

Call `dune build`.

## Running VeriMZ

Call `dune exec verimz`.

## Cleaning the generated files

Call `dune clean`.