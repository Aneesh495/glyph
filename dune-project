(lang dune 3.14)
(name glyph)
(version 0.1.0)
(generate_opam_files true)

(license MIT)
(authors "Aneesh Krishna")
(maintainers "Aneesh Krishna")
(source (github Aneesh495/glyph))
(documentation "https://github.com/Aneesh495/glyph")

(package
 (name glyph)
 (synopsis "A typed functional language with an optimizing compiler and bytecode VM")
 (description "\
Glyph is a strict functional programming language implemented entirely in OCaml.
It includes a hand-written lexer and parser, Hindley-Milner type inference with
let-polymorphism, pattern compilation to decision trees, SSA-based mid-level IR,
classic optimization passes, a register-based bytecode ISA, and a virtual machine
with a semi-space copying garbage collector.")
 (depends
  (ocaml (>= 5.2))
  dune
  cmdliner))
