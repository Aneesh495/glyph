(lang dune 3.14)
(name glyph)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Aneesh Krishna")
(maintainers "Aneesh Krishna")
(source (github Aneesh495/glyph))

(package
 (name glyph)
 (synopsis "A typed functional language with an optimizing compiler and bytecode VM")
 (description "\
Glyph is a strict functional programming language implemented entirely in OCaml.
It includes a hand-written lexer and parser, Hindley-Milner type inference,
pattern compilation, SSA mid-level IR, classical optimizations, a bytecode ISA,
and a semi-space copying GC.")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.14))
  cmdliner
  (alcotest :with-test)
  (qcheck :with-test)))
