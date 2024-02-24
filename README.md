# Glyph

A strict functional language frontend and compiler research project in OCaml.

The working CLI parses and typechecks Glyph programs with Hindley–Milner
inference. `research/` contains HIR, SSA, optimization, bytecode, and VM/GC
modules that are still being integrated into the runtime pipeline.

```
source ──▶ lex ──▶ parse ──▶ HM infer ──▶ (research: HIR → SSA MIR → opts → VM)
```

## Quick start

```bash
# OCaml 5.2+ (Homebrew ocaml 5.5 works well on recent macOS)
opam switch create glyph-sys ocaml-system   # once
eval $(opam env --switch=glyph-sys)
opam install dune cmdliner -y

dune build
dune exec glyph -- typecheck examples/fib.gl
dune exec glyph -- parse examples/fib.gl
```

`glyph run` currently typechecks the input; it does not execute it yet.

## Language tour

```glyph
let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let main = print_int (fib 10)
```

Hindley–Milner inference means you almost never write annotations. Algebraic
data types and pattern matching are first-class in the surface language.

## Layout

```
lib/util/      spans, diagnostics, identifiers
lib/syntax/    tokens, lexer, parser, AST
lib/types/     type representation, unification, Algorithm W
bin/           glyph CLI
examples/      sample programs
docs/          architecture notes and diagrams
research/      HIR, SSA MIR, opts, bytecode, GC/VM (in progress wiring)
```

## Docs

- [Architecture](docs/architecture.md)
- [Language](docs/language.md)
- [Type system](docs/type-system.md)
- [IR / SSA](docs/ir.md)
- [VM / GC](docs/vm.md)

## License

MIT
