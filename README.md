# Glyph

A strict functional programming language, compiler, and bytecode virtual machine —
written end-to-end in OCaml.

Glyph started as a personal deep-dive into the guts of language implementation:
type inference that actually works, SSA construction you can step through, and a
GC that moves objects for real. Every pass is real code, not a sketch.

```
┌──────────┐   ┌────────┐   ┌───────────┐   ┌─────┐   ┌─────┐   ┌──────┐   ┌────────┐   ┌────┐
│  source  │──▶│  lex   │──▶│   parse   │──▶│  HM │──▶│ HIR │──▶│  MIR │──▶│  opts  │──▶│ VM │
└──────────┘   └────────┘   └───────────┘   └─────┘   └─────┘   └──────┘   └────────┘   └────┘
```

## Quick start

```bash
# requires OCaml 5.2+ and dune
opam install . --deps-only -y
dune build
dune exec glyph -- run examples/fib.gl
dune exec glyph -- compile examples/fib.gl -o fib.gbc
dune exec glyph -- disasm fib.gbc
```

## Language tour

```glyph
type List a = Nil | Cons a (List a)

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons x rest -> Cons (f x) (map f rest)

let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let main = print_int (fib 20)
```

Hindley–Milner inference means you almost never write type annotations. Algebraic
data types and pattern matching are first-class. Closures capture their
environment; the VM allocates them on the heap and the collector reclaims them.

## What's inside

| Layer | What it does |
|-------|----------------|
| **syntax** | Hand-written lexer + recursive-descent parser with Pratt operators |
| **types** | Algorithm W, unification, let-polymorphism, occurs-check |
| **hir** | Desugaring and pattern compilation to decision trees |
| **mir** | CFG + SSA (dominance frontiers / Cytron-style φ insertion) |
| **opt** | Const prop, copy prop, SCCP, DCE, CSE, inlining |
| **codegen** | Register-based bytecode emission |
| **vm** | Interpreter + semi-space copying GC + builtins |

## Docs

| Doc | Contents |
|-----|----------|
| [docs/architecture.md](docs/architecture.md) | End-to-end pipeline, modules, data flow |
| [docs/language.md](docs/language.md) | Lexical structure, EBNF, patterns, examples |
| [docs/type-system.md](docs/type-system.md) | Hindley–Milner, unification, levels, occurs check |
| [docs/ir.md](docs/ir.md) | HIR, MIR, SSA / φ nodes, lowering |
| [docs/vm.md](docs/vm.md) | Bytecode ISA, frames, semi-space GC |
| [docs/optimizations.md](docs/optimizations.md) | Each opt pass with before/after IR |
| [docs/diagrams/pipeline.md](docs/diagrams/pipeline.md) | Mermaid diagrams (full set) |
| [docs/diagrams/types.md](docs/diagrams/types.md) | Inference / unify sequence |
| [docs/diagrams/ssa.md](docs/diagrams/ssa.md) | Dominators → φ → rename |
| [docs/diagrams/gc.md](docs/diagrams/gc.md) | Cheney semi-space GC |
| [docs/contributing.md](docs/contributing.md) | Build, test, add a pass |

## Project layout

```
lib/
  util/       spans, diagnostics, identifiers, union-find, graphs
  syntax/     tokens, lexer, parser, AST, pretty-printer
  types/      type AST, environment, inference, unification
  hir/        high-level IR, desugar, pattern compiler
  mir/        SSA MIR, CFG, dominators
  opt/        optimization passes
  codegen/    bytecode ISA + emitter
  vm/         values, heap, GC, interpreter
  driver/     compile pipeline orchestration
bin/glyph.ml  CLI
examples/     sample programs
std/          prelude
docs/         design notes and diagrams
test/         alcotest suites
```

## License

MIT
