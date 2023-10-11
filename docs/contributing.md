# Contributing

Glyph is a passion compiler project — patches that deepen a real pass (better
diagnostics, a tighter GC, a new opt) are more welcome than drive-by renames.
This page is how to build, test, and add something without fighting the
pipeline.

## Prerequisites

- OCaml **5.2+**
- [opam](https://opam.ocaml.org/) and [dune](https://dune.build/)
- `cmdliner`, `alcotest` (pulled via the package file)

```bash
# from the repo root
opam install . --deps-only -y
eval $(opam env)
```

## Build

```bash
dune build
```

The CLI binary:

```bash
dune exec glyph -- --help
```

Useful invocations:

```bash
dune exec glyph -- run examples/fib.gl
dune exec glyph -- compile examples/fib.gl -o fib.gbc
dune exec glyph -- disasm fib.gbc
dune exec glyph -- typecheck examples/fib.gl
dune exec glyph -- dump-mir examples/fib.gl
```

## Test

```bash
dune test
```

Alcotest suites live under `test/`. Prefer small, focused tests:

| Kind | What to assert |
|------|----------------|
| Lexer/parser | Token stream / AST snippets |
| Types | Infer succeeds with expected scheme; known rejects fail |
| HIR/MIR | Dump or structural checks on tiny functions |
| VM | `run` of an example prints the right stdout |

Golden stdout tests for `examples/*.gl` are great once the VM is stable.

## Project map (where to edit)

| Want to change… | Start here |
|-----------------|------------|
| Syntax / precedence | `lib/syntax` |
| Inference / unify | `lib/types` |
| Desugar / patterns | `lib/hir` |
| CFG / SSA | `lib/mir` |
| Opts | `lib/opt` |
| ISA / emit | `lib/codegen` |
| Interpreter / GC | `lib/vm` |
| CLI wiring | `lib/driver`, `bin/glyph.ml` |

Package dependency order is documented in [architecture.md](architecture.md).
Don’t create reverse dependencies (e.g. `mir` must not import `codegen`).

## Adding an optimization pass

1. **Create** `lib/opt/my_pass.ml` (+ `.mli`) that takes SSA MIR and returns SSA
   MIR. Keep it pure w.r.t. global compiler state.
2. **Register** the pass in `pipeline.ml` (e.g. in the `O2` list after whatever
   it depends on — CSE likes to follow SCCP; DCE likes to be last).
3. **Document** a before/after snippet in [optimizations.md](optimizations.md).
4. **Test** with a MIR fixture or a tiny `.gl` file whose dump changes in the
   expected way:

```bash
dune exec glyph -- dump-mir --opt O2 examples/my_case.gl
```

Checklist for the pass itself:

- [ ] Pure ops only folded / eliminated when side-effect-free
- [ ] φ operands updated if blocks or names change
- [ ] Unreachable blocks either left for DCE or removed carefully
- [ ] No use of a name outside its SSA dominance

### Minimal pass skeleton (conceptual)

```text
let run (f : Mir.func) : Mir.func =
  (* 1. build use-def or value lattice *)
  (* 2. rewrite instructions *)
  (* 3. return { f with blocks = … } *)
  f
```

Wire it next to `const_prop` / `dce` in the pass manager — don’t invent a
second pipeline.

## Adding a bytecode opcode

1. Extend `lib/codegen/opcode.ml` (and the emitter + disassembler).
2. Teach `lib/vm/interp.ml` the new case.
3. If it allocates, ensure a GC safepoint / heap check runs first.
4. Add a disasm + VM smoke test.

## Style

- Real algorithms over stubs; comments for non-obvious invariants (levels,
  forwarding pointers, dominance frontiers).
- Spans on user-facing errors; internals can use `Ident.to_string`.
- No drive-by reformatting of unrelated modules.
- Don’t commit secrets, huge `_build` artifacts, or generated `.gbc` fixtures
  unless they’re tiny and intentional.

## Docs

Design notes under `docs/`:

| Doc | Topic |
|-----|--------|
| [architecture.md](architecture.md) | Pipeline & packages |
| [language.md](language.md) | Surface language |
| [type-system.md](type-system.md) | HM, unify, levels |
| [ir.md](ir.md) | HIR, MIR, SSA |
| [vm.md](vm.md) | ISA & GC |
| [optimizations.md](optimizations.md) | Each opt pass |
| [diagrams/pipeline.md](diagrams/pipeline.md) | Mermaid only |

If you change a pass’s contract, update the matching doc in the same PR.

## License

MIT — see `LICENSE` when present.
