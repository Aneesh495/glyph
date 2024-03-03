# Glyph Repair and Integration Status

## 1. Current Phase and Next Action
- **Current Phase**: Phase 2 — Diagnostics and Stage Contracts & Phase 3 — HIR Lowering and ANF Integrity
- **Next Action**:
  1. Define and refine stage contracts and structured diagnostics across stages.
  2. Complete `lib/mir/mir_lower.ml` (HIR -> MIR lowering) and `lib/mir/mir_verify.ml` (MIR verification).
  3. Verify pattern compilation and decision tree emission.

## 2. Verified Commit and Working Tree State
- **Audit Anchor Commit**: `75869344842700d8a3eec5836bf5b954ee49e1d7`
- **Starting HEAD Commit**: `0e9f5fdbcd4c93f2f5c0f45bbc6ff7440cb608cb`
- **Dirty State**: Backend promoted from `research/` to `lib/` with dedicated Dune libraries. All libraries and binaries build cleanly.

## 3. Exact Commands Run and Results
1. `git status --short`: Verified clean state at start.
2. `opam switch list`: Identified switch `glyph-sys` (OCaml 5.5.1).
3. `opam switch set glyph-sys`: Activated switch with OCaml 5.5.1 (code 0).
4. `eval $(opam env) && opam install -y alcotest qcheck`: Installed test frameworks (code 0).
5. `git mv research/codegen lib/bytecode && git mv research/driver lib/driver && git mv research/hir lib/hir && git mv research/mir lib/mir && git mv research/opt lib/opt && git mv research/vm lib/vm`: Promoted backend code into `lib/` (code 0).
6. Removed dead files: `lib/mir/.design_a`, `lib/vm/builtins.ml.aside`, `lib/hir/pattern_compile.ml` (code 0).
7. Resolved all compilation errors across `lib/hir`, `lib/mir`, `lib/opt`, `lib/bytecode`, `lib/vm`, and `lib/driver`.
8. `opam exec -- dune build @all`: **Exited 0!** All 9 libraries (`glyph_util`, `glyph_syntax`, `glyph_types`, `glyph_hir`, `glyph_mir`, `glyph_opt`, `glyph_bytecode`, `glyph_vm`, `glyph_driver`) and `bin/glyph_cli.exe` compile cleanly without broad warning silences.
9. `find . -name "*.aside" -o -name "*.bak" -o -name "*~"`: Returned empty (no orphan/dead files).
10. `ls -d research`: Returned "No such file or directory" (research island eliminated).

## 4. Compiler Pipeline Architecture and Module Mapping

```
source (.gl)
    │
    ▼ (lib/syntax/lexer.ml, token.ml)
Token Stream
    │
    ▼ (lib/syntax/parser.ml)
Surface AST (lib/syntax/ast.ml)
    │
    ▼ (lib/types/infer.ml, ty.ml, env.ml, unify.ml)
Typed AST
    │
    ▼ (lib/hir/desugar.ml)
HIR (lib/hir/hir.ml)
    │
    ▼ (lib/hir/pattern.ml)
Pattern-Compiled HIR (Decision Trees / Switches)
    │
    ▼ (lib/mir/mir_lower.ml)
MIR (lib/mir/mir.ml)
    │
    ▼ (lib/mir/mir_verify.ml)
Verified MIR
    │
    ▼ (lib/opt/*)
Optimized MIR (O0 / O1 / O2)
    │
    ▼ (lib/mir/mir_verify.ml)
Verified Optimized MIR
    │
    ▼ (lib/bytecode/emit.ml, regalloc.ml)
Bytecode Chunk (lib/bytecode/chunk.ml, opcode.ml)
    │
    ▼ (lib/bytecode/bytecode_verify.ml)
Verified Bytecode (.gbc)
    │
    ▼ (lib/vm/interp.ml, vm.ml, heap.ml, gc.ml, builtin.ml)
VM Execution & Runtime
```

### Module Inventory & Status

| Module | Location | Stage | Status / Defects |
|---|---|---|---|
| `Token`, `Lexer`, `Parser`, `Ast`, `Pretty` | `lib/syntax/` | Syntax | Built. AST defines `type_def`, `lit` with `int64`. |
| `Ty`, `Env`, `Error`, `Unify`, `Infer` | `lib/types/` | Types | Built. `Infer.infer_program` returns `(Env.t, Diagnostic.t list) result`. |
| `Span`, `Diagnostic`, `Ident`, `Intern`, `Util` | `lib/util/` | Utility | Built. Structured diagnostics. |
| `Hir`, `Desugar` | `lib/hir/` | HIR | Built. Desugar supports functions, lets, ADTs, tuples, sequences, primops; explicitly rejects deferred records with source span. |
| `Pattern` | `lib/hir/` | Pattern | Built. Decision tree compilation with Maranget algorithm. Dead file `pattern_compile.ml` removed. |
| `Mir` | `lib/mir/` | MIR | Built. Int-vreg CFG representation. |
| `Mir_lower` | `lib/mir/` | Lowering | **Next up (Phase 5)**: HIR to MIR translation. |
| `Mir_verify` | `lib/mir/` | Verification | **Phase 5**: Full verifier for MIR CFG, types, SSA/vreg invariants. |
| `Pass_manager`, Opts | `lib/opt/` | Optimization | Built. `pass.ml` defines `Pass.context`, passes updated. |
| `Opcode`, `Chunk`, `Emit`, `Regalloc`, `Disasm` | `lib/bytecode/` | Bytecode | Built. Chunk, opcodes, register allocator, disassembler, emit. |
| `Bytecode_verify` | `lib/bytecode/` | Bytecode Verifier | **Phase 7**: Validate bytecode chunk before execution. |
| `Value`, `Object_`, `Heap`, `Gc`, `Builtin`, `Interp`, `Vm` | `lib/vm/` | VM & GC | Built. Cheney copying GC, heap, builtins, interpreter. |
| `Compile` | `lib/driver/` | Driver | Built. End-to-end interface. |
| `Glyph_cli` | `bin/` | CLI | Built. CLI commands ready to be wired to full pipeline. |

## 5. Representation Invariants
- **AST**: Expressions have `{ expr_desc; expr_span }`. Program is `{ items : item list; span : Span.t }`. `Int` literal is `int64`.
- **Types**: Polymorphic type schemes generalized at `let`. Built-in types: `Int`, `Float`, `Bool`, `String`, `Char`, `Unit`, Tuple, Arrow, ADT.
- **HIR**: ANF-style; atoms are variables or literals; constructors and primops are explicit. `Switch_ctor` and `Switch_lit` for compiled matches.
- **MIR**: CFG of basic blocks. Virtual registers `vreg = int`. Labels `label = int`. Function IDs `fn_id = int`. Explicit terminators (`TJump`, `TBranch`, `TSwitch`, `TRet`, `TTailCall`, `TTailCallClosure`, `THalt`).
- **Bytecode**: Chunk with magic `GLYPH01`, version, constant pool, function descriptors, instruction array, global string table.
- **VM Values**: Immediates (`Int`, `Float`, `Bool`, `Unit`, `Native`) and Heap pointers (`Ptr loc`).
- **Heap & GC**: Semi-space bump allocator with Cheney copying collector. Evacuation leaves `forward` pointer. Stack frames and globals are scanned as roots.

## 6. Resolved Defects from Audit
1. Local opam switch and dependencies: Configured `glyph-sys` (OCaml 5.5.1) with `cmdliner`, `alcotest`, `qcheck`.
2. Frontend-only Dune build: Promoted all backend code to `lib/` and wired `dune` files for all modules.
3. Dead side files: Removed `lib/mir/.design_a`, `lib/vm/builtins.ml.aside`, `lib/hir/pattern_compile.ml`.
4. Warning 16 in `lib/mir/mir.ml`: Resolved optional `string_table` argument.
5. Builtin shadowing: `Stdlib.print_string` used in `lib/vm/builtin.ml`.
6. Missing `rec`: Fixed on `head_of_pat` in `lib/hir/pattern.ml`.
7. `Infer.result` references: Updated to actual `(Env.t, Diagnostic.t list) result`.
8. AST mismatch: Reconciled `type_decl` with `type_def` in `lib/hir/desugar.ml`.
9. Disassembler record field: Added `open Chunk` and type annotations in `lib/bytecode/disasm.ml`.
10. Pass manager / Pass context: Defined in `lib/opt/pass.ml` and connected passes.
11. `heap.mli` and `gc.mli`: Exported `stats`, `reset`, `dump`, `iter`, `check_heap`, `force_collect`.
12. Whole tree compiles cleanly via `opam exec -- dune build @all`.

## 7. Mandatory Completion Gates Status
- [x] Local switch dependency setup documented and verified
- [x] Complete frontend and backend compile via Dune (@all)
- [ ] All test suites pass via Dune (@runtest)
- [ ] Every v1 source construct lowers through HIR and MIR to verified bytecode
- [ ] MIR verification runs after lowering and optimization
- [ ] Bytecode verification runs before execution and after loading
- [ ] O0, O1, O2 equivalence tests pass
- [ ] Forced-GC and tiny-heap tests pass
- [ ] Tail-recursive stress has bounded frame growth
- [ ] CLI commands `parse`, `typecheck`, `run`, `compile`, `disasm` all pass integration tests
- [ ] Fibonacci prints 55, Ackermann prints 61, list-map sum prints 30
- [ ] Direct run and compiled bytecode run match
- [ ] Malformed source and bytecode fail safely with non-zero exit status
- [ ] No backend code left in an unbuilt island
- [ ] No `.aside`, placeholder field index 0, or provisional tag remains
- [ ] CI workflow configured and passes
- [ ] Documentation and verification summary artifact complete
