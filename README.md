# Glyph

A strict, functional programming language compiler and virtual machine implemented in OCaml.

Glyph features Hindley–Milner type inference, pattern compilation with Maranget decision trees, SSA-form Middle Intermediate Representation (MIR), an optimizing compiler pass pipeline, bytecode emission and verification, and a register-based VM with a Cheney copying garbage collector.

```
source ──▶ lexer ──▶ parser ──▶ typed AST ──▶ HIR ──▶ pattern compilation
   ──▶ SSA MIR ──▶ verifier ──▶ optimizer (O0/O1/O2) ──▶ bytecode (.gbc)
   ──▶ bytecode verifier ──▶ VM & Cheney GC
```

---

## Quick Start

### Prerequisites
- OCaml 5.2+ (tested on OCaml 5.5)
- [OPAM](https://opam.ocaml.org/) & [Dune](https://dune.build/) (>= 3.7)

```bash
# 1. Setup local environment
opam switch create glyph-sys ocaml-system   # or use existing 5.2+ switch
eval $(opam env --switch=glyph-sys)
opam install -y dune cmdliner alcotest qcheck qcheck-alcotest

# 2. Build the entire compiler, runtime, and CLI
dune build @all

# 3. Run the full test suite (49/49 tests passing)
dune runtest

# 4. Run automated verification suite
bash scripts/verify.sh
```

---

## CLI Usage

The `glyph` command-line tool provides full access to every compiler stage:

```bash
# Parse a source file and display AST / diagnostics
glyph parse examples/fib.gl

# Typecheck and run Hindley-Milner type inference
glyph typecheck examples/fib.gl

# Compile source to a verified bytecode chunk (.gbc)
glyph compile examples/fib.gl -o fib.gbc

# Disassemble a bytecode chunk
glyph disasm fib.gbc

# Execute a program directly from source (compiles & runs on the VM)
glyph run examples/fib.gl

# Execute a precompiled bytecode chunk
glyph run fib.gbc
```

---

## Canonical Examples

All canonical example programs compile and run end-to-end:

### 1. Fibonacci (`examples/fib.gl`)
```glyph
let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let main = print_int (fib 10)
```
```bash
glyph run examples/fib.gl
# Output: 55
```

### 2. Ackermann Function (`examples/ackermann.gl`)
```glyph
let rec ack m n =
  if m = 0 then n + 1
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let main = print_int (ack 3 3)
```
```bash
glyph run examples/ackermann.gl
# Output: 61
```

### 3. List Map & Fold (`examples/list_map.gl`)
```glyph
type List a = Nil | Cons a (List a)

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons x rest -> Cons (f x) (map f rest)

let rec sum xs =
  match xs with
  | Nil -> 0
  | Cons x rest -> x + sum rest

let nums = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

let main = print_int (sum (map (fun x -> x * x) nums))
```
```bash
glyph run examples/list_map.gl
# Output: 30
```

---

## Compiler Pipeline Architecture

- **`lib/util/` (`glyph_util`)**: Source coordinate spans, structured diagnostics, interned identifiers.
- **`lib/syntax/` (`glyph_syntax`)**: Lexer, recursive-descent parser, AST definitions.
- **`lib/types/` (`glyph_types`)**: Types, unification, HM Algorithm W type inference.
- **`lib/hir/` (`glyph_hir`)**: A-normal form (ANF) desugaring and Maranget decision-tree pattern compilation.
- **`lib/mir/` (`glyph_mir`)**: SSA control flow graph (CFG) representation, HIR-to-MIR lowering, MIR verifier.
- **`lib/opt/` (`glyph_opt`)**: Optimization pipeline (constant folding, CFG simplification, DCE, copy propagation).
- **`lib/bytecode/` (`glyph_bytecode`)**: Bytecode chunk format (`GLYPH01`), opcode encoding, register allocator, disassembler, and bytecode verifier.
- **`lib/vm/` (`glyph_vm`)**: Register-based VM, value model, heap objects, semi-space Cheney copying GC, and runtime builtins.
- **`lib/driver/` (`glyph_driver`)**: End-to-end pipeline driver coordinating all stages.
- **`bin/`**: `glyph` CLI frontend.
- **`test/`**: Comprehensive Alcotest and QCheck test suites (positive, negative, differential, property, GC stress, golden).

---

## Documentation

- [Language Specification (v1)](docs/LANGUAGE_V1.md)
- [Repair & Integration Status](docs/REPAIR_STATUS.md)
- [Architecture Overview](docs/architecture.md)
- [IR & SSA Design](docs/ir.md)
- [Optimizations](docs/optimizations.md)
- [VM & Garbage Collection](docs/vm.md)

---

## License

MIT
