# Glyph v1 Language Specification & Contract

This document freezes the language specification and executable subset for Glyph v1.
Every feature marked as **Supported** must parse, typecheck, lower to HIR and MIR, compile to bytecode, verify, and execute deterministically on the VM.
Features marked as **Explicitly Rejected / Deferred** produce source-located diagnostic errors with non-zero exit codes.

---

## 1. Feature Matrix

| Feature | Status | Specification / Semantics |
|---|---|---|
| **Unit literal** `()` | Supported | Type `Unit`, value `Unit` |
| **Boolean literals** `true`, `false` | Supported | Type `Bool`, value `Bool b` |
| **Integer literals** (e.g. `0`, `42`, `-7`) | Supported | Type `Int`, 63/64-bit integer values |
| **Float literals** (e.g. `3.14`) | Supported | Type `Float`, IEEE double precision |
| **Character literals** (e.g. `'a'`, `'\n'`) | Supported | Type `Char`, standard character literal |
| **String literals** (e.g. `"hello"`) | Supported | Type `String`, immutable heap-allocated string |
| **Tuples** (e.g. `(a, b, c)`) | Supported | Type `(t1, t2, ...)`, heap-allocated tuple object, 0-indexed projection |
| **Lists** (e.g. `Cons 1 (Cons 2 Nil)`) | Supported | Algebraic type `List a = Nil \| Cons a (List a)`. Constructor arity: `Nil` = 0, `Cons` = 2. |
| **Algebraic Data Types (ADTs)** | Supported | Declared with `type T a = C1 ... \| C2 ...`. Disjoint tagged heap representation. |
| **Variables** | Supported | Lexically scoped, immutable identifiers. |
| **Let binding** `let x = e1 in e2` | Supported | Evaluates `e1`, binds to `x` in `e2`. Non-recursive. |
| **Let rec binding** `let rec f x = ... in ...` | Supported | Single and mutual recursive functions with lexical closure creation. |
| **First-class functions & Closures** | Supported | Lambda abstractions `fun x -> ...` or `\x -> ...` capturing environment. |
| **Function application** | Supported | Left-to-right evaluation order, call-by-value, curried multi-arg application. |
| **Tail calls** | Supported | Tail-call optimization for self and mutual recursion without unbounded frame growth. |
| **Conditionals** `if c then t else e` | Supported | Strict boolean condition; `then` and `else` branches must unify to same type. |
| **Arithmetic operators** `+`, `-`, `*`, `/`, `%` | Supported | Integer and float arithmetic (`+` / `+.` via typing or operator overloading). Division by zero raises runtime error. |
| **Comparison operators** `==`, `!=`, `<`, `<=`, `>`, `>=` | Supported | Polymorphic equality on values, ordering on numbers. Note: `=` is supported as equality alias in syntax where parsed. |
| **Boolean operators** `&&`, `\|\|`, `not` | Supported | Short-circuit boolean conjunction and disjunction lowered to nested conditionals. |
| **Sequences** `e1; e2` or `begin ... end` | Supported | Evaluates `e1` for effect, discards result, evaluates `e2`. |
| **Pattern matching** `match e with \| p -> b` | Supported | Compiled to Maranget decision trees. Supports first-match semantics. |
| **Pattern: Wildcard** `_` | Supported | Matches any value without binding. |
| **Pattern: Variable** `x` | Supported | Matches any value and binds to identifier `x`. |
| **Pattern: Literals** | Supported | Matches unit, bool, int, float, char, string. |
| **Pattern: Constructors** `C p1 ... pn` | Supported | Checks constructor tag and matches sub-patterns. |
| **Pattern: Tuples** `(p1, ..., pn)` | Supported | Matches tuple arity and elements. |
| **Pattern: Or-patterns** `p1 \| p2` | Supported | Matches either pattern; bindings must be consistent. |
| **Pattern: As-patterns** `p as x` | Supported | Matches `p` and binds entire scrutinee to `x`. |
| **Pattern: Guards** `p when g -> b` | Supported | Guard evaluated only after structural match succeeds. |
| **Builtins** | Supported | `print_int`, `print_bool`, `print_string`, `print`, `string_of_int`, `string_concat`, `abort`. |
| **Records** `{ f1 = e1; f2 = e2 }`, `e.f` | **Deferred** | Records are explicitly deferred in v1. Construction and field access emit an explicit compile-time diagnostic: `"records are deferred in Glyph v1; use tuples or algebraic data types instead"`. |
| **Mutable references** `ref`, `:=`, `!` | **Deferred** | Not present in v1 core. |
| **Modules / Functors** | **Deferred** | Programs are single-file units optionally prepended with `std/prelude.gl`. |

---

## 2. Standard Program Entry Point

An executable Glyph program is evaluated top-down:
- Top-level `type` definitions declare ADTs and constructors.
- Top-level `let` and `let rec` bind module-scope values and functions.
- If a top-level `let main = ...` or top-level expression is present, `glyph run` executes it and prints results.
- Builtins like `print_int` print outputs deterministically to `stdout`.

---

## 3. Runtime Contracts & Value Representations

### Values (`Value.t`)
- `Int of int`: Immediate integer.
- `Float of float`: Immediate float.
- `Bool of bool`: Immediate boolean.
- `Unit`: Immediate unit `()`.
- `Ptr of int`: Heap pointer to a managed object.
- `Native of string * native`: Host built-in function.

### Heap Objects (`Heap.obj_kind`)
- `String of string`: Immutable string.
- `Tuple of Value.t array`: Elements accessible by integer index.
- `Adt of int * Value.t array`: Tagged union object with integer constructor tag and payload arguments.
- `Closure of int * Value.t array`: Code pointer (`fn_id`) + environment array of captured variables.

### Garbage Collector
- Semi-space bump allocator (`fromspace` and `tospace`).
- Cheney copying collection triggered upon space exhaustion or forced by test harness.
- Roots scanned: all active call-stack frames (`regs`), globals, and temporary roots during allocation.
