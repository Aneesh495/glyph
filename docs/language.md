# Language

Glyph is a strict, expression-oriented functional language with algebraic data
types, pattern matching, and Hindley–Milner type inference.

## Lexical structure

- Comments: `//` line comments and nested `/* ... */` block comments
- Identifiers: `foo`, `bar_baz`, `_ignored`
- Constructors: capitalized, `None`, `Cons`, `Ok`
- Integers: `0`, `42`, `0xFF`
- Floats: `3.14`, `1.0e-3`
- Strings: `"hello\n"` with common escapes
- Characters: `'a'`, `'\n'`

## Types

```glyph
type Option a = None | Some a

type Result a e = Ok a | Err e

type List a = Nil | Cons a (List a)

type Point = { x : Int, y : Int }
```

Type variables are lowercase. The prelude binds `Int`, `Float`, `Bool`,
`String`, `Char`, `Unit`, and the usual option/list/result constructors.

## Expressions

```glyph
let add x y = x + y

let rec fact n =
  if n <= 1 then 1 else n * fact (n - 1)

let pipe =
  [1; 2; 3]
  |> map (fun x -> x * 2)
  |> filter (fun x -> x > 2)

let inspect x =
  match x with
  | None -> 0
  | Some n when n > 0 -> n
  | Some n -> -n
```

### Functions

`fun x y -> body` and `let f x y = body` are equivalent. Multi-argument
functions are curried. Recursive bindings use `let rec`; mutual recursion
uses `let rec ... and ...`.

### Operators

| Prec | Assoc | Operators |
|------|-------|-----------|
| 1 | left | `\|\|` |
| 2 | left | `&&` |
| 3 | left | `=\|\|`, `<>`, `<`, `<=`, `>`, `>=` |
| 4 | right | `::` |
| 5 | left | `+`, `-` |
| 6 | left | `*`, `/`, `%` |
| 7 | left | `\|>` |
| app | left | juxtaposition |

(Exact tables live in the parser; this is the mental model.)

## Patterns

Patterns appear in `match`, `let`, and function arguments:

- `_`, variables, literals
- constructor patterns: `Cons x xs`
- tuples: `(a, b, c)`
- as-patterns: `xs as all`
- or-patterns: `0 | 1`
- guards: `n when n > 0`

The pattern compiler checks basic exhaustiveness and lowers nested matches
into decision trees before SSA construction.

## Modules (lightweight)

A file is a sequence of toplevel type declarations and `let` bindings. The
entry point is a binding named `main` (value or nullary function). The CLI
evaluates `main` after loading the prelude.

## Prelude

`std/prelude.gl` defines list helpers, option helpers, and printing wrappers
that bottom out in VM builtins (`print_int`, `print_string`, …).
