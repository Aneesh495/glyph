# Glyph language reference

Glyph is a small, strict, mostly-functional language with Hindley–Milner types,
algebraic data types, and pattern matching. Evaluation order is left-to-right,
call-by-value. There is no laziness and no mutable cells in the core language
(`mut` may appear as a reserved word for future use).

This is the surface language the lexer/parser accept and the typechecker
understands. HIR and MIR are described in [ir.md](ir.md).

## Lexical structure

### Characters and encoding

Source is UTF-8. Identifiers are currently ASCII letters, digits, and `_`.
Keywords are reserved and never treated as identifiers.

### Whitespace and comments

Whitespace (spaces, tabs, newlines) separates tokens and is otherwise ignored.

Comments:

```glyph
(* single-line-ish comment *)
(* nested (* comments *) are supported when the lexer implements nesting *)
```

### Identifiers

```ebnf
ident      = letter { letter | digit | "_" } ;
letter     = "a"…"z" | "A"…"Z" ;
digit      = "0"…"9" ;
```

Convention: value names and type variables start lowercase (`map`, `a`);
type constructors and data constructors start uppercase (`List`, `Cons`).
The compiler does not enforce case, but the prelude and examples do.

`_` alone is the wildcard pattern / ignored binding.

### Keywords

```
and  else  extern  false  fn  if  in  let  match  mut  not  or  rec  then  true  type  with
```

(`fn` / `mut` may be reserved for alternate surface syntax or future features;
the primary binding form in Glyph is `let` / `let rec`, matching the README.)

### Literals

| Form | Type | Examples |
|------|------|----------|
| Integer | `Int` | `0`, `42`, `-7` (unary `-`) |
| Float | `Float` | `3.14`, `0.5` |
| Boolean | `Bool` | `true`, `false` |
| String | `String` | `"hello"`, `"line\n"` |
| Unit | `Unit` | `()` |

String escapes (typical): `\n`, `\t`, `\\`, `\"`.

### Operators and punctuation

```
+  -  *  /  %
==  !=  <  <=  >  >=
&&  ||
::  |>
->  |  =
(  )  [  ]  ,  ;
```

### Operator precedence (high → low)

Pratt binding powers (approximate):

| Level | Operators | Assoc |
|------:|-----------|-------|
| 90 | application (juxtaposition) | left |
| 80 | unary `-`, `not` | right |
| 70 | `*`, `/`, `%` | left |
| 60 | `+`, `-` | left |
| 50 | `::` | right |
| 40 | comparisons `== != < <= > >=` | none |
| 30 | `&&` | left |
| 20 | `\|\|` | left |
| 10 | `\|>` | left |

Application binds tighter than operators: `f x + 1` = `(f x) + 1`.

## Grammar (EBNF)

Simplified but faithful to the intended grammar. Optional sugar (`fn`,
bracket generics) may be accepted by the parser as it evolves; the core below
matches the ML-style surface used throughout the docs and examples.

```ebnf
program     = { item } ;

item        = type_def
            | let_item
            | extern_item ;

type_def    = "type" type_name { type_var } "=" constr { "|" constr } ;
type_name   = uppercase_ident ;
type_var    = lowercase_ident ;
constr      = uppercase_ident { type_expr } ;

let_item    = "let" [ "rec" ] binding { "and" binding } ;
binding     = ident { param } [ ":" type_expr ] "=" expr ;
param       = ident | "(" ident ":" type_expr ")" ;

extern_item = "extern" ident ":" type_expr ;

(* ---- types in surface syntax ---- *)

type_expr   = type_atom { "->" type_expr } ;   (* right-assoc arrow *)
type_atom   = type_name { type_atom }          (* type application *)
            | type_var
            | "(" type_expr { "," type_expr } ")"   (* unit / tuple / grouping *)
            | "Int" | "Float" | "Bool" | "String" | "Unit" ;

(* ---- expressions ---- *)

expr        = let_expr
            | if_expr
            | match_expr
            | lambda
            | infix_expr ;

let_expr    = "let" [ "rec" ] binding { "and" binding } "in" expr ;
if_expr     = "if" expr "then" expr "else" expr ;
match_expr  = "match" expr "with" { "|" pattern "->" expr } ;
lambda      = "\\" { pattern } "->" expr ;     (* if supported *)
                                           (* or: fun form via sugar *)

infix_expr  = pratt_expr ;                 (* see precedence table *)

atom        = literal
            | ident
            | constructor_app
            | "(" expr { "," expr } ")"    (* unit / tuple / grouping *)
            | "(" expr ")" ;

constructor_app = uppercase_ident { atom } ;

(* application is juxtaposition of atoms / postfix *)

(* ---- patterns ---- *)

pattern     = pattern "as" ident
            | pattern "|" pattern          (* or-pattern *)
            | constr_pat
            | literal
            | ident
            | "_"
            | "(" pattern { "," pattern } ")" ;

constr_pat  = uppercase_ident { pattern } ;
```

### Program shape

A compilation unit is a sequence of top-level items. Executable programs
define `main` (a value of a printable or unit-producing type); `glyph run`
evaluates `main`.

```glyph
type List a = Nil | Cons a (List a)

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons x rest -> Cons (f x) (map f rest)

let main = print_int (fib 20)
```

## Types

### Built-in types

| Name | Kind | Notes |
|------|------|-------|
| `Int` | `*` | Machine integers (platform `int` at runtime) |
| `Float` | `*` | IEEE-ish floats |
| `Bool` | `*` | `true` / `false` |
| `String` | `*` | Immutable, heap-allocated |
| `Unit` | `*` | Single value `()` |
| `(t1 * t2 * …)` | product | Tuples; `()` is unit |
| `t1 -> t2` | function | Strict function space |

### Algebraic data types

```glyph
type Option a = None | Some a
type Either a b = Left a | Right b
type List a = Nil | Cons a (List a)
```

Constructors are functions (or constants) in the value namespace:

- `None : forall a. Option a`
- `Some : forall a. a -> Option a`
- `Cons : forall a. a -> List a -> List a`

Nullary constructors need no parentheses: `Nil`, `None`.

### Polymorphism

Types of `let`-bound names are generalized (Hindley–Milner). Function
parameters and lambda-bound variables stay monomorphic within a single
instantiation — classic HM, not System F with first-class polymorphism.

```glyph
let id x = x
(* id : forall a. a -> a *)

let weird =
  let f x = x in
  (f 1, f true)   (* OK: f generalized at let *)
```

See [type-system.md](type-system.md).

### Annotations

Optional annotations on binders and `extern`:

```glyph
let add (x : Int) (y : Int) : Int = x + y
extern print_int : Int -> Unit
```

Annotations are checked against inferred types (unified), not trusted blindly.

## Pattern matching

`match` is exhaustive (typechecker / pattern compiler should warn or error on
missing constructors). Clauses are tried via a compiled decision tree, not
naïve left-to-right backtracking at runtime.

### Pattern forms

| Pattern | Meaning |
|---------|---------|
| `_` | Ignore |
| `x` | Bind variable |
| `42` / `true` / `"hi"` | Literal match |
| `Nil` | Nullary constructor |
| `Cons x xs` | Constructor + subpatterns |
| `(a, b)` | Tuple |
| `p1 \| p2` | Or-pattern (same bindings on both sides) |
| `p as x` | Match `p` and bind whole value to `x` |

### Examples

```glyph
let rec length xs =
  match xs with
  | Nil -> 0
  | Cons _ rest -> 1 + length rest

let head_or xs default =
  match xs with
  | Nil -> default
  | Cons x _ -> x

let classify x =
  match x with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "many"
```

Nested matches and nested constructors are fine; the pattern compiler
specializes one column at a time.

## Expressions

### Let and let-rec

```glyph
let x = 1 in
let y = x + 2 in
y * 3

let rec even n = if n == 0 then true else odd (n - 1)
and odd n = if n == 0 then false else even (n - 1)
```

Top-level `let rec` may omit `in` (the rest of the file is the body scope).

### Conditionals

```glyph
if n <= 1 then n else fib (n - 1) + fib (n - 2)
```

`if` is an expression; both branches must unify to the same type. Desugaring
turns `&&` / `||` into nested `if` for short-circuit behavior.

### Application and pipelines

```glyph
map (fun x -> x + 1) xs
xs |> map (fun x -> x + 1) |> length
```

(`fun` / `\` sugar depends on parser support; the typed core is a lambda.)

### Tuples

```glyph
let p = (1, true, "x")
let (a, b, c) = p
```

## Prelude and builtins

`std/prelude.gl` is prepended or opened by the driver. Typical builtins:

| Name | Type | Effect |
|------|------|--------|
| `print_int` | `Int -> Unit` | Print decimal + newline |
| `print_string` | `String -> Unit` | Print string |
| `print_bool` | `Bool -> Unit` | Print `true`/`false` |

Arithmetic and comparisons on `Int`/`Float`/`Bool` are primitive operators,
not library calls.

## Full examples

### Fibonacci

```glyph
let rec fib n =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

let main = print_int (fib 20)
```

### Lists

```glyph
type List a = Nil | Cons a (List a)

let rec sum xs =
  match xs with
  | Nil -> 0
  | Cons x rest -> x + sum rest

let main =
  print_int (sum (Cons 1 (Cons 2 (Cons 3 Nil))))
```

### Closures

```glyph
let make_adder n =
  let add x = n + x in
  add

let main =
  let add5 = make_adder 5 in
  print_int (add5 10)
```

### Ackermann

```glyph
let rec ack m n =
  if m == 0 then n + 1
  else if n == 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let main = print_int (ack 3 3)
```

## What Glyph is not (yet)

- Modules / separate compilation units beyond one file + prelude
- Type classes / traits
- First-class polymorphism (rank-N)
- Mutable references in the core language
- Exceptions (use `Option` / `Either`)

The compiler pipeline is built so these can grow without rewriting the VM —
HIR and MIR already have room for new expression forms.
