(** Lexical tokens for Glyph — matches the hand-written lexer. *)

type keyword =
  | Kw_let
  | Kw_rec
  | Kw_and
  | Kw_in
  | Kw_if
  | Kw_then
  | Kw_else
  | Kw_match
  | Kw_with
  | Kw_type
  | Kw_of
  | Kw_fun
  | Kw_fn
  | Kw_true
  | Kw_false
  | Kw_extern
  | Kw_external
  | Kw_module
  | Kw_open
  | Kw_as
  | Kw_when
  | Kw_mutable
  | Kw_not

type binop =
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_mod
  | Op_eq
  | Op_neq
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
  | Op_cons
  | Op_pipe

type unop =
  | Op_neg
  | Op_not

type kind =
  | Ident of string
  | Ctor of string
  | Int of int64
  | Float of float
  | String of string
  | Char of char
  | Keyword of keyword
  | Binop of binop
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Colon
  | Semicolon
  | Arrow
  | FatArrow
  | Equal
  | Pipe
  | Underscore
  | Eof

type t = {
  kind : kind;
  span : Span.t;
  lexeme : string;
}

let make kind ~span ~lexeme = { kind; span; lexeme }

let keyword_of_string = function
  | "let" -> Some Kw_let
  | "rec" -> Some Kw_rec
  | "and" -> Some Kw_and
  | "in" -> Some Kw_in
  | "if" -> Some Kw_if
  | "then" -> Some Kw_then
  | "else" -> Some Kw_else
  | "match" -> Some Kw_match
  | "with" -> Some Kw_with
  | "type" -> Some Kw_type
  | "of" -> Some Kw_of
  | "fun" -> Some Kw_fun
  | "fn" -> Some Kw_fn
  | "true" -> Some Kw_true
  | "false" -> Some Kw_false
  | "extern" | "external" -> Some Kw_external
  | "module" -> Some Kw_module
  | "open" -> Some Kw_open
  | "as" -> Some Kw_as
  | "when" -> Some Kw_when
  | "mutable" -> Some Kw_mutable
  | "not" -> Some Kw_not
  | _ -> None

let keyword_to_string = function
  | Kw_let -> "let"
  | Kw_rec -> "rec"
  | Kw_and -> "and"
  | Kw_in -> "in"
  | Kw_if -> "if"
  | Kw_then -> "then"
  | Kw_else -> "else"
  | Kw_match -> "match"
  | Kw_with -> "with"
  | Kw_type -> "type"
  | Kw_of -> "of"
  | Kw_fun -> "fun"
  | Kw_fn -> "fn"
  | Kw_true -> "true"
  | Kw_false -> "false"
  | Kw_extern | Kw_external -> "external"
  | Kw_module -> "module"
  | Kw_open -> "open"
  | Kw_as -> "as"
  | Kw_when -> "when"
  | Kw_mutable -> "mutable"
  | Kw_not -> "not"

let binop_to_string = function
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_mod -> "%"
  | Op_eq -> "="
  | Op_neq -> "<>"
  | Op_lt -> "<"
  | Op_le -> "<="
  | Op_gt -> ">"
  | Op_ge -> ">="
  | Op_and -> "&&"
  | Op_or -> "||"
  | Op_cons -> "::"
  | Op_pipe -> "|>"

let kind_to_string = function
  | Ident s | Ctor s -> s
  | Int n -> Int64.to_string n
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "%S" s
  | Char c -> Printf.sprintf "%C" c
  | Keyword kw -> keyword_to_string kw
  | Binop op -> binop_to_string op
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Colon -> ":"
  | Semicolon -> ";"
  | Arrow -> "->"
  | FatArrow -> "=>"
  | Equal -> "="
  | Pipe -> "|"
  | Underscore -> "_"
  | Eof -> "<eof>"

let is_eof t = match t.kind with Eof -> true | _ -> false
let is_keyword t kw = match t.kind with Keyword kw' -> kw' = kw | _ -> false
let is_kind t k = t.kind = k

let span t = t.span
let kind t = t.kind
let lexeme t = t.lexeme
let raw t = t.lexeme
