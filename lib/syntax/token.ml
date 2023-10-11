(** Lexical tokens for the Glyph surface language. *)

type keyword =
  | Kw_fn
  | Kw_type
  | Kw_let
  | Kw_in
  | Kw_if
  | Kw_then
  | Kw_else
  | Kw_match
  | Kw_with
  | Kw_true
  | Kw_false
  | Kw_and
  | Kw_or
  | Kw_not
  | Kw_extern
  | Kw_mut
  | Kw_rec

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
let kind t = t.kind
let span t = t.span
let lexeme t = t.lexeme

let keyword_of_string = function
  | "fn" -> Some Kw_fn
  | "type" -> Some Kw_type
  | "let" -> Some Kw_let
  | "in" -> Some Kw_in
  | "if" -> Some Kw_if
  | "then" -> Some Kw_then
  | "else" -> Some Kw_else
  | "match" -> Some Kw_match
  | "with" -> Some Kw_with
  | "true" -> Some Kw_true
  | "false" -> Some Kw_false
  | "and" -> Some Kw_and
  | "or" -> Some Kw_or
  | "not" -> Some Kw_not
  | "extern" -> Some Kw_extern
  | "mut" -> Some Kw_mut
  | "rec" -> Some Kw_rec
  | _ -> None

let string_of_keyword = function
  | Kw_fn -> "fn"
  | Kw_type -> "type"
  | Kw_let -> "let"
  | Kw_in -> "in"
  | Kw_if -> "if"
  | Kw_then -> "then"
  | Kw_else -> "else"
  | Kw_match -> "match"
  | Kw_with -> "with"
  | Kw_true -> "true"
  | Kw_false -> "false"
  | Kw_and -> "and"
  | Kw_or -> "or"
  | Kw_not -> "not"
  | Kw_extern -> "extern"
  | Kw_mut -> "mut"
  | Kw_rec -> "rec"

let string_of_binop = function
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_mod -> "%"
  | Op_eq -> "=="
  | Op_neq -> "!="
  | Op_lt -> "<"
  | Op_le -> "<="
  | Op_gt -> ">"
  | Op_ge -> ">="
  | Op_and -> "&&"
  | Op_or -> "||"
  | Op_cons -> "::"
  | Op_pipe -> "|>"

let string_of_unop = function
  | Op_neg -> "-"
  | Op_not -> "not"

let string_of_kind = function
  | Ident s -> s
  | Ctor s -> s
  | Int n -> Int64.to_string n
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "%S" s
  | Char c -> Printf.sprintf "%C" c
  | Keyword kw -> string_of_keyword kw
  | Binop op -> string_of_binop op
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

let is_eof t = t.kind = Eof
let is_keyword t kw = t.kind = Keyword kw

let is_ident t =
  match t.kind with
  | Ident _ -> true
  | _ -> false

let is_ctor t =
  match t.kind with
  | Ctor _ -> true
  | _ -> false

let pp_kind fmt k = Format.pp_print_string fmt (string_of_kind k)
let pp fmt t = Format.fprintf fmt "%s@%a" (string_of_kind t.kind) Span.pp t.span
