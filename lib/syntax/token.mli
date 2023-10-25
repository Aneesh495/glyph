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

val make : kind -> span:Span.t -> lexeme:string -> t
val kind : t -> kind
val span : t -> Span.t
val lexeme : t -> string

val keyword_of_string : string -> keyword option
val string_of_keyword : keyword -> string
val string_of_binop : binop -> string
val string_of_unop : unop -> string
val string_of_kind : kind -> string

val is_eof : t -> bool
val is_keyword : t -> keyword -> bool
val is_ident : t -> bool
val is_ctor : t -> bool

val pp : Format.formatter -> t -> unit
val pp_kind : Format.formatter -> kind -> unit
