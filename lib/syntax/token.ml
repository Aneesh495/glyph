(** Lexical tokens for Glyph source. *)

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
  | Kw_external
  | Kw_module
  | Kw_open
  | Kw_as
  | Kw_when
  | Kw_mutable

type punct =
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Semicolon
  | Colon
  | ColonColon
  | Arrow
  | FatArrow
  | Pipe
  | Dot
  | Underscore
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | AmpAmp
  | PipePipe
  | AtAt
  | DotDot
  | Bang
  | Question
  | Tilde
  | Backslash
  | Apostrophe

type kind =
  | Keyword of keyword
  | Punct of punct
  | Operator of string
  | Lit_int of string
  | Lit_float of string
  | Lit_string of string
  | Lit_char of char
  | Ident of string
  | UpperIdent of string
  | Comment of string
  | Eof
  | Error of string

type t = {
  kind : kind;
  span : Span.t;
  raw : string;
}

let make kind span raw = { kind; span; raw }

let keyword_table : (string, keyword) Hashtbl.t =
  let tbl = Hashtbl.create 32 in
  List.iter
    (fun (name, kw) -> Hashtbl.add tbl name kw)
    [
      ("let", Kw_let);
      ("rec", Kw_rec);
      ("and", Kw_and);
      ("in", Kw_in);
      ("if", Kw_if);
      ("then", Kw_then);
      ("else", Kw_else);
      ("match", Kw_match);
      ("with", Kw_with);
      ("type", Kw_type);
      ("of", Kw_of);
      ("fun", Kw_fun);
      ("fn", Kw_fn);
      ("true", Kw_true);
      ("false", Kw_false);
      ("external", Kw_external);
      ("module", Kw_module);
      ("open", Kw_open);
      ("as", Kw_as);
      ("when", Kw_when);
      ("mutable", Kw_mutable);
    ];
  tbl

let lookup_keyword name = Hashtbl.find_opt keyword_table name

let keyword_of_ident name =
  match lookup_keyword name with
  | Some kw -> Keyword kw
  | None ->
      if String.length name > 0 && name.[0] >= 'A' && name.[0] <= 'Z' then
        UpperIdent name
      else Ident name

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
  | Kw_external -> "external"
  | Kw_module -> "module"
  | Kw_open -> "open"
  | Kw_as -> "as"
  | Kw_when -> "when"
  | Kw_mutable -> "mutable"

let punct_to_string = function
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonColon -> "::"
  | Arrow -> "->"
  | FatArrow -> "=>"
  | Pipe -> "|"
  | Dot -> "."
  | Underscore -> "_"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | AmpAmp -> "&&"
  | PipePipe -> "||"
  | AtAt -> "@@"
  | DotDot -> ".."
  | Bang -> "!"
  | Question -> "?"
  | Tilde -> "~"
  | Backslash -> "\\"
  | Apostrophe -> "'"

let kind_to_string = function
  | Keyword kw -> keyword_to_string kw
  | Punct p -> punct_to_string p
  | Operator op -> op
  | Lit_int s -> s
  | Lit_float s -> s
  | Lit_string s -> Printf.sprintf "%S" s
  | Lit_char c -> Printf.sprintf "%C" c
  | Ident s -> s
  | UpperIdent s -> s
  | Comment s -> Printf.sprintf "(* %s *)" s
  | Eof -> "<eof>"
  | Error msg -> Printf.sprintf "<error: %s>" msg

let to_string tok =
  Printf.sprintf "%s @ %s" (kind_to_string tok.kind) (Span.to_string tok.span)

let pp fmt tok = Format.pp_print_string fmt (to_string tok)

let is_eof tok = match tok.kind with Eof -> true | _ -> false
let is_error tok = match tok.kind with Error _ -> true | _ -> false

let is_keyword tok kw =
  match tok.kind with Keyword k -> k = kw | _ -> false

let is_punct tok p =
  match tok.kind with Punct q -> q = p | _ -> false

let is_ident tok =
  match tok.kind with Ident _ | UpperIdent _ -> true | _ -> false

let is_literal tok =
  match tok.kind with
  | Lit_int _ | Lit_float _ | Lit_string _ | Lit_char _
  | Keyword (Kw_true | Kw_false) ->
      true
  | _ -> false

(** Operator characters that may form multi-character operators. *)
let is_op_char = function
  | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
  | '?' | '@' | '^' | '|' | '~' | '#' ->
      true
  | _ -> false

(** Map a known operator lexeme to a punctuation token when possible. *)
let classify_operator op =
  match op with
  | "->" -> Punct Arrow
  | "=>" -> Punct FatArrow
  | "::" -> Punct ColonColon
  | ":" -> Punct Colon
  | "=" -> Punct Eq
  | "<>" -> Punct Neq
  | "<" -> Punct Lt
  | "<=" -> Punct Le
  | ">" -> Punct Gt
  | ">=" -> Punct Ge
  | "+" -> Punct Plus
  | "-" -> Punct Minus
  | "*" -> Punct Star
  | "/" -> Punct Slash
  | "%" -> Punct Percent
  | "&&" -> Punct AmpAmp
  | "||" -> Punct PipePipe
  | "@@" -> Punct AtAt
  | ".." -> Punct DotDot
  | "|" -> Punct Pipe
  | "!" -> Punct Bang
  | "?" -> Punct Question
  | "~" -> Punct Tilde
  | "." -> Punct Dot
  | _ -> Operator op

let all_keywords =
  [
    Kw_let;
    Kw_rec;
    Kw_and;
    Kw_in;
    Kw_if;
    Kw_then;
    Kw_else;
    Kw_match;
    Kw_with;
    Kw_type;
    Kw_of;
    Kw_fun;
    Kw_fn;
    Kw_true;
    Kw_false;
    Kw_external;
    Kw_module;
    Kw_open;
    Kw_as;
    Kw_when;
    Kw_mutable;
  ]
