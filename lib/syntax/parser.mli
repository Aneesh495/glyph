(** Hand-written Pratt / recursive-descent parser for Glyph. *)

type error = {
  message : string;
  span : Span.t;
  hint : string option;
}

exception Error of error

val parse_program : ?file:string -> string -> (Ast.program, error) result
val parse_program_exn : ?file:string -> string -> Ast.program
val parse_expr : ?file:string -> string -> (Ast.expr, error) result
val parse_ty : ?file:string -> string -> (Ast.ty, error) result

val error_to_diagnostic : error -> Diagnostic.t
