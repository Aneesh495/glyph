(** Pretty-printer for Glyph surface AST. *)

val pp_lit : Format.formatter -> Ast.lit -> unit
val pp_ty : Format.formatter -> Ast.ty -> unit
val pp_pat : Format.formatter -> Ast.pat -> unit
val pp_expr : Format.formatter -> Ast.expr -> unit
val pp_item : Format.formatter -> Ast.item -> unit
val pp_program : Format.formatter -> Ast.program -> unit

val show_ty : Ast.ty -> string
val show_pat : Ast.pat -> string
val show_expr : Ast.expr -> string
val show_item : Ast.item -> string
val show_program : Ast.program -> string

(** Render with a maximum line width (default 80). *)
val to_string : ?width:int -> Ast.program -> string
