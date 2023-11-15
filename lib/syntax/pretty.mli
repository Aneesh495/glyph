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
val to_string : ?width:int -> Ast.program -> string
val program_to_string : Ast.program -> string
val expr_to_string : Ast.expr -> string
val pattern_to_string : Ast.pat -> string
val type_to_string : Ast.ty -> string
val item_to_string : Ast.item -> string
val print_expr : Ast.expr -> unit
val print_program : Ast.program -> unit
