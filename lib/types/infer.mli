(** Algorithm W / Hindley–Milner inference for Glyph. *)

val infer_program :
  ?env:Env.t -> Ast.program -> (Env.t, Diagnostic.t list) result

val infer_expr :
  ?env:Env.t -> Ast.expr -> (Ty.ty, Diagnostic.t list) result

val infer_pat :
  ?env:Env.t ->
  expected:Ty.ty ->
  Ast.pat ->
  ((Ident.t * Ty.ty) list, Diagnostic.t list) result
