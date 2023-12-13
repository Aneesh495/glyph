(** Algorithm W / Hindley–Milner inference for Glyph. *)

(** Infer types for a whole program, returning the enriched environment
    (prelude + top-level bindings) or a list of diagnostics. *)
val infer_program :
  ?env:Env.t -> Ast.program -> (Env.t, Diagnostic.t list) result

(** Infer the type of a single expression. *)
val infer_expr :
  ?env:Env.t -> Ast.expr -> (Ty.ty, Diagnostic.t list) result

(** Infer a pattern against an expected type, returning bound variables. *)
val infer_pat :
  ?env:Env.t ->
  expected:Ty.ty ->
  Ast.pat ->
  ((Ident.t * Ty.ty) list, Diagnostic.t list) result
