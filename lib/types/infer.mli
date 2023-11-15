(** Algorithm W / Hindley–Milner inference for Glyph.

    {[
      Infer.infer_program :
        ?env:Env.t -> Ast.program ->
        (Infer.result, Diagnostic.t list) result
    ]}
*)

type result = {
  env : Env.t;
  (** Top-level value identifier → generalized scheme. *)
  schemes : Ty.scheme Ident.Map.t;
  (** Binding-site monotypes (instantiated principals). *)
  bindings : Ty.ty Ident.Map.t;
  (** Expression span → inferred type. *)
  expr_types : (Span.t * Ty.ty) list;
}

(** Infer a whole program. On success returns annotations + final env;
    on hard type errors returns diagnostics. *)
val infer_program :
  ?env:Env.t -> Ast.program -> (result, Diagnostic.t list) result

(** Infer a single expression, returning its type. *)
val infer_expr :
  ?env:Env.t -> Ast.expr -> (Ty.ty, Diagnostic.t list) result

(** Infer a pattern against [expected], returning bound variables. *)
val infer_pat :
  ?env:Env.t ->
  expected:Ty.ty ->
  Ast.pat ->
  ((Ident.t * Ty.ty) list, Diagnostic.t list) result
