(** Maranget-style pattern match compilation to decision trees. *)

val compile_match :
  scrutinee:Hir.atom -> Hir.match_arm list -> span:Span.t -> Hir.expr

val compile_expr : Hir.expr -> Hir.expr
val compile_program : Hir.program -> Hir.program

(** Conservative exhaustiveness check given a universe of constructors. *)
val is_exhaustive :
  universe:Hir.ctor_info list -> Hir.match_arm list -> bool
