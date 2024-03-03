(** Lowering from high-level intermediate representation (HIR) to mid-level IR (MIR). *)

val lower_program : Hir.program -> Mir.program
val lower_expr_standalone : Hir.expr -> Mir.program
