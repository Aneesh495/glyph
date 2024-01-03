(** Desugaring from surface [Ast] to [Hir]. *)

val desugar_program : Ast.program -> Hir.program
val desugar_expr_standalone : Ast.expr -> Hir.expr
