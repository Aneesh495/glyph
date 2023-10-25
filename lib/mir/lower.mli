(** Lowering from [Hir] to [Mir] (CFG + optional SSA). *)

val lower_program : ?ssa:bool -> Hir.program -> Mir.program
val lower_program_non_ssa : Hir.program -> Mir.program
