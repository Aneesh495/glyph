(** MIR Verifier: validates CFG consistency, terminator targets, and register invariants. *)

val verify_func : Mir.program -> Mir.func -> (unit, string list) result
val verify_program : Mir.program -> (unit, string list) result
val check_program : Mir.program -> unit
