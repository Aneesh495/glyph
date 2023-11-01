(** Emit MIR into bytecode. *)

exception Emit_error of string

val emit_program : Mir.program -> Chunk.t
val emit : Mir.program -> Chunk.t
