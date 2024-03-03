(** Bytecode Verifier: validates chunk integrity, register bounds, constant references, and branch targets before execution. *)

val verify_chunk : Chunk.t -> (unit, string list) result
val check_chunk : Chunk.t -> unit
