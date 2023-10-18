(** Bytecode disassembler. *)

val to_string : Chunk.t -> string
val pp : Format.formatter -> Chunk.t -> unit
val disasm_instr : Chunk.t -> int -> Opcode.instr -> string
val disasm_func : Chunk.t -> Chunk.func -> string
