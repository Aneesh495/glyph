(** Bytecode disassembler. *)

val to_string : Chunk.t -> string
val pp : Format.formatter -> Chunk.t -> unit
val print : ?oc:out_channel -> Chunk.t -> unit
val disassemble_file : string -> string

val disasm_instr : Chunk.t -> int -> Opcode.instr -> string
val disasm_func : Chunk.t -> Chunk.func -> string

type stats = {
  n_funcs : int;
  n_constants : int;
  n_instructions : int;
  max_regs : int;
}

val stats : Chunk.t -> stats
val pp_stats : Format.formatter -> stats -> unit
val cfg_edges : Chunk.t -> Chunk.func -> (int * int) list
val verify_encoding : Chunk.t -> (unit, string) result
