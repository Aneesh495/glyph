(** Bytecode interpreter with stack frames and GC roots. *)

type frame = {
  mutable regs : Value.t array;
  mutable ip : int;
  fn_id : int;
  caller_dst : int option;
      (** Register in the caller to receive the return value. *)
}

type vm = {
  chunk : Chunk.t;
  heap : Heap.t;
  mutable frames : frame list;
  mutable globals : Value.t array;
  mutable halted : bool;
  mutable exit_value : Value.t;
}

type result =
  | Ok of Value.t
  | Runtime_error of string

val create : ?heap_capacity:int -> Chunk.t -> vm
val run : vm -> result
val run_chunk : Chunk.t -> result
val step : vm -> unit
(** Execute a single instruction (for debugging). *)

val current_frame : vm -> frame option
val pp_frame : Format.formatter -> frame -> unit
