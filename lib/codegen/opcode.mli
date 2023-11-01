(** Glyph bytecode instruction set. *)

type op =
  | Op_nop
  | Op_load_const
  | Op_move
  | Op_load_global
  | Op_store_global
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
  | Op_neg | Op_neg_f | Op_not
  | Op_eq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
  | Op_eq_f | Op_ne_f | Op_lt_f | Op_le_f | Op_gt_f | Op_ge_f
  | Op_and | Op_or
  | Op_jump | Op_jump_if | Op_jump_if_not | Op_switch
  | Op_call | Op_call_closure | Op_tail_call | Op_tail_call_closure
  | Op_ret | Op_ret_void
  | Op_alloc_tuple | Op_alloc_adt | Op_alloc_closure
  | Op_get_field | Op_set_field | Op_get_tag | Op_tuple_get
  | Op_cons | Op_car | Op_cdr
  | Op_gc_safepoint
  | Op_print | Op_print_int | Op_print_string | Op_print_bool
  | Op_halt

type instr = { op : op; a : int; b : int; c : int; extra : int array }

val op_to_int : op -> int
val op_of_int : int -> op
val op_name : op -> string
val op_arity_regs : op -> int
val make : ?a:int -> ?b:int -> ?c:int -> ?extra:int array -> op -> unit -> instr
val pp_instr : Format.formatter -> instr -> unit
val pp_op : Format.formatter -> op -> unit
val pp_opcode : Format.formatter -> instr -> unit
val encode : instr -> bytes
val decode : bytes -> int -> instr * int
val encode_words : instr -> int32 list
val decode_words : int32 array -> int -> instr * int
val is_terminator : op -> bool
