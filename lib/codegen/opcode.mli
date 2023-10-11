(** Glyph bytecode instruction set.

    Register-based ISA. Operands are unsigned 16-bit register indices or
    signed/unsigned immediates packed beside the opcode. Multi-operand
    instructions (calls, alloc) encode arity in [a] and take trailing
    register lists from the instruction stream via [extra]. *)

(** Primitive opcodes. *)
type op =
  (* Moves / constants *)
  | Op_nop
  | Op_load_const (** [dst, const_idx] *)
  | Op_move (** [dst, src] *)
  | Op_load_global (** [dst, global_idx] *)
  | Op_store_global (** [global_idx, src] *)
  (* Integer arithmetic *)
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_mod
  (* Float arithmetic *)
  | Op_add_f
  | Op_sub_f
  | Op_mul_f
  | Op_div_f
  (* Unary *)
  | Op_neg
  | Op_neg_f
  | Op_not
  (* Comparisons (result is Bool) *)
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_eq_f
  | Op_ne_f
  | Op_lt_f
  | Op_le_f
  | Op_gt_f
  | Op_ge_f
  | Op_and
  | Op_or
  (* Control flow — [a] is target IP or condition register *)
  | Op_jump (** [target] *)
  | Op_jump_if (** [cond, target] *)
  | Op_jump_if_not (** [cond, target] *)
  | Op_switch (** [scrutinee, n_cases] + case table in [extra] *)
  (* Calls *)
  | Op_call (** [dst, fn_id, argc] + arg regs *)
  | Op_call_closure (** [dst, clo_reg, argc] + arg regs *)
  | Op_tail_call (** [fn_id, argc] + arg regs *)
  | Op_tail_call_closure (** [clo_reg, argc] + arg regs *)
  | Op_ret (** [src] — optional; [a]=0 and flag in b for void *)
  | Op_ret_void
  (* Heap *)
  | Op_alloc_tuple (** [dst, nfields] + field regs *)
  | Op_alloc_adt (** [dst, tag, nfields] + field regs *)
  | Op_alloc_closure (** [dst, fn_id, nenv] + env regs *)
  | Op_get_field (** [dst, obj, index] *)
  | Op_set_field (** [obj, index, src] *)
  | Op_get_tag (** [dst, obj] *)
  | Op_tuple_get (** [dst, tup, index] *)
  (* List helpers *)
  | Op_cons (** [dst, head, tail] *)
  | Op_car (** [dst, cell] *)
  | Op_cdr (** [dst, cell] *)
  (* GC / effects *)
  | Op_gc_safepoint (** no-op placeholder for cooperative GC *)
  | Op_print (** [src] — polymorphic print *)
  | Op_print_int
  | Op_print_string
  | Op_print_bool
  | Op_halt (** optional value in [a]; [b]=1 if present *)

(** A single decoded instruction. [extra] holds trailing register indices
    or switch (tag, target) pairs flattened as [tag0; ip0; tag1; ip1; ...]. *)
type instr = {
  op : op;
  a : int;
  b : int;
  c : int;
  extra : int array;
}

val op_to_int : op -> int
val op_of_int : int -> op
val op_name : op -> string
val op_arity_regs : op -> int
(** Number of primary register/immediate slots used (a/b/c), not counting [extra]. *)

val make : ?a:int -> ?b:int -> ?c:int -> ?extra:int array -> op -> instr
val pp_instr : Format.formatter -> instr -> unit
val pp_op : Format.formatter -> op -> unit

(** Binary encode/decode of a single instruction (for .gbc). *)
val encode : instr -> bytes
val decode : bytes -> int -> instr * int
(** [decode buf off] returns instruction and next offset. *)
