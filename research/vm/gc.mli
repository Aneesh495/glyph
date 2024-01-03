(** Semi-space copying garbage collector (Cheney's algorithm).

    Two equal-sized spaces: [fromspace] (allocation) and [tospace]
    (evacuation destination). Collection proceeds as:

    {v}
      1. Flip: tospace becomes the new allocation area (bump = 0).
      2. Evacuate every root (stack regs, globals) into tospace.
      3. Scan tospace left-to-right ("gray" objects). For each field that
         is a [Value.Ptr], evacuate the referent if needed and rewrite
         the field to the new location.
      4. When the scan pointer meets the bump pointer, all reachable
         objects have been copied. Discard fromspace.
    {v}

    Forwarding pointers make evacuation idempotent: the first visit copies
    the object and stores [Some new_loc] in [cell.forward]; subsequent
    visits return that location without copying again. *)

type roots = {
  get_stack_roots : unit -> Value.t array array;
      (** Each frame's register file. *)
  get_globals : unit -> Value.t array;
  set_stack_roots : Value.t array array -> unit;
  set_globals : Value.t array -> unit;
}

val collect : Heap.t -> roots -> unit
(** Run a full Cheney collection. *)

val install : Heap.t -> roots -> unit
(** Wire [Heap.set_collect_fn] so allocation pressure triggers [collect]. *)

val stats : unit -> int * int
(** [(collections, objects_copied)] since process start. *)
