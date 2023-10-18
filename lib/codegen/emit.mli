(** Emit MIR SSA/CFG into a register-based bytecode [Chunk.t]. *)

val emit : Mir.program -> Chunk.t
(** Lower a whole MIR program.

    Strategy:
    - Assign each SSA [vreg] a dense local register index (identity map when
      [n_vregs] is small; otherwise compact used-vreg remapping).
    - Emit blocks in reverse-postorder-ish source order; record label→IP.
    - Translate φ-nodes into [Op_move]s placed at the end of each predecessor
      (before that predecessor's terminator), once all label IPs are known.
    - Resolve jump/switch/call targets in a second pass. *)

val emit_func : Chunk.t -> Mir.func -> unit
(** Append one function's code into an existing chunk (used by tests). *)
