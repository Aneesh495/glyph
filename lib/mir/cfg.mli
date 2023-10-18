(** Control-flow graph utilities for [Mir.func]. *)

type t

val build : Mir.func -> t
val label_to_index : t -> Mir.label -> int
val index_to_label : t -> int -> Mir.label
val successors : t -> Mir.label -> Mir.label list
val predecessors : t -> Mir.label -> Mir.label list
val reverse_postorder : t -> Mir.label list
val postorder : t -> Mir.label list
val reachable : t -> Mir.label list
val node_count : t -> int
val pred_map : t -> (Mir.label, Mir.label list) Hashtbl.t
val succ_map : t -> (Mir.label, Mir.label list) Hashtbl.t
val prune_unreachable : Mir.func -> Mir.func
val split_critical_edges : Mir.func -> Mir.func
val pp : Format.formatter -> t -> unit
