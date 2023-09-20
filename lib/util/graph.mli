(** Directed graphs for CFG and dependency analysis. *)

type node = int

type 'a t
(** Graph with node payloads of type ['a]. *)

val create : ?size:int -> unit -> 'a t
val copy : 'a t -> 'a t
val clear : 'a t -> unit

val add_node : 'a t -> 'a -> node
val set_payload : 'a t -> node -> 'a -> unit
val payload : 'a t -> node -> 'a
val node_count : 'a t -> int
val mem_node : 'a t -> node -> bool

val add_edge : 'a t -> src:node -> dst:node -> unit
val remove_edge : 'a t -> src:node -> dst:node -> unit
val has_edge : 'a t -> src:node -> dst:node -> bool
val successors : 'a t -> node -> node list
val predecessors : 'a t -> node -> node list
val out_degree : 'a t -> node -> int
val in_degree : 'a t -> node -> int
val edge_count : 'a t -> int

val iter_nodes : 'a t -> (node -> 'a -> unit) -> unit
val fold_nodes : 'a t -> (node -> 'a -> 'acc -> 'acc) -> 'acc -> 'acc
val iter_edges : 'a t -> (src:node -> dst:node -> unit) -> unit

(** Depth-first traversal from [start], calling [f] on discovery. *)
val dfs : 'a t -> start:node -> (node -> unit) -> unit

(** Breadth-first traversal from [start]. *)
val bfs : 'a t -> start:node -> (node -> unit) -> unit

(** Nodes reachable from [start]. *)
val reachable : 'a t -> start:node -> node list

(** Reverse post-order numbering starting from [entry]. *)
val reverse_postorder : 'a t -> entry:node -> node list

(** Kahn topological sort. Returns [Error cycle_nodes] on failure. *)
val topo_sort : 'a t -> (node list, node list) result

(** Tarjan strongly connected components (each component is a node list). *)
val sccs : 'a t -> node list list

(** Condensation DAG of SCCs: nodes are component indices. *)
val condensation : 'a t -> int t * (node -> int)

(** Dominance helpers (Lengauer-Tarjan style skeleton / iterative dataflow). *)
module Dom : sig
  (** Immediate dominator tree: [idom.(n)] is the immediate dominator of [n],
      or [n] itself for the entry. *)
  val immediate_dominators : 'a t -> entry:node -> node array

  (** Dominance frontier of each node (Cytron). *)
  val dominance_frontiers :
    'a t -> entry:node -> idom:node array -> node list array

  (** [dominates idom a b] is true if [a] dominates [b]. *)
  val dominates : idom:node array -> node -> node -> bool

  (** Dominator tree children. *)
  val children : idom:node array -> node list array
end
