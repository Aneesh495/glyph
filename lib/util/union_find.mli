(** Union-find (disjoint-set) with path compression and union by rank. *)

type 'a node
type 'a t
type 'a snapshot

val create : unit -> 'a t
val make : 'a t -> 'a -> 'a node
val find : 'a t -> 'a node -> 'a node
val get : 'a t -> 'a node -> 'a
val set : 'a t -> 'a node -> 'a -> unit
val union : 'a t -> 'a node -> 'a node -> 'a node
val union_with :
  'a t ->
  merge:('a -> 'a -> 'a) ->
  'a node ->
  'a node ->
  'a node
val same : 'a t -> 'a node -> 'a node -> bool
val rank : 'a t -> 'a node -> int
val length : 'a t -> int
val snapshot : 'a t -> 'a snapshot
val restore : 'a t -> 'a snapshot -> unit
val iter_roots : 'a t -> ('a node -> 'a -> unit) -> unit
val fold_roots : 'a t -> ('a node -> 'a -> 'acc -> 'acc) -> 'acc -> 'acc

module Persistent : sig
  type 'a state
  type 'a node

  val empty : 'a state
  val fresh : 'a state -> 'a -> 'a state * 'a node
  val find : 'a state -> 'a node -> 'a state * 'a node
  val get : 'a state -> 'a node -> 'a state * 'a
  val set : 'a state -> 'a node -> 'a -> 'a state
  val union :
    'a state ->
    merge:('a -> 'a -> 'a) ->
    'a node ->
    'a node ->
    'a state * 'a node
  val same : 'a state -> 'a node -> 'a node -> 'a state * bool
end
