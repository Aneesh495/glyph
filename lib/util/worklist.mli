(** Worklist algorithm utilities for iterative dataflow. *)

type 'a t

val create : unit -> 'a t
val of_list : 'a list -> 'a t
val clear : 'a t -> unit
val is_empty : 'a t -> bool
val length : 'a t -> int
val push : 'a t -> 'a -> unit
val push_front : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val peek : 'a t -> 'a option
val mem : 'a t -> equal:('a -> 'a -> bool) -> 'a -> bool

(** Unique worklist: elements are inserted at most once until removed.
    Requires a hashable key projection. *)
module Unique : sig
  type ('k, 'a) t

  val create :
    hash:('k -> int) -> equal:('k -> 'k -> bool) -> key:('a -> 'k) -> ('k, 'a) t
  val clear : ('k, 'a) t -> unit
  val is_empty : ('k, 'a) t -> bool
  val length : ('k, 'a) t -> int
  val push : ('k, 'a) t -> 'a -> bool
  (** Returns [true] if the element was newly inserted. *)
  val pop : ('k, 'a) t -> 'a option
end

(** Classic worklist fixed-point iteration.
    [transfer node] returns neighboring nodes that must be re-scheduled when
    [node]'s state changed. *)
val run :
  initial:'a list ->
  transfer:('a -> 'a list) ->
  ?max_iters:int ->
  unit ->
  int

(** Integer-node worklist over a dense index space [0..n). *)
module Int : sig
  type t
  val create : n:int -> t
  val clear : t -> unit
  val is_empty : t -> bool
  val push : t -> int -> bool
  val pop : t -> int option
  val length : t -> int
end

(** Bitset-backed worklist for dense integer domains. *)
module Bitset : sig
  type t
  val create : n:int -> t
  val clear : t -> unit
  val is_empty : t -> bool
  val push : t -> int -> bool
  val pop : t -> int option
  val length : t -> int
  val mem : t -> int -> bool
end
