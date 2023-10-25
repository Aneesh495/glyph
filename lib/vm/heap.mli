(** Semi-space heap allocation API.

    Objects live in a bump-allocated space of [cell]s. Each live object is
    referenced by [Value.Ptr loc]. Allocation may trigger GC via the callback
    installed with [set_collect_fn]. *)

type loc = int

type obj_kind =
  | String of string
  | Tuple of Value.t array
  | Adt of int * Value.t array
  | Closure of int * Value.t array

type cell = {
  mutable forward : loc option;
      (** During GC: [Some tospace_loc] after evacuation. *)
  mutable kind : obj_kind;
}

type t = {
  mutable fromspace : cell option array;
  mutable tospace : cell option array;
  mutable top : int;
      (** Bump pointer into [fromspace]. *)
  mutable capacity : int;
  mutable allocs_since_gc : int;
  mutable collect : (t -> unit) option;
}

val create : ?capacity:int -> unit -> t
val set_collect_fn : t -> (t -> unit) -> unit

val alloc : t -> obj_kind -> Value.t
val alloc_string : t -> string -> Value.t
val alloc_tuple : t -> Value.t array -> Value.t
val alloc_adt : t -> int -> Value.t array -> Value.t
val alloc_closure : t -> int -> Value.t array -> Value.t

val get : t -> loc -> obj_kind
val get_cell : t -> loc -> cell
val resolve : t -> Value.t -> Value.t
(** Expand a [Ptr] into a view [String]/[Tuple]/[Adt]/[Closure] value. *)

val size : t -> int
val capacity : t -> int
val flip_spaces : t -> unit
(** Swap fromspace/tospace and reset the bump pointer (used by GC). *)

val tospace_alloc : t -> cell -> loc
(** Allocate a cell directly in tospace during evacuation. *)
