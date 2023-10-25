(** Runtime values for the Glyph VM. *)

type native = t list -> t

and t =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Unit
  | Tuple of t array
  | Adt of int * t array
      (** Constructor [tag] with payload fields. *)
  | Closure of int * t array
      (** [fn_id] plus captured environment. *)
  | Native of string * native
  | Ptr of int
      (** Index into the GC-managed semi-space heap. Payloads are
          [String]/[Tuple]/[Adt]/[Closure] cells; use [Heap.get] to
          materialize a view. *)

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit

val is_truthy : t -> bool
val as_int : t -> int
val as_float : t -> float
val as_bool : t -> bool
val as_string : t -> string
val tag_of : t -> int
val fields_of : t -> t array
val is_heap : t -> bool
