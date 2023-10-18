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
  | Ptr of Heap_loc.t
      (** Opaque heap pointer used by the GC-managed heap. Payload is
          recovered via [Heap.get]. Immediate constructors above are also
          produced as views of heap objects for convenience. *)

module Heap_loc : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit

val is_truthy : t -> bool
val as_int : t -> int
val as_bool : t -> bool
val as_string : t -> string
val tag_of : t -> int
val fields_of : t -> t array
