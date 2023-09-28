(** Growable bit vectors for dataflow analysis. *)

type t

val create : ?size:int -> unit -> t
(** Create a bit vector; [size] is the initial capacity in bits. *)

val copy : t -> t
val clear : t -> unit
val length : t -> int
(** Current logical length in bits. *)

val ensure : t -> int -> unit
(** Grow so that bit index [n] (0-based) is addressable. *)

val get : t -> int -> bool
val set : t -> int -> bool -> unit
val set_bit : t -> int -> unit
val clear_bit : t -> int -> unit
val toggle : t -> int -> unit

val is_empty : t -> bool
val popcount : t -> int
val first_set : t -> int option
val iter_set : t -> (int -> unit) -> unit
val fold_set : t -> (int -> 'a -> 'a) -> 'a -> 'a

(** In-place bitwise operations. Return [true] if the destination changed. *)
val union_into : dst:t -> src:t -> bool
val inter_into : dst:t -> src:t -> bool
val diff_into : dst:t -> src:t -> bool
val copy_into : dst:t -> src:t -> bool

val equal : t -> t -> bool
val subset : t -> t -> bool

val of_list : int list -> t
val to_list : t -> int list
val pp : Format.formatter -> t -> unit
