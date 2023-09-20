(** String interning table with stable integer ids. *)

type id = int

type t

val create : ?size:int -> unit -> t
val clear : t -> unit
val length : t -> int

(** Intern [s], returning a dense id. Repeated calls yield the same id. *)
val intern : t -> string -> id

val of_id : t -> id -> string
val mem : t -> string -> bool
val find_opt : t -> string -> id option

val iter : t -> (id -> string -> unit) -> unit
val fold : t -> (id -> string -> 'a -> 'a) -> 'a -> 'a

(** Snapshot of all interned strings in id order. *)
val to_array : t -> string array

(** Process-global default table. *)
module Global : sig
  val intern : string -> id
  val of_id : id -> string
  val mem : string -> bool
  val reset : unit -> unit
  val length : unit -> int
end
