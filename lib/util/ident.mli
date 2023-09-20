(** Interned identifiers with unique generation stamps. *)

type t

val name : t -> string
val stamp : t -> int

(** Create a raw (uninterned) identifier with stamp 0. *)
val of_string : string -> t
val raw : string -> t

(** Fresh identifier with a unique positive stamp. *)
val fresh : string -> t
val gensym : ?prefix:string -> unit -> t
val refresh : t -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val to_string : t -> string
val pp : Format.formatter -> t -> unit

val is_underscore : t -> bool
val is_fresh : t -> bool
val starts_with : t -> prefix:string -> bool

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

module Tbl : sig
  include Hashtbl.S with type key = t
  val of_list : (t * 'a) list -> 'a t
  val to_list : 'a t -> (t * 'a) list
end

(** Process-wide interning of source-level names (stamp 0). *)
module Intern : sig
  val intern : string -> t
  val mem : string -> bool
  val reset : unit -> unit
  val size : unit -> int
  val fold : (string -> t -> 'a -> 'a) -> 'a -> 'a
end

(** Predefined common identifiers. *)
module Predef : sig
  val underscore : t
  val main : t
  val unit : t
  val bool : t
  val int : t
  val float : t
  val string : t
  val list : t
  val nil : t
  val cons : t
end
