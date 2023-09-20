(** Structured compiler diagnostics with source snippets. *)

type severity =
  | Error
  | Warning
  | Note
  | Hint

type label = {
  span : Span.t;
  message : string;
  primary : bool;
}

type t = {
  severity : severity;
  code : string option;
  message : string;
  span : Span.t;
  labels : label list;
  notes : string list;
  help : string option;
}

val severity_to_string : severity -> string
val severity_rank : severity -> int

val make :
  ?code:string ->
  ?labels:label list ->
  ?notes:string list ->
  ?help:string ->
  severity ->
  Span.t ->
  string ->
  t

val error :
  ?code:string ->
  ?labels:label list ->
  ?notes:string list ->
  ?help:string ->
  Span.t ->
  string ->
  t

val warning :
  ?code:string ->
  ?labels:label list ->
  ?notes:string list ->
  ?help:string ->
  Span.t ->
  string ->
  t

val note : Span.t -> string -> t
val hint : Span.t -> string -> t

val label : ?primary:bool -> Span.t -> string -> label
val with_label : t -> label -> t
val with_note : t -> string -> t
val with_help : t -> string -> t
val with_code : t -> string -> t

(** Render a diagnostic, optionally with a multi-line source snippet. *)
val render : ?source:string option -> ?context_lines:int -> t -> string

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

(** Mutable collection of diagnostics for a compilation unit. *)
module Bag : sig
  type diag = t
  type t

  val create : unit -> t
  val add : t -> diag -> unit
  val error :
    t ->
    ?code:string ->
    ?labels:label list ->
    ?notes:string list ->
    ?help:string ->
    Span.t ->
    string ->
    unit
  val warning :
    t ->
    ?code:string ->
    ?labels:label list ->
    ?notes:string list ->
    ?help:string ->
    Span.t ->
    string ->
    unit
  val to_list : t -> diag list
  val has_errors : t -> bool
  val error_count : t -> int
  val warning_count : t -> int
  val clear : t -> unit
  val length : t -> int
  val emit : ?source:string option -> t -> unit
  val render_all : ?source:string option -> t -> string
  val merge : t -> t -> unit
end

(** Helpers for extracting numbered source lines. *)
module Source : sig
  type t

  val of_string : string -> t
  val line : t -> int -> string option
  val line_count : t -> int
  val snippet :
    t ->
    span:Span.t ->
    ?context:int ->
    ?primary_message:string ->
    unit ->
    string
end
