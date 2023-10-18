(** Structured type errors and pretty-printed diagnostics. *)

module Span = Span
module Diagnostic = Diagnostic
module Ident = Ident

(** Classification of type errors for tooling / error codes. *)
type error_kind =
  | Unify_mismatch
  | Occurs_check
  | Unbound_value
  | Unbound_constructor
  | Unbound_type
  | Unbound_field
  | Arity_mismatch
  | Pattern_mismatch
  | Not_a_function
  | Not_a_record
  | Immutable_field
  | Recursive_type
  | Escaping_type_variable
  | Other

type t = {
  span : Span.t;
  message : string;
  expected : Ty.ty option;
  actual : Ty.ty option;
  notes : (Span.t * string) list;
  kind : error_kind;
}

exception Type_error of t

val error_kind_code : error_kind -> string
val error_kind_to_string : error_kind -> string

val make :
  ?expected:Ty.ty ->
  ?actual:Ty.ty ->
  ?notes:(Span.t * string) list ->
  ?kind:error_kind ->
  Span.t ->
  string ->
  t

val raise_error :
  ?expected:Ty.ty ->
  ?actual:Ty.ty ->
  ?notes:(Span.t * string) list ->
  ?kind:error_kind ->
  Span.t ->
  string ->
  'a

val mismatch :
  ?notes:(Span.t * string) list ->
  Span.t ->
  expected:Ty.ty ->
  actual:Ty.ty ->
  'a

val occurs_error : Span.t -> tv:Ty.tv -> ty:Ty.ty -> 'a

val unbound_value : Span.t -> Ident.t -> 'a
val unbound_constructor : Span.t -> Ident.t -> 'a
val unbound_type : Span.t -> Ident.t -> 'a
val unbound_field : Span.t -> Ident.t -> 'a

val not_a_function : Span.t -> Ty.ty -> 'a
val arity_mismatch : Span.t -> expected:int -> actual:int -> 'a

(** Render a type error as a [Diagnostic.t]. *)
val to_diagnostic : ?source:string -> t -> Diagnostic.t

(** Human-readable multi-line rendering. *)
val format : ?source:string -> t -> string

(** Format and print to [stderr]. *)
val report : ?source:string -> t -> unit

(** Catch [Type_error] and convert to [Result]. *)
val catch : (unit -> 'a) -> ('a, t) result

(** Pretty-print for Format. *)
val pp : Format.formatter -> t -> unit
