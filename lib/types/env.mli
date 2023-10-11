(** Typing environments: value schemes and algebraic data type constructors. *)

module Ast = Glyph_syntax.Ast
module Ident = Glyph_util.Ident
module Span = Glyph_util.Span

(** Information about a type constructor (ADT, alias, record, …). *)
type type_ctor = {
  name : Ident.t;
  params : Ident.t list;
  kind : type_ctor_kind;
  span : Span.t;
}

and type_ctor_kind =
  | Variant of constructor_info list
  | Record of (Ident.t * Ty.ty * bool) list
  | Abbrev of Ty.ty
  | Abstract

(** Value constructor (e.g. [Cons], [Nil]) with its type scheme. *)
and constructor_info = {
  cname : Ident.t;
  (** Fully applied result type of the constructor, as a scheme.
      Example: [Cons : ∀a. a -> List a -> List a] *)
  scheme : Ty.scheme;
  (** Arity (number of value arguments). *)
  arity : int;
  (** Parent type constructor name. *)
  parent : Ident.t;
  span : Span.t;
}

(** Record field metadata. *)
type field_info = {
  fname : Ident.t;
  (** Scheme for the field projection: ∀α. RecordType α → field_ty *)
  scheme : Ty.scheme;
  mutable_ : bool;
  parent : Ident.t;
  span : Span.t;
}

(** The typing environment. *)
type t = {
  values : Ty.scheme Ident.Map.t;
  types : type_ctor Ident.Map.t;
  constructors : constructor_info Ident.Map.t;
  fields : field_info Ident.Map.t;
  (** Lexical parent for nested scopes (modules / opens). *)
  parent : t option;
}

val empty : t

(** Built-in prelude: Int/Bool/… ops, List, Option, ref primitives. *)
val prelude : unit -> t

val extend : t -> Ident.t -> Ty.scheme -> t
val extend_many : t -> (Ident.t * Ty.scheme) list -> t

val find_value : t -> Ident.t -> Ty.scheme option
val find_value_exn : t -> Ident.t -> Span.t -> Ty.scheme

val add_type : t -> type_ctor -> t
val find_type : t -> Ident.t -> type_ctor option

val add_constructor : t -> constructor_info -> t
val find_constructor : t -> Ident.t -> constructor_info option
val find_constructor_exn : t -> Ident.t -> Span.t -> constructor_info

val add_field : t -> field_info -> t
val find_field : t -> Ident.t -> field_info option

(** Free type variables across all value schemes in the environment. *)
val free_vars : t -> Ty.tv list

(** Register a parsed [Ast.type_decl] (variants, records, aliases). *)
val add_type_decl : t -> Ast.type_decl -> translate:(Ast.type_expr -> Ty.ty) -> t

(** Snapshot value bindings as an association list (nearest scope first). *)
val bindings : t -> (Ident.t * Ty.scheme) list

(** Pretty-print environment contents (debugging). *)
val pp : Format.formatter -> t -> unit
