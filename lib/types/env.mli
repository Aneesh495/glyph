(** Typing environments: value schemes and algebraic data type constructors. *)

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

and constructor_info = {
  cname : Ident.t;
  scheme : Ty.scheme;
  arity : int;
  parent : Ident.t;
  span : Span.t;
}

type field_info = {
  fname : Ident.t;
  scheme : Ty.scheme;
  mutable_ : bool;
  parent : Ident.t;
  span : Span.t;
}

type t = {
  values : Ty.scheme Ident.Map.t;
  types : type_ctor Ident.Map.t;
  constructors : constructor_info Ident.Map.t;
  fields : field_info Ident.Map.t;
  parent : t option;
}

val empty : t
val prelude : unit -> t
val extend : t -> Ident.t -> Ty.scheme -> t
val extend_mono : t -> Ident.t -> Ty.ty -> t
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
val free_vars : t -> Ty.tv list
val translate_ast_ty : (Ident.t * Ty.ty) list -> Ast.ty -> Ty.ty
val add_type_def : t -> Ast.type_def -> t
val bindings : t -> (Ident.t * Ty.scheme) list
val binop_name : Token.binop -> string
val unop_name : Token.unop -> string
val find_binop : t -> Token.binop -> Ty.scheme option
val find_unop : t -> Token.unop -> Ty.scheme option
val pp : Format.formatter -> t -> unit
val to_string : t -> string
