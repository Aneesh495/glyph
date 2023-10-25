(** Unification with occurs-check and Rémy-style level updates. *)

module Span = Glyph_util.Span

(** Unify [expected] with [actual], updating variable bindings in place.
    On failure, raises [Error.Type_error] with [span]. *)
val unify : span:Span.t -> Ty.ty -> Ty.ty -> unit

(** Same as [unify], but returns a result instead of raising. *)
val try_unify : span:Span.t -> Ty.ty -> Ty.ty -> (unit, Error.t) result

(** Unify a list of type pairs left-to-right. *)
val unify_list : span:Span.t -> (Ty.ty * Ty.ty) list -> unit

(** Force [ty] to be an arrow; returns argument and result types.
    If [ty] is a variable, instantiates it to a fresh arrow. *)
val as_function : span:Span.t -> Ty.ty -> Ty.ty * Ty.ty

(** Apply a function type to an argument type, returning the result type. *)
val apply : span:Span.t -> fun_ty:Ty.ty -> arg_ty:Ty.ty -> Ty.ty

(** Apply a curried function to many arguments. *)
val apply_many : span:Span.t -> fun_ty:Ty.ty -> arg_tys:Ty.ty list -> Ty.ty
