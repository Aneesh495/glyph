(** Unification with occurs-check and Rémy-style level updates. *)

val unify : span:Span.t -> Ty.ty -> Ty.ty -> unit
val try_unify : span:Span.t -> Ty.ty -> Ty.ty -> (unit, Error.t) result
val unify_list : span:Span.t -> (Ty.ty * Ty.ty) list -> unit
val as_function : span:Span.t -> Ty.ty -> Ty.ty * Ty.ty
val apply : span:Span.t -> fun_ty:Ty.ty -> arg_ty:Ty.ty -> Ty.ty
val apply_many : span:Span.t -> fun_ty:Ty.ty -> arg_tys:Ty.ty list -> Ty.ty
val as_tuple : span:Span.t -> arity:int -> Ty.ty -> Ty.ty list
val project_field : span:Span.t -> Ty.ty -> Ident.t -> Ty.ty
val with_level : (unit -> 'a) -> 'a
val can_unify : Ty.ty -> Ty.ty -> bool
