(** Internal type representation for Hindley–Milner inference.

    Types use mutable unification variables with quantification levels
    (Rémy-style) so that generalization for [let] is O(size of type)
    rather than a full free-variable scan of the environment. *)

(** Quantification / binding level. Outer scopes have lower levels. *)
type level = int

(** A type variable identity. *)
type tv = {
  id : int;
  mutable level : level;
  mutable namehint : string option;
}

(** Mutable binding of a type variable. *)
type tv_state =
  | Unbound of tv
  | Link of ty
  | Generic of tv
      (** Quantified variable inside a scheme (after generalization). *)

(** Monotypes. *)
and ty =
  | TVar of tv_state ref
  | TCon of string
  | TApp of ty * ty
  | TArrow of ty * ty
  | TTuple of ty list
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TString
  | TChar
  | TArray of ty
  | TRef of ty
  | TRecord of (string * ty * bool) list
      (** field name, field type, mutable? *)

(** Polymorphic type schemes [∀ α₁…αₙ. τ]. *)
type scheme = Forall of tv list * ty

(** Current binding level used for generalization. *)
val current_level : level ref

(** Enter / leave a binding scope (increments / decrements [current_level]). *)
val enter_level : unit -> unit
val leave_level : unit -> unit
val reset_level : unit -> unit

(** Fresh unbound type variable at the current level. *)
val fresh_var : ?name:string -> unit -> ty

(** Fresh unbound variable forced to a specific level. *)
val fresh_var_at : ?name:string -> level -> ty

(** Follow [Link] chains; does not allocate. *)
val repr : ty -> ty

(** Fully chase links and rebuild the type (zonking). *)
val zonk : ty -> ty

(** Zonk every type in a scheme. *)
val zonk_scheme : scheme -> scheme

(** Occurs-check: does [tv] occur in [ty]? Updates levels of unbound vars. *)
val occurs : tv -> ty -> bool

(** Update the level of every unbound variable in [ty] to be ≤ [level]. *)
val update_level : level -> ty -> unit

(** Free type variables of a monotype (after [repr]). *)
val free_vars : ty -> tv list

(** Free type variables of a scheme (body minus quantified). *)
val free_vars_scheme : scheme -> tv list

(** Generalize unbound variables whose level is strictly greater than
    [current_level], producing a scheme. *)
val generalize : ty -> scheme

(** Instantiate a scheme with fresh variables at the current level. *)
val instantiate : scheme -> ty

(** Monomorphic scheme (no quantifiers). *)
val mono : ty -> scheme

(** Structural equality after zonking (does not unify). *)
val equal : ty -> ty -> bool

(** Pretty-print a type. *)
val pp : Format.formatter -> ty -> unit
val to_string : ty -> string

(** Pretty-print a scheme. *)
val pp_scheme : Format.formatter -> scheme -> unit
val scheme_to_string : scheme -> string

(** Built-in type constructors as [TCon] / primitives. *)
val t_unit : ty
val t_int : ty
val t_float : ty
val t_bool : ty
val t_string : ty
val t_char : ty
val t_list : ty -> ty
val t_option : ty -> ty
val t_ref : ty -> ty
val t_array : ty -> ty
val arrow : ty -> ty -> ty
val arrows : ty list -> ty -> ty
val tuple : ty list -> ty
val app_con : string -> ty list -> ty

(** Apply a type constructor name to argument types (curried [TApp]s). *)
val apply_constructor : string -> ty list -> ty

(** Destructure an arrow type into argument and result, if possible. *)
val as_arrow : ty -> (ty * ty) option

(** Destructure nested arrows into argument list and final result. *)
val peel_arrows : ty -> ty list * ty

(** Whether a type is a fully-known ground type (no unbound vars). *)
val is_ground : ty -> bool

(** Reset the global fresh-variable counter (for tests / REPL). *)
val reset_gensym : unit -> unit

(** Substitute quantified variables according to an association list. *)
val subst : (tv * ty) list -> ty -> ty

(** Collect constructor / type-name references appearing in a type. *)
val type_constructors : ty -> string list
