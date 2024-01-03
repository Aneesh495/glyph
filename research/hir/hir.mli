(** High-level intermediate representation (ANF-flavoured). *)

type lit =
  | Lit_unit
  | Lit_bool of bool
  | Lit_int of int
  | Lit_float of float
  | Lit_string of string
  | Lit_char of char

type atom =
  | Atom_var of Ident.t
  | Atom_lit of lit

type primop =
  | Prim_add
  | Prim_sub
  | Prim_mul
  | Prim_div
  | Prim_mod
  | Prim_eq
  | Prim_ne
  | Prim_lt
  | Prim_le
  | Prim_gt
  | Prim_ge
  | Prim_and
  | Prim_or
  | Prim_not
  | Prim_neg
  | Prim_fadd
  | Prim_fsub
  | Prim_fmul
  | Prim_fdiv
  | Prim_string_concat
  | Prim_print
  | Prim_print_int
  | Prim_print_bool
  | Prim_abort
  | Prim_is_unit
  | Prim_tag_of
  | Prim_box
  | Prim_unbox

type ctor_info = {
  ctor_name : Ident.t;
  ctor_tag : int;
  ctor_arity : int;
  ctor_type : Ident.t option;
}

type pat =
  | Pat_any of Span.t
  | Pat_var of Ident.t * Span.t
  | Pat_lit of lit * Span.t
  | Pat_ctor of ctor_info * pat list * Span.t
  | Pat_or of pat * pat * Span.t
  | Pat_as of pat * Ident.t * Span.t
  | Pat_tuple of pat list * Span.t

val pat_span : pat -> Span.t

type match_arm = {
  arm_pat : pat;
  arm_guard : expr option;
  arm_body : expr;
  arm_span : Span.t;
}

and expr =
  | Atom of atom * Span.t
  | App of atom * atom list * Span.t
  | Prim of primop * atom list * Span.t
  | Let of Ident.t * expr * expr * Span.t
  | Let_rec of (Ident.t * expr) list * expr * Span.t
  | Fun of Ident.t list * expr * Span.t
  | If of atom * expr * expr * Span.t
  | Match of atom * match_arm list * Span.t
  | Ctor of ctor_info * atom list * Span.t
  | Tuple of atom list * Span.t
  | Project of atom * int * Span.t
  | Seq of expr * expr * Span.t
  | Raise of atom * Span.t
  | Switch_ctor of
      atom
      * (ctor_info * Ident.t list * expr) list
      * expr option
      * Span.t
  | Switch_lit of atom * (lit * expr) list * expr option * Span.t
  | Fail_match of Span.t

type toplevel =
  | Toplevel_fun of {
      name : Ident.t;
      params : Ident.t list;
      body : expr;
      recursive : bool;
      span : Span.t;
    }
  | Toplevel_val of {
      name : Ident.t;
      body : expr;
      span : Span.t;
    }
  | Toplevel_type of {
      name : Ident.t;
      params : Ident.t list;
      ctors : ctor_info list;
      span : Span.t;
    }
  | Toplevel_extern of {
      name : Ident.t;
      arity : int;
      span : Span.t;
    }

type program = {
  items : toplevel list;
  span : Span.t;
}

val atom_var : Ident.t -> atom
val atom_lit : lit -> atom
val lit_int : int -> lit
val lit_bool : bool -> lit
val lit_unit : lit
val lit_string : string -> lit
val lit_float : float -> lit
val lit_char : char -> lit
val mk_ctor : ?ty:Ident.t option -> Ident.t -> int -> int -> ctor_info

val expr_span : expr -> Span.t
val with_span : expr -> Span.t -> expr
val free_vars : expr -> Ident.Set.t
val pat_bound : pat -> Ident.Set.t
val expr_size : expr -> int
val map_expr : (expr -> expr) -> expr -> expr

val lit_to_string : lit -> string
val atom_to_string : atom -> string
val primop_to_string : primop -> string
val pp_pat : Format.formatter -> pat -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_toplevel : Format.formatter -> toplevel -> unit
val pp_program : Format.formatter -> program -> unit
val program_of_items : ?span:Span.t -> toplevel list -> program
