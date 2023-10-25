(** Surface abstract syntax tree for Glyph. *)

type binop = Token.binop
type unop = Token.unop

type lit =
  | Lit_unit
  | Lit_bool of bool
  | Lit_int of int64
  | Lit_float of float
  | Lit_string of string
  | Lit_char of char

type ty = {
  ty_desc : ty_desc;
  ty_span : Span.t;
}

and ty_desc =
  | Ty_named of Ident.t * ty list
  | Ty_var of Ident.t
  | Ty_arrow of ty * ty
  | Ty_tuple of ty list
  | Ty_unit
  | Ty_hole

type pat = {
  pat_desc : pat_desc;
  pat_span : Span.t;
}

and pat_desc =
  | Pat_wild
  | Pat_var of Ident.t
  | Pat_lit of lit
  | Pat_ctor of Ident.t * pat list
  | Pat_tuple of pat list
  | Pat_or of pat * pat
  | Pat_as of pat * Ident.t
  | Pat_annotate of pat * ty

type expr = {
  expr_desc : expr_desc;
  expr_span : Span.t;
}

and expr_desc =
  | Expr_lit of lit
  | Expr_var of Ident.t
  | Expr_ctor of Ident.t
  | Expr_app of expr * expr list
  | Expr_lambda of param list * expr
  | Expr_let of let_binding * expr
  | Expr_let_rec of let_binding list * expr
  | Expr_if of expr * expr * expr
  | Expr_match of expr * case list
  | Expr_bin of binop * expr * expr
  | Expr_un of unop * expr
  | Expr_tuple of expr list
  | Expr_record of (Ident.t * expr) list
  | Expr_field of expr * Ident.t
  | Expr_block of expr list
  | Expr_annotate of expr * ty
  | Expr_pipe of expr * expr

and param = {
  param_name : Ident.t;
  param_ty : ty option;
  param_span : Span.t;
}

and let_binding = {
  lb_name : Ident.t;
  lb_params : param list;
  lb_ty : ty option;
  lb_body : expr;
  lb_span : Span.t;
  lb_rec : bool;
}

and case = {
  case_pat : pat;
  case_guard : expr option;
  case_body : expr;
  case_span : Span.t;
}

type ctor_decl = {
  ctor_name : Ident.t;
  ctor_args : ty list;
  ctor_span : Span.t;
}

type type_def = {
  td_name : Ident.t;
  td_params : Ident.t list;
  td_ctors : ctor_decl list;
  td_span : Span.t;
}

type extern_decl = {
  ext_name : Ident.t;
  ext_params : ty list;
  ext_ret : ty;
  ext_span : Span.t;
}

type item =
  | Item_fn of let_binding
  | Item_let of let_binding
  | Item_type of type_def
  | Item_extern of extern_decl

type program = {
  items : item list;
  span : Span.t;
}

val lit_to_string : lit -> string
val pp_lit : Format.formatter -> lit -> unit

val ty : ty_desc -> Span.t -> ty
val pat : pat_desc -> Span.t -> pat
val expr : expr_desc -> Span.t -> expr

val span_of_item : item -> Span.t
val is_fn_item : item -> bool

val unit_expr : Span.t -> expr
val bool_expr : bool -> Span.t -> expr
val int_expr : int64 -> Span.t -> expr
val var_expr : Ident.t -> Span.t -> expr
