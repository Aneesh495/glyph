(** Algorithm W / Hindley–Milner inference for Glyph. *)

module Ast = Glyph_syntax.Ast
module Ident = Glyph_util.Ident
module Span = Glyph_util.Span
module Diagnostic = Glyph_util.Diagnostic

(** Typed expression node (elaboration). *)
type texpr = {
  texp_desc : texpr_desc;
  texp_ty : Ty.ty;
  texp_span : Span.t;
}

and texpr_desc =
  | Texp_var of Ident.t
  | Texp_lit of Ast.lit
  | Texp_app of texpr * texpr list
  | Texp_abs of Ident.t list * texpr
  | Texp_let of (Ident.t * Ty.scheme * texpr) list * texpr
  | Texp_letrec of (Ident.t * Ty.scheme * texpr) list * texpr
  | Texp_if of texpr * texpr * texpr option
  | Texp_match of texpr * tcase list
  | Texp_tuple of texpr list
  | Texp_record of (Ident.t * texpr) list
  | Texp_field of texpr * Ident.t
  | Texp_constructor of Ident.t * texpr list
  | Texp_binop of Ast.binop * texpr * texpr
  | Texp_unop of Ast.unop * texpr
  | Texp_seq of texpr * texpr
  | Texp_list of texpr list
  | Texp_cons of texpr * texpr
  | Texp_array of texpr list
  | Texp_unit
  | Texp_annotated of texpr * Ty.ty

and tcase = {
  tcase_pat : tpat;
  tcase_guard : texpr option;
  tcase_body : texpr;
}

and tpat = {
  tpat_desc : tpat_desc;
  tpat_ty : Ty.ty;
  tpat_span : Span.t;
}

and tpat_desc =
  | Tpat_wildcard
  | Tpat_var of Ident.t
  | Tpat_lit of Ast.lit
  | Tpat_constructor of Ident.t * tpat list
  | Tpat_tuple of tpat list
  | Tpat_or of tpat * tpat
  | Tpat_as of tpat * Ident.t
  | Tpat_cons of tpat * tpat
  | Tpat_list of tpat list

type typed_item =
  | Titem_value of Ident.t * Ty.scheme * texpr
  | Titem_type of Ast.type_decl
  | Titem_external of Ident.t * Ty.scheme
  | Titem_expr of texpr

type result = {
  env : Env.t;
  items : typed_item list;
  (** Identifier → generalized scheme at binding sites. *)
  schemes : Ty.scheme Ident.Map.t;
  (** Expression span → inferred type (for tooling). *)
  expr_types : (Span.t * Ty.ty) list;
}

val infer_expr : Env.t -> Ast.expr -> Ty.ty * texpr
val infer_pat : Env.t -> Ast.pattern -> expected:Ty.ty -> Env.t * tpat
val infer_item : Env.t -> Ast.toplevel -> Env.t * typed_item list
val infer_program : ?env:Env.t -> Ast.program -> result

(** Run inference, converting [Error.Type_error] into diagnostics. *)
val infer_program_result :
  ?env:Env.t ->
  ?source:string ->
  Ast.program ->
  (result, Diagnostic.t list) result
