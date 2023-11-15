(** Pretty-printer for Glyph surface AST using [Format]. *)

open Format
open Ast

let pp_ident fmt id = pp_print_string fmt (Ident.to_string id)

let pp_separated_list ~sep pp_item fmt items =
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp_item fmt x
    | x :: xs ->
        pp_item fmt x;
        pp_print_string fmt sep;
        loop xs
  in
  loop items

let pp_comma_list pp_item fmt items = pp_separated_list ~sep:", " pp_item fmt items
let pp_lit = Ast.pp_lit
let pp_binop fmt op = pp_print_string fmt (Token.string_of_binop op)
let pp_unop fmt op = pp_print_string fmt (Token.string_of_unop op)

let rec pp_ty fmt ty =
  match ty.ty_desc with
  | Ty_hole -> pp_print_string fmt "_"
  | Ty_unit -> pp_print_string fmt "()"
  | Ty_var id -> pp_ident fmt id
  | Ty_named (name, []) -> pp_ident fmt name
  | Ty_named (name, args) -> fprintf fmt "%a[%a]" pp_ident name (pp_comma_list pp_ty) args
  | Ty_arrow (a, b) -> fprintf fmt "%a -> %a" pp_ty_atom a pp_ty b
  | Ty_tuple [] -> pp_print_string fmt "()"
  | Ty_tuple [ t ] -> fprintf fmt "(%a,)" pp_ty t
  | Ty_tuple ts -> fprintf fmt "(%a)" (pp_comma_list pp_ty) ts

and pp_ty_atom fmt ty =
  match ty.ty_desc with Ty_arrow _ -> fprintf fmt "(%a)" pp_ty ty | _ -> pp_ty fmt ty

let pp_ty_annot fmt ty = fprintf fmt ": %a" pp_ty ty

let rec pp_pat fmt pat =
  match pat.pat_desc with
  | Pat_wild -> pp_print_string fmt "_"
  | Pat_var id -> pp_ident fmt id
  | Pat_lit lit -> pp_lit fmt lit
  | Pat_ctor (name, []) -> pp_ident fmt name
  | Pat_ctor (name, args) -> fprintf fmt "%a(%a)" pp_ident name (pp_comma_list pp_pat) args
  | Pat_tuple [] -> pp_print_string fmt "()"
  | Pat_tuple [ p ] -> fprintf fmt "(%a,)" pp_pat p
  | Pat_tuple ps -> fprintf fmt "(%a)" (pp_comma_list pp_pat) ps
  | Pat_or (a, b) -> fprintf fmt "%a | %a" pp_pat a pp_pat b
  | Pat_as (p, id) -> fprintf fmt "%a as %a" pp_pat p pp_ident id
  | Pat_annotate (p, ty) -> fprintf fmt "(%a%a)" pp_pat p pp_ty_annot ty

let pp_param fmt (p : param) =
  match p.param_ty with
  | None -> pp_ident fmt p.param_name
  | Some ty -> fprintf fmt "%a: %a" pp_ident p.param_name pp_ty ty

let pp_params fmt params = fprintf fmt "(%a)" (pp_comma_list pp_param) params

let rec pp_expr fmt expr =
  match expr.expr_desc with
  | Expr_lit lit -> pp_lit fmt lit
  | Expr_var id | Expr_ctor id -> pp_ident fmt id
  | Expr_app (f, []) -> fprintf fmt "%a()" pp_expr_atom f
  | Expr_app (f, args) -> fprintf fmt "%a(%a)" pp_expr_atom f (pp_comma_list pp_expr) args
  | Expr_lambda (params, body) -> fprintf fmt "@[<hov1>fn%a ->@ %a@]" pp_params params pp_expr body
  | Expr_let (lb, body) -> fprintf fmt "@[<v0>@[<hov1>let%a@ in@]@,%a@]" pp_binding lb pp_expr body
  | Expr_let_rec (lbs, body) ->
      fprintf fmt "@[<v0>@[<hov1>let rec %a@ in@]@,%a@]"
        (pp_separated_list ~sep:"@ and " pp_binding_body) lbs pp_expr body
  | Expr_if (c, t, e) -> fprintf fmt "@[<hov1>if@ %a@ then@ %a@ else@ %a@]" pp_expr c pp_expr t pp_expr e
  | Expr_match (e, cases) ->
      fprintf fmt "@[<v0>@[<hov1>match@ %a@ with@]@,%a@]" pp_expr e
        (fun fmt cases -> List.iter (fun c -> fprintf fmt "%a@," pp_case c) cases) cases
  | Expr_bin (op, a, b) -> fprintf fmt "@[<hov1>%a@ %a@ %a@]" pp_expr_atom a pp_binop op pp_expr_atom b
  | Expr_un (op, e) -> (
      match op with
      | Token.Op_not -> fprintf fmt "not %a" pp_expr_atom e
      | Token.Op_neg -> fprintf fmt "-%a" pp_expr_atom e)
  | Expr_tuple [] -> pp_print_string fmt "()"
  | Expr_tuple [ e ] -> fprintf fmt "(%a,)" pp_expr e
  | Expr_tuple es -> fprintf fmt "(%a)" (pp_comma_list pp_expr) es
  | Expr_record fields ->
      fprintf fmt "{%a}"
        (pp_separated_list ~sep:", " (fun fmt (n, e) -> fprintf fmt "%a = %a" pp_ident n pp_expr e))
        fields
  | Expr_field (e, f) -> fprintf fmt "%a.%a" pp_expr_atom e pp_ident f
  | Expr_block [] -> pp_print_string fmt "{}"
  | Expr_block es -> fprintf fmt "@[<hv0>{@ %a@ }@]" (pp_separated_list ~sep:";@ " pp_expr) es
  | Expr_annotate (e, ty) -> fprintf fmt "(%a%a)" pp_expr e pp_ty_annot ty
  | Expr_pipe (a, b) -> fprintf fmt "@[<hov1>%a@ |>@ %a@]" pp_expr_atom a pp_expr_atom b

and pp_expr_atom fmt expr =
  match expr.expr_desc with
  | Expr_bin _ | Expr_pipe _ | Expr_if _ | Expr_match _ | Expr_let _
  | Expr_let_rec _ | Expr_lambda _ | Expr_annotate _ | Expr_un _ ->
      fprintf fmt "(%a)" pp_expr expr
  | _ -> pp_expr fmt expr

and pp_binding_body fmt (lb : let_binding) =
  pp_ident fmt lb.lb_name;
  if lb.lb_params <> [] then fprintf fmt "%a" pp_params lb.lb_params;
  (match lb.lb_ty with None -> () | Some ty -> fprintf fmt ": %a" pp_ty ty);
  fprintf fmt " =@ %a" pp_expr lb.lb_body

and pp_binding fmt (lb : let_binding) =
  if lb.lb_rec then pp_print_string fmt " rec";
  pp_print_string fmt " ";
  pp_binding_body fmt lb

and pp_case fmt (c : case) =
  fprintf fmt "| %a" pp_pat c.case_pat;
  (match c.case_guard with None -> () | Some g -> fprintf fmt " if %a" pp_expr g);
  fprintf fmt " -> %a" pp_expr c.case_body

let pp_ctor_decl fmt (c : ctor_decl) =
  match c.ctor_args with
  | [] -> pp_ident fmt c.ctor_name
  | args -> fprintf fmt "%a(%a)" pp_ident c.ctor_name (pp_comma_list pp_ty) args

let pp_type_def fmt (td : type_def) =
  fprintf fmt "@[<hov1>type %a" pp_ident td.td_name;
  (match td.td_params with [] -> () | ps -> fprintf fmt "[%a]" (pp_comma_list pp_ident) ps);
  fprintf fmt " =@ %a@]" (pp_separated_list ~sep:" | " pp_ctor_decl) td.td_ctors

let pp_extern fmt (e : extern_decl) =
  fprintf fmt "@[<hov1>extern fn %a(%a) -> %a@]" pp_ident e.ext_name
    (pp_comma_list pp_ty) e.ext_params pp_ty e.ext_ret

let pp_item fmt = function
  | Item_fn lb ->
      fprintf fmt "@[<hov1>fn %a%a" pp_ident lb.lb_name pp_params lb.lb_params;
      (match lb.lb_ty with None -> () | Some ty -> fprintf fmt " -> %a" pp_ty ty);
      fprintf fmt " =@ %a@]" pp_expr lb.lb_body
  | Item_let lb ->
      fprintf fmt "@[<hov1>let";
      if lb.lb_rec then pp_print_string fmt " rec";
      fprintf fmt " %a@]" pp_binding_body lb
  | Item_type td -> pp_type_def fmt td
  | Item_extern e -> pp_extern fmt e

let pp_program fmt prog =
  fprintf fmt "@[<v0>";
  List.iteri (fun i item -> if i > 0 then fprintf fmt "@,@,"; pp_item fmt item) prog.items;
  fprintf fmt "@]"

let show_with ?(width = 80) pp_item item =
  let buf = Buffer.create 128 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt width;
  pp_item fmt item;
  pp_print_flush fmt ();
  Buffer.contents buf

let show_ty = show_with pp_ty
let show_pat = show_with pp_pat
let show_expr = show_with pp_expr
let show_item = show_with pp_item
let show_program = show_with pp_program
let to_string ?(width = 80) prog = show_with ~width pp_program prog
let program_to_string = show_program
let expr_to_string = show_expr
let pattern_to_string = show_pat
let type_to_string = show_ty
let item_to_string = show_item
let print_expr e = printf "%a@." pp_expr e
let print_program p = printf "%a@." pp_program p
