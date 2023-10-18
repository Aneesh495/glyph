(** Pretty-printer for Glyph surface AST. *)

open Ast

let pp_lit = Ast.pp_lit

let pp_ident fmt id = Format.pp_print_string fmt (Ident.to_string id)

let rec pp_ty fmt ty =
  match ty.ty_desc with
  | Ty_hole -> Format.pp_print_string fmt "_"
  | Ty_unit -> Format.pp_print_string fmt "()"
  | Ty_var id -> pp_ident fmt id
  | Ty_named (name, []) -> pp_ident fmt name
  | Ty_named (name, args) ->
      Format.fprintf fmt "%a[%a]" pp_ident name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_ty)
        args
  | Ty_arrow (a, b) ->
      Format.fprintf fmt "%a -> %a" pp_ty_atom a pp_ty b
  | Ty_tuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_ty)
        ts

and pp_ty_atom fmt ty =
  match ty.ty_desc with
  | Ty_arrow _ -> Format.fprintf fmt "(%a)" pp_ty ty
  | _ -> pp_ty fmt ty

let rec pp_pat fmt pat =
  match pat.pat_desc with
  | Pat_wild -> Format.pp_print_string fmt "_"
  | Pat_var id -> pp_ident fmt id
  | Pat_lit lit -> pp_lit fmt lit
  | Pat_ctor (name, []) -> pp_ident fmt name
  | Pat_ctor (name, args) ->
      Format.fprintf fmt "%a(%a)" pp_ident name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_pat)
        args
  | Pat_tuple ps ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_pat)
        ps
  | Pat_or (a, b) -> Format.fprintf fmt "%a | %a" pp_pat a pp_pat b
  | Pat_as (p, id) -> Format.fprintf fmt "%a as %a" pp_pat p pp_ident id
  | Pat_annotate (p, ty) -> Format.fprintf fmt "%a: %a" pp_pat p pp_ty ty

let pp_binop fmt op =
  Format.pp_print_string fmt (Token.string_of_binop op)

let pp_unop fmt op = Format.pp_print_string fmt (Token.string_of_unop op)

let pp_param fmt (p : param) =
  match p.param_ty with
  | None -> pp_ident fmt p.param_name
  | Some ty -> Format.fprintf fmt "%a: %a" pp_ident p.param_name pp_ty ty

let pp_params fmt params =
  Format.fprintf fmt "(%a)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
       pp_param)
    params

let rec pp_expr fmt expr =
  match expr.expr_desc with
  | Expr_lit lit -> pp_lit fmt lit
  | Expr_var id | Expr_ctor id -> pp_ident fmt id
  | Expr_app (f, args) ->
      Format.fprintf fmt "%a(%a)" pp_expr_atom f
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_expr)
        args
  | Expr_lambda (params, body) ->
      Format.fprintf fmt "fn%a -> %a" pp_params params pp_expr body
  | Expr_let (lb, body) ->
      Format.fprintf fmt "let%a in@ %a" pp_binding lb pp_expr body
  | Expr_let_rec (lbs, body) ->
      Format.fprintf fmt "let rec %a in@ %a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ and ")
           pp_binding_body)
        lbs pp_expr body
  | Expr_if (c, t, e) ->
      Format.fprintf fmt "if %a then %a else %a" pp_expr c pp_expr t pp_expr e
  | Expr_match (e, cases) ->
      Format.fprintf fmt "match %a with@ %a" pp_expr e
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
           pp_case)
        cases
  | Expr_bin (op, a, b) ->
      Format.fprintf fmt "%a %a %a" pp_expr_atom a pp_binop op pp_expr_atom b
  | Expr_un (op, e) -> Format.fprintf fmt "%a %a" pp_unop op pp_expr_atom e
  | Expr_tuple es ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_expr)
        es
  | Expr_record fields ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           (fun fmt (n, e) ->
             Format.fprintf fmt "%a = %a" pp_ident n pp_expr e))
        fields
  | Expr_field (e, f) -> Format.fprintf fmt "%a.%a" pp_expr_atom e pp_ident f
  | Expr_block es ->
      Format.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           pp_expr)
        es
  | Expr_annotate (e, ty) -> Format.fprintf fmt "%a: %a" pp_expr e pp_ty ty
  | Expr_pipe (a, b) ->
      Format.fprintf fmt "%a |> %a" pp_expr_atom a pp_expr_atom b

and pp_expr_atom fmt expr =
  match expr.expr_desc with
  | Expr_bin _ | Expr_pipe _ | Expr_if _ | Expr_match _ | Expr_let _
  | Expr_let_rec _ | Expr_lambda _ | Expr_annotate _ | Expr_un _ ->
      Format.fprintf fmt "(%a)" pp_expr expr
  | _ -> pp_expr fmt expr

and pp_binding_body fmt (lb : let_binding) =
  Format.fprintf fmt "%a" pp_ident lb.lb_name;
  if lb.lb_params <> [] then Format.fprintf fmt "%a" pp_params lb.lb_params;
  (match lb.lb_ty with
  | None -> ()
  | Some ty -> Format.fprintf fmt ": %a" pp_ty ty);
  Format.fprintf fmt " = %a" pp_expr lb.lb_body

and pp_binding fmt (lb : let_binding) =
  if lb.lb_rec then Format.pp_print_string fmt " rec ";
  Format.pp_print_string fmt " ";
  pp_binding_body fmt lb

and pp_case fmt (c : case) =
  Format.fprintf fmt "| %a" pp_pat c.case_pat;
  (match c.case_guard with
  | None -> ()
  | Some g -> Format.fprintf fmt " if %a" pp_expr g);
  Format.fprintf fmt " -> %a" pp_expr c.case_body

let pp_ctor_decl fmt (c : ctor_decl) =
  match c.ctor_args with
  | [] -> pp_ident fmt c.ctor_name
  | args ->
      Format.fprintf fmt "%a(%a)" pp_ident c.ctor_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_ty)
        args

let pp_type_def fmt (td : type_def) =
  Format.fprintf fmt "type %a" pp_ident td.td_name;
  (match td.td_params with
  | [] -> ()
  | ps ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_ident)
        ps);
  Format.fprintf fmt " = %a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " | ")
       pp_ctor_decl)
    td.td_ctors

let pp_extern fmt (e : extern_decl) =
  Format.fprintf fmt "extern fn %a(%a) -> %a" pp_ident e.ext_name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
       pp_ty)
    e.ext_params pp_ty e.ext_ret

let pp_item fmt = function
  | Item_fn lb ->
      Format.fprintf fmt "fn %a" pp_ident lb.lb_name;
      Format.fprintf fmt "%a" pp_params lb.lb_params;
      (match lb.lb_ty with
      | None -> ()
      | Some ty -> Format.fprintf fmt " -> %a" pp_ty ty);
      Format.fprintf fmt " =@   %a" pp_expr lb.lb_body
  | Item_let lb ->
      Format.fprintf fmt "let";
      if lb.lb_rec then Format.pp_print_string fmt " rec";
      Format.fprintf fmt " %a" pp_binding_body lb
  | Item_type td -> pp_type_def fmt td
  | Item_extern e -> pp_extern fmt e

let pp_program fmt prog =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.@.")
    pp_item fmt prog.items

let show_with pp x =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let show_ty = show_with pp_ty
let show_pat = show_with pp_pat
let show_expr = show_with pp_expr
let show_item = show_with pp_item
let show_program = show_with pp_program

let to_string ?(width = 80) prog =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt width;
  pp_program fmt prog;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
