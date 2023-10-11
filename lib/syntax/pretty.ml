(** Pretty-printer for Glyph ASTs using [Format]. *)

open Format

(* -------------------------------------------------------------------------- *)
(* Utilities                                                                  *)
(* -------------------------------------------------------------------------- *)

let pp_ident fmt id = pp_print_string fmt (Ident.to_string id)

let pp_list pp_item ~sep fmt items =
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp_item fmt x
    | x :: xs ->
        pp_item fmt x;
        pp_print_string fmt sep;
        loop xs
  in
  loop items

let pp_comma_list pp_item fmt items = pp_list pp_item ~sep:", " fmt items

(* -------------------------------------------------------------------------- *)
(* Literals                                                                   *)
(* -------------------------------------------------------------------------- *)

let pp_lit fmt = function
  | Ast.Lit_int n -> fprintf fmt "%Ld" n
  | Ast.Lit_float f -> fprintf fmt "%g" f
  | Ast.Lit_string s -> fprintf fmt "%S" s
  | Ast.Lit_char c -> fprintf fmt "%C" c
  | Ast.Lit_bool b -> pp_print_string fmt (if b then "true" else "false")
  | Ast.Lit_unit -> pp_print_string fmt "()"

(* -------------------------------------------------------------------------- *)
(* Operators                                                                  *)
(* -------------------------------------------------------------------------- *)

let pp_binop fmt op = pp_print_string fmt (Ast.binop_to_string op)
let pp_unop fmt op = pp_print_string fmt (Ast.unop_to_string op)

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

let rec pp_type fmt t =
  match t.Ast.typ_desc with
  | Ast.Typ_var id | Ast.Typ_con id -> pp_ident fmt id
  | Ast.Typ_arrow (a, b) ->
      fprintf fmt "@[<hov1>";
      (match a.Ast.typ_desc with
      | Ast.Typ_arrow _ -> fprintf fmt "(%a)" pp_type a
      | _ -> pp_type fmt a);
      fprintf fmt "@ ->@ %a@]" pp_type b
  | Ast.Typ_tuple ts ->
      fprintf fmt "@[<hov1>(%a)@]" (pp_comma_list pp_type) ts
  | Ast.Typ_record fields ->
      fprintf fmt "@[<hov1>{@ %a@ }@]"
        (pp_list
           ~sep:"; "
           (fun fmt (name, ty, mut) ->
             if mut then fprintf fmt "mutable ";
             fprintf fmt "%a:@ %a" pp_ident name pp_type ty))
        fields
  | Ast.Typ_app (f, args) ->
      fprintf fmt "@[<hov1>%a@ %a@]" pp_type f
        (pp_list ~sep:" " pp_type_atom) args
  | Ast.Typ_array t -> fprintf fmt "[%a]" pp_type t
  | Ast.Typ_paren t -> fprintf fmt "(%a)" pp_type t

and pp_type_atom fmt t =
  match t.Ast.typ_desc with
  | Ast.Typ_arrow _ | Ast.Typ_app _ -> fprintf fmt "(%a)" pp_type t
  | _ -> pp_type fmt t

(* -------------------------------------------------------------------------- *)
(* Patterns                                                                   *)
(* -------------------------------------------------------------------------- *)

let rec pp_pattern fmt p =
  match p.Ast.pat_desc with
  | Ast.Pat_wildcard -> pp_print_string fmt "_"
  | Ast.Pat_var id -> pp_ident fmt id
  | Ast.Pat_lit l -> pp_lit fmt l
  | Ast.Pat_constructor (name, []) -> pp_ident fmt name
  | Ast.Pat_constructor (name, args) ->
      fprintf fmt "@[<hov1>%a@ %a@]" pp_ident name
        (pp_list ~sep:" " pp_pattern_atom) args
  | Ast.Pat_tuple ps ->
      fprintf fmt "@[<hov1>(%a)@]" (pp_comma_list pp_pattern) ps
  | Ast.Pat_record (fields, open_) ->
      fprintf fmt "@[<hov1>{@ %a%s@ }@]"
        (pp_list ~sep:"; "
           (fun fmt (name, pat_opt) ->
             match pat_opt with
             | None -> pp_ident fmt name
             | Some pat -> fprintf fmt "%a@ =@ %a" pp_ident name pp_pattern pat))
        fields
        (if open_ then "; .." else "")
  | Ast.Pat_list ps ->
      fprintf fmt "@[<hov1>[%a]@]"
        (pp_list ~sep:"; " pp_pattern) ps
  | Ast.Pat_cons (h, t) ->
      fprintf fmt "@[<hov1>%a@ ::@ %a@]" pp_pattern_atom h pp_pattern t
  | Ast.Pat_or (a, b) ->
      fprintf fmt "@[<hov1>%a@ |@ %a@]" pp_pattern a pp_pattern b
  | Ast.Pat_as (p, id) ->
      fprintf fmt "@[<hov1>%a@ as@ %a@]" pp_pattern p pp_ident id
  | Ast.Pat_annotated (p, t) ->
      fprintf fmt "@[<hov1>(%a@ :@ %a)@]" pp_pattern p pp_type t

and pp_pattern_atom fmt p =
  match p.Ast.pat_desc with
  | Ast.Pat_constructor (_, _ :: _)
  | Ast.Pat_cons _ | Ast.Pat_or _ | Ast.Pat_as _ | Ast.Pat_annotated _ ->
      fprintf fmt "(%a)" pp_pattern p
  | _ -> pp_pattern fmt p

(* -------------------------------------------------------------------------- *)
(* Expressions                                                                *)
(* -------------------------------------------------------------------------- *)

let rec pp_expr fmt e =
  match e.Ast.exp_desc with
  | Ast.Exp_var id -> pp_ident fmt id
  | Ast.Exp_lit l -> pp_lit fmt l
  | Ast.Exp_unit -> pp_print_string fmt "()"
  | Ast.Exp_app (f, args) ->
      fprintf fmt "@[<hov1>%a@ %a@]" pp_expr_atom f
        (pp_list ~sep:" " pp_expr_atom) args
  | Ast.Exp_abs (params, body) ->
      fprintf fmt "@[<hov1>fun@ %a@ ->@ %a@]"
        (pp_list ~sep:" " pp_pattern_atom)
        params pp_expr body
  | Ast.Exp_let (vbs, body) ->
      fprintf fmt "@[<v0>@[<hov1>let@ %a@ in@]@,%a@]"
        (pp_list ~sep:"@ and@ " pp_value_binding)
        vbs pp_expr body
  | Ast.Exp_letrec (vbs, body) ->
      fprintf fmt "@[<v0>@[<hov1>let rec@ %a@ in@]@,%a@]"
        (pp_list ~sep:"@ and@ " pp_value_binding)
        vbs pp_expr body
  | Ast.Exp_if (c, t, None) ->
      fprintf fmt "@[<hov1>if@ %a@ then@ %a@]" pp_expr c pp_expr t
  | Ast.Exp_if (c, t, Some e) ->
      fprintf fmt "@[<hov1>if@ %a@ then@ %a@ else@ %a@]" pp_expr c pp_expr t
        pp_expr e
  | Ast.Exp_match (scrut, cases) ->
      fprintf fmt "@[<v0>@[<hov1>match@ %a@ with@]@,%a@]" pp_expr scrut
        (fun fmt cases ->
          List.iter
            (fun c -> fprintf fmt "@[%a@]@," pp_case c)
            cases)
        cases
  | Ast.Exp_tuple es ->
      fprintf fmt "@[<hov1>(%a)@]" (pp_comma_list pp_expr) es
  | Ast.Exp_record fields ->
      fprintf fmt "@[<hov1>{@ %a@ }@]"
        (pp_list ~sep:"; "
           (fun fmt (n, e) -> fprintf fmt "%a@ =@ %a" pp_ident n pp_expr e))
        fields
  | Ast.Exp_record_update (base, fields) ->
      fprintf fmt "@[<hov1>{@ %a@ with@ %a@ }@]" pp_expr base
        (pp_list ~sep:"; "
           (fun fmt (n, e) -> fprintf fmt "%a@ =@ %a" pp_ident n pp_expr e))
        fields
  | Ast.Exp_field (e, name) ->
      fprintf fmt "%a.%a" pp_expr_atom e pp_ident name
  | Ast.Exp_constructor (name, []) -> pp_ident fmt name
  | Ast.Exp_constructor (name, args) ->
      fprintf fmt "@[<hov1>%a@ %a@]" pp_ident name
        (pp_list ~sep:" " pp_expr_atom) args
  | Ast.Exp_binop (op, l, r) ->
      fprintf fmt "@[<hov1>%a@ %a@ %a@]" pp_expr_atom l pp_binop op pp_expr_atom
        r
  | Ast.Exp_unop (op, e) ->
      fprintf fmt "%a%a" pp_unop op pp_expr_atom e
  | Ast.Exp_seq (a, b) ->
      fprintf fmt "@[<hv0>%a;@ %a@]" pp_expr a pp_expr b
  | Ast.Exp_annotated (e, t) ->
      fprintf fmt "@[<hov1>(%a@ :@ %a)@]" pp_expr e pp_type t
  | Ast.Exp_array es ->
      fprintf fmt "@[<hov1>[|%a|]@]" (pp_list ~sep:"; " pp_expr) es
  | Ast.Exp_index (e, i) ->
      fprintf fmt "%a[%a]" pp_expr_atom e pp_expr i
  | Ast.Exp_list es ->
      fprintf fmt "@[<hov1>[%a]@]" (pp_list ~sep:"; " pp_expr) es
  | Ast.Exp_cons (h, t) ->
      fprintf fmt "@[<hov1>%a@ ::@ %a@]" pp_expr_atom h pp_expr t

and pp_expr_atom fmt e =
  match e.Ast.exp_desc with
  | Ast.Exp_var _ | Ast.Exp_lit _ | Ast.Exp_unit | Ast.Exp_tuple _
  | Ast.Exp_record _ | Ast.Exp_list _ | Ast.Exp_array _ | Ast.Exp_field _
  | Ast.Exp_index _ ->
      pp_expr fmt e
  | Ast.Exp_constructor (_, []) -> pp_expr fmt e
  | _ -> fprintf fmt "(%a)" pp_expr e

and pp_value_binding fmt vb =
  let pp_lhs fmt () =
    match vb.Ast.vb_params with
    | [] -> pp_pattern fmt vb.Ast.vb_pat
    | params ->
        fprintf fmt "%a@ %a" pp_pattern vb.Ast.vb_pat
          (pp_list ~sep:" " pp_pattern_atom)
          params
  in
  fprintf fmt "@[<hov1>%a@ =@ %a@]" pp_lhs () pp_expr vb.Ast.vb_expr

and pp_case fmt c =
  fprintf fmt "|@ %a" pp_pattern c.Ast.case_pat;
  (match c.Ast.case_guard with
  | None -> ()
  | Some g -> fprintf fmt "@ when@ %a" pp_expr g);
  fprintf fmt "@ ->@ %a" pp_expr c.Ast.case_expr

(* -------------------------------------------------------------------------- *)
(* Declarations                                                               *)
(* -------------------------------------------------------------------------- *)

let pp_constructor_decl fmt cd =
  match cd.Ast.cd_args with
  | [] -> pp_ident fmt cd.Ast.cd_name
  | args ->
      fprintf fmt "%a@ of@ %a" pp_ident cd.Ast.cd_name
        (pp_list ~sep:"@ *@ " pp_type)
        args

let pp_type_kind fmt = function
  | Ast.Type_variant ctors ->
      fprintf fmt "@[<v0>%a@]"
        (fun fmt ctors ->
          List.iter
            (fun cd -> fprintf fmt "|@ %a@," pp_constructor_decl cd)
            ctors)
        ctors
  | Ast.Type_record fields ->
      fprintf fmt "@[<hov1>{@ %a@ }@]"
        (pp_list ~sep:"; "
           (fun fmt (n, t, mut) ->
             if mut then fprintf fmt "mutable ";
             fprintf fmt "%a:@ %a" pp_ident n pp_type t))
        fields
  | Ast.Type_abbrev t -> pp_type fmt t
  | Ast.Type_abstract -> pp_print_string fmt "<abstract>"

let pp_type_decl fmt td =
  fprintf fmt "@[<hov1>type";
  List.iter (fun p -> fprintf fmt "@ %a" pp_ident p) td.Ast.td_params;
  fprintf fmt "@ %a@ =@ %a@]" pp_ident td.Ast.td_name pp_type_kind
    td.Ast.td_kind

let rec pp_toplevel fmt = function
  | Ast.Top_let vbs ->
      fprintf fmt "@[<hov1>let@ %a@]"
        (pp_list ~sep:"@ and@ " pp_value_binding)
        vbs
  | Ast.Top_letrec vbs ->
      fprintf fmt "@[<hov1>let rec@ %a@]"
        (pp_list ~sep:"@ and@ " pp_value_binding)
        vbs
  | Ast.Top_type tds ->
      fprintf fmt "@[%a@]"
        (pp_list ~sep:"@ and@ " pp_type_decl)
        tds
  | Ast.Top_open (path, _) ->
      fprintf fmt "open %a" (pp_list ~sep:"." pp_ident) path
  | Ast.Top_external (name, ty, prim, _) ->
      fprintf fmt "@[<hov1>external@ %a@ :@ %a@ =@ %S@]" pp_ident name pp_type
        ty prim
  | Ast.Top_module (name, items, _) ->
      fprintf fmt "@[<v0>module %a = {@,%a@,}@]" pp_ident name
        (fun fmt items ->
          List.iter (fun it -> fprintf fmt "%a@," pp_toplevel it) items)
        items
  | Ast.Top_expr e -> pp_expr fmt e

let pp_program fmt prog =
  fprintf fmt "@[<v0>";
  List.iter
    (fun item ->
      pp_toplevel fmt item;
      fprintf fmt "@,")
    prog.Ast.prog_items;
  fprintf fmt "@]"

(* -------------------------------------------------------------------------- *)
(* String helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let to_string pp_item item =
  let buf = Buffer.create 256 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  pp_item fmt item;
  pp_print_flush fmt ();
  Buffer.contents buf

let expr_to_string e = to_string pp_expr e
let pattern_to_string p = to_string pp_pattern p
let type_to_string t = to_string pp_type t
let program_to_string p = to_string pp_program p
let toplevel_to_string t = to_string pp_toplevel t

let print_expr e = printf "%a@." pp_expr e
let print_program p = printf "%a@." pp_program p
