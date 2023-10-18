(** Abstract syntax tree for Glyph. *)

(* -------------------------------------------------------------------------- *)
(* Literals                                                                   *)
(* -------------------------------------------------------------------------- *)

type lit =
  | Lit_int of int64
  | Lit_float of float
  | Lit_string of string
  | Lit_char of char
  | Lit_bool of bool
  | Lit_unit

(* -------------------------------------------------------------------------- *)
(* Binary / unary operators                                                   *)
(* -------------------------------------------------------------------------- *)

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Cons
  | Append
  | Pipe
  | Compose
  | Apply

type unop =
  | Neg
  | Not
  | Ref
  | Deref

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
  | Cons -> "::"
  | Append -> "@"
  | Pipe -> "|>"
  | Compose -> ">>"
  | Apply -> "@@"

let unop_to_string = function
  | Neg -> "-"
  | Not -> "not"
  | Ref -> "ref"
  | Deref -> "!"

(* -------------------------------------------------------------------------- *)
(* Type expressions                                                           *)
(* -------------------------------------------------------------------------- *)

type type_expr = {
  typ_desc : type_expr_desc;
  typ_span : Span.t;
}

and type_expr_desc =
  | Typ_var of Ident.t
  | Typ_con of Ident.t
  | Typ_arrow of type_expr * type_expr
  | Typ_tuple of type_expr list
  | Typ_record of (Ident.t * type_expr * bool) list
  | Typ_app of type_expr * type_expr list
  | Typ_array of type_expr
  | Typ_paren of type_expr

(* -------------------------------------------------------------------------- *)
(* Patterns                                                                   *)
(* -------------------------------------------------------------------------- *)

type pattern = {
  pat_desc : pattern_desc;
  pat_span : Span.t;
}

and pattern_desc =
  | Pat_wildcard
  | Pat_var of Ident.t
  | Pat_lit of lit
  | Pat_constructor of Ident.t * pattern list
  | Pat_tuple of pattern list
  | Pat_record of (Ident.t * pattern option) list * bool
  | Pat_list of pattern list
  | Pat_cons of pattern * pattern
  | Pat_or of pattern * pattern
  | Pat_as of pattern * Ident.t
  | Pat_annotated of pattern * type_expr

(* -------------------------------------------------------------------------- *)
(* Bindings                                                                   *)
(* -------------------------------------------------------------------------- *)

type binder = {
  binder_name : Ident.t;
  binder_span : Span.t;
}

type value_binding = {
  vb_pat : pattern;
  vb_params : pattern list;
  vb_expr : expr;
  vb_rec : bool;
  vb_span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Expressions                                                                *)
(* -------------------------------------------------------------------------- *)

and expr = {
  exp_desc : expr_desc;
  exp_span : Span.t;
}

and expr_desc =
  | Exp_var of Ident.t
  | Exp_lit of lit
  | Exp_app of expr * expr list
  | Exp_abs of pattern list * expr
  | Exp_let of value_binding list * expr
  | Exp_letrec of value_binding list * expr
  | Exp_if of expr * expr * expr option
  | Exp_match of expr * case list
  | Exp_tuple of expr list
  | Exp_record of (Ident.t * expr) list
  | Exp_record_update of expr * (Ident.t * expr) list
  | Exp_field of expr * Ident.t
  | Exp_constructor of Ident.t * expr list
  | Exp_binop of binop * expr * expr
  | Exp_unop of unop * expr
  | Exp_seq of expr * expr
  | Exp_annotated of expr * type_expr
  | Exp_array of expr list
  | Exp_index of expr * expr
  | Exp_list of expr list
  | Exp_cons of expr * expr
  | Exp_unit

and case = {
  case_pat : pattern;
  case_guard : expr option;
  case_expr : expr;
  case_span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Type declarations                                                          *)
(* -------------------------------------------------------------------------- *)

type constructor_decl = {
  cd_name : Ident.t;
  cd_args : type_expr list;
  cd_span : Span.t;
}

type type_kind =
  | Type_variant of constructor_decl list
  | Type_record of (Ident.t * type_expr * bool) list
  | Type_abbrev of type_expr
  | Type_abstract

type type_decl = {
  td_name : Ident.t;
  td_params : Ident.t list;
  td_kind : type_kind;
  td_span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Toplevel / program                                                         *)
(* -------------------------------------------------------------------------- *)

type toplevel =
  | Top_let of value_binding list
  | Top_letrec of value_binding list
  | Top_type of type_decl list
  | Top_open of Ident.t list * Span.t
  | Top_external of Ident.t * type_expr * string * Span.t
  | Top_module of Ident.t * toplevel list * Span.t
  | Top_expr of expr

type program = {
  prog_items : toplevel list;
  prog_span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Helper constructors                                                        *)
(* -------------------------------------------------------------------------- *)

let typ desc span = { typ_desc = desc; typ_span = span }
let pat desc span = { pat_desc = desc; pat_span = span }
let exp desc span = { exp_desc = desc; exp_span = span }

let binder name span = { binder_name = name; binder_span = span }

let value_binding ?(is_rec = false) ?(params = []) pat expr span =
  {
    vb_pat = pat;
    vb_params = params;
    vb_expr = expr;
    vb_rec = is_rec;
    vb_span = span;
  }

let case ?(guard = None) pat expr span =
  { case_pat = pat; case_guard = guard; case_expr = expr; case_span = span }

let constructor_decl name args span =
  { cd_name = name; cd_args = args; cd_span = span }

let type_decl name params kind span =
  { td_name = name; td_params = params; td_kind = kind; td_span = span }

let program items =
  let span =
    match items with
    | [] -> Span.dummy
    | _ ->
        let spans =
          List.map
            (function
              | Top_let vbs | Top_letrec vbs ->
                  Span.merge_list (List.map (fun vb -> vb.vb_span) vbs)
              | Top_type tds ->
                  Span.merge_list (List.map (fun td -> td.td_span) tds)
              | Top_open (_, sp) -> sp
              | Top_external (_, _, _, sp) -> sp
              | Top_module (_, _, sp) -> sp
              | Top_expr e -> e.exp_span)
            items
        in
        Span.merge_list spans
  in
  { prog_items = items; prog_span = span }

let var name span = exp (Exp_var name) span
let lit l span = exp (Exp_lit l) span
let unit span = exp Exp_unit span
let app f args span = exp (Exp_app (f, args)) span
let abs params body span = exp (Exp_abs (params, body)) span
let if_ c t e span = exp (Exp_if (c, t, e)) span
let match_ e cases span = exp (Exp_match (e, cases)) span
let tuple es span = exp (Exp_tuple es) span
let record fields span = exp (Exp_record fields) span
let field e name span = exp (Exp_field (e, name)) span
let construct name args span = exp (Exp_constructor (name, args)) span
let binop op l r span = exp (Exp_binop (op, l, r)) span
let unop op e span = exp (Exp_unop (op, e)) span
let seq a b span = exp (Exp_seq (a, b)) span
let annotated e t span = exp (Exp_annotated (e, t)) span
let array es span = exp (Exp_array es) span
let index e i span = exp (Exp_index (e, i)) span
let list es span = exp (Exp_list es) span
let cons h t span = exp (Exp_cons (h, t)) span
let let_ vbs body span = exp (Exp_let (vbs, body)) span
let letrec vbs body span = exp (Exp_letrec (vbs, body)) span

let pwildcard span = pat Pat_wildcard span
let pvar name span = pat (Pat_var name) span
let plit l span = pat (Pat_lit l) span
let pconstruct name args span = pat (Pat_constructor (name, args)) span
let ptuple ps span = pat (Pat_tuple ps) span
let por a b span = pat (Pat_or (a, b)) span
let pas p name span = pat (Pat_as (p, name)) span
let pann p t span = pat (Pat_annotated (p, t)) span
let plist ps span = pat (Pat_list ps) span
let pcons h t span = pat (Pat_cons (h, t)) span

let tvar name span = typ (Typ_var name) span
let tcon name span = typ (Typ_con name) span
let tarrow a b span = typ (Typ_arrow (a, b)) span
let ttuple ts span = typ (Typ_tuple ts) span
let tapp f args span = typ (Typ_app (f, args)) span
let tarray t span = typ (Typ_array t) span

let expr_span e = e.exp_span
let pattern_span p = p.pat_span
let type_span t = t.typ_span

let is_lident id =
  let name = Ident.name id in
  String.length name = 0
  ||
  let c = name.[0] in
  (c >= 'a' && c <= 'z') || c = '_' || c = '\''

let is_uident id =
  let name = Ident.name id in
  String.length name > 0
  &&
  let c = name.[0] in
  c >= 'A' && c <= 'Z'

let rec fold_expr f acc e =
  let acc = f acc e in
  match e.exp_desc with
  | Exp_var _ | Exp_lit _ | Exp_unit -> acc
  | Exp_app (fn, args) ->
      List.fold_left (fold_expr f) (fold_expr f acc fn) args
  | Exp_abs (_, body) -> fold_expr f acc body
  | Exp_let (vbs, body) | Exp_letrec (vbs, body) ->
      let acc =
        List.fold_left (fun a vb -> fold_expr f a vb.vb_expr) acc vbs
      in
      fold_expr f acc body
  | Exp_if (c, t, e_opt) ->
      let acc = fold_expr f (fold_expr f acc c) t in
      (match e_opt with None -> acc | Some e -> fold_expr f acc e)
  | Exp_match (scrut, cases) ->
      let acc = fold_expr f acc scrut in
      List.fold_left
        (fun a c ->
          let a =
            match c.case_guard with None -> a | Some g -> fold_expr f a g
          in
          fold_expr f a c.case_expr)
        acc cases
  | Exp_tuple es | Exp_array es | Exp_list es ->
      List.fold_left (fold_expr f) acc es
  | Exp_record fields ->
      List.fold_left (fun a (_, e) -> fold_expr f a e) acc fields
  | Exp_record_update (base, fields) ->
      let acc = fold_expr f acc base in
      List.fold_left (fun a (_, e) -> fold_expr f a e) acc fields
  | Exp_field (e, _) | Exp_unop (_, e) | Exp_annotated (e, _) ->
      fold_expr f acc e
  | Exp_constructor (_, args) -> List.fold_left (fold_expr f) acc args
  | Exp_binop (_, l, r) | Exp_seq (l, r) | Exp_cons (l, r) ->
      fold_expr f (fold_expr f acc l) r
  | Exp_index (e, i) -> fold_expr f (fold_expr f acc e) i

let map_expr_span f e = { e with exp_span = f e.exp_span }

let lit_to_string = function
  | Lit_int n -> Int64.to_string n
  | Lit_float f -> string_of_float f
  | Lit_string s -> Printf.sprintf "%S" s
  | Lit_char c -> Printf.sprintf "%C" c
  | Lit_bool b -> if b then "true" else "false"
  | Lit_unit -> "()"
