(** High-level intermediate representation.

    Glyph HIR is an ANF-flavoured lambda calculus produced after desugaring
    surface syntax. Pattern matches may still be present as [Match] nodes;
    [Pattern_compile] rewrites them into explicit decision trees. *)

(* -------------------------------------------------------------------------- *)
(* Literals & atoms                                                           *)
(* -------------------------------------------------------------------------- *)

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

(** Primitive operations that survive into MIR as binops / unops / calls. *)
type primop =
  | Prim_add | Prim_sub | Prim_mul | Prim_div | Prim_mod
  | Prim_eq | Prim_ne | Prim_lt | Prim_le | Prim_gt | Prim_ge
  | Prim_and | Prim_or | Prim_not
  | Prim_neg | Prim_fadd | Prim_fsub | Prim_fmul | Prim_fdiv
  | Prim_string_concat
  | Prim_print | Prim_print_int | Prim_print_bool
  | Prim_abort
  | Prim_is_unit
  | Prim_tag_of
  | Prim_box | Prim_unbox

(** Algebraic data constructor identity (name + arity + tag index). *)
type ctor_info = {
  ctor_name : Ident.t;
  ctor_tag : int;
  ctor_arity : int;
  ctor_type : Ident.t option;
}

(* -------------------------------------------------------------------------- *)
(* Patterns (input to pattern compilation)                                    *)
(* -------------------------------------------------------------------------- *)

type pat =
  | Pat_any of Span.t
  | Pat_var of Ident.t * Span.t
  | Pat_lit of lit * Span.t
  | Pat_ctor of ctor_info * pat list * Span.t
  | Pat_or of pat * pat * Span.t
  | Pat_as of pat * Ident.t * Span.t
  | Pat_tuple of pat list * Span.t

let pat_span = function
  | Pat_any sp | Pat_var (_, sp) | Pat_lit (_, sp)
  | Pat_ctor (_, _, sp) | Pat_or (_, _, sp) | Pat_as (_, _, sp)
  | Pat_tuple (_, sp) -> sp

type match_arm = {
  arm_pat : pat;
  arm_guard : expr option;
  arm_body : expr;
  arm_span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Expressions                                                                *)
(* -------------------------------------------------------------------------- *)

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
  (** Decision-tree nodes emitted by [Pattern_compile]. *)
  | Switch_ctor of atom * (ctor_info * Ident.t list * expr) list * expr option * Span.t
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

(* -------------------------------------------------------------------------- *)
(* Constructors                                                               *)
(* -------------------------------------------------------------------------- *)

let atom_var id = Atom_var id
let atom_lit lit = Atom_lit lit

let lit_int n = Lit_int n
let lit_bool b = Lit_bool b
let lit_unit = Lit_unit
let lit_string s = Lit_string s
let lit_float f = Lit_float f
let lit_char c = Lit_char c

let mk_ctor ?(ty = None) name tag arity =
  { ctor_name = name; ctor_tag = tag; ctor_arity = arity; ctor_type = ty }

let expr_span = function
  | Atom (_, sp) | App (_, _, sp) | Prim (_, _, sp) | Let (_, _, _, sp)
  | Let_rec (_, _, sp) | Fun (_, _, sp) | If (_, _, _, sp)
  | Match (_, _, sp) | Ctor (_, _, sp) | Tuple (_, sp) | Project (_, _, sp)
  | Seq (_, _, sp) | Raise (_, sp) | Switch_ctor (_, _, _, sp)
  | Switch_lit (_, _, _, sp) | Fail_match sp -> sp

let with_span e sp =
  match e with
  | Atom (a, _) -> Atom (a, sp)
  | App (f, args, _) -> App (f, args, sp)
  | Prim (p, args, _) -> Prim (p, args, sp)
  | Let (x, rhs, body, _) -> Let (x, rhs, body, sp)
  | Let_rec (bs, body, _) -> Let_rec (bs, body, sp)
  | Fun (ps, body, _) -> Fun (ps, body, sp)
  | If (c, t, e, _) -> If (c, t, e, sp)
  | Match (s, arms, _) -> Match (s, arms, sp)
  | Ctor (c, args, _) -> Ctor (c, args, sp)
  | Tuple (xs, _) -> Tuple (xs, sp)
  | Project (a, i, _) -> Project (a, i, sp)
  | Seq (a, b, _) -> Seq (a, b, sp)
  | Raise (a, _) -> Raise (a, sp)
  | Switch_ctor (s, cases, d, _) -> Switch_ctor (s, cases, d, sp)
  | Switch_lit (s, cases, d, _) -> Switch_lit (s, cases, d, sp)
  | Fail_match _ -> Fail_match sp

(* -------------------------------------------------------------------------- *)
(* Free / bound variables                                                     *)
(* -------------------------------------------------------------------------- *)

let atom_free = function
  | Atom_var v -> Ident.Set.singleton v
  | Atom_lit _ -> Ident.Set.empty

let atoms_free atoms =
  List.fold_left
    (fun acc a -> Ident.Set.union acc (atom_free a))
    Ident.Set.empty atoms

let rec pat_bound = function
  | Pat_any _ | Pat_lit _ -> Ident.Set.empty
  | Pat_var (x, _) -> Ident.Set.singleton x
  | Pat_ctor (_, ps, _) | Pat_tuple (ps, _) ->
      List.fold_left
        (fun acc p -> Ident.Set.union acc (pat_bound p))
        Ident.Set.empty ps
  | Pat_or (p1, p2, _) -> Ident.Set.union (pat_bound p1) (pat_bound p2)
  | Pat_as (p, x, _) -> Ident.Set.add x (pat_bound p)

let rec free_vars e =
  match e with
  | Atom (a, _) -> atom_free a
  | App (f, args, _) -> Ident.Set.union (atom_free f) (atoms_free args)
  | Prim (_, args, _) -> atoms_free args
  | Let (x, rhs, body, _) ->
      Ident.Set.union (free_vars rhs)
        (Ident.Set.remove x (free_vars body))
  | Let_rec (bs, body, _) ->
      let names = List.fold_left (fun s (n, _) -> Ident.Set.add n s) Ident.Set.empty bs in
      let rhs_fvs =
        List.fold_left
          (fun acc (_, rhs) ->
            Ident.Set.union acc (Ident.Set.diff (free_vars rhs) names))
          Ident.Set.empty bs
      in
      Ident.Set.union rhs_fvs (Ident.Set.diff (free_vars body) names)
  | Fun (ps, body, _) ->
      let bound = List.fold_left (fun s p -> Ident.Set.add p s) Ident.Set.empty ps in
      Ident.Set.diff (free_vars body) bound
  | If (c, t, f, _) ->
      Ident.Set.union (atom_free c)
        (Ident.Set.union (free_vars t) (free_vars f))
  | Match (scrut, arms, _) ->
      let base = atom_free scrut in
      List.fold_left
        (fun acc arm ->
          let bound = pat_bound arm.arm_pat in
          let guard_fv =
            match arm.arm_guard with
            | None -> Ident.Set.empty
            | Some g -> Ident.Set.diff (free_vars g) bound
          in
          let body_fv = Ident.Set.diff (free_vars arm.arm_body) bound in
          Ident.Set.union acc (Ident.Set.union guard_fv body_fv))
        base arms
  | Ctor (_, args, _) | Tuple (args, _) -> atoms_free args
  | Project (a, _, _) | Raise (a, _) -> atom_free a
  | Seq (a, b, _) -> Ident.Set.union (free_vars a) (free_vars b)
  | Switch_ctor (scrut, cases, default, _) ->
      let base = atom_free scrut in
      let case_fvs =
        List.fold_left
          (fun acc (_ctor, binds, body) ->
            let bound =
              List.fold_left (fun s x -> Ident.Set.add x s) Ident.Set.empty binds
            in
            Ident.Set.union acc (Ident.Set.diff (free_vars body) bound))
          Ident.Set.empty cases
      in
      let def_fv =
        match default with
        | None -> Ident.Set.empty
        | Some d -> free_vars d
      in
      Ident.Set.union base (Ident.Set.union case_fvs def_fv)
  | Switch_lit (scrut, cases, default, _) ->
      let base = atom_free scrut in
      let case_fvs =
        List.fold_left
          (fun acc (_, body) -> Ident.Set.union acc (free_vars body))
          Ident.Set.empty cases
      in
      let def_fv =
        match default with None -> Ident.Set.empty | Some d -> free_vars d
      in
      Ident.Set.union base (Ident.Set.union case_fvs def_fv)
  | Fail_match _ -> Ident.Set.empty

(* -------------------------------------------------------------------------- *)
(* Size / complexity heuristics                                               *)
(* -------------------------------------------------------------------------- *)

let rec expr_size e =
  match e with
  | Atom _ | Fail_match _ -> 1
  | App (_, args, _) | Prim (_, args, _) | Ctor (_, args, _) | Tuple (args, _) ->
      1 + List.length args
  | Project _ | Raise _ -> 2
  | Let (_, rhs, body, _) | Seq (rhs, body, _) ->
      1 + expr_size rhs + expr_size body
  | Let_rec (bs, body, _) ->
      1
      + List.fold_left (fun n (_, e) -> n + expr_size e) 0 bs
      + expr_size body
  | Fun (_, body, _) -> 1 + expr_size body
  | If (_, t, f, _) -> 1 + expr_size t + expr_size f
  | Match (_, arms, _) ->
      1
      + List.fold_left
          (fun n arm ->
            let g =
              match arm.arm_guard with None -> 0 | Some e -> expr_size e
            in
            n + g + expr_size arm.arm_body)
          0 arms
  | Switch_ctor (_, cases, default, _) ->
      let n =
        List.fold_left (fun n (_, _, b) -> n + expr_size b) 1 cases
      in
      (match default with None -> n | Some d -> n + expr_size d)
  | Switch_lit (_, cases, default, _) ->
      let n = List.fold_left (fun n (_, b) -> n + expr_size b) 1 cases in
      (match default with None -> n | Some d -> n + expr_size d)

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                            *)
(* -------------------------------------------------------------------------- *)

let lit_to_string = function
  | Lit_unit -> "()"
  | Lit_bool true -> "true"
  | Lit_bool false -> "false"
  | Lit_int n -> string_of_int n
  | Lit_float f -> string_of_float f
  | Lit_string s -> Printf.sprintf "%S" s
  | Lit_char c -> Printf.sprintf "%C" c

let atom_to_string = function
  | Atom_var v -> Ident.to_string v
  | Atom_lit l -> lit_to_string l

let primop_to_string = function
  | Prim_add -> "+"
  | Prim_sub -> "-"
  | Prim_mul -> "*"
  | Prim_div -> "/"
  | Prim_mod -> "%"
  | Prim_eq -> "="
  | Prim_ne -> "<>"
  | Prim_lt -> "<"
  | Prim_le -> "<="
  | Prim_gt -> ">"
  | Prim_ge -> ">="
  | Prim_and -> "&&"
  | Prim_or -> "||"
  | Prim_not -> "not"
  | Prim_neg -> "neg"
  | Prim_fadd -> "+."
  | Prim_fsub -> "-."
  | Prim_fmul -> "*."
  | Prim_fdiv -> "/."
  | Prim_string_concat -> "^"
  | Prim_print -> "print"
  | Prim_print_int -> "print_int"
  | Prim_print_bool -> "print_bool"
  | Prim_abort -> "abort"
  | Prim_is_unit -> "is_unit"
  | Prim_tag_of -> "tag_of"
  | Prim_box -> "box"
  | Prim_unbox -> "unbox"

let rec pp_pat fmt = function
  | Pat_any _ -> Format.pp_print_string fmt "_"
  | Pat_var (x, _) -> Ident.pp fmt x
  | Pat_lit (l, _) -> Format.pp_print_string fmt (lit_to_string l)
  | Pat_ctor (c, [], _) -> Ident.pp fmt c.ctor_name
  | Pat_ctor (c, ps, _) ->
      Format.fprintf fmt "%a(" Ident.pp c.ctor_name;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_pat fmt ps;
      Format.pp_print_string fmt ")"
  | Pat_or (p1, p2, _) -> Format.fprintf fmt "(%a | %a)" pp_pat p1 pp_pat p2
  | Pat_as (p, x, _) -> Format.fprintf fmt "(%a as %a)" pp_pat p Ident.pp x
  | Pat_tuple (ps, _) ->
      Format.pp_print_string fmt "(";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_pat fmt ps;
      Format.pp_print_string fmt ")"

let rec pp_expr fmt e =
  match e with
  | Atom (a, _) -> Format.pp_print_string fmt (atom_to_string a)
  | App (f, args, _) ->
      Format.fprintf fmt "(%s" (atom_to_string f);
      List.iter (fun a -> Format.fprintf fmt " %s" (atom_to_string a)) args;
      Format.pp_print_string fmt ")"
  | Prim (p, args, _) ->
      Format.fprintf fmt "(%s" (primop_to_string p);
      List.iter (fun a -> Format.fprintf fmt " %s" (atom_to_string a)) args;
      Format.pp_print_string fmt ")"
  | Let (x, rhs, body, _) ->
      Format.fprintf fmt "(let %a = %a in %a)" Ident.pp x pp_expr rhs pp_expr body
  | Let_rec (bs, body, _) ->
      Format.pp_print_string fmt "(let rec ";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " and ")
        (fun fmt (n, e) -> Format.fprintf fmt "%a = %a" Ident.pp n pp_expr e)
        fmt bs;
      Format.fprintf fmt " in %a)" pp_expr body
  | Fun (ps, body, _) ->
      Format.pp_print_string fmt "(fun";
      List.iter (fun p -> Format.fprintf fmt " %a" Ident.pp p) ps;
      Format.fprintf fmt " -> %a)" pp_expr body
  | If (c, t, f, _) ->
      Format.fprintf fmt "(if %s then %a else %a)" (atom_to_string c) pp_expr t
        pp_expr f
  | Match (s, arms, _) ->
      Format.fprintf fmt "(match %s with" (atom_to_string s);
      List.iter
        (fun arm ->
          Format.fprintf fmt " | %a -> %a" pp_pat arm.arm_pat pp_expr arm.arm_body)
        arms;
      Format.pp_print_string fmt ")"
  | Ctor (c, args, _) ->
      Format.fprintf fmt "(%a" Ident.pp c.ctor_name;
      List.iter (fun a -> Format.fprintf fmt " %s" (atom_to_string a)) args;
      Format.pp_print_string fmt ")"
  | Tuple (xs, _) ->
      Format.pp_print_string fmt "(";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt a -> Format.pp_print_string fmt (atom_to_string a))
        fmt xs;
      Format.pp_print_string fmt ")"
  | Project (a, i, _) -> Format.fprintf fmt "%s.#%d" (atom_to_string a) i
  | Seq (a, b, _) -> Format.fprintf fmt "(%a; %a)" pp_expr a pp_expr b
  | Raise (a, _) -> Format.fprintf fmt "(raise %s)" (atom_to_string a)
  | Switch_ctor (s, cases, default, _) ->
      Format.fprintf fmt "(switch-ctor %s" (atom_to_string s);
      List.iter
        (fun (c, binds, body) ->
          Format.fprintf fmt " | %a" Ident.pp c.ctor_name;
          List.iter (fun b -> Format.fprintf fmt " %a" Ident.pp b) binds;
          Format.fprintf fmt " -> %a" pp_expr body)
        cases;
      (match default with
      | None -> ()
      | Some d -> Format.fprintf fmt " | _ -> %a" pp_expr d);
      Format.pp_print_string fmt ")"
  | Switch_lit (s, cases, default, _) ->
      Format.fprintf fmt "(switch-lit %s" (atom_to_string s);
      List.iter
        (fun (l, body) ->
          Format.fprintf fmt " | %s -> %a" (lit_to_string l) pp_expr body)
        cases;
      (match default with
      | None -> ()
      | Some d -> Format.fprintf fmt " | _ -> %a" pp_expr d);
      Format.pp_print_string fmt ")"
  | Fail_match _ -> Format.pp_print_string fmt "(fail-match)"

let pp_toplevel fmt = function
  | Toplevel_fun { name; params; body; recursive; _ } ->
      Format.fprintf fmt "%s %a"
        (if recursive then "fun rec" else "fun")
        Ident.pp name;
      List.iter (fun p -> Format.fprintf fmt " %a" Ident.pp p) params;
      Format.fprintf fmt " = %a" pp_expr body
  | Toplevel_val { name; body; _ } ->
      Format.fprintf fmt "val %a = %a" Ident.pp name pp_expr body
  | Toplevel_type { name; params; ctors; _ } ->
      Format.fprintf fmt "type %a" Ident.pp name;
      List.iter (fun p -> Format.fprintf fmt " %a" Ident.pp p) params;
      Format.pp_print_string fmt " =";
      List.iter
        (fun c ->
          Format.fprintf fmt " | %a/%d" Ident.pp c.ctor_name c.ctor_arity)
        ctors
  | Toplevel_extern { name; arity; _ } ->
      Format.fprintf fmt "extern %a/%d" Ident.pp name arity

let pp_program fmt prog =
  List.iter
    (fun item ->
      pp_toplevel fmt item;
      Format.pp_print_newline fmt ())
    prog.items

let program_of_items ?(span = Span.dummy) items = { items; span }

(** Map an expression transformer bottom-up. *)
let rec map_expr f e =
  let e' =
    match e with
    | Atom _ | Fail_match _ | Project _ | Raise _ -> e
    | App _ | Prim _ | Ctor _ | Tuple _ -> e
    | Let (x, rhs, body, sp) -> Let (x, map_expr f rhs, map_expr f body, sp)
    | Let_rec (bs, body, sp) ->
        Let_rec
          ( List.map (fun (n, e) -> (n, map_expr f e)) bs,
            map_expr f body,
            sp )
    | Fun (ps, body, sp) -> Fun (ps, map_expr f body, sp)
    | If (c, t, e2, sp) -> If (c, map_expr f t, map_expr f e2, sp)
    | Match (s, arms, sp) ->
        Match
          ( s,
            List.map
              (fun arm ->
                {
                  arm with
                  arm_guard = Option.map (map_expr f) arm.arm_guard;
                  arm_body = map_expr f arm.arm_body;
                })
              arms,
            sp )
    | Seq (a, b, sp) -> Seq (map_expr f a, map_expr f b, sp)
    | Switch_ctor (s, cases, default, sp) ->
        Switch_ctor
          ( s,
            List.map (fun (c, bs, b) -> (c, bs, map_expr f b)) cases,
            Option.map (map_expr f) default,
            sp )
    | Switch_lit (s, cases, default, sp) ->
        Switch_lit
          ( s,
            List.map (fun (l, b) -> (l, map_expr f b)) cases,
            Option.map (map_expr f) default,
            sp )
  in
  f e'
