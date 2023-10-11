(** Pattern-match compilation to constructor / literal decision trees.

    Implements a Maranget-style matrix algorithm (simplified from
    "Compiling Pattern Matching to Good Decision Trees", ML'08):

    - Represent a match as a matrix of patterns with right-hand sides.
    - At each step choose a column (heuristic: fewest constructors first).
    - Specialize the matrix on each head constructor / literal.
    - Emit [Hir.Switch_ctor] / [Hir.Switch_lit] / nested lets for binders.

    Also expands or-patterns and as-patterns before compilation. *)

open Hir

(* -------------------------------------------------------------------------- *)
(* Clause matrix                                                              *)
(* -------------------------------------------------------------------------- *)

type row = {
  pats : pat list;
  (** Remaining patterns for each occurrence column. *)
  binds : (Ident.t * Ident.t) list;
  (** [(user_name, occurrence_temp)] assignments to emit before the body. *)
  body : expr;
  span : Span.t;
}

type matrix = row list

type occurrence = Ident.t
(** Named temporary holding a value being scrutinized. *)

(* -------------------------------------------------------------------------- *)
(* Or-pattern expansion                                                       *)
(* -------------------------------------------------------------------------- *)

let rec expand_or (p : pat) : pat list =
  match p with
  | Pat_or (a, b, _) -> expand_or a @ expand_or b
  | Pat_as (inner, x, sp) ->
      List.map (fun p -> Pat_as (p, x, sp)) (expand_or inner)
  | Pat_ctor (c, args, sp) ->
      (* Cartesian product of expanded argument patterns. *)
      let expanded_args = List.map expand_or args in
      let rec cart = function
        | [] -> [ [] ]
        | xs :: rest ->
            let tails = cart rest in
            List.concat_map (fun x -> List.map (fun t -> x :: t) tails) xs
      in
      List.map (fun args' -> Pat_ctor (c, args', sp)) (cart expanded_args)
  | Pat_tuple (ps, sp) ->
      let expanded = List.map expand_or ps in
      let rec cart = function
        | [] -> [ [] ]
        | xs :: rest ->
            let tails = cart rest in
            List.concat_map (fun x -> List.map (fun t -> x :: t) tails) xs
      in
      List.map (fun ps' -> Pat_tuple (ps', sp)) (cart expanded)
  | p -> [ p ]

let expand_row_ors (row : row) : row list =
  match row.pats with
  | [] -> [ row ]
  | p :: rest ->
      List.map
        (fun p' -> { row with pats = p' :: rest })
        (expand_or p)

let expand_matrix (m : matrix) : matrix =
  List.concat_map expand_row_ors m

(* -------------------------------------------------------------------------- *)
(* Head constructors / literals                                               *)
(* -------------------------------------------------------------------------- *)

type head =
  | Head_ctor of ctor_info
  | Head_lit of lit
  | Head_tuple of int
  | Head_var
  | Head_any

let lit_equal a b =
  match (a, b) with
  | Lit_unit, Lit_unit -> true
  | Lit_bool x, Lit_bool y -> x = y
  | Lit_int x, Lit_int y -> x = y
  | Lit_float x, Lit_float y -> Float.equal x y
  | Lit_string x, Lit_string y -> String.equal x y
  | Lit_char x, Lit_char y -> x = y
  | _ -> false

let ctor_equal a b =
  a.ctor_tag = b.ctor_tag
  && Ident.equal a.ctor_name b.ctor_name

let head_of_pat = function
  | Pat_any _ | Pat_var _ -> Head_any
  | Pat_as (p, _, _) -> head_of_pat p
  | Pat_lit (l, _) -> Head_lit l
  | Pat_ctor (c, _, _) -> Head_ctor c
  | Pat_tuple (ps, _) -> Head_tuple (List.length ps)
  | Pat_or _ -> Head_any (* should be expanded already *)

let strip_as p =
  let rec go binds = function
    | Pat_as (inner, x, _) -> go (x :: binds) inner
    | p -> (List.rev binds, p)
  in
  go [] p

(* -------------------------------------------------------------------------- *)
(* Column heuristics                                                          *)
(* -------------------------------------------------------------------------- *)

(** Count distinct constructor / literal heads in column [j]. *)
let column_score (m : matrix) j =
  let seen_ctors = ref [] in
  let seen_lits = ref [] in
  let tuples = ref None in
  let has_var = ref false in
  List.iter
    (fun row ->
      match List.nth_opt row.pats j with
      | None -> ()
      | Some p -> (
          let _binds, p = strip_as p in
          match head_of_pat p with
          | Head_ctor c ->
              if not (List.exists (ctor_equal c) !seen_ctors) then
                seen_ctors := c :: !seen_ctors
          | Head_lit l ->
              if not (List.exists (lit_equal l) !seen_lits) then
                seen_lits := l :: !seen_lits
          | Head_tuple n -> tuples := Some n
          | Head_any | Head_var -> has_var := true))
    m;
  let distinct =
    List.length !seen_ctors + List.length !seen_lits
    + (match !tuples with None -> 0 | Some _ -> 1)
  in
  (* Prefer columns with fewer distinct heads (branching factor). *)
  (distinct, if !has_var then 1 else 0)

let choose_column (m : matrix) =
  match m with
  | [] -> 0
  | row :: _ ->
      let width = List.length row.pats in
      if width = 0 then 0
      else
        let best = ref 0 in
        let best_score = ref (max_int, max_int) in
        for j = 0 to width - 1 do
          let score = column_score m j in
          if score < !best_score then (
            best_score := score;
            best := j)
        done;
        !best

let swap_column pats j =
  if j = 0 then pats
  else
    let arr = Array.of_list pats in
    let tmp = arr.(0) in
    arr.(0) <- arr.(j);
    arr.(j) <- tmp;
    Array.to_list arr

let swap_occs occs j =
  if j = 0 then occs
  else
    let arr = Array.of_list occs in
    let tmp = arr.(0) in
    arr.(0) <- arr.(j);
    arr.(j) <- tmp;
    Array.to_list arr

(* -------------------------------------------------------------------------- *)
(* Specialization                                                             *)
(* -------------------------------------------------------------------------- *)

let fresh ?(prefix = "p") () = Ident.fresh prefix

(** Specialize matrix on constructor [c]: keep rows whose head is [c] or
    a wildcard/variable; replace the head pattern with the constructor's
    sub-patterns. *)
let specialize_ctor (m : matrix) (c : ctor_info) : matrix =
  List.filter_map
    (fun row ->
      match row.pats with
      | [] -> None
      | p0 :: rest ->
          let as_binds, p0 = strip_as p0 in
          let add_as occ binds =
            List.fold_left (fun acc x -> (x, occ) :: acc) binds as_binds
          in
          (match p0 with
          | Pat_ctor (c', args, _) when ctor_equal c c' ->
              let args =
                let n = c.ctor_arity in
                let got = List.length args in
                if got = n then args
                else if got < n then
                  args @ List.init (n - got) (fun _ -> Pat_any Span.dummy)
                else List.filteri (fun i _ -> i < n) args
              in
              Some
                {
                  row with
                  pats = args @ rest;
                  binds = add_as (Ident.of_string "/*occ*/") row.binds;
                }
          | Pat_any _ ->
              let wilds =
                List.init c.ctor_arity (fun _ -> Pat_any Span.dummy)
              in
              Some { row with pats = wilds @ rest }
          | Pat_var (x, _) ->
              let wilds =
                List.init c.ctor_arity (fun _ -> Pat_any Span.dummy)
              in
              Some
                {
                  row with
                  pats = wilds @ rest;
                  binds = (x, Ident.of_string "/*occ*/") :: row.binds;
                }
          | _ -> None))
    m

(** Like [specialize_ctor] but records the real occurrence name. *)
let specialize_ctor_occ (m : matrix) (c : ctor_info) (occ : occurrence) :
    matrix =
  List.filter_map
    (fun row ->
      match row.pats with
      | [] -> None
      | p0 :: rest ->
          let as_binds, p0 = strip_as p0 in
          let add_as binds =
            List.fold_left (fun acc x -> (x, occ) :: acc) binds as_binds
          in
          (match p0 with
          | Pat_ctor (c', args, _) when ctor_equal c c' ->
              let args =
                let n = c.ctor_arity in
                let got = List.length args in
                if got = n then args
                else if got < n then
                  args @ List.init (n - got) (fun _ -> Pat_any Span.dummy)
                else List.filteri (fun i _ -> i < n) args
              in
              Some { row with pats = args @ rest; binds = add_as row.binds }
          | Pat_any _ ->
              let wilds =
                List.init c.ctor_arity (fun _ -> Pat_any Span.dummy)
              in
              Some { row with pats = wilds @ rest }
          | Pat_var (x, _) ->
              let wilds =
                List.init c.ctor_arity (fun _ -> Pat_any Span.dummy)
              in
              Some
                {
                  row with
                  pats = wilds @ rest;
                  binds = (x, occ) :: add_as row.binds;
                }
          | _ -> None))
    m

let specialize_lit (m : matrix) (lit : lit) (occ : occurrence) : matrix =
  List.filter_map
    (fun row ->
      match row.pats with
      | [] -> None
      | p0 :: rest ->
          let as_binds, p0 = strip_as p0 in
          let add_as binds =
            List.fold_left (fun acc x -> (x, occ) :: acc) binds as_binds
          in
          (match p0 with
          | Pat_lit (l, _) when lit_equal l lit ->
              Some { row with pats = rest; binds = add_as row.binds }
          | Pat_any _ -> Some { row with pats = rest }
          | Pat_var (x, _) ->
              Some
                {
                  row with
                  pats = rest;
                  binds = (x, occ) :: add_as row.binds;
                }
          | _ -> None))
    m

let specialize_tuple (m : matrix) (arity : int) (occ : occurrence) : matrix =
  List.filter_map
    (fun row ->
      match row.pats with
      | [] -> None
      | p0 :: rest ->
          let as_binds, p0 = strip_as p0 in
          let add_as binds =
            List.fold_left (fun acc x -> (x, occ) :: acc) binds as_binds
          in
          (match p0 with
          | Pat_tuple (ps, _) when List.length ps = arity ->
              Some { row with pats = ps @ rest; binds = add_as row.binds }
          | Pat_any _ ->
              let wilds = List.init arity (fun _ -> Pat_any Span.dummy) in
              Some { row with pats = wilds @ rest }
          | Pat_var (x, _) ->
              let wilds = List.init arity (fun _ -> Pat_any Span.dummy) in
              Some
                {
                  row with
                  pats = wilds @ rest;
                  binds = (x, occ) :: add_as row.binds;
                }
          | _ -> None))
    m

(** Default matrix: rows that can match any head (wild / var). *)
let default_matrix (m : matrix) (occ : occurrence) : matrix =
  List.filter_map
    (fun row ->
      match row.pats with
      | [] -> None
      | p0 :: rest ->
          let as_binds, p0 = strip_as p0 in
          let add_as binds =
            List.fold_left (fun acc x -> (x, occ) :: acc) binds as_binds
          in
          (match p0 with
          | Pat_any _ -> Some { row with pats = rest }
          | Pat_var (x, _) ->
              Some
                {
                  row with
                  pats = rest;
                  binds = (x, occ) :: add_as row.binds;
                }
          | Pat_as _ -> None
          | _ -> None))
    m

(* -------------------------------------------------------------------------- *)
(* Collect heads from a column                                                *)
(* -------------------------------------------------------------------------- *)

let collect_ctors (m : matrix) =
  let acc = ref [] in
  List.iter
    (fun row ->
      match row.pats with
      | p0 :: _ -> (
          let _, p0 = strip_as p0 in
          match p0 with
          | Pat_ctor (c, _, _) ->
              if not (List.exists (ctor_equal c) !acc) then
                acc := c :: !acc
          | _ -> ())
      | [] -> ())
    m;
  List.rev !acc

let collect_lits (m : matrix) =
  let acc = ref [] in
  List.iter
    (fun row ->
      match row.pats with
      | p0 :: _ -> (
          let _, p0 = strip_as p0 in
          match p0 with
          | Pat_lit (l, _) ->
              if not (List.exists (lit_equal l) !acc) then
                acc := l :: !acc
          | _ -> ())
      | [] -> ())
    m;
  List.rev !acc

let column_kind (m : matrix) =
  let has_ctor = ref false in
  let has_lit = ref false in
  let tuple_arity = ref None in
  List.iter
    (fun row ->
      match row.pats with
      | p0 :: _ -> (
          let _, p0 = strip_as p0 in
          match head_of_pat p0 with
          | Head_ctor _ -> has_ctor := true
          | Head_lit _ -> has_lit := true
          | Head_tuple n -> tuple_arity := Some n
          | _ -> ())
      | [] -> ())
    m;
  if !has_ctor then `Ctor
  else if !has_lit then `Lit
  else
    match !tuple_arity with
    | Some n -> `Tuple n
    | None -> `Var

(* -------------------------------------------------------------------------- *)
(* Emit binders                                                               *)
(* -------------------------------------------------------------------------- *)

let emit_binds binds body =
  List.fold_right
    (fun (user, occ) acc ->
      if Ident.equal user occ then acc
      else
        Let
          ( user,
            Atom (Atom_var occ, Span.dummy),
            acc,
            Span.dummy ))
    binds body

(* -------------------------------------------------------------------------- *)
(* Exhaustiveness hint (simple)                                               *)
(* -------------------------------------------------------------------------- *)

let has_irrefutable_row (m : matrix) =
  List.exists
    (fun row ->
      List.for_all
        (fun p ->
          let _, p = strip_as p in
          match p with Pat_any _ | Pat_var _ -> true | _ -> false)
        row.pats)
    m

(* -------------------------------------------------------------------------- *)
(* Main compilation                                                           *)
(* -------------------------------------------------------------------------- *)

let rec compile ~(occs : occurrence list) (m : matrix) (span : Span.t) : expr
    =
  let m = expand_matrix m in
  match m with
  | [] -> Fail_match span
  | row :: _ when row.pats = [] ->
      (* Success: emit binders then body. *)
      emit_binds row.binds row.body
  | _ ->
      let j = choose_column m in
      let m =
        List.map (fun row -> { row with pats = swap_column row.pats j }) m
      in
      let occs = swap_occs occs j in
      let occ =
        match occs with
        | o :: _ -> o
        | [] -> fresh ~prefix:"scrut" ()
      in
      (match column_kind m with
      | `Var ->
          (* All wild/var — bind and continue. *)
          let m' =
            List.map
              (fun row ->
                match row.pats with
                | p0 :: rest ->
                    let as_binds, p0 = strip_as p0 in
                    let binds =
                      List.fold_left
                        (fun acc x -> (x, occ) :: acc)
                        row.binds as_binds
                    in
                    let binds =
                      match p0 with
                      | Pat_var (x, _) -> (x, occ) :: binds
                      | _ -> binds
                    in
                    { row with pats = rest; binds }
                | [] -> row)
              m
          in
          (match occs with
          | _ :: rest_occs -> compile ~occs:rest_occs m' span
          | [] -> compile ~occs:[] m' span)
      | `Ctor ->
          let ctors = collect_ctors m in
          let cases =
            List.map
              (fun c ->
                let field_occs =
                  List.init c.ctor_arity (fun i ->
                      fresh ~prefix:(Printf.sprintf "f%d_" i) ())
                in
                let m' = specialize_ctor_occ m c occ in
                let body =
                  compile ~occs:(field_occs @ List.tl occs) m' span
                in
                (c, field_occs, body))
              ctors
          in
          let default =
            let dm = default_matrix m occ in
            if dm = [] then
              if has_irrefutable_row m then None
              else Some (Fail_match span)
            else Some (compile ~occs:(List.tl occs) dm span)
          in
          Switch_ctor (Atom_var occ, cases, default, span)
      | `Lit ->
          let lits = collect_lits m in
          let cases =
            List.map
              (fun lit ->
                let m' = specialize_lit m lit occ in
                let body = compile ~occs:(List.tl occs) m' span in
                (lit, body))
              lits
          in
          let default =
            let dm = default_matrix m occ in
            if dm = [] then Some (Fail_match span)
            else Some (compile ~occs:(List.tl occs) dm span)
          in
          Switch_lit (Atom_var occ, cases, default, span)
      | `Tuple arity ->
          let field_occs =
            List.init arity (fun i ->
                fresh ~prefix:(Printf.sprintf "t%d_" i) ())
          in
          let m' = specialize_tuple m arity occ in
          let body =
            compile ~occs:(field_occs @ List.tl occs) m' span
          in
          (* Bind projections then continue. *)
          List.fold_right
            (fun (i, foc) acc ->
              Let
                ( foc,
                  Project (Atom_var occ, i, span),
                  acc,
                  span ))
            (List.mapi (fun i o -> (i, o)) field_occs)
            body)

(* -------------------------------------------------------------------------- *)
(* Public entry points                                                        *)
(* -------------------------------------------------------------------------- *)

let arms_to_matrix (arms : match_arm list) : matrix =
  List.concat_map
    (fun arm ->
      let base =
        {
          pats = [ arm.arm_pat ];
          binds = [];
          body =
            (match arm.arm_guard with
            | None -> arm.arm_body
            | Some g ->
                (* guard g ==> if g then body else fallthrough — approximate
                   by nesting; full fallthrough needs decision-tree default.
                   Emit as If; non-exhaustive fallthrough becomes Fail. *)
                If
                  ( (match g with
                    | Atom (a, _) -> a
                    | _ ->
                        (* Force atom via temp — desugar should have ANF'd. *)
                        Atom_lit (Lit_bool true)),
                    arm.arm_body,
                    Fail_match arm.arm_span,
                    arm.arm_span ));
          span = arm.arm_span;
        }
      in
      expand_row_ors base)
    arms

let compile_match ~(scrutinee : atom) (arms : match_arm list) ~span : expr =
  match scrutinee with
  | Atom_var occ ->
      let m = arms_to_matrix arms in
      compile ~occs:[ occ ] m span
  | Atom_lit _ as lit ->
      (* Bind literal to a temp then match. *)
      let occ = fresh ~prefix:"lit" () in
      Let
        ( occ,
          Atom (lit, span),
          compile ~occs:[ occ ] (arms_to_matrix arms) span,
          span )

(** Rewrite every [Match] in an expression into decision trees. *)
let rec compile_expr (e : expr) : expr =
  match e with
  | Match (scrut, arms, sp) ->
      let arms =
        List.map
          (fun arm ->
            {
              arm with
              arm_guard = Option.map compile_expr arm.arm_guard;
              arm_body = compile_expr arm.arm_body;
            })
          arms
      in
      compile_match ~scrutinee:scrut arms ~span:sp
  | Let (x, rhs, body, sp) -> Let (x, compile_expr rhs, compile_expr body, sp)
  | Let_rec (bs, body, sp) ->
      Let_rec
        ( List.map (fun (n, e) -> (n, compile_expr e)) bs,
          compile_expr body,
          sp )
  | Fun (ps, body, sp) -> Fun (ps, compile_expr body, sp)
  | If (c, t, f, sp) -> If (c, compile_expr t, compile_expr f, sp)
  | Seq (a, b, sp) -> Seq (compile_expr a, compile_expr b, sp)
  | Switch_ctor (s, cases, default, sp) ->
      Switch_ctor
        ( s,
          List.map (fun (c, bs, b) -> (c, bs, compile_expr b)) cases,
          Option.map compile_expr default,
          sp )
  | Switch_lit (s, cases, default, sp) ->
      Switch_lit
        ( s,
          List.map (fun (l, b) -> (l, compile_expr b)) cases,
          Option.map compile_expr default,
          sp )
  | Atom _ | App _ | Prim _ | Ctor _ | Tuple _ | Project _ | Raise _
  | Fail_match _ ->
      e

let compile_toplevel = function
  | Toplevel_fun ({ body; _ } as f) ->
      Toplevel_fun { f with body = compile_expr body }
  | Toplevel_val ({ body; _ } as v) ->
      Toplevel_val { v with body = compile_expr body }
  | other -> other

let compile_program (prog : program) : program =
  { prog with items = List.map compile_toplevel prog.items }

(** Usefulness / exhaustiveness sketch for diagnostics.
    Returns [true] if the matrix is clearly exhaustive for the known
    constructor set [universe]. *)
let is_exhaustive ~(universe : ctor_info list) (arms : match_arm list) =
  let m = arms_to_matrix arms in
  let rec check m =
    match m with
    | [] -> false
    | row :: _ when row.pats = [] -> true
    | _ -> (
        match column_kind m with
        | `Var ->
            check
              (List.map
                 (fun row ->
                   match row.pats with
                   | _ :: rest -> { row with pats = rest }
                   | [] -> row)
                 m)
        | `Ctor ->
            let present = collect_ctors m in
            let all_covered =
              universe = []
              || List.for_all
                   (fun u -> List.exists (ctor_equal u) present)
                   universe
            in
            let cases_ok =
              List.for_all
                (fun c -> check (specialize_ctor_occ m c (Ident.of_string "_")))
                present
            in
            let default_ok =
              if all_covered then true
              else check (default_matrix m (Ident.of_string "_"))
            in
            cases_ok && default_ok
        | `Lit ->
            (* Literals are never exhaustive without a default. *)
            check (default_matrix m (Ident.of_string "_"))
        | `Tuple n ->
            check (specialize_tuple m n (Ident.of_string "_")))
  in
  check m
