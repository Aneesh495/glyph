(** Pattern compilation to decision trees (Maranget / Pettersson style).

    Compiles [Hir.Match] nodes into [Switch_ctor] / [Switch_lit] decision
    DAGs, performing basic usefulness and exhaustiveness analysis.

    Algorithm overview (after Maranget, JFP 2008 / ML workshop notes):
    - Represent the match as a matrix of pattern rows with associated bodies.
    - Specialize the matrix on a chosen column's head constructor / literal.
    - Default (wildcard) columns are handled by the default matrix.
    - Usefulness of a clause = whether the clause's pattern row is useful
      relative to previous rows (can match some value the previous ones miss).
    - Exhaustiveness = whether the wildcard row is useless against the full
      matrix (no value falls through). *)

type diagnostic = Diagnostic.t

(* -------------------------------------------------------------------------- *)
(* Clause matrix                                                              *)
(* -------------------------------------------------------------------------- *)

type clause = {
  pats : Hir.pat list;
  (** Remaining patterns for columns 0..n-1. *)
  binds : (Ident.t * int * int) list;
  (** Deferred [as]-bindings: (name, column, depth) reconstructed later. *)
  guard : Hir.expr option;
  body : Hir.expr;
  span : Span.t;
  index : int;
  (** Original clause index for usefulness reporting. *)
}

type matrix = clause list

type head_ctor =
  | Head_ctor of Hir.ctor_info
  | Head_lit of Hir.lit
  | Head_tuple of int
  | Head_wildcard

let lit_equal a b =
  match (a, b) with
  | Hir.Lit_unit, Hir.Lit_unit -> true
  | Hir.Lit_bool x, Hir.Lit_bool y -> x = y
  | Hir.Lit_int x, Hir.Lit_int y -> x = y
  | Hir.Lit_float x, Hir.Lit_float y -> Float.equal x y
  | Hir.Lit_string x, Hir.Lit_string y -> String.equal x y
  | Hir.Lit_char x, Hir.Lit_char y -> Char.equal x y
  | _ -> false

let ctor_equal a b =
  a.Hir.ctor_tag = b.Hir.ctor_tag
  && Ident.equal a.Hir.ctor_name b.Hir.ctor_name

(* -------------------------------------------------------------------------- *)
(* Pattern utilities                                                          *)
(* -------------------------------------------------------------------------- *)

(** Strip [as] wrappers, accumulating binders. *)
let rec strip_as (p : Hir.pat) : Hir.pat * Ident.t list =
  match p with
  | Hir.Pat_as (inner, x, _) ->
      let p', xs = strip_as inner in
      (p', x :: xs)
  | _ -> (p, [])

(** Expand or-patterns into multiple rows (left-biased). *)
let rec expand_ors (p : Hir.pat) : Hir.pat list =
  match p with
  | Hir.Pat_or (p1, p2, _) -> expand_ors p1 @ expand_ors p2
  | Hir.Pat_as (inner, x, sp) ->
      List.map (fun p -> Hir.Pat_as (p, x, sp)) (expand_ors inner)
  | _ -> [ p ]

let head_of_pat (p : Hir.pat) : head_ctor =
  let p, _ = strip_as p in
  match p with
  | Hir.Pat_any _ | Hir.Pat_var _ -> Head_wildcard
  | Hir.Pat_lit (l, _) -> Head_lit l
  | Hir.Pat_ctor (c, _, _) -> Head_ctor c
  | Hir.Pat_tuple (ps, _) -> Head_tuple (List.length ps)
  | Hir.Pat_or _ | Hir.Pat_as _ -> Head_wildcard (* should be expanded *)

let arity_of_head = function
  | Head_ctor c -> c.Hir.ctor_arity
  | Head_tuple n -> n
  | Head_lit _ | Head_wildcard -> 0

(** Sub-patterns under a constructor / tuple head; wildcards pad arity. *)
let specialize_pat (head : head_ctor) (p : Hir.pat) : Hir.pat list option =
  let p, _as_binds = strip_as p in
  let wild sp n =
    List.init n (fun _ -> Hir.Pat_any sp)
  in
  match (head, p) with
  | _, (Hir.Pat_any sp | Hir.Pat_var (_, sp)) ->
      Some (wild sp (arity_of_head head))
  | Head_ctor c, Hir.Pat_ctor (c', args, _) when ctor_equal c c' -> Some args
  | Head_lit l, Hir.Pat_lit (l', _) when lit_equal l l' -> Some []
  | Head_tuple n, Hir.Pat_tuple (ps, _) when List.length ps = n -> Some ps
  | Head_ctor _, Hir.Pat_ctor _ -> None
  | Head_lit _, Hir.Pat_lit _ -> None
  | Head_tuple _, Hir.Pat_tuple _ -> None
  | Head_wildcard, _ -> Some []
  | _ -> None

(** Default-matrix row: keep rows whose first pattern is a wildcard/var. *)
let default_pat (p : Hir.pat) : Hir.pat list option =
  let p, _ = strip_as p in
  match p with
  | Hir.Pat_any _ | Hir.Pat_var _ -> Some []
  | Hir.Pat_ctor _ | Hir.Pat_lit _ | Hir.Pat_tuple _ -> None
  | Hir.Pat_or _ | Hir.Pat_as _ -> None

(* -------------------------------------------------------------------------- *)
(* Matrix specialization                                                      *)
(* -------------------------------------------------------------------------- *)

let specialize_matrix (head : head_ctor) (rows : matrix) : matrix =
  List.filter_map
    (fun clause ->
      match clause.pats with
      | [] -> Some clause
      | p :: rest -> (
          match specialize_pat head p with
          | None -> None
          | Some sub ->
              let _, as_binds = strip_as p in
              let binds =
                List.fold_left
                  (fun acc x -> (x, 0, 0) :: acc)
                  clause.binds as_binds
              in
              Some { clause with pats = sub @ rest; binds }))
    rows

let default_matrix (rows : matrix) : matrix =
  List.filter_map
    (fun clause ->
      match clause.pats with
      | [] -> Some clause
      | p :: rest -> (
          match default_pat p with
          | None -> None
          | Some sub ->
              let _, as_binds = strip_as p in
              let binds =
                List.fold_left
                  (fun acc x -> (x, 0, 0) :: acc)
                  clause.binds as_binds
              in
              Some { clause with pats = sub @ rest; binds }))
    rows

(** Collect distinct head constructors / lits appearing in column 0. *)
let column_heads (rows : matrix) : head_ctor list =
  let seen_ctors = ref [] in
  let seen_lits = ref [] in
  let seen_tuples = ref [] in
  List.iter
    (fun clause ->
      match clause.pats with
      | [] -> ()
      | p :: _ -> (
          match head_of_pat p with
          | Head_wildcard -> ()
          | Head_ctor c as h ->
              if not (List.exists (function Head_ctor c' -> ctor_equal c c' | _ -> false) !seen_ctors)
              then seen_ctors := h :: !seen_ctors
          | Head_lit l as h ->
              if not (List.exists (function Head_lit l' -> lit_equal l l' | _ -> false) !seen_lits)
              then seen_lits := h :: !seen_lits
          | Head_tuple n as h ->
              if not (List.exists (function Head_tuple n' -> n = n' | _ -> false) !seen_tuples)
              then seen_tuples := h :: !seen_tuples))
    rows;
  List.rev !seen_ctors @ List.rev !seen_lits @ List.rev !seen_tuples

(** Signature completeness: all constructors of a type present? *)
let signature_complete (heads : head_ctor list) : bool =
  match heads with
  | [] -> false
  | Head_ctor c :: _ as cs -> (
      let ctors = List.filter_map (function Head_ctor c -> Some c | _ -> None) cs in
      (* Without a full type decl, approximate: if we see tags 0..n-1 contiguous
         and max tag + 1 equals count, treat as complete when arity info agrees. *)
      let tags = List.map (fun c -> c.Hir.ctor_tag) ctors |> List.sort_uniq Int.compare in
      match tags with
      | [] -> false
      | _ ->
          let max_tag = List.fold_left max 0 tags in
          List.length tags = max_tag + 1
          &&
          (* Prefer type-name agreement when available *)
          (match c.Hir.ctor_type with
          | None -> true
          | Some ty ->
              List.for_all
                (fun c' ->
                  match c'.Hir.ctor_type with
                  | None -> true
                  | Some ty' -> Ident.equal ty ty')
                ctors))
  | Head_lit (Hir.Lit_bool _) :: _ as ls ->
      let bools =
        List.filter_map
          (function Head_lit (Hir.Lit_bool b) -> Some b | _ -> None)
          ls
      in
      List.mem true bools && List.mem false bools
  | Head_lit Hir.Lit_unit :: _ -> true
  | Head_tuple _ :: _ -> true (* only one tuple arity per column *)
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* Usefulness / exhaustiveness (Maranget)                                     *)
(* -------------------------------------------------------------------------- *)

(** [useful P q] — is pattern row [q] useful with respect to matrix [P]?
    Classic recursive formulation on the first column. *)
let rec useful (matrix : Hir.pat list list) (query : Hir.pat list) : bool =
  match query with
  | [] -> (
      match matrix with
      | [] -> true
      | _ -> false)
  | q0 :: qs ->
      let q0, _ = strip_as q0 in
      match q0 with
      | Hir.Pat_ctor (c, args, _) ->
          let head = Head_ctor c in
          let specialized =
            List.filter_map
              (fun row ->
                match row with
                | [] -> Some []
                | p :: ps -> (
                    match specialize_pat head p with
                    | None -> None
                    | Some sub -> Some (sub @ ps)))
              matrix
          in
          useful specialized (args @ qs)
      | Hir.Pat_lit (l, _) ->
          let head = Head_lit l in
          let specialized =
            List.filter_map
              (fun row ->
                match row with
                | [] -> Some []
                | p :: ps -> (
                    match specialize_pat head p with
                    | None -> None
                    | Some sub -> Some (sub @ ps)))
              matrix
          in
          useful specialized qs
      | Hir.Pat_tuple (args, _) ->
          let head = Head_tuple (List.length args) in
          let specialized =
            List.filter_map
              (fun row ->
                match row with
                | [] -> Some []
                | p :: ps -> (
                    match specialize_pat head p with
                    | None -> None
                    | Some sub -> Some (sub @ ps)))
              matrix
          in
          useful specialized (args @ qs)
      | Hir.Pat_any _ | Hir.Pat_var _ ->
          let heads =
            List.filter_map
              (fun row ->
                match row with
                | [] -> None
                | p :: _ -> (
                    match head_of_pat p with
                    | Head_wildcard -> None
                    | h -> Some h))
              matrix
            |> List.sort_uniq (fun a b ->
                   match (a, b) with
                   | Head_ctor c1, Head_ctor c2 -> Int.compare c1.ctor_tag c2.ctor_tag
                   | Head_lit l1, Head_lit l2 ->
                       String.compare (Hir.lit_to_string l1) (Hir.lit_to_string l2)
                   | Head_tuple n1, Head_tuple n2 -> Int.compare n1 n2
                   | _ -> Stdlib.compare a b)
          in
          if heads <> [] && signature_complete heads then
            (* Useful if useful for some constructor in the signature. *)
            List.exists
              (fun head ->
                let arity = arity_of_head head in
                let wilds =
                  List.init arity (fun _ -> Hir.Pat_any Span.dummy)
                in
                let specialized =
                  List.filter_map
                    (fun row ->
                      match row with
                      | [] -> Some []
                      | p :: ps -> (
                          match specialize_pat head p with
                          | None -> None
                          | Some sub -> Some (sub @ ps)))
                    matrix
                in
                useful specialized (wilds @ qs))
              heads
          else
            (* Default case: useful against default matrix. *)
            let defaults =
              List.filter_map
                (fun row ->
                  match row with
                  | [] -> Some []
                  | p :: ps -> (
                      match default_pat p with
                      | None -> None
                      | Some sub -> Some (sub @ ps)))
                matrix
            in
            useful defaults qs
      | Hir.Pat_or (p1, p2, _) ->
          useful matrix (p1 :: qs) || useful matrix (p2 :: qs)
      | Hir.Pat_as (inner, _, _) -> useful matrix (inner :: qs)

let is_exhaustive (matrix : Hir.pat list list) ~(ncols : int) : bool =
  let wilds = List.init ncols (fun _ -> Hir.Pat_any Span.dummy) in
  not (useful matrix wilds)

type usefulness_result = {
  redundant : int list;
  exhaustive : bool;
  diagnostics : diagnostic list;
}

let analyze_usefulness (arms : Hir.match_arm list) : usefulness_result =
  let rows_rev = ref [] in
  let redundant = ref [] in
  let diags = ref [] in
  List.iteri
    (fun i arm ->
      let variants = expand_ors arm.Hir.arm_pat in
      let useful_any =
        List.exists
          (fun p ->
            let q = [ p ] in
            let u = useful (List.rev !rows_rev) q in
            rows_rev := q :: !rows_rev;
            u)
          variants
      in
      if not useful_any then (
        redundant := i :: !redundant;
        diags :=
          Diagnostic.warning arm.arm_span
            (Printf.sprintf "redundant pattern clause #%d" i)
          :: !diags))
    arms;
  let matrix = List.rev !rows_rev in
  let exhaustive = is_exhaustive matrix ~ncols:1 in
  if not exhaustive then
    diags :=
      (match arms with
      | [] ->
          Diagnostic.warning Span.dummy "empty match is not exhaustive"
      | a :: _ ->
          Diagnostic.warning a.arm_span "this pattern matching is not exhaustive")
      :: !diags;
  { redundant = List.rev !redundant; exhaustive; diagnostics = List.rev !diags }

(* -------------------------------------------------------------------------- *)
(* Decision tree compilation                                                  *)
(* -------------------------------------------------------------------------- *)

type decision =
  | Leaf of clause
  | Switch_ctors of {
      column : int;
      cases : (Hir.ctor_info * Ident.t list * decision) list;
      default : decision option;
    }
  | Switch_lits of {
      column : int;
      cases : (Hir.lit * decision) list;
      default : decision option;
    }
  | Switch_tuple of {
      column : int;
      arity : int;
      binds : Ident.t list;
      body : decision;
    }
  | Fail

(** Fresh binders for constructor / tuple fields. *)
let fresh_binds arity =
  List.init arity (fun i -> Ident.fresh (Printf.sprintf "p%d" i))

(** Heuristic: pick the column with the most constructor heads (simple
    "fewest defaults" score). *)
let pick_column (rows : matrix) : int =
  match rows with
  | [] -> 0
  | c :: _ ->
      let ncols = List.length c.pats in
      if ncols <= 1 then 0
      else
        let best = ref 0 in
        let best_score = ref (-1) in
        for col = 0 to ncols - 1 do
          let heads = ref 0 in
          List.iter
            (fun clause ->
              match List.nth_opt clause.pats col with
              | None -> ()
              | Some p -> (
                  match head_of_pat p with
                  | Head_wildcard -> ()
                  | _ -> incr heads))
            rows;
          if !heads > !best_score then (
            best_score := !heads;
            best := col)
        done;
        !best

(** Swap column [i] to the front of every row. *)
let swap_column (rows : matrix) (i : int) : matrix =
  if i = 0 then rows
  else
    List.map
      (fun clause ->
        let pats = Array.of_list clause.pats in
        if i >= Array.length pats then clause
        else (
          let tmp = pats.(0) in
          pats.(0) <- pats.(i);
          pats.(i) <- tmp;
          { clause with pats = Array.to_list pats }))
      rows

let rec compile_matrix (rows : matrix) : decision =
  match rows with
  | [] -> Fail
  | clause :: _ as rows ->
      if List.for_all (fun c -> c.pats = []) rows then
        (* All patterns consumed — take the first clause (leftmost match). *)
        Leaf (List.hd rows)
      else if
        List.for_all
          (fun c ->
            match c.pats with
            | [] -> true
            | p :: _ -> (
                match head_of_pat p with
                | Head_wildcard -> true
                | _ -> false))
          rows
      then
        (* All first-column wildcards: strip and continue. *)
        let rows' =
          List.map
            (fun c ->
              match c.pats with
              | [] -> c
              | p :: rest ->
                  let _, as_binds = strip_as p in
                  let binds =
                    match p with
                    | Hir.Pat_var (x, _) -> (x, 0, 0) :: c.binds
                    | _ ->
                        List.fold_left
                          (fun acc x -> (x, 0, 0) :: acc)
                          c.binds as_binds
                  in
                  (* If it's a variable pattern, bind the scrutinee column. *)
                  let binds =
                    match strip_as p |> fst with
                    | Hir.Pat_var (x, _) -> (x, 0, 0) :: binds
                    | _ -> binds
                  in
                  { c with pats = rest; binds })
            rows
        in
        compile_matrix rows'
      else
        let col = pick_column rows in
        let rows = swap_column rows col in
        let heads = column_heads rows in
        let complete = signature_complete heads in
        let is_lit =
          List.exists (function Head_lit _ -> true | _ -> false) heads
        in
        let is_tuple =
          List.exists (function Head_tuple _ -> true | _ -> false) heads
        in
        if is_tuple then (
          match heads with
          | Head_tuple n :: _ ->
              let binds = fresh_binds n in
              let body = compile_matrix (specialize_matrix (Head_tuple n) rows) in
              Switch_tuple { column = col; arity = n; binds; body }
          | _ -> Fail)
        else if is_lit then
          let cases =
            List.filter_map
              (function
                | Head_lit l ->
                    Some (l, compile_matrix (specialize_matrix (Head_lit l) rows))
                | _ -> None)
              heads
          in
          let default =
            if complete then None
            else
              match default_matrix rows with
              | [] -> Some Fail
              | drows -> Some (compile_matrix drows)
          in
          Switch_lits { column = col; cases; default }
        else
          let cases =
            List.filter_map
              (function
                | Head_ctor c ->
                    let binds = fresh_binds c.Hir.ctor_arity in
                    let body =
                      compile_matrix (specialize_matrix (Head_ctor c) rows)
                    in
                    Some (c, binds, body)
                | _ -> None)
              heads
          in
          let default =
            if complete then None
            else
              match default_matrix rows with
              | [] -> Some Fail
              | drows -> Some (compile_matrix drows)
          in
          Switch_ctors { column = col; cases; default }

(* -------------------------------------------------------------------------- *)
(* Decision tree → Hir                                                        *)
(* -------------------------------------------------------------------------- *)

(** Apply deferred variable bindings from a leaf clause to its body.
    Column 0 refers to the current scrutinee atom. *)
let apply_binds ~(scrut : Hir.atom) (clause : clause) : Hir.expr =
  let body = clause.body in
  let body =
    match clause.guard with
    | None -> body
    | Some g ->
        (* guard => body else fall through is encoded as nested if; since we
           already committed to this leaf, a failing guard becomes Fail_match. *)
        let sp = clause.span in
        bind_guard g body sp
  in
  List.fold_left
    (fun body (x, _col, _depth) ->
      (* Simplified: bind all pattern variables to the scrutinee atom.
         Nested field binders are introduced explicitly by Switch_* nodes. *)
      Hir.Let (x, Hir.Atom (scrut, clause.span), body, clause.span))
    body clause.binds

and bind_guard g body sp =
  match g with
  | Hir.Atom (a, _) ->
      Hir.If (a, body, Hir.Fail_match sp, sp)
  | _ ->
      let t = Ident.fresh "guard" in
      Hir.Let
        ( t,
          g,
          Hir.If (Hir.Atom_var t, body, Hir.Fail_match sp, sp),
          sp )

let rec decision_to_expr ~(scrut : Hir.atom) ~(span : Span.t) (d : decision) :
    Hir.expr =
  match d with
  | Fail -> Hir.Fail_match span
  | Leaf clause -> apply_binds ~scrut clause
  | Switch_ctors { cases; default; _ } ->
      let cases' =
        List.map
          (fun (c, binds, body) ->
            (* Nested decision uses first binder as new scrutinee when arity>0;
               for multi-field we keep the original and rely on Project in
               subsequent lowering. For decision-tree sub-columns we introduce
               a synthetic tuple of binders as the environment. *)
            let body_expr =
              match binds with
              | [] -> decision_to_expr ~scrut ~span body
              | b0 :: _ ->
                  (* Sub-matrix columns refer to extracted fields. We wrap the
                     body so field binders are in scope; recursive switches
                     still key off [scrut] for tag tests on the same value,
                     while leaf bindings use the field idents introduced here. *)
                  let inner = decision_to_expr ~scrut:(Hir.Atom_var b0) ~span body in
                  (* Ensure all binders are "defined" via GetField-style projects
                     done in lower_hir; here just nest lets from projects. *)
                  List.fold_right
                    (fun (i, b) e ->
                      Hir.Let
                        ( b,
                          Hir.Project (scrut, i, span),
                          e,
                          span ))
                    (List.mapi (fun i b -> (i, b)) binds)
                    inner
            in
            (c, binds, body_expr))
          cases
      in
      let default' = Option.map (decision_to_expr ~scrut ~span) default in
      Hir.Switch_ctor (scrut, cases', default', span)
  | Switch_lits { cases; default; _ } ->
      let cases' =
        List.map
          (fun (l, body) -> (l, decision_to_expr ~scrut ~span body))
          cases
      in
      let default' = Option.map (decision_to_expr ~scrut ~span) default in
      Hir.Switch_lit (scrut, cases', default', span)
  | Switch_tuple { arity; binds; body; _ } ->
      let inner = decision_to_expr ~scrut ~span body in
      List.fold_right
        (fun (i, b) e ->
          if i < arity then
            Hir.Let (b, Hir.Project (scrut, i, span), e, span)
          else e)
        (List.mapi (fun i b -> (i, b)) binds)
        inner

(* -------------------------------------------------------------------------- *)
(* Public entry points                                                        *)
(* -------------------------------------------------------------------------- *)

let arms_to_matrix (arms : Hir.match_arm list) : matrix =
  let rows = ref [] in
  List.iteri
    (fun index arm ->
      let variants = expand_ors arm.Hir.arm_pat in
      List.iter
        (fun p ->
          rows :=
            {
              pats = [ p ];
              binds = [];
              guard = arm.arm_guard;
              body = arm.arm_body;
              span = arm.arm_span;
              index;
            }
            :: !rows)
        variants)
    arms;
  List.rev !rows

let compile_match ~(scrut : Hir.atom) ~(arms : Hir.match_arm list)
    ~(span : Span.t) : Hir.expr * usefulness_result =
  let analysis = analyze_usefulness arms in
  let matrix = arms_to_matrix arms in
  let decision = compile_matrix matrix in
  let expr = decision_to_expr ~scrut ~span decision in
  (expr, analysis)

(** Rewrite all [Match] nodes in an expression. *)
let rec compile_expr (e : Hir.expr) : Hir.expr * diagnostic list =
  match e with
  | Hir.Match (scrut, arms, sp) ->
      let e', analysis = compile_match ~scrut ~arms ~span:sp in
      let e'', diags = compile_expr e' in
      (e'', analysis.diagnostics @ diags)
  | Hir.Let (x, rhs, body, sp) ->
      let rhs', d1 = compile_expr rhs in
      let body', d2 = compile_expr body in
      (Hir.Let (x, rhs', body', sp), d1 @ d2)
  | Hir.Let_rec (bs, body, sp) ->
      let diags = ref [] in
      let bs' =
        List.map
          (fun (n, e) ->
            let e', d = compile_expr e in
            diags := d @ !diags;
            (n, e'))
          bs
      in
      let body', d = compile_expr body in
      (Hir.Let_rec (bs', body', sp), List.rev !diags @ d)
  | Hir.Fun (ps, body, sp) ->
      let body', d = compile_expr body in
      (Hir.Fun (ps, body', sp), d)
  | Hir.If (c, t, f, sp) ->
      let t', d1 = compile_expr t in
      let f', d2 = compile_expr f in
      (Hir.If (c, t', f', sp), d1 @ d2)
  | Hir.Seq (a, b, sp) ->
      let a', d1 = compile_expr a in
      let b', d2 = compile_expr b in
      (Hir.Seq (a', b', sp), d1 @ d2)
  | Hir.Switch_ctor (s, cases, default, sp) ->
      let diags = ref [] in
      let cases' =
        List.map
          (fun (c, bs, b) ->
            let b', d = compile_expr b in
            diags := d @ !diags;
            (c, bs, b'))
          cases
      in
      let default', d =
        match default with
        | None -> (None, [])
        | Some e ->
            let e', d = compile_expr e in
            (Some e', d)
      in
      (Hir.Switch_ctor (s, cases', default', sp), List.rev !diags @ d)
  | Hir.Switch_lit (s, cases, default, sp) ->
      let diags = ref [] in
      let cases' =
        List.map
          (fun (l, b) ->
            let b', d = compile_expr b in
            diags := d @ !diags;
            (l, b'))
          cases
      in
      let default', d =
        match default with
        | None -> (None, [])
        | Some e ->
            let e', d = compile_expr e in
            (Some e', d)
      in
      (Hir.Switch_lit (s, cases', default', sp), List.rev !diags @ d)
  | ( Atom _ | App _ | Prim _ | Ctor _ | Tuple _ | Project _ | Raise _
    | Fail_match _ ) as e ->
      (e, [])

let compile_toplevel = function
  | Hir.Toplevel_fun ({ body; _ } as f) ->
      let body', diags = compile_expr body in
      (Hir.Toplevel_fun { f with body = body' }, diags)
  | Hir.Toplevel_val ({ body; _ } as v) ->
      let body', diags = compile_expr body in
      (Hir.Toplevel_val { v with body = body' }, diags)
  | (Hir.Toplevel_type _ | Hir.Toplevel_extern _) as t -> (t, [])

let compile_program (prog : Hir.program) : Hir.program * diagnostic list =
  let diags = ref [] in
  let items =
    List.map
      (fun item ->
        let item', d = compile_toplevel item in
        diags := d @ !diags;
        item')
      prog.items
  in
  ({ prog with items }, List.rev !diags)

(** Run usefulness analysis alone (no rewrite). *)
let check_match arms = analyze_usefulness arms
