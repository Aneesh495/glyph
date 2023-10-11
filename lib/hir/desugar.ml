(** Desugar surface [Ast] into ANF-flavoured [Hir].

    Responsibilities:
    - lower [&&] / [||] to [If]
    - lower [|>] to reverse application
    - flatten multi-arg [fun]/[let] into nested/curried form kept as multi-param
    - name intermediate results (ANF) so applications only take atoms
    - resolve constructor tags from type declarations
    - leave [Match] for [Pattern] to compile into decision trees *)

open Ast

type ctor_env = (Ident.t, Hir.ctor_info) Hashtbl.t

type ctx = {
  ctors : ctor_env;
  mutable temp : int;
}

let make_ctx () =
  { ctors = Hashtbl.create 64; temp = 0 }

let fresh_temp ctx ?(prefix = "t") () =
  ctx.temp <- ctx.temp + 1;
  Ident.fresh (Printf.sprintf "%s%d" prefix ctx.temp)

let register_variant ctx (td : type_decl) =
  match td with
  | Alias _ -> ()
  | Variant { name; constructors; _ } ->
      List.iteri
        (fun tag (c : constructor_decl) ->
          let info =
            Hir.mk_ctor ~ty:(Some name) c.name tag (List.length c.args)
          in
          Hashtbl.replace ctx.ctors c.name info)
        constructors

let lookup_ctor ctx name =
  match Hashtbl.find_opt ctx.ctors name with
  | Some info -> info
  | None ->
      (* Unknown constructors get a provisional tag; typechecking should
         have rejected bad programs already. *)
      Hir.mk_ctor name 0 0

let lit_of_ast = function
  | Int n -> Hir.Lit_int n
  | Float f -> Hir.Lit_float f
  | Bool b -> Hir.Lit_bool b
  | Char c -> Hir.Lit_char c
  | String s -> Hir.Lit_string s
  | Unit -> Hir.Lit_unit

let binop_to_prim = function
  | Add -> Some Hir.Prim_add
  | Sub -> Some Hir.Prim_sub
  | Mul -> Some Hir.Prim_mul
  | Div -> Some Hir.Prim_div
  | Mod -> Some Hir.Prim_mod
  | Eq -> Some Hir.Prim_eq
  | Neq -> Some Hir.Prim_ne
  | Lt -> Some Hir.Prim_lt
  | Le -> Some Hir.Prim_le
  | Gt -> Some Hir.Prim_gt
  | Ge -> Some Hir.Prim_ge
  | And | Or | Cons | Append | Pipe -> None

let unop_to_prim = function
  | Neg -> Hir.Prim_neg
  | Not -> Hir.Prim_not
  | NegF -> Hir.Prim_neg

(** Wrap [body] under successive [Let] bindings. Bindings are applied
    innermost-first (list head is outermost). *)
let wrap_lets binds body =
  List.fold_right
    (fun (x, rhs, sp) acc -> Hir.Let (x, rhs, acc, sp))
    binds body

(** Convert an expression to ANF, returning (bindings, atom). *)
let rec anf_atom ctx (e : expr) : (Ident.t * Hir.expr * Span.t) list * Hir.atom =
  match e with
  | Var (id, _) -> ([], Hir.Atom_var id)
  | Lit (lit, _) -> ([], Hir.Atom_lit (lit_of_ast lit))
  | Annotated (inner, _, _) -> anf_atom ctx inner
  | _ ->
      let sp = expr_span e in
      let binds, hexpr = anf_expr ctx e in
      let tmp = fresh_temp ctx () in
      (binds @ [ (tmp, hexpr, sp) ], Hir.Atom_var tmp)

and anf_expr ctx (e : expr) : (Ident.t * Hir.expr * Span.t) list * Hir.expr =
  let sp = expr_span e in
  match e with
  | Var (id, sp) -> ([], Hir.Atom (Hir.Atom_var id, sp))
  | Lit (lit, sp) -> ([], Hir.Atom (Hir.Atom_lit (lit_of_ast lit), sp))
  | Annotated (inner, _, _) -> anf_expr ctx inner
  | Binop (And, l, r, sp) ->
      (* l && r  ==>  if l then r else false *)
      let bl, al = anf_atom ctx l in
      let br, er = anf_expr ctx r in
      let false_e = Hir.Atom (Hir.Atom_lit (Hir.Lit_bool false), sp) in
      (bl, wrap_lets br (Hir.If (al, er, false_e, sp)))
  | Binop (Or, l, r, sp) ->
      let bl, al = anf_atom ctx l in
      let br, er = anf_expr ctx r in
      let true_e = Hir.Atom (Hir.Atom_lit (Hir.Lit_bool true), sp) in
      (bl, wrap_lets br (Hir.If (al, true_e, er, sp)))
  | Binop (Pipe, l, r, sp) ->
      (* x |> f  ==>  f x *)
      anf_expr ctx (App (r, l, sp))
  | Binop (Cons, l, r, sp) ->
      let bl, al = anf_atom ctx l in
      let br, ar = anf_atom ctx r in
      let cons = lookup_ctor ctx Ident.Predef.cons in
      let cons =
        if cons.Hir.ctor_arity = 0 then
          { cons with Hir.ctor_arity = 2; ctor_tag = 1 }
        else cons
      in
      (bl @ br, Hir.Ctor (cons, [ al; ar ], sp))
  | Binop (Append, l, r, sp) ->
      let bl, al = anf_atom ctx l in
      let br, ar = anf_atom ctx r in
      (bl @ br, Hir.Prim (Hir.Prim_string_concat, [ al; ar ], sp))
  | Binop (op, l, r, sp) -> (
      match binop_to_prim op with
      | Some prim ->
          let bl, al = anf_atom ctx l in
          let br, ar = anf_atom ctx r in
          (bl @ br, Hir.Prim (prim, [ al; ar ], sp))
      | None ->
          (* Should be handled above; treat as error-ish prim abort. *)
          ([], Hir.Prim (Hir.Prim_abort, [], sp)))
  | Unop (op, e, sp) ->
      let b, a = anf_atom ctx e in
      (b, Hir.Prim (unop_to_prim op, [ a ], sp))
  | App (f, x, sp) ->
      (* Flatten left-nested applications into multi-arg form. *)
      let rec gather acc = function
        | App (f', x', _) -> gather (x' :: acc) f'
        | f0 -> (f0, acc)
      in
      let f0, args = gather [ x ] f in
      let bf, af = anf_atom ctx f0 in
      let binds_args, atoms =
        List.fold_left
          (fun (bs, ats) arg ->
            let b, a = anf_atom ctx arg in
            (bs @ b, ats @ [ a ]))
          ([], []) args
      in
      (bf @ binds_args, Hir.App (af, atoms, sp))
  | Fun (params, body, sp) ->
      let bb, eb = anf_expr ctx body in
      ([], Hir.Fun (params, wrap_lets bb eb, sp))
  | Let (name, params, rhs, body, sp) ->
      let br, er =
        match params with
        | [] -> anf_expr ctx rhs
        | ps ->
            let bb, eb = anf_expr ctx rhs in
            ([], Hir.Fun (ps, wrap_lets bb eb, expr_span rhs))
      in
      let bb, eb = anf_expr ctx body in
      (br, Hir.Let (name, er, wrap_lets bb eb, sp))
  | LetRec (bindings, body, sp) ->
      let hbinds =
        List.map
          (fun (name, params, rhs) ->
            let br, er =
              match params with
              | [] -> anf_expr ctx rhs
              | ps ->
                  let bb, eb = anf_expr ctx rhs in
                  ([], Hir.Fun (ps, wrap_lets bb eb, expr_span rhs))
            in
            (name, wrap_lets br er))
          bindings
      in
      let bb, eb = anf_expr ctx body in
      ([], Hir.Let_rec (hbinds, wrap_lets bb eb, sp))
  | If (c, t, e, sp) ->
      let bc, ac = anf_atom ctx c in
      let bt, et = anf_expr ctx t in
      let be, ee = anf_expr ctx e in
      (bc, Hir.If (ac, wrap_lets bt et, wrap_lets be ee, sp))
  | Match (scrut, arms, sp) ->
      let bs, ascrut = anf_atom ctx scrut in
      let harms =
        List.map
          (fun (pat, body) ->
            let bp, eb = anf_expr ctx body in
            {
              Hir.arm_pat = desugar_pat ctx pat;
              arm_guard = None;
              arm_body = wrap_lets bp eb;
              arm_span = Span.merge (pattern_span pat) (expr_span body);
            })
          arms
      in
      (bs, Hir.Match (ascrut, harms, sp))
  | Tuple (es, sp) ->
      let binds, atoms =
        List.fold_left
          (fun (bs, ats) e ->
            let b, a = anf_atom ctx e in
            (bs @ b, ats @ [ a ]))
          ([], []) es
      in
      (binds, Hir.Tuple (atoms, sp))
  | Record (fields, sp) ->
      (* Records lower to tagged tuples ordered by field declaration order. *)
      let binds, atoms =
        List.fold_left
          (fun (bs, ats) (_name, e) ->
            let b, a = anf_atom ctx e in
            (bs @ b, ats @ [ a ]))
          ([], []) fields
      in
      (binds, Hir.Tuple (atoms, sp))
  | Field (e, _name, sp) ->
      (* Field projection by name is resolved later; emit project 0 as placeholder
         when we lack layout info — prefer Project after type layout. *)
      let b, a = anf_atom ctx e in
      (b, Hir.Project (a, 0, sp))
  | Constructor (name, args, sp) ->
      let info =
        let base = lookup_ctor ctx name in
        if base.Hir.ctor_arity = 0 && args <> [] then
          { base with Hir.ctor_arity = List.length args }
        else base
      in
      let binds, atoms =
        List.fold_left
          (fun (bs, ats) e ->
            let b, a = anf_atom ctx e in
            (bs @ b, ats @ [ a ]))
          ([], []) args
      in
      (binds, Hir.Ctor (info, atoms, sp))
  | Seq (a, b, sp) ->
      let ba, ea = anf_expr ctx a in
      let bb, eb = anf_expr ctx b in
      (ba, Hir.Seq (ea, wrap_lets bb eb, sp))

and desugar_pat ctx (p : pattern) : Hir.pat =
  match p with
  | PWild sp -> Hir.Pat_any sp
  | PVar (id, sp) -> Hir.Pat_var (id, sp)
  | PLit (lit, sp) -> Hir.Pat_lit (lit_of_ast lit, sp)
  | PTuple (ps, sp) -> Hir.Pat_tuple (List.map (desugar_pat ctx) ps, sp)
  | PRecord (fields, sp) ->
      Hir.Pat_tuple (List.map (fun (_, p) -> desugar_pat ctx p) fields, sp)
  | PConstructor (name, args, sp) ->
      let info =
        let base = lookup_ctor ctx name in
        if base.Hir.ctor_arity = 0 && args <> [] then
          { base with Hir.ctor_arity = List.length args }
        else base
      in
      Hir.Pat_ctor (info, List.map (desugar_pat ctx) args, sp)
  | POr (a, b, sp) -> Hir.Pat_or (desugar_pat ctx a, desugar_pat ctx b, sp)
  | PAs (p, id, sp) -> Hir.Pat_as (desugar_pat ctx p, id, sp)
  | PAnnotated (p, _, _) -> desugar_pat ctx p

let desugar_expr ctx e =
  let binds, he = anf_expr ctx e in
  wrap_lets binds he

let desugar_toplevel ctx = function
  | TypeDecl td ->
      register_variant ctx td;
      (match td with
      | Alias { name; params; span; _ } ->
          Some
            (Hir.Toplevel_type
               {
                 name;
                 params = List.map Ident.of_string params;
                 ctors = [];
                 span;
               })
      | Variant { name; params; constructors; span } ->
          let ctors =
            List.mapi
              (fun tag (c : constructor_decl) ->
                Hir.mk_ctor ~ty:(Some name) c.name tag (List.length c.args))
              constructors
          in
          Some
            (Hir.Toplevel_type
               {
                 name;
                 params = List.map Ident.of_string params;
                 ctors;
                 span;
               }))
  | Value { name; params; body; recursive; span } ->
      let body = desugar_expr ctx body in
      Some (Hir.Toplevel_fun { name; params; body; recursive; span })
  | ValueRec { bindings; span } ->
      let hbinds =
        List.map
          (fun (name, params, body, _sp) ->
            let body = desugar_expr ctx body in
            match params with
            | [] -> (name, body)
            | ps -> (name, Hir.Fun (ps, body, Hir.expr_span body)))
          bindings
      in
      (* Encode mutually recursive values as a single let-rec thunk
         wrapped in a dummy main continuation — expose as separate funs. *)
      let items =
        List.map2
          (fun (name, params, _body, sp) (_n, he) ->
            match he with
            | Hir.Fun (ps, body, _) ->
                Hir.Toplevel_fun
                  { name; params = (if params = [] then ps else params); body; recursive = true; span = sp }
            | body ->
                Hir.Toplevel_fun
                  { name; params; body; recursive = true; span = sp })
          bindings hbinds
      in
      (* Return first; caller flattens via desugar_program. *)
      ignore span;
      (* We need multi-item; handled in desugar_program. *)
      Some (List.hd items)
  | Expr e ->
      let body = desugar_expr ctx e in
      Some
        (Hir.Toplevel_val
           { name = Ident.Predef.main; body; span = expr_span e })

let desugar_program (prog : program) : Hir.program =
  let ctx = make_ctx () in
  (* Ensure Nil/Cons exist with conventional tags. *)
  Hashtbl.replace ctx.ctors Ident.Predef.nil
    (Hir.mk_ctor ~ty:(Some Ident.Predef.list) Ident.Predef.nil 0 0);
  Hashtbl.replace ctx.ctors Ident.Predef.cons
    (Hir.mk_ctor ~ty:(Some Ident.Predef.list) Ident.Predef.cons 1 2);
  let items = ref [] in
  List.iter
    (fun item ->
      match item with
      | ValueRec { bindings; span } ->
          List.iter
            (fun (name, params, body, sp) ->
              let body = desugar_expr ctx body in
              let body =
                match params with
                | [] -> body
                | ps -> Hir.Fun (ps, body, Hir.expr_span body)
              in
              items :=
                Hir.Toplevel_fun
                  { name; params; body; recursive = true; span = sp }
                :: !items)
            bindings;
          ignore span
      | other -> (
          match desugar_toplevel ctx other with
          | None -> ()
          | Some it -> items := it :: !items))
    prog.items;
  Hir.program_of_items ~span:prog.span (List.rev !items)

(** Convenience: desugar a single expression in an empty constructor env. *)
let desugar_expr_standalone e =
  let ctx = make_ctx () in
  desugar_expr ctx e
