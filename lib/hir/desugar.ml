(** Desugar surface [Ast] into ANF-flavoured [Hir].

    Responsibilities:
    - lower [&&] / [||] to [If]
    - lower [|>] to application
    - flatten multi-arg [fun]/[let] into explicit parameter lists
    - name intermediate results (ANF) so applications only take atoms
    - resolve constructor tags and arities from type declarations
    - leave [Match] for [Pattern] to compile into decision trees
    - explicitly reject deferred record constructs with clear diagnostic *)

open Ast

type ctor_env = (Ident.t, Hir.ctor_info) Hashtbl.t

type ctx = {
  ctors : ctor_env;
  mutable temp : int;
}

let make_ctx () =
  let ctors = Hashtbl.create 64 in
  (* Register standard built-in ADT constructors with stable tags *)
  let list_id = Ident.of_string "List" in
  Hashtbl.replace ctors (Ident.of_string "Nil")
    (Hir.mk_ctor ~ty:(Some list_id) (Ident.of_string "Nil") 0 0);
  Hashtbl.replace ctors (Ident.of_string "Cons")
    (Hir.mk_ctor ~ty:(Some list_id) (Ident.of_string "Cons") 1 2);
  let opt_id = Ident.of_string "Option" in
  Hashtbl.replace ctors (Ident.of_string "None")
    (Hir.mk_ctor ~ty:(Some opt_id) (Ident.of_string "None") 0 0);
  Hashtbl.replace ctors (Ident.of_string "Some")
    (Hir.mk_ctor ~ty:(Some opt_id) (Ident.of_string "Some") 1 1);
  let res_id = Ident.of_string "Result" in
  Hashtbl.replace ctors (Ident.of_string "Ok")
    (Hir.mk_ctor ~ty:(Some res_id) (Ident.of_string "Ok") 0 1);
  Hashtbl.replace ctors (Ident.of_string "Err")
    (Hir.mk_ctor ~ty:(Some res_id) (Ident.of_string "Err") 1 1);
  let bool_id = Ident.of_string "Bool" in
  Hashtbl.replace ctors (Ident.of_string "false")
    (Hir.mk_ctor ~ty:(Some bool_id) (Ident.of_string "false") 0 0);
  Hashtbl.replace ctors (Ident.of_string "true")
    (Hir.mk_ctor ~ty:(Some bool_id) (Ident.of_string "true") 1 0);
  { ctors; temp = 0 }

let fresh_temp ctx ?(prefix = "t") () =
  ctx.temp <- ctx.temp + 1;
  Ident.fresh (Printf.sprintf "%s%d" prefix ctx.temp)

let register_variant ctx (td : Ast.type_def) =
  List.iteri
    (fun tag (c : Ast.ctor_decl) ->
      let info =
        Hir.mk_ctor ~ty:(Some td.td_name) c.ctor_name tag
          (List.length c.ctor_args)
      in
      Hashtbl.replace ctx.ctors c.ctor_name info)
    td.td_ctors

let lookup_ctor ctx name =
  match Hashtbl.find_opt ctx.ctors name with
  | Some info -> info
  | None ->
      let s = Ident.name name in
      if s = "Nil" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "List")) name 0 0
      else if s = "Cons" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "List")) name 1 2
      else if s = "None" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "Option")) name 0 0
      else if s = "Some" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "Option")) name 1 1
      else if s = "Ok" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "Result")) name 0 1
      else if s = "Err" then
        Hir.mk_ctor ~ty:(Some (Ident.of_string "Result")) name 1 1
      else
        failwith (Printf.sprintf "unknown constructor: %s" s)

let lit_of_ast = function
  | Ast.Lit_unit -> Hir.Lit_unit
  | Ast.Lit_bool b -> Hir.Lit_bool b
  | Ast.Lit_int n -> Hir.Lit_int (Int64.to_int n)
  | Ast.Lit_float f -> Hir.Lit_float f
  | Ast.Lit_char c -> Hir.Lit_char c
  | Ast.Lit_string s -> Hir.Lit_string s

let binop_to_prim = function
  | Token.Op_add -> Some Hir.Prim_add
  | Token.Op_sub -> Some Hir.Prim_sub
  | Token.Op_mul -> Some Hir.Prim_mul
  | Token.Op_div -> Some Hir.Prim_div
  | Token.Op_mod -> Some Hir.Prim_mod
  | Token.Op_eq -> Some Hir.Prim_eq
  | Token.Op_neq -> Some Hir.Prim_ne
  | Token.Op_lt -> Some Hir.Prim_lt
  | Token.Op_le -> Some Hir.Prim_le
  | Token.Op_gt -> Some Hir.Prim_gt
  | Token.Op_ge -> Some Hir.Prim_ge
  | Token.Op_and | Token.Op_or | Token.Op_cons | Token.Op_pipe -> None

let unop_to_prim = function
  | Token.Op_neg -> Hir.Prim_neg
  | Token.Op_not -> Hir.Prim_not

(** Wrap [body] under successive [Let] bindings. Bindings are applied
    innermost-first (list head is outermost). *)
let wrap_lets binds body =
  List.fold_right
    (fun (x, rhs, sp) acc -> Hir.Let (x, rhs, acc, sp))
    binds body

(** Convert an expression to ANF, returning (bindings, atom). *)
let rec anf_atom ctx (e : Ast.expr) :
    (Ident.t * Hir.expr * Span.t) list * Hir.atom =
  match e.expr_desc with
  | Ast.Expr_var id -> ([], Hir.Atom_var id)
  | Ast.Expr_lit lit -> ([], Hir.Atom_lit (lit_of_ast lit))
  | Ast.Expr_annotate (inner, _) -> anf_atom ctx inner
  | _ ->
      let sp = e.expr_span in
      let binds, hexpr = anf_expr ctx e in
      let tmp = fresh_temp ctx () in
      (binds @ [ (tmp, hexpr, sp) ], Hir.Atom_var tmp)

and anf_expr ctx (e : Ast.expr) :
    (Ident.t * Hir.expr * Span.t) list * Hir.expr =
  let sp = e.expr_span in
  match e.expr_desc with
  | Ast.Expr_var id -> ([], Hir.Atom (Hir.Atom_var id, sp))
  | Ast.Expr_lit lit -> ([], Hir.Atom (Hir.Atom_lit (lit_of_ast lit), sp))
  | Ast.Expr_annotate (inner, _) -> anf_expr ctx inner
  | Ast.Expr_ctor name ->
      let info = lookup_ctor ctx name in
      ([], Hir.Ctor (info, [], sp))
  | Ast.Expr_app ({ expr_desc = Ast.Expr_ctor name; _ }, args) ->
      let info = lookup_ctor ctx name in
      let binds_args, atoms =
        List.fold_left
          (fun (bs, ats) arg ->
            let b, a = anf_atom ctx arg in
            (bs @ b, ats @ [ a ]))
          ([], []) args
      in
      (binds_args, Hir.Ctor (info, atoms, sp))
  | Ast.Expr_app (f, args) ->
      let bf, af = anf_atom ctx f in
      let binds_args, atoms =
        List.fold_left
          (fun (bs, ats) arg ->
            let b, a = anf_atom ctx arg in
            (bs @ b, ats @ [ a ]))
          ([], []) args
      in
      (bf @ binds_args, Hir.App (af, atoms, sp))
  | Ast.Expr_lambda (params, body) ->
      let param_ids = List.map (fun p -> p.Ast.param_name) params in
      let bb, eb = anf_expr ctx body in
      ([], Hir.Fun (param_ids, wrap_lets bb eb, sp))
  | Ast.Expr_let (lb, body) ->
      let er =
        match lb.Ast.lb_params with
        | [] -> desugar_expr ctx lb.Ast.lb_body
        | ps ->
            let param_ids = List.map (fun p -> p.Ast.param_name) ps in
            let bb, eb = anf_expr ctx lb.Ast.lb_body in
            Hir.Fun (param_ids, wrap_lets bb eb, lb.Ast.lb_span)
      in
      let bb, eb = anf_expr ctx body in
      ([], Hir.Let (lb.Ast.lb_name, er, wrap_lets bb eb, sp))
  | Ast.Expr_let_rec (bindings, body) ->
      let hbinds =
        List.map
          (fun lb ->
            let er =
              match lb.Ast.lb_params with
              | [] -> desugar_expr ctx lb.Ast.lb_body
              | ps ->
                  let param_ids = List.map (fun p -> p.Ast.param_name) ps in
                  let bb, eb = anf_expr ctx lb.Ast.lb_body in
                  Hir.Fun (param_ids, wrap_lets bb eb, lb.Ast.lb_span)
            in
            (lb.Ast.lb_name, er))
          bindings
      in
      let bb, eb = anf_expr ctx body in
      ([], Hir.Let_rec (hbinds, wrap_lets bb eb, sp))
  | Ast.Expr_if (c, t, e) ->
      let bc, ac = anf_atom ctx c in
      let bt, et = anf_expr ctx t in
      let be, ee = anf_expr ctx e in
      (bc, Hir.If (ac, wrap_lets bt et, wrap_lets be ee, sp))
  | Ast.Expr_match (scrut, cases) ->
      let bs, ascrut = anf_atom ctx scrut in
      let arms =
        List.map
          (fun c ->
            let _bg, eg =
              match c.Ast.case_guard with
              | None -> ([], None)
              | Some g ->
                  let bg, eg = anf_expr ctx g in
                  (bg, Some (wrap_lets bg eg))
            in
            let bb, eb = anf_expr ctx c.Ast.case_body in
            {
              Hir.arm_pat = desugar_pat ctx c.Ast.case_pat;
              arm_guard = eg;
              arm_body = wrap_lets bb eb;
              arm_span = c.Ast.case_span;
            })
          cases
      in
      (bs, Hir.Match (ascrut, arms, sp))
  | Ast.Expr_bin (Token.Op_and, l, r) ->
      let bl, al = anf_atom ctx l in
      let br, er = anf_expr ctx r in
      let false_e = Hir.Atom (Hir.Atom_lit (Hir.Lit_bool false), sp) in
      (bl, wrap_lets br (Hir.If (al, er, false_e, sp)))
  | Ast.Expr_bin (Token.Op_or, l, r) ->
      let bl, al = anf_atom ctx l in
      let br, er = anf_expr ctx r in
      let true_e = Hir.Atom (Hir.Atom_lit (Hir.Lit_bool true), sp) in
      (bl, wrap_lets br (Hir.If (al, true_e, er, sp)))
  | Ast.Expr_pipe (l, r) | Ast.Expr_bin (Token.Op_pipe, l, r) ->
      anf_expr ctx
        { expr_desc = Ast.Expr_app (r, [ l ]); expr_span = sp }
  | Ast.Expr_bin (Token.Op_cons, l, r) ->
      let bl, al = anf_atom ctx l in
      let br, ar = anf_atom ctx r in
      let cons = lookup_ctor ctx (Ident.of_string "Cons") in
      (bl @ br, Hir.Ctor (cons, [ al; ar ], sp))
  | Ast.Expr_bin (op, l, r) -> (
      match binop_to_prim op with
      | Some prim ->
          let bl, al = anf_atom ctx l in
          let br, ar = anf_atom ctx r in
          (bl @ br, Hir.Prim (prim, [ al; ar ], sp))
      | None ->
          failwith
            (Printf.sprintf "unhandled binop at %s" (Span.to_string sp)))
  | Ast.Expr_un (op, e) ->
      let b, a = anf_atom ctx e in
      (b, Hir.Prim (unop_to_prim op, [ a ], sp))
  | Ast.Expr_tuple es ->
      let binds, atoms =
        List.fold_left
          (fun (bs, ats) e ->
            let b, a = anf_atom ctx e in
            (bs @ b, ats @ [ a ]))
          ([], []) es
      in
      (binds, Hir.Tuple (atoms, sp))
  | Ast.Expr_block es -> (
      match es with
      | [] -> ([], Hir.Atom (Hir.Atom_lit Hir.Lit_unit, sp))
      | [ e ] -> anf_expr ctx e
      | e :: rest ->
          let be, ee = anf_expr ctx e in
          let br, er =
            anf_expr ctx
              { expr_desc = Ast.Expr_block rest; expr_span = sp }
          in
          (be, Hir.Seq (ee, wrap_lets br er, sp)))
  | Ast.Expr_record _ | Ast.Expr_field _ ->
      failwith
        (Printf.sprintf
           "%s: records are deferred in Glyph v1; use tuples or algebraic \
            data types instead"
           (Span.to_string sp))

and desugar_pat ctx (p : Ast.pat) : Hir.pat =
  let sp = p.Ast.pat_span in
  match p.Ast.pat_desc with
  | Ast.Pat_wild -> Hir.Pat_any sp
  | Ast.Pat_var id -> Hir.Pat_var (id, sp)
  | Ast.Pat_lit lit -> Hir.Pat_lit (lit_of_ast lit, sp)
  | Ast.Pat_tuple ps -> Hir.Pat_tuple (List.map (desugar_pat ctx) ps, sp)
  | Ast.Pat_ctor (name, args) ->
      let info = lookup_ctor ctx name in
      Hir.Pat_ctor (info, List.map (desugar_pat ctx) args, sp)
  | Ast.Pat_or (a, b) -> Hir.Pat_or (desugar_pat ctx a, desugar_pat ctx b, sp)
  | Ast.Pat_as (inner, id) -> Hir.Pat_as (desugar_pat ctx inner, id, sp)
  | Ast.Pat_annotate (inner, _) -> desugar_pat ctx inner

and desugar_expr ctx e =
  let binds, he = anf_expr ctx e in
  wrap_lets binds he

let desugar_item ctx = function
  | Ast.Item_type td ->
      register_variant ctx td;
      let ctors =
        List.mapi
          (fun tag (c : Ast.ctor_decl) ->
            Hir.mk_ctor ~ty:(Some td.td_name) c.ctor_name tag
              (List.length c.ctor_args))
          td.td_ctors
      in
      Some
        (Hir.Toplevel_type
           {
             name = td.td_name;
             params = td.td_params;
             ctors;
             span = td.td_span;
           })
  | Ast.Item_fn lb | Ast.Item_let lb -> (
      let body = desugar_expr ctx lb.Ast.lb_body in
      match lb.Ast.lb_params with
      | [] -> (
          match body with
          | Hir.Fun (ps, b, _) ->
              Some
                (Hir.Toplevel_fun
                   {
                     name = lb.Ast.lb_name;
                     params = ps;
                     body = b;
                     recursive = lb.Ast.lb_rec;
                     span = lb.Ast.lb_span;
                   })
          | _ ->
              Some
                (Hir.Toplevel_val
                   {
                     name = lb.Ast.lb_name;
                     body;
                     span = lb.Ast.lb_span;
                   }))
      | ps ->
          let param_ids = List.map (fun p -> p.Ast.param_name) ps in
          Some
            (Hir.Toplevel_fun
               {
                 name = lb.Ast.lb_name;
                 params = param_ids;
                 body;
                 recursive = lb.Ast.lb_rec;
                 span = lb.Ast.lb_span;
               }))
  | Ast.Item_extern ext ->
      Some
        (Hir.Toplevel_extern
           {
             name = ext.ext_name;
             arity = List.length ext.ext_params;
             span = ext.ext_span;
           })

let desugar_program (prog : Ast.program) : Hir.program =
  let ctx = make_ctx () in
  let items = ref [] in
  List.iter
    (fun item ->
      match desugar_item ctx item with
      | None -> ()
      | Some it -> items := it :: !items)
    prog.items;
  Hir.program_of_items ~span:prog.span (List.rev !items)

let desugar_expr_standalone e =
  let ctx = make_ctx () in
  desugar_expr ctx e
