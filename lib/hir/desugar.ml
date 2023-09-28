(** Desugaring from a simplified surface AST into [Hir].

    The full Glyph parser/AST may not yet exist in this tree, so this module
    defines a self-contained surface language ([Desugar.Surface]) that is
    intentionally close to what a future [Ast] module will produce. Pattern
    matching is preserved as [Hir.Match] for [Pattern_compile] to handle. *)

(* -------------------------------------------------------------------------- *)
(* Surface language                                                           *)
(* -------------------------------------------------------------------------- *)

module Surface = struct
  type lit = Hir.lit

  type binop =
    | Add | Sub | Mul | Div | Mod
    | Eq | Ne | Lt | Le | Gt | Ge
    | And | Or
    | FAdd | FSub | FMul | FDiv
    | Concat

  type unop = Neg | Not | Box | Unbox

  type pat =
    | PAny of Span.t
    | PVar of Ident.t * Span.t
    | PLit of lit * Span.t
    | PCtor of Ident.t * pat list * Span.t
    | POr of pat * pat * Span.t
    | PAs of pat * Ident.t * Span.t
    | PTuple of pat list * Span.t

  type expr =
    | ELit of lit * Span.t
    | EVar of Ident.t * Span.t
    | EApp of expr * expr list * Span.t
    | EBinop of binop * expr * expr * Span.t
    | EUnop of unop * expr * Span.t
    | EIf of expr * expr * expr * Span.t
    | ELet of Ident.t * expr * expr * Span.t
    | ELetRec of (Ident.t * Ident.t list * expr) list * expr * Span.t
    | EFun of Ident.t list * expr * Span.t
    | EMatch of expr * (pat * expr option * expr * Span.t) list * Span.t
    | ECtor of Ident.t * expr list * Span.t
    | ETuple of expr list * Span.t
    | ESeq of expr * expr * Span.t
    | EWhile of expr * expr * Span.t
    | EFor of Ident.t * expr * expr * expr * Span.t
    | ERecord of (Ident.t * expr) list * Span.t
    | EField of expr * Ident.t * Span.t
    | EAssign of expr * expr * Span.t
    | EList of expr list * Span.t
    | ECons of expr * expr * Span.t

  type type_decl = {
    name : Ident.t;
    params : Ident.t list;
    ctors : (Ident.t * int) list;
    span : Span.t;
  }

  type item =
    | IFun of {
        name : Ident.t;
        params : Ident.t list;
        body : expr;
        recursive : bool;
        span : Span.t;
      }
    | IVal of Ident.t * expr * Span.t
    | IType of type_decl
    | IExtern of Ident.t * int * Span.t

  type program = {
    items : item list;
    span : Span.t;
  }
end

(* -------------------------------------------------------------------------- *)
(* Constructor environment                                                    *)
(* -------------------------------------------------------------------------- *)

type ctor_env = Hir.ctor_info Ident.Map.t

type env = {
  ctors : ctor_env;
  diagnostics : Diagnostic.Bag.t;
}

let empty_env ?(diagnostics = Diagnostic.Bag.create ()) () =
  { ctors = Ident.Map.empty; diagnostics }

let register_type env (decl : Surface.type_decl) =
  let ctors =
    List.mapi
      (fun tag (name, arity) ->
        Hir.mk_ctor ~ty:(Some decl.name) name tag arity)
      decl.ctors
  in
  let ctors_map =
    List.fold_left
      (fun m c -> Ident.Map.add c.Hir.ctor_name c m)
      env.ctors ctors
  in
  ({ env with ctors = ctors_map }, ctors)

let lookup_ctor env name span =
  match Ident.Map.find_opt name env.ctors with
  | Some info -> info
  | None ->
      Diagnostic.Bag.error env.diagnostics span
        (Printf.sprintf "unknown constructor %s" (Ident.to_string name));
      Hir.mk_ctor name 0 0

(* -------------------------------------------------------------------------- *)
(* ANF helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let fresh_tmp prefix = Ident.fresh prefix

(** Bind a complex expression to a fresh temporary, returning an atom and a
    continuation builder that wraps the eventual body in the binding. *)
type 'a anf_cont = Hir.expr -> Hir.expr

let identity_cont : Hir.expr -> Hir.expr = Fun.id

let bind_atom (e : Hir.expr) (k : Hir.atom -> Hir.expr) : Hir.expr =
  match e with
  | Hir.Atom (a, _) -> k a
  | _ ->
      let x = fresh_tmp "t" in
      let sp = Hir.expr_span e in
      Hir.Let (x, e, k (Hir.Atom_var x), sp)

let bind_atoms (es : Hir.expr list) (k : Hir.atom list -> Hir.expr) : Hir.expr =
  let rec go acc = function
    | [] -> k (List.rev acc)
    | e :: rest -> bind_atom e (fun a -> go (a :: acc) rest)
  in
  go [] es

(* -------------------------------------------------------------------------- *)
(* Operators                                                                  *)
(* -------------------------------------------------------------------------- *)

let binop_to_prim : Surface.binop -> Hir.primop = function
  | Add -> Prim_add
  | Sub -> Prim_sub
  | Mul -> Prim_mul
  | Div -> Prim_div
  | Mod -> Prim_mod
  | Eq -> Prim_eq
  | Ne -> Prim_ne
  | Lt -> Prim_lt
  | Le -> Prim_le
  | Gt -> Prim_gt
  | Ge -> Prim_ge
  | And -> Prim_and
  | Or -> Prim_or
  | FAdd -> Prim_fadd
  | FSub -> Prim_fsub
  | FMul -> Prim_fmul
  | FDiv -> Prim_fdiv
  | Concat -> Prim_string_concat

let unop_to_prim : Surface.unop -> Hir.primop = function
  | Neg -> Prim_neg
  | Not -> Prim_not
  | Box -> Prim_box
  | Unbox -> Prim_unbox

(* -------------------------------------------------------------------------- *)
(* Pattern desugaring (structure only; compilation deferred)                  *)
(* -------------------------------------------------------------------------- *)

let rec desugar_pat env (p : Surface.pat) : Hir.pat =
  match p with
  | PAny sp -> Hir.Pat_any sp
  | PVar (x, sp) -> Hir.Pat_var (x, sp)
  | PLit (l, sp) -> Hir.Pat_lit (l, sp)
  | PCtor (name, ps, sp) ->
      let info = lookup_ctor env name sp in
      let ps' = List.map (desugar_pat env) ps in
      if List.length ps' <> info.ctor_arity && info.ctor_arity > 0 then
        Diagnostic.Bag.warning env.diagnostics sp
          (Printf.sprintf "constructor %s expects %d arguments, got %d"
             (Ident.to_string name) info.ctor_arity (List.length ps'));
      Hir.Pat_ctor (info, ps', sp)
  | POr (p1, p2, sp) -> Hir.Pat_or (desugar_pat env p1, desugar_pat env p2, sp)
  | PAs (p, x, sp) -> Hir.Pat_as (desugar_pat env p, x, sp)
  | PTuple (ps, sp) -> Hir.Pat_tuple (List.map (desugar_pat env) ps, sp)

(* -------------------------------------------------------------------------- *)
(* Built-in list constructors                                                 *)
(* -------------------------------------------------------------------------- *)

let list_nil_info =
  Hir.mk_ctor ~ty:(Some (Ident.of_string "List")) (Ident.of_string "Nil") 0 0

let list_cons_info =
  Hir.mk_ctor ~ty:(Some (Ident.of_string "List")) (Ident.of_string "Cons") 1 2

let ensure_list_ctors env =
  let ctors =
    env.ctors
    |> Ident.Map.add list_nil_info.ctor_name list_nil_info
    |> Ident.Map.add list_cons_info.ctor_name list_cons_info
  in
  { env with ctors }

(* -------------------------------------------------------------------------- *)
(* Expression desugaring                                                      *)
(* -------------------------------------------------------------------------- *)

let rec desugar_expr env (e : Surface.expr) : Hir.expr =
  match e with
  | ELit (l, sp) -> Hir.Atom (Hir.Atom_lit l, sp)
  | EVar (x, sp) -> Hir.Atom (Hir.Atom_var x, sp)

  | EApp (f, args, sp) ->
      let f' = desugar_expr env f in
      let args' = List.map (desugar_expr env) args in
      bind_atom f' (fun fa ->
          bind_atoms args' (fun aas -> Hir.App (fa, aas, sp)))

  | EBinop (And, lhs, rhs, sp) ->
      (* Short-circuit: if lhs then rhs else false *)
      let lhs' = desugar_expr env lhs in
      bind_atom lhs' (fun a ->
          Hir.If
            ( a,
              desugar_expr env rhs,
              Hir.Atom (Hir.Atom_lit (Hir.Lit_bool false), sp),
              sp ))

  | EBinop (Or, lhs, rhs, sp) ->
      let lhs' = desugar_expr env lhs in
      bind_atom lhs' (fun a ->
          Hir.If
            ( a,
              Hir.Atom (Hir.Atom_lit (Hir.Lit_bool true), sp),
              desugar_expr env rhs,
              sp ))

  | EBinop (op, lhs, rhs, sp) ->
      let lhs' = desugar_expr env lhs in
      let rhs' = desugar_expr env rhs in
      bind_atom lhs' (fun a ->
          bind_atom rhs' (fun b -> Hir.Prim (binop_to_prim op, [ a; b ], sp)))

  | EUnop (op, arg, sp) ->
      let arg' = desugar_expr env arg in
      bind_atom arg' (fun a -> Hir.Prim (unop_to_prim op, [ a ], sp))

  | EIf (cond, thn, els, sp) ->
      let cond' = desugar_expr env cond in
      bind_atom cond' (fun c ->
          Hir.If (c, desugar_expr env thn, desugar_expr env els, sp))

  | ELet (x, rhs, body, sp) ->
      Hir.Let (x, desugar_expr env rhs, desugar_expr env body, sp)

  | ELetRec (bindings, body, sp) ->
      let bs =
        List.map
          (fun (name, params, rhs) ->
            let rhs' = desugar_expr env rhs in
            let fun_e =
              match params with
              | [] -> rhs'
              | _ -> Hir.Fun (params, rhs', sp)
            in
            (name, fun_e))
          bindings
      in
      Hir.Let_rec (bs, desugar_expr env body, sp)

  | EFun (params, body, sp) ->
      Hir.Fun (params, desugar_expr env body, sp)

  | EMatch (scrut, arms, sp) ->
      let scrut' = desugar_expr env scrut in
      bind_atom scrut' (fun s ->
          let arms' =
            List.map
              (fun (pat, guard, body, arm_sp) ->
                {
                  Hir.arm_pat = desugar_pat env pat;
                  arm_guard = Option.map (desugar_expr env) guard;
                  arm_body = desugar_expr env body;
                  arm_span = arm_sp;
                })
              arms
          in
          Hir.Match (s, arms', sp))

  | ECtor (name, args, sp) ->
      let info = lookup_ctor env name sp in
      let args' = List.map (desugar_expr env) args in
      bind_atoms args' (fun aas -> Hir.Ctor (info, aas, sp))

  | ETuple (es, sp) ->
      let es' = List.map (desugar_expr env) es in
      bind_atoms es' (fun aas -> Hir.Tuple (aas, sp))

  | ESeq (a, b, sp) ->
      Hir.Seq (desugar_expr env a, desugar_expr env b, sp)

  | EWhile (cond, body, sp) ->
      (* Desugar to a recursive local function:
           let rec loop = fun () -> if cond then (body; loop ()) else () in
           loop () *)
      let loop = fresh_tmp "while_loop" in
      let unit_param = fresh_tmp "_" in
      let cond' = desugar_expr env cond in
      let body' = desugar_expr env body in
      let call_loop =
        Hir.App
          ( Hir.Atom_var loop,
            [ Hir.Atom_lit Hir.Lit_unit ],
            sp )
      in
      let loop_body =
        bind_atom cond' (fun c ->
            Hir.If
              ( c,
                Hir.Seq (body', call_loop, sp),
                Hir.Atom (Hir.Atom_lit Hir.Lit_unit, sp),
                sp ))
      in
      Hir.Let_rec
        ( [ (loop, Hir.Fun ([ unit_param ], loop_body, sp)) ],
          call_loop,
          sp )

  | EFor (x, lo, hi, body, sp) ->
      (* for x = lo to hi do body
         =>
         let rec for_loop i =
           if i <= hi then (let x = i in body; for_loop (i+1)) else ()
         in for_loop lo *)
      let loop = fresh_tmp "for_loop" in
      let i = fresh_tmp "i" in
      let lo' = desugar_expr env lo in
      let hi' = desugar_expr env hi in
      let body' = desugar_expr env body in
      bind_atom lo' (fun lo_a ->
          bind_atom hi' (fun hi_a ->
              let incr =
                Hir.Prim
                  ( Prim_add,
                    [ Hir.Atom_var i; Hir.Atom_lit (Hir.Lit_int 1) ],
                    sp )
              in
              let next = fresh_tmp "next" in
              let call_next =
                Hir.Let
                  ( next,
                    incr,
                    Hir.App (Hir.Atom_var loop, [ Hir.Atom_var next ], sp),
                    sp )
              in
              let iter_body =
                Hir.Let
                  ( x,
                    Hir.Atom (Hir.Atom_var i, sp),
                    Hir.Seq (body', call_next, sp),
                    sp )
              in
              let cond =
                Hir.Prim (Prim_le, [ Hir.Atom_var i; hi_a ], sp)
              in
              let ctmp = fresh_tmp "c" in
              let loop_body =
                Hir.Let
                  ( ctmp,
                    cond,
                    Hir.If
                      ( Hir.Atom_var ctmp,
                        iter_body,
                        Hir.Atom (Hir.Atom_lit Hir.Lit_unit, sp),
                        sp ),
                    sp )
              in
              Hir.Let_rec
                ( [ (loop, Hir.Fun ([ i ], loop_body, sp)) ],
                  Hir.App (Hir.Atom_var loop, [ lo_a ], sp),
                  sp )))

  | ERecord (fields, sp) ->
      (* Records desugar to ordered tuples; field names are erased here.
         A future typed lowering would keep offsets from the type env. *)
      let sorted =
        List.sort
          (fun (a, _) (b, _) -> String.compare (Ident.name a) (Ident.name b))
          fields
      in
      let es = List.map (fun (_, e) -> desugar_expr env e) sorted in
      bind_atoms es (fun aas -> Hir.Tuple (aas, sp))

  | EField (obj, field, sp) ->
      (* Without type info we cannot resolve field offsets; emit project 0
         and warn. Typed pipelines should rewrite this earlier. *)
      Diagnostic.Bag.warning env.diagnostics sp
        (Printf.sprintf
           "field access %s lowered without type info; using offset 0"
           (Ident.to_string field));
      let obj' = desugar_expr env obj in
      bind_atom obj' (fun a -> Hir.Project (a, 0, sp))

  | EAssign (lhs, rhs, sp) ->
      (* Mutable assignment is not yet first-class in HIR; lower to seq of
         evaluating both sides and returning unit. *)
      Diagnostic.Bag.warning env.diagnostics sp
        "assignment desugared to sequencing (no mutable cells in HIR yet)";
      Hir.Seq
        ( desugar_expr env lhs,
          Hir.Seq
            ( desugar_expr env rhs,
              Hir.Atom (Hir.Atom_lit Hir.Lit_unit, sp),
              sp ),
          sp )

  | EList (es, sp) ->
      let env = ensure_list_ctors env in
      let rec build = function
        | [] -> Hir.Ctor (list_nil_info, [], sp)
        | hd :: tl ->
            let hd' = desugar_expr env hd in
            let tl' = build tl in
            bind_atom hd' (fun h ->
                bind_atom tl' (fun t -> Hir.Ctor (list_cons_info, [ h; t ], sp)))
      in
      build es

  | ECons (hd, tl, sp) ->
      let env = ensure_list_ctors env in
      let hd' = desugar_expr env hd in
      let tl' = desugar_expr env tl in
      bind_atom hd' (fun h ->
          bind_atom tl' (fun t -> Hir.Ctor (list_cons_info, [ h; t ], sp)))

(* -------------------------------------------------------------------------- *)
(* Toplevel / program                                                         *)
(* -------------------------------------------------------------------------- *)

let desugar_item env (item : Surface.item) :
    env * Hir.toplevel option =
  match item with
  | IFun { name; params; body; recursive; span } ->
      let body' = desugar_expr env body in
      ( env,
        Some
          (Hir.Toplevel_fun
             { name; params; body = body'; recursive; span }) )
  | IVal (name, body, span) ->
      ( env,
        Some (Hir.Toplevel_val { name; body = desugar_expr env body; span }) )
  | IType decl ->
      let env, ctors = register_type env decl in
      ( env,
        Some
          (Hir.Toplevel_type
             {
               name = decl.name;
               params = decl.params;
               ctors;
               span = decl.span;
             }) )
  | IExtern (name, arity, span) ->
      (env, Some (Hir.Toplevel_extern { name; arity; span }))

let desugar_program ?(diagnostics = Diagnostic.Bag.create ())
    (prog : Surface.program) : Hir.program * Diagnostic.Bag.t =
  let env = empty_env ~diagnostics () |> ensure_list_ctors in
  let env, items_rev =
    List.fold_left
      (fun (env, acc) item ->
        let env, opt = desugar_item env item in
        match opt with
        | None -> (env, acc)
        | Some t -> (env, t :: acc))
      (env, []) prog.items
  in
  ( Hir.program_of_items ~span:prog.span (List.rev items_rev),
    env.diagnostics )

(** Convenience: desugar a single expression (for tests / REPL). *)
let desugar_expr_only ?(diagnostics = Diagnostic.Bag.create ()) e =
  let env = empty_env ~diagnostics () |> ensure_list_ctors in
  (desugar_expr env e, env.diagnostics)

(* -------------------------------------------------------------------------- *)
(* Hir builder API for hand-written IR                                        *)
(* -------------------------------------------------------------------------- *)

module Builder = struct
  let unit ?(span = Span.dummy) () = Hir.Atom (Hir.Atom_lit Hir.Lit_unit, span)
  let bool ?(span = Span.dummy) b =
    Hir.Atom (Hir.Atom_lit (Hir.Lit_bool b), span)
  let int ?(span = Span.dummy) n =
    Hir.Atom (Hir.Atom_lit (Hir.Lit_int n), span)
  let var ?(span = Span.dummy) x = Hir.Atom (Hir.Atom_var x, span)

  let let_ ?(span = Span.dummy) x rhs body = Hir.Let (x, rhs, body, span)
  let fun_ ?(span = Span.dummy) params body = Hir.Fun (params, body, span)
  let if_ ?(span = Span.dummy) c t e = Hir.If (c, t, e, span)
  let app ?(span = Span.dummy) f args = Hir.App (f, args, span)
  let prim ?(span = Span.dummy) op args = Hir.Prim (op, args, span)
  let ctor ?(span = Span.dummy) info args = Hir.Ctor (info, args, span)
  let match_ ?(span = Span.dummy) scrut arms = Hir.Match (scrut, arms, span)

  let arm ?(span = Span.dummy) ?guard pat body =
    { Hir.arm_pat = pat; arm_guard = guard; arm_body = body; arm_span = span }

  let toplevel_fun ?(recursive = false) ?(span = Span.dummy) name params body =
    Hir.Toplevel_fun { name; params; body; recursive; span }

  let program ?(span = Span.dummy) items = Hir.program_of_items ~span items
end

(* -------------------------------------------------------------------------- *)
(* Optional Ast adapter                                                       *)
(* -------------------------------------------------------------------------- *)

(** If a future [Ast] module exposes a compatible shape, call
    [of_ast_program] after converting nodes to [Surface]. Direct Ast
    dependency is intentionally avoided so this file compiles standalone. *)
let of_surface_program = desugar_program
let of_surface_expr = desugar_expr_only
