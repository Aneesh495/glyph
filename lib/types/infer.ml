(** Algorithm W / Hindley–Milner type inference for Glyph.

    Expected [Ast] surface (see [lib/syntax/ast.ml]):
    - [Ast.expr]: Var, Lit, App, Fun, Let, LetRec, If, Match, Tuple,
      Record, Field, Constructor, Binop, Unop, Seq, Annotated
    - [Ast.pattern]: PWild, PVar, PLit, PTuple, PRecord, PConstructor,
      POr, PAs, PAnnotated
    - [Ast.type_expr], [Ast.type_decl], [Ast.toplevel], [Ast.program]

    Public entry point:
    {[
      Infer.infer_program : ?env:Env.t -> Ast.program ->
        (Infer.result, Diagnostic.t list) result
    ]}
    On success, [result] carries expression annotations, binding schemes,
    the final environment, and any non-fatal diagnostics. On failure
    (hard errors), returns [Error diags]. *)

open Ty

(* -------------------------------------------------------------------------- *)
(* Results & context                                                           *)
(* -------------------------------------------------------------------------- *)

type annot_map = ty Ident.Map.t

type result = {
  env : Env.t;
  bindings : annot_map;
  schemes : scheme Ident.Map.t;
  expr_types : (Span.t * ty) list;
  diagnostics : Diagnostic.t list;
}

type ctx = {
  mutable env : Env.t;
  mutable bindings : annot_map;
  mutable schemes : scheme Ident.Map.t;
  mutable expr_types : (Span.t * ty) list;
  diags : Diagnostic.Bag.t;
}

let create_ctx ?(env = Env.prelude ()) () =
  {
    env;
    bindings = Ident.Map.empty;
    schemes = Ident.Map.empty;
    expr_types = [];
    diags = Diagnostic.Bag.create ();
  }

let record_expr ctx span ty =
  ctx.expr_types <- (span, ty) :: ctx.expr_types

let record_binding ctx name ty =
  ctx.bindings <- Ident.Map.add name ty ctx.bindings

let record_scheme ctx name scheme =
  ctx.schemes <- Ident.Map.add name scheme ctx.schemes;
  ctx.env <- Env.extend name scheme ctx.env;
  record_binding ctx name (instantiate_scheme scheme)

let error ctx span msg = Diagnostic.Bag.error ctx.diags span msg

let warning ctx span msg = Diagnostic.Bag.warning ctx.diags span msg

let unify_ok ctx span expected actual =
  match Unify.unify_span ~span expected actual with
  | Ok () -> true
  | Error (msg, _) ->
      error ctx span msg;
      false

let with_env_snapshot ctx f =
  let snap = ctx.env in
  Fun.protect ~finally:(fun () -> ctx.env <- snap) f

(* -------------------------------------------------------------------------- *)
(* Operators & literals                                                        *)
(* -------------------------------------------------------------------------- *)

let binop_name : Ast.binop -> string = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"
  | Ast.Eq -> "="
  | Ast.Neq -> "<>"
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.And -> "&&"
  | Ast.Or -> "||"
  | Ast.Cons -> "::"
  | Ast.Append -> "@"
  | Ast.Pipe -> "|>"

let unop_name : Ast.unop -> string = function
  | Ast.Neg -> "~-"
  | Ast.NegF -> "~-."
  | Ast.Not -> "not"

let lit_type : Ast.lit -> ty = function
  | Ast.Int _ -> Builtin.int
  | Ast.Float _ -> Builtin.float
  | Ast.Bool _ -> Builtin.bool
  | Ast.Char _ -> Builtin.char
  | Ast.String _ -> Builtin.string
  | Ast.Unit -> Builtin.unit

(* -------------------------------------------------------------------------- *)
(* Surface type translation                                                    *)
(* -------------------------------------------------------------------------- *)

let rec translate_type_expr ctx (env_params : (string * ty) list)
    (te : Ast.type_expr) : ty =
  match te with
  | Ast.TVar (name, span) -> (
      match List.assoc_opt name env_params with
      | Some t -> t
      | None ->
          warning ctx span
            (Printf.sprintf
               "unbound type variable '%s; introducing a fresh variable" name);
          new_var ~name ())
  | Ast.TCon (name, args, span) -> (
      match Env.lookup_type name ctx.env with
      | None ->
          error ctx span
            (Printf.sprintf "unbound type constructor %s"
               (Ident.to_string name));
          fresh ()
      | Some info ->
          if List.length args <> info.arity then
            error ctx span
              (Printf.sprintf
                 "type constructor %s expects %d argument(s), got %d"
                 (Ident.to_string name) info.arity (List.length args));
          Con (name, List.map (translate_type_expr ctx env_params) args))
  | Ast.TArrow (a, b, _) ->
      Arrow
        ( translate_type_expr ctx env_params a,
          translate_type_expr ctx env_params b )
  | Ast.TTuple (ts, _) ->
      tuple (List.map (translate_type_expr ctx env_params) ts)
  | Ast.TRecord (fields, _) ->
      Record
        (List.map
           (fun (n, t) -> (n, translate_type_expr ctx env_params t))
           fields)
  | Ast.TForall (qs, body, _) ->
      let env_params =
        List.fold_left (fun acc q -> (q, qvar q) :: acc) env_params qs
      in
      translate_type_expr ctx env_params body

let translate_to_scheme ctx params (te : Ast.type_expr) : scheme =
  match te with
  | Ast.TForall (qs, body, _) ->
      let env_params = List.map (fun q -> (q, qvar q)) qs in
      Forall (qs, translate_type_expr ctx env_params body)
  | _ ->
      let env_params = List.map (fun q -> (q, qvar q)) params in
      let ty = translate_type_expr ctx env_params te in
      if params = [] then mono ty else Forall (params, ty)

(** Translate constructor argument type expressions under [params]. *)
let translate_ctor_args ctx params args =
  let env_params = List.map (fun q -> (q, qvar q)) params in
  List.map (translate_type_expr ctx env_params) args

(* -------------------------------------------------------------------------- *)
(* Pattern inference                                                           *)
(* -------------------------------------------------------------------------- *)

let rec infer_pattern ctx (expected : ty) (pat : Ast.pattern) :
    (Ident.t * ty) list =
  let span = Ast.pattern_span pat in
  match pat with
  | Ast.PWild _ -> []
  | Ast.PVar (id, _) ->
      if Ident.is_underscore id then []
      else (
        record_binding ctx id expected;
        [ (id, expected) ])
  | Ast.PLit (lit, _) ->
      ignore (unify_ok ctx span expected (lit_type lit));
      []
  | Ast.PTuple (ps, _) ->
      let tys =
        match as_tuple (repr expected) with
        | Some ts when List.length ts = List.length ps -> ts
        | Some ts ->
            error ctx span
              (Printf.sprintf
                 "tuple pattern has %d elements but type has %d"
                 (List.length ps) (List.length ts));
            List.map (fun _ -> fresh ()) ps
        | None ->
            let tys = List.map (fun _ -> fresh ()) ps in
            ignore (unify_ok ctx span expected (tuple tys));
            List.map repr tys
      in
      List.concat (List.map2 (infer_pattern ctx) tys ps)
  | Ast.PRecord (fields, _) ->
      let field_tys =
        match as_record (repr expected) with
        | Some existing ->
            List.map
              (fun (name, _) ->
                match
                  List.find_opt (fun (n, _) -> Ident.equal n name) existing
                with
                | Some (_, ty) -> (name, ty)
                | None ->
                    error ctx span
                      (Printf.sprintf "record has no field %s"
                         (Ident.to_string name));
                    (name, fresh ()))
              fields
        | None ->
            let field_tys =
              List.map (fun (name, _) -> (name, fresh ())) fields
            in
            ignore (unify_ok ctx span expected (Record field_tys));
            field_tys
      in
      List.concat
        (List.map2
           (fun (_, p) (_, ty) -> infer_pattern ctx ty p)
           fields field_tys)
  | Ast.PConstructor (name, args, _) -> (
      match Env.lookup_constructor name ctx.env with
      | None ->
          error ctx span
            (Printf.sprintf "unbound constructor %s" (Ident.to_string name));
          List.concat (List.map (fun p -> infer_pattern ctx (fresh ()) p) args)
      | Some info ->
          let ctor_ty = instantiate_scheme info.scheme in
          let rec peel n ty acc =
            if n = 0 then (List.rev acc, ty)
            else
              match as_arrow (repr ty) with
              | Some (a, b) -> peel (n - 1) b (a :: acc)
              | None ->
                  error ctx span
                    (Printf.sprintf
                       "constructor %s applied to too many arguments"
                       (Ident.to_string name));
                  (List.rev acc, ty)
          in
          let arg_tys, result_ty = peel (List.length args) ctor_ty [] in
          ignore (unify_ok ctx span expected result_ty);
          if List.length arg_tys <> List.length args then (
            error ctx span
              (Printf.sprintf
                 "constructor %s expects %d argument(s), got %d"
                 (Ident.to_string name)
                 (List.length info.arg_tys)
                 (List.length args));
            List.concat
              (List.map (fun p -> infer_pattern ctx (fresh ()) p) args))
          else List.concat (List.map2 (infer_pattern ctx) arg_tys args))
  | Ast.POr (p1, p2, _) ->
      let b1 = infer_pattern ctx expected p1 in
      let b2 = infer_pattern ctx expected p2 in
      let sort = List.sort (fun (a, _) (b, _) -> Ident.compare a b) in
      let rec check a b =
        match (a, b) with
        | [], [] -> ()
        | (n1, t1) :: a, (n2, t2) :: b when Ident.equal n1 n2 ->
            ignore (unify_ok ctx span t1 t2);
            check a b
        | (n1, _) :: _, (n2, _) :: _ ->
            error ctx span
              (Printf.sprintf "or-pattern binds different names (%s vs %s)"
                 (Ident.to_string n1) (Ident.to_string n2))
        | _ ->
            error ctx span
              "or-pattern branches bind different sets of variables"
      in
      check (sort b1) (sort b2);
      b1
  | Ast.PAs (p, id, _) ->
      let binds = infer_pattern ctx expected p in
      if Ident.is_underscore id then binds
      else (
        record_binding ctx id expected;
        (id, expected) :: binds)
  | Ast.PAnnotated (p, te, _) ->
      let ann = translate_type_expr ctx [] te in
      ignore (unify_ok ctx span expected ann);
      infer_pattern ctx expected p

let extend_with_binds ctx binds =
  List.iter
    (fun (name, ty) ->
      ctx.env <- Env.extend_mono name ty ctx.env;
      record_binding ctx name ty)
    binds

(* -------------------------------------------------------------------------- *)
(* Expression inference (Algorithm W)                                          *)
(* -------------------------------------------------------------------------- *)

let rec infer_expr ctx (e : Ast.expr) : ty =
  let span = Ast.expr_span e in
  let ty = infer_expr_raw ctx e in
  record_expr ctx span ty;
  ty

and infer_expr_raw ctx (e : Ast.expr) : ty =
  match e with
  | Ast.Var (id, span) -> (
      match Env.lookup_value id ctx.env with
      | Some scheme -> instantiate_scheme scheme
      | None -> (
          match Env.lookup_constructor id ctx.env with
          | Some info -> instantiate_scheme info.scheme
          | None ->
              error ctx span
                (Printf.sprintf "unbound value %s" (Ident.to_string id));
              fresh ()))
  | Ast.Lit (lit, _) -> lit_type lit
  | Ast.App (fn, arg, span) -> (
      let fn_ty = infer_expr ctx fn in
      let arg_ty = infer_expr ctx arg in
      match Unify.apply_fun fn_ty arg_ty with
      | Ok ret -> ret
      | Error (msg, _) ->
          error ctx span msg;
          fresh ())
  | Ast.Fun (params, body, _) ->
      with_env_snapshot ctx (fun () ->
          let param_tys =
            List.map
              (fun p ->
                let tv = fresh () in
                ctx.env <- Env.extend_mono p tv ctx.env;
                record_binding ctx p tv;
                tv)
              params
          in
          let body_ty = infer_expr ctx body in
          arrows param_tys body_ty)
  | Ast.Let (name, params, rhs, body, span) ->
      infer_let ctx ~span name params rhs body
  | Ast.LetRec (bindings, body, span) ->
      infer_letrec ctx ~span bindings body
  | Ast.If (cond, thn, els, span) ->
      let cond_ty = infer_expr ctx cond in
      ignore (unify_ok ctx span Builtin.bool cond_ty);
      let t_ty = infer_expr ctx thn in
      let e_ty = infer_expr ctx els in
      ignore (unify_ok ctx (Ast.expr_span els) t_ty e_ty);
      t_ty
  | Ast.Match (scrut, arms, span) -> infer_match ctx ~span scrut arms
  | Ast.Tuple (es, _) -> tuple (List.map (infer_expr ctx) es)
  | Ast.Record (fields, span) ->
      let typed = List.map (fun (n, e) -> (n, infer_expr ctx e)) fields in
      let seen = ref Ident.Set.empty in
      List.iter
        (fun (n, _) ->
          if Ident.Set.mem n !seen then
            error ctx span
              (Printf.sprintf "duplicate record field %s"
                 (Ident.to_string n))
          else seen := Ident.Set.add n !seen)
        typed;
      Record typed
  | Ast.Field (e, name, span) -> (
      let e_ty = infer_expr ctx e in
      match Unify.project_field e_ty name with
      | Ok ty -> ty
      | Error (msg, _) ->
          error ctx span msg;
          fresh ())
  | Ast.Constructor (name, args, span) -> (
      match Env.lookup_constructor name ctx.env with
      | None ->
          error ctx span
            (Printf.sprintf "unbound constructor %s" (Ident.to_string name));
          fresh ()
      | Some info ->
          let ctor_ty = instantiate_scheme info.scheme in
          List.fold_left
            (fun ft arg ->
              let arg_ty = infer_expr ctx arg in
              match Unify.apply_fun ft arg_ty with
              | Ok ret -> ret
              | Error (msg, _) ->
                  error ctx (Ast.expr_span arg) msg;
                  fresh ())
            ctor_ty args)
  | Ast.Binop (op, lhs, rhs, span) -> infer_binop ctx ~span op lhs rhs
  | Ast.Unop (op, e, span) -> infer_unop ctx ~span op e
  | Ast.Seq (a, b, span) ->
      let a_ty = infer_expr ctx a in
      ignore (unify_ok ctx span Builtin.unit a_ty);
      infer_expr ctx b
  | Ast.Annotated (e, te, span) ->
      let ann = translate_type_expr ctx [] te in
      let e_ty = infer_expr ctx e in
      ignore (unify_ok ctx span ann e_ty);
      ann

and infer_binop ctx ~span op lhs rhs =
  let name = binop_name op in
  match Env.lookup_value (Ident.Intern.intern name) ctx.env with
  | None ->
      error ctx span (Printf.sprintf "unbound operator %s" name);
      fresh ()
  | Some scheme -> (
      let op_ty = instantiate_scheme scheme in
      let l_ty = infer_expr ctx lhs in
      let r_ty = infer_expr ctx rhs in
      match Unify.apply_fun op_ty l_ty with
      | Error (msg, _) ->
          error ctx span msg;
          fresh ()
      | Ok mid -> (
          match Unify.apply_fun mid r_ty with
          | Ok ret -> ret
          | Error (msg, _) ->
              error ctx span msg;
              fresh ()))

and infer_unop ctx ~span op e =
  let name = unop_name op in
  match Env.lookup_value (Ident.Intern.intern name) ctx.env with
  | None ->
      error ctx span (Printf.sprintf "unbound unary operator %s" name);
      fresh ()
  | Some scheme -> (
      let op_ty = instantiate_scheme scheme in
      let e_ty = infer_expr ctx e in
      match Unify.apply_fun op_ty e_ty with
      | Ok ret -> ret
      | Error (msg, _) ->
          error ctx span msg;
          fresh ())

and infer_let ctx ~span name params rhs body =
  ignore span;
  let env_snapshot = ctx.env in
  ctx.env <- Env.enter_level ctx.env;
  let fun_ty =
    with_env_snapshot ctx (fun () ->
        let param_tys =
          List.map
            (fun p ->
              let tv = fresh () in
              ctx.env <- Env.extend_mono p tv ctx.env;
              record_binding ctx p tv;
              tv)
            params
        in
        let rhs_ty = infer_expr ctx rhs in
        arrows param_tys rhs_ty)
  in
  ctx.env <- Env.exit_level ctx.env;
  ctx.env <- env_snapshot;
  let scheme = Env.generalize_in ctx.env fun_ty in
  record_scheme ctx name scheme;
  infer_expr ctx body

and infer_letrec ctx ~span bindings body =
  let env_snapshot = ctx.env in
  ctx.env <- Env.enter_level ctx.env;
  let stubs =
    List.map
      (fun (name, params, _rhs) ->
        let param_tys = List.map (fun _ -> fresh ()) params in
        let ret = fresh () in
        let stub = arrows param_tys ret in
        ctx.env <- Env.extend_mono name stub ctx.env;
        record_binding ctx name stub;
        (name, params, param_tys, ret, stub))
      bindings
  in
  List.iter2
    (fun (_name, params, rhs) (_n, _ps, param_tys, ret, stub) ->
      with_env_snapshot ctx (fun () ->
          List.iter2
            (fun p ty ->
              ctx.env <- Env.extend_mono p ty ctx.env;
              record_binding ctx p ty)
            params param_tys;
          let rhs_ty = infer_expr ctx rhs in
          ignore (unify_ok ctx (Ast.expr_span rhs) ret rhs_ty);
          let inferred = arrows param_tys rhs_ty in
          ignore (unify_ok ctx span stub inferred)))
    bindings stubs;
  ctx.env <- Env.exit_level ctx.env;
  ctx.env <- env_snapshot;
  List.iter
    (fun (name, _params, _param_tys, _ret, stub) ->
      let scheme = Env.generalize_in ctx.env (repr stub) in
      record_scheme ctx name scheme)
    stubs;
  infer_expr ctx body

and infer_match ctx ~span scrut arms =
  if arms = [] then (
    error ctx span "match expression has no arms";
    fresh ())
  else
    let scrut_ty = infer_expr ctx scrut in
    let result_ty = fresh () in
    List.iter
      (fun (pat, arm) ->
        with_env_snapshot ctx (fun () ->
            let binds = infer_pattern ctx scrut_ty pat in
            extend_with_binds ctx binds;
            let arm_ty = infer_expr ctx arm in
            ignore (unify_ok ctx (Ast.expr_span arm) result_ty arm_ty)))
      arms;
    result_ty

(* -------------------------------------------------------------------------- *)
(* Type declarations                                                           *)
(* -------------------------------------------------------------------------- *)

let register_type_decl ctx (td : Ast.type_decl) =
  match td with
  | Ast.Alias { name; params; body; span } ->
      if Env.mem_type name ctx.env then
        warning ctx span
          (Printf.sprintf "shadowing type %s" (Ident.to_string name));
      let _scheme = translate_to_scheme ctx params body in
      ctx.env <- Env.add_alias ~name ~params ~body ctx.env
  | Ast.Variant { name; params; constructors; span } ->
      if Env.mem_type name ctx.env then
        warning ctx span
          (Printf.sprintf "redefining type %s" (Ident.to_string name));
      List.iter
        (fun (c : Ast.constructor_decl) ->
          if Env.mem_constructor c.name ctx.env then
            warning ctx c.span
              (Printf.sprintf "shadowing constructor %s"
                 (Ident.to_string c.name)))
        constructors;
      let ctors =
        List.map
          (fun (c : Ast.constructor_decl) ->
            let args = translate_ctor_args ctx params c.args in
            (c.name, args))
          constructors
      in
      ctx.env <- Env.add_variant ~name ~params ~ctors ctx.env;
      (* Also bind nullary constructors as values for convenience. *)
      List.iter
        (fun (c : Ast.constructor_decl) ->
          match Env.lookup_constructor c.name ctx.env with
          | Some info ->
              ctx.env <- Env.extend c.name info.scheme ctx.env;
              record_scheme ctx c.name info.scheme
          | None -> ())
        constructors

(* -------------------------------------------------------------------------- *)
(* Top-level items                                                             *)
(* -------------------------------------------------------------------------- *)

let infer_value ctx ~recursive ~span name params body =
  ignore span;
  if recursive then
    ignore
      (infer_letrec ctx ~span
         [ (name, params, body) ]
         (Ast.Var (name, span)))
  else
    let env_snapshot = ctx.env in
    ctx.env <- Env.enter_level ctx.env;
    let fun_ty =
      with_env_snapshot ctx (fun () ->
          let param_tys =
            List.map
              (fun p ->
                let tv = fresh () in
                ctx.env <- Env.extend_mono p tv ctx.env;
                record_binding ctx p tv;
                tv)
              params
          in
          let body_ty = infer_expr ctx body in
          arrows param_tys body_ty)
    in
    ctx.env <- Env.exit_level ctx.env;
    ctx.env <- env_snapshot;
    let scheme = Env.generalize_in ctx.env fun_ty in
    record_scheme ctx name scheme

let infer_value_rec_group ctx ~span bindings =
  let triples =
    List.map (fun (name, params, body, _sp) -> (name, params, body)) bindings
  in
  ignore
    (infer_letrec ctx ~span triples
       (match bindings with
       | (name, _, _, sp) :: _ -> Ast.Var (name, sp)
       | [] -> Ast.Lit (Ast.Unit, span)))

let infer_toplevel ctx (item : Ast.toplevel) =
  match item with
  | Ast.TypeDecl td -> register_type_decl ctx td
  | Ast.Value { name; params; body; recursive; span } ->
      infer_value ctx ~recursive ~span name params body
  | Ast.ValueRec { bindings; span } ->
      infer_value_rec_group ctx ~span bindings
  | Ast.Expr e -> ignore (infer_expr ctx e)

(* -------------------------------------------------------------------------- *)
(* Program entry                                                               *)
(* -------------------------------------------------------------------------- *)

let finish ctx : result =
  {
    env = ctx.env;
    bindings = ctx.bindings;
    schemes = ctx.schemes;
    expr_types = List.rev ctx.expr_types;
    diagnostics = Diagnostic.Bag.to_list ctx.diags;
  }

let infer_program ?(env = Env.prelude ()) (prog : Ast.program) :
    (result, Diagnostic.t list) result =
  Ty.reset_level ();
  let ctx = create_ctx ~env () in
  List.iter (infer_toplevel ctx) prog.items;
  let res = finish ctx in
  if Diagnostic.Bag.has_errors ctx.diags then Error res.diagnostics
  else Ok res

(** Infer a standalone expression under [env], returning its type. *)
let infer_expr_top ?(env = Env.prelude ()) (e : Ast.expr) :
    (ty * result, Diagnostic.t list) result =
  Ty.reset_level ();
  let ctx = create_ctx ~env () in
  let ty = infer_expr ctx e in
  let res = finish ctx in
  if Diagnostic.Bag.has_errors ctx.diags then Error res.diagnostics
  else Ok (ty, res)

(** Infer a pattern against [expected], returning bound names. *)
let infer_pattern_top ?(env = Env.prelude ()) expected pat =
  Ty.reset_level ();
  let ctx = create_ctx ~env () in
  let binds = infer_pattern ctx expected pat in
  let res = finish ctx in
  if Diagnostic.Bag.has_errors ctx.diags then Error res.diagnostics
  else Ok (binds, res)

(* -------------------------------------------------------------------------- *)
(* Query helpers                                                               *)
(* -------------------------------------------------------------------------- *)

let find_expr_type (res : result) (span : Span.t) : ty option =
  List.find_map
    (fun (sp, ty) -> if Span.equal sp span then Some ty else None)
    res.expr_types

let lookup_binding (res : result) name =
  Ident.Map.find_opt name res.bindings

let lookup_scheme (res : result) name =
  Ident.Map.find_opt name res.schemes

let pp_result fmt (res : result) =
  Format.fprintf fmt "@[<v>bindings:@,";
  Ident.Map.iter
    (fun name scheme ->
      Format.fprintf fmt "  %a : %a@," Ident.pp name pp_scheme scheme)
    res.schemes;
  Format.fprintf fmt "diagnostics: %d@," (List.length res.diagnostics);
  Format.fprintf fmt "@]"

let result_to_string res =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  pp_result fmt res;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* -------------------------------------------------------------------------- *)
(* Exhaustiveness / usefulness (lightweight checks)                            *)
(* -------------------------------------------------------------------------- *)

(** Emit a warning when a match has no wildcard and only covers some known
    constructors of a variant (best-effort; not a full usefulness analysis). *)
let check_match_coverage ctx scrut_ty arms span =
  match as_con (repr scrut_ty) with
  | None -> ()
  | Some (type_name, _) -> (
      match Env.lookup_type type_name ctx.env with
      | None -> ()
      | Some info when info.constructors = [] -> ()
      | Some info ->
          let covered = ref Ident.Set.empty in
          let has_wild = ref false in
          let rec collect_pat = function
            | Ast.PWild _ | Ast.PVar _ -> has_wild := true
            | Ast.PConstructor (n, args, _) ->
                covered := Ident.Set.add n !covered;
                List.iter collect_pat args
            | Ast.POr (a, b, _) ->
                collect_pat a;
                collect_pat b
            | Ast.PAs (p, _, _) | Ast.PAnnotated (p, _, _) -> collect_pat p
            | Ast.PTuple (ps, _) -> List.iter collect_pat ps
            | Ast.PRecord (fs, _) -> List.iter (fun (_, p) -> collect_pat p) fs
            | Ast.PLit _ -> ()
          in
          List.iter (fun (p, _) -> collect_pat p) arms;
          if not !has_wild then
            List.iter
              (fun ctor ->
                if not (Ident.Set.mem ctor !covered) then
                  warning ctx span
                    (Printf.sprintf
                       "pattern matching may be incomplete: constructor %s \
                        is not covered"
                       (Ident.to_string ctor)))
              info.constructors)

(** Re-run coverage after inferring a match — hooked via a wrapper. *)
let infer_match_with_coverage ctx ~span scrut arms =
  let ty = infer_match ctx ~span scrut arms in
  check_match_coverage ctx (infer_expr ctx scrut) arms span;
  (* Note: re-inferring scrut is wasteful; use recorded type instead. *)
  ignore ty;
  let scrut_ty =
    match find_expr_type (finish ctx) (Ast.expr_span scrut) with
    | Some t -> t
    | None -> fresh ()
  in
  check_match_coverage ctx scrut_ty arms span;
  ty

(* The coverage wrapper above double-infers; keep the simple infer_match as
   the default used by infer_expr_raw. Coverage is available separately: *)

let analyze_match_coverage ctx scrut_ty arms span =
  check_match_coverage ctx scrut_ty arms span

(* -------------------------------------------------------------------------- *)
(* Mutual recursion helpers                                                    *)
(* -------------------------------------------------------------------------- *)

(** Infer a group of mutually recursive top-level functions. *)
let infer_mutual ctx (bindings : (Ident.t * Ident.t list * Ast.expr * Span.t) list)
    =
  match bindings with
  | [] -> ()
  | (_, _, _, span) :: _ -> infer_value_rec_group ctx ~span bindings

(** Generalize all monomorphic bindings whose names are in [names]. *)
let generalize_names ctx names =
  List.iter
    (fun name ->
      match Ident.Map.find_opt name ctx.bindings with
      | None -> ()
      | Some ty ->
          let scheme = Env.generalize_in ctx.env (repr ty) in
          record_scheme ctx name scheme)
    names

(* -------------------------------------------------------------------------- *)
(* Annotation collection                                                       *)
(* -------------------------------------------------------------------------- *)

let expr_annot_map (res : result) : ty Ident.Map.t = res.bindings

let all_schemes (res : result) = res.schemes

let errors_of (res : result) =
  List.filter (fun d -> d.Diagnostic.severity = Diagnostic.Error) res.diagnostics

let warnings_of (res : result) =
  List.filter
    (fun d -> d.Diagnostic.severity = Diagnostic.Warning)
    res.diagnostics

(** Pretty-print all bindings in a result. *)
let dump_bindings fmt res =
  Ident.Map.iter
    (fun name scheme ->
      Format.fprintf fmt "%a : %a@." Ident.pp name pp_scheme scheme)
    res.schemes

(** Type-check a program and return only the final env (or diagnostics). *)
let typecheck ?(env = Env.prelude ()) prog =
  match infer_program ~env prog with
  | Ok res -> Ok res.env
  | Error diags -> Error diags

(** Type-check and return schemes for top-level values. *)
let typecheck_schemes ?(env = Env.prelude ()) prog =
  match infer_program ~env prog with
  | Ok res -> Ok res.schemes
  | Error diags -> Error diags
