(** Algorithm W / Hindley–Milner type inference for Glyph.

    Expected [Ast] surface (see [lib/syntax/ast.ml]):
    - [Ast.expr]: Var, Lit, App, Fun, Let, LetRec, If, Match, Tuple,
      Record, Field, Constructor, Binop, Unop, Seq, Annotated
    - [Ast.pattern]: PWild, PVar, PLit, PTuple, PRecord, PConstructor,
      POr, PAs, PAnnotated
    - [Ast.type_expr], [Ast.type_decl], [Ast.toplevel], [Ast.program]

    Public entry point:
    {[
      Infer.infer_program : Ast.program ->
        (Infer.result, Diagnostic.t list) result
    ]}
    where [result] carries expression type annotations, the final
    environment, and any non-fatal warnings. *)

open Ty

(* -------------------------------------------------------------------------- *)
(* Inference state                                                             *)
(* -------------------------------------------------------------------------- *)

type annot_map = ty Ident.Map.t
(** Maps value identifiers to their inferred (possibly polymorphic-instantiated
    then re-generalized) types at binding sites. *)

type expr_annot = (Span.t * ty) list
(** Span-keyed expression types for tooling / IDE hover. *)

type result = {
  env : Env.t;
  bindings : annot_map;
  (** Top-level and let-bound identifiers → principal type (monomorphized
      representative after generalization stored as the scheme body opened). *)
  schemes : scheme Ident.Map.t;
  (** Identifier → generalized scheme. *)
  expr_types : expr_annot;
  diagnostics : Diagnostic.t list;
}

type ctx = {
  mutable env : Env.t;
  mutable bindings : annot_map;
  mutable schemes : scheme Ident.Map.t;
  mutable expr_types : expr_annot;
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

let error ctx span msg =
  Diagnostic.Bag.error ctx.diags span msg

let warning ctx span msg =
  Diagnostic.Bag.warning ctx.diags span msg

let unify_or_error ctx span expected actual =
  match Unify.unify_span ~span expected actual with
  | Ok () -> true
  | Error (msg, _) ->
      error ctx span msg;
      false

(* -------------------------------------------------------------------------- *)
(* Binop / unop typing                                                         *)
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
(* Translate surface type expressions                                          *)
(* -------------------------------------------------------------------------- *)

let rec translate_type_expr ctx (env_params : (string * ty) list)
    (te : Ast.type_expr) : ty =
  match te with
  | Ast.TVar (name, span) -> (
      match List.assoc_opt name env_params with
      | Some t -> t
      | None ->
          (* Unbound type variable in annotation: treat as fresh rigid-ish var. *)
          warning ctx span
            (Printf.sprintf "unbound type variable '%s; introducing fresh variable"
               name);
          new_var ~name ())
  | Ast.TCon (name, args, span) -> (
      match Env.lookup_type name ctx.env with
      | None ->
          error ctx span
            (Printf.sprintf "unbound type constructor %s"
               (Ident.to_string name));
          fresh ()
      | Some info ->
          if List.length args <> info.arity then (
            error ctx span
              (Printf.sprintf
                 "type constructor %s expects %d argument(s), got %d"
                 (Ident.to_string name) info.arity (List.length args));
            Con (name, List.map (translate_type_expr ctx env_params) args))
          else
            Con
              ( name,
                List.map (translate_type_expr ctx env_params) args ))
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
        List.fold_left
          (fun acc q -> (q, qvar q) :: acc)
          env_params qs
      in
      (* Return body with QVars; caller may wrap as scheme. *)
      translate_type_expr ctx env_params body

let translate_type_scheme ctx params (te : Ast.type_expr) : scheme =
  match te with
  | Ast.TForall (qs, body, _) ->
      let env_params = List.map (fun q -> (q, qvar q)) qs in
      let ty = translate_type_expr ctx env_params body in
      Forall (qs, ty)
  | _ ->
      let env_params = List.map (fun q -> (q, qvar q)) params in
      let ty = translate_type_expr ctx env_params te in
      if params = [] then mono ty else Forall (params, ty)

(* -------------------------------------------------------------------------- *)
(* Pattern inference                                                           *)
(* -------------------------------------------------------------------------- *)

(** Infer a pattern against expected type [expected].
    Returns bindings introduced by the pattern (identifier → type). *)
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
      let t = lit_type lit in
      ignore (unify_or_error ctx span expected t);
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
            ignore (unify_or_error ctx span expected (tuple tys));
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
                  List.find_opt
                    (fun (n, _) -> Ident.equal n name)
                    existing
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
            ignore
              (unify_or_error ctx span expected (Record field_tys));
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
            (Printf.sprintf "unbound constructor %s"
               (Ident.to_string name));
          List.concat
            (List.map (fun p -> infer_pattern ctx (fresh ()) p) args)
      | Some info ->
          let ctor_ty = instantiate_scheme info.scheme in
          (* Peel arrows to get argument types and result. *)
          let rec peel n ty acc =
            if n = 0 then (List.rev acc, ty)
            else
              match as_arrow (repr ty) with
              | Some (a, b) -> peel (n - 1) b (a :: acc)
              | None ->
                  error ctx span
                    (Printf.sprintf
                       "constructor %s applied to %d argument(s) but \
                        expects fewer"
                       (Ident.to_string name) (List.length args));
                  (List.rev acc, ty)
          in
          let arg_tys, result_ty = peel (List.length args) ctor_ty [] in
          ignore (unify_or_error ctx span expected result_ty);
          if List.length arg_tys <> List.length args then (
            error ctx span
              (Printf.sprintf
                 "constructor %s expects %d argument(s), got %d"
                 (Ident.to_string name) (List.length info.arg_tys)
                 (List.length args));
            List.concat
              (List.map (fun p -> infer_pattern ctx (fresh ()) p) args))
          else
            List.concat
              (List.map2 (infer_pattern ctx) arg_tys args))
  | Ast.POr (p1, p2, _) ->
      (* Both branches must bind the same names at the same types. *)
      let b1 = infer_pattern ctx expected p1 in
      let b2 = infer_pattern ctx expected p2 in
      let names1 =
        List.sort
          (fun (a, _) (b, _) -> Ident.compare a b)
          b1
      in
      let names2 =
        List.sort
          (fun (a, _) (b, _) -> Ident.compare a b)
          b2
      in
      let rec check a b =
        match (a, b) with
        | [], [] -> ()
        | (n1, t1) :: a, (n2, t2) :: b when Ident.equal n1 n2 ->
            ignore (unify_or_error ctx span t1 t2);
            check a b
        | (n1, _) :: _, (n2, _) :: _ ->
            error ctx span
              (Printf.sprintf
                 "or-pattern binds different names (%s vs %s)"
                 (Ident.to_string n1) (Ident.to_string n2))
        | _ ->
            error ctx span
              "or-pattern branches bind different sets of variables"
      in
      check names1 names2;
      b1
  | Ast.PAs (p, id, _) ->
      let binds = infer_pattern ctx expected p in
      if Ident.is_underscore id then binds
      else (
        record_binding ctx id expected;
        (id, expected) :: binds)
  | Ast.PAnnotated (p, te, _) ->
      let ann = translate_type_expr ctx [] te in
      ignore (unify_or_error ctx span expected ann);
      infer_pattern ctx expected p

(** Extend environment with pattern bindings as monomorphic types. *)
let extend_with_pattern_binds ctx binds =
  List.iter
    (fun (name, ty) ->
      ctx.env <- Env.extend_mono name ty ctx.env;
      record_binding ctx name ty)
    binds

(* -------------------------------------------------------------------------- *)
(* Expression inference                                                        *)
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
      (* Restore is handled by caller scopes for top-level; for nested Fun
         we deliberately keep extensions until the enclosing let restores.
         Snapshot/restore: *)
      arrows param_tys body_ty
  | Ast.Let (name, params, rhs, body, span) ->
      infer_let ctx ~recursive:false ~span name params rhs body
  | Ast.LetRec (bindings, body, span) ->
      infer_letrec ctx ~span bindings body
  | Ast.If (cond, thn, els, span) ->
      let cond_ty = infer_expr ctx cond in
      ignore (unify_or_error ctx span Builtin.bool cond_ty);
      let t_ty = infer_expr ctx thn in
      let e_ty = infer_expr ctx els in
      ignore (unify_or_error ctx (Ast.expr_span els) t_ty e_ty);
      t_ty
  | Ast.Match (scrut, arms, span) ->
      infer_match ctx ~span scrut arms
  | Ast.Tuple (es, _) ->
      tuple (List.map (infer_expr ctx) es)
  | Ast.Record (fields, span) ->
      let typed =
        List.map
          (fun (name, e) -> (name, infer_expr ctx e))
          fields
      in
      (* Check duplicate fields. *)
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
            (Printf.sprintf "unbound constructor %s"
               (Ident.to_string name));
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
  | Ast.Binop (op, lhs, rhs, span) ->
      infer_binop ctx ~span op lhs rhs
  | Ast.Unop (op, e, span) ->
      infer_unop ctx ~span op e
  | Ast.Seq (a, b, span) ->
      let a_ty = infer_expr ctx a in
      if not (is_unit (repr a_ty) || is_var (repr a_ty)) then
        ignore (unify_or_error ctx span Builtin.unit a_ty)
      else ignore (unify_or_error ctx span Builtin.unit a_ty);
      infer_expr ctx b
  | Ast.Annotated (e, te, span) ->
      let ann = translate_type_expr ctx [] te in
      let e_ty = infer_expr ctx e in
      ignore (unify_or_error ctx span ann e_ty);
      ann

and infer_binop ctx ~span op lhs rhs =
  let name = binop_name op in
  match Env.lookup_value (Ident.Intern.intern name) ctx.env with
  | None ->
      error ctx span (Printf.sprintf "unbound operator %s" name);
      fresh ()
  | Some scheme ->
      let op_ty = instantiate_scheme scheme in
      let l_ty = infer_expr ctx lhs in
      let r_ty = infer_expr ctx rhs in
      (match Unify.apply_fun op_ty l_ty with
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
  | Some scheme ->
      let op_ty = instantiate_scheme scheme in
      let e_ty = infer_expr ctx e in
      (match Unify.apply_fun op_ty e_ty with
      | Ok ret -> ret
      | Error (msg, _) ->
          error ctx span msg;
          fresh ())

and infer_let ctx ~recursive ~span name params rhs body =
  ignore recursive;
  (* Non-recursive let: infer RHS at raised level, generalize, then body. *)
  let env_snapshot = ctx.env in
  ctx.env <- Env.enter_level ctx.env;
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
  let fun_ty = arrows param_tys rhs_ty in
  ctx.env <- Env.exit_level ctx.env;
  (* Restore params from snapshot then add generalized binding. *)
  ctx.env <- env_snapshot;
  let scheme = Env.generalize_in ctx.env fun_ty in
  record_scheme ctx name scheme;
  infer_expr ctx body

and infer_letrec ctx ~span bindings body =
  let env_snapshot = ctx.env in
  ctx.env <- Env.enter_level ctx.env;
  (* Declare all names at monomorphic fresh types first. *)
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
  (* Infer each RHS and unify with stub. *)
  List.iter2
    (fun (_name, params, rhs) (_n, _ps, param_tys, ret, stub) ->
      let env_before = ctx.env in
      List.iter2
        (fun p ty ->
          ctx.env <- Env.extend_mono p ty ctx.env;
          record_binding ctx p ty)
        params param_tys;
      let rhs_ty = infer_expr ctx rhs in
      ignore (unify_or_error ctx (Ast.expr_span rhs) ret rhs_ty);
      let inferred = arrows param_tys rhs_ty in
      ignore (unify_or_error ctx span stub inferred);
      ctx.env <- env_before)
    bindings stubs;
  ctx.env <- Env.exit_level ctx.env;
  (* Generalize each binding. *)
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
        let env_snapshot = ctx.env in
        let binds = infer_pattern ctx scrut_ty pat in
        extend_with_pattern_binds ctx binds;
        let arm_ty = infer_expr ctx arm in
        ignore
          (unify_or_error ctx (Ast.expr_span arm) result_ty arm_ty);
        ctx.env <- env_snapshot)
      arms;
    result_ty

(* -------------------------------------------------------------------------- *)
(* Scoped expression inference (snapshot env)                                  *)
(* -------------------------------------------------------------------------- *)

let with_env_snapshot ctx f =
  let snap = ctx.env in
  let result = f () in
  ctx.env <- snap;
  result

let infer_fun_scoped ctx params body =
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

(* Re-bind Fun to use scoped inference for hygiene. *)
let infer_expr_raw ctx e =
  match e with
  | Ast.Fun (params, body, _) -> infer_fun_scoped ctx params body
  | _ -> infer_expr_raw ctx e

(* Wait — that shadows and recurses infinitely. Fix by inlining properly.
   The earlier infer_expr_raw already handles Fun; we'll patch Fun via
   replacing the Fun branch. The duplicate definition above is wrong.
   Remove it by not redefining — instead the first Fun branch should use
   with_env_snapshot. *)
