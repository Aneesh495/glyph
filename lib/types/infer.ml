(** Algorithm W / Hindley–Milner type inference over the Glyph surface AST.

    Public API:
    {[
      Infer.infer_program : ?env:Env.t -> Ast.program ->
        (Env.t, Diagnostic.t list) result
      Infer.infer_expr : ?env:Env.t -> Ast.expr ->
        (Ty.ty, Diagnostic.t list) result
    ]}

    Handles every [Ast.expr_desc] case, patterns, let / let-rec,
    type definitions, and extern items. *)

open Ty

type ctx = {
  mutable env : Env.t;
  diags : Diagnostic.Bag.t;
}

let create_ctx ?(env = Env.prelude ()) () =
  { env; diags = Diagnostic.Bag.create () }

let error ctx span msg = Diagnostic.Bag.error ctx.diags span msg

let unify ctx span a b =
  match Unify.try_unify ~span a b with
  | Ok () -> ()
  | Error e -> Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e)

let with_env_snapshot ctx f =
  let snap = ctx.env in
  Fun.protect ~finally:(fun () -> ctx.env <- snap) f

(* -------------------------------------------------------------------------- *)
(* Literals & surface types                                                   *)
(* -------------------------------------------------------------------------- *)

let lit_type = function
  | Ast.Lit_unit -> t_unit
  | Ast.Lit_bool _ -> t_bool
  | Ast.Lit_int _ -> t_int
  | Ast.Lit_float _ -> t_float
  | Ast.Lit_string _ -> t_string
  | Ast.Lit_char _ -> t_char

let rec translate_ty ctx (param_map : (Ident.t * ty) list) (aty : Ast.ty) : ty =
  match aty.Ast.ty_desc with
  | Ast.Ty_hole -> fresh_var ()
  | Ast.Ty_unit -> t_unit
  | Ast.Ty_var id -> (
      match List.find_opt (fun (p, _) -> Ident.equal p id) param_map with
      | Some (_, t) -> t
      | None -> fresh_var ~name:(Ident.name id) ())
  | Ast.Ty_named (name, args) ->
      let args = List.map (translate_ty ctx param_map) args in
      (match String.lowercase_ascii (Ident.name name) with
      | "int" -> t_int
      | "float" -> t_float
      | "bool" -> t_bool
      | "string" -> t_string
      | "char" -> t_char
      | "unit" -> t_unit
      | "list" -> (
          match args with
          | [ a ] -> t_list a
          | _ ->
              error ctx aty.ty_span "List expects 1 type argument";
              t_list (fresh_var ()))
      | "option" -> (
          match args with
          | [ a ] -> t_option a
          | _ ->
              error ctx aty.ty_span "Option expects 1 type argument";
              t_option (fresh_var ()))
      | "array" -> (
          match args with
          | [ a ] -> t_array a
          | _ ->
              error ctx aty.ty_span "Array expects 1 type argument";
              t_array (fresh_var ()))
      | "ref" -> (
          match args with
          | [ a ] -> t_ref a
          | _ ->
              error ctx aty.ty_span "Ref expects 1 type argument";
              t_ref (fresh_var ()))
      | _ -> (
          match Env.find_type ctx.env name with
          | Some info ->
              let arity = List.length info.params in
              if List.length args <> arity && info.kind <> Env.Abstract then
                error ctx aty.ty_span
                  (Printf.sprintf
                     "type constructor %s expects %d argument(s), got %d"
                     (Ident.to_string name) arity (List.length args));
              apply_constructor (Ident.name name) args
          | None -> apply_constructor (Ident.name name) args))
  | Ast.Ty_arrow (a, b) ->
      arrow (translate_ty ctx param_map a) (translate_ty ctx param_map b)
  | Ast.Ty_tuple ts -> tuple (List.map (translate_ty ctx param_map) ts)

let translate_ty0 ctx aty = translate_ty ctx [] aty

(* -------------------------------------------------------------------------- *)
(* Patterns                                                                   *)
(* -------------------------------------------------------------------------- *)

let rec infer_pat_ctx ctx (expected : ty) (pat : Ast.pat) : (Ident.t * ty) list =
  let span = pat.Ast.pat_span in
  match pat.pat_desc with
  | Ast.Pat_wild -> []
  | Ast.Pat_var id ->
      if Ident.is_underscore id then [] else [ (id, expected) ]
  | Ast.Pat_lit lit ->
      unify ctx span expected (lit_type lit);
      []
  | Ast.Pat_tuple ps ->
      let tys =
        match repr expected with
        | TTuple ts when List.length ts = List.length ps -> ts
        | _ ->
            let tys = List.map (fun _ -> fresh_var ()) ps in
            unify ctx span expected (tuple tys);
            tys
      in
      if List.length tys <> List.length ps then (
        error ctx span
          (Printf.sprintf "tuple pattern has %d elements but type has %d"
             (List.length ps) (List.length tys));
        List.concat_map (fun p -> infer_pat_ctx ctx (fresh_var ()) p) ps)
      else List.concat (List.map2 (infer_pat_ctx ctx) tys ps)
  | Ast.Pat_ctor (name, args) -> (
      match Env.find_constructor ctx.env name with
      | None ->
          error ctx span
            (Printf.sprintf "unbound constructor %s" (Ident.to_string name));
          List.concat_map (fun p -> infer_pat_ctx ctx (fresh_var ()) p) args
      | Some info ->
          let ctor_ty = instantiate info.scheme in
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
          unify ctx span expected result_ty;
          if List.length arg_tys <> List.length args then (
            error ctx span
              (Printf.sprintf "constructor %s expects %d argument(s), got %d"
                 (Ident.to_string name) info.arity (List.length args));
            List.concat_map (fun p -> infer_pat_ctx ctx (fresh_var ()) p) args)
          else List.concat (List.map2 (infer_pat_ctx ctx) arg_tys args))
  | Ast.Pat_or (p1, p2) ->
      let b1 = infer_pat_ctx ctx expected p1 in
      let b2 = infer_pat_ctx ctx expected p2 in
      let sort = List.sort (fun (a, _) (b, _) -> Ident.compare a b) in
      let rec check a b =
        match (a, b) with
        | [], [] -> ()
        | (n1, t1) :: a, (n2, t2) :: b when Ident.equal n1 n2 ->
            unify ctx span t1 t2;
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
  | Ast.Pat_as (p, id) ->
      let binds = infer_pat_ctx ctx expected p in
      if Ident.is_underscore id then binds else (id, expected) :: binds
  | Ast.Pat_annotate (p, ty) ->
      let ann = translate_ty0 ctx ty in
      unify ctx span expected ann;
      infer_pat_ctx ctx expected p

let extend_binds ctx binds =
  List.iter
    (fun (name, ty) -> ctx.env <- Env.extend_mono ctx.env name ty)
    binds

(* -------------------------------------------------------------------------- *)
(* Expressions                                                                *)
(* -------------------------------------------------------------------------- *)

let rec infer_expr_ctx ctx (e : Ast.expr) : ty =
  let span = e.Ast.expr_span in
  match e.expr_desc with
  | Ast.Expr_lit lit -> lit_type lit
  | Ast.Expr_var id -> (
      match Env.find_value ctx.env id with
      | Some scheme -> instantiate scheme
      | None -> (
          match Env.find_constructor ctx.env id with
          | Some info -> instantiate info.scheme
          | None ->
              error ctx span
                (Printf.sprintf "unbound value %s" (Ident.to_string id));
              fresh_var ()))
  | Ast.Expr_ctor id -> (
      match Env.find_constructor ctx.env id with
      | Some info -> instantiate info.scheme
      | None -> (
          match Env.find_value ctx.env id with
          | Some scheme -> instantiate scheme
          | None ->
              error ctx span
                (Printf.sprintf "unbound constructor %s" (Ident.to_string id));
              fresh_var ()))
  | Ast.Expr_app (fn, args) ->
      let fn_ty = infer_expr_ctx ctx fn in
      let arg_tys = List.map (infer_expr_ctx ctx) args in
      (match
         try
           Ok (Unify.apply_many ~span ~fun_ty:fn_ty ~arg_tys)
         with Error.Type_error e -> Error e
       with
      | Ok ret -> ret
      | Error e ->
          Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e);
          fresh_var ())
  | Ast.Expr_lambda (params, body) ->
      with_env_snapshot ctx (fun () ->
          let param_tys =
            List.map
              (fun (p : Ast.param) ->
                let tv =
                  match p.param_ty with
                  | None -> fresh_var ~name:(Ident.name p.param_name) ()
                  | Some ty -> translate_ty0 ctx ty
                in
                ctx.env <- Env.extend_mono ctx.env p.param_name tv;
                tv)
              params
          in
          let body_ty = infer_expr_ctx ctx body in
          arrows param_tys body_ty)
  | Ast.Expr_let (lb, body) -> infer_let ctx lb body
  | Ast.Expr_let_rec (lbs, body) -> infer_let_rec ctx lbs body
  | Ast.Expr_if (c, t, e) ->
      let c_ty = infer_expr_ctx ctx c in
      unify ctx c.expr_span c_ty t_bool;
      let t_ty = infer_expr_ctx ctx t in
      let e_ty = infer_expr_ctx ctx e in
      unify ctx span t_ty e_ty;
      t_ty
  | Ast.Expr_match (scrut, cases) ->
      let scrut_ty = infer_expr_ctx ctx scrut in
      let result = fresh_var () in
      List.iter
        (fun (c : Ast.case) ->
          with_env_snapshot ctx (fun () ->
              let binds = infer_pat_ctx ctx scrut_ty c.case_pat in
              extend_binds ctx binds;
              (match c.case_guard with
              | None -> ()
              | Some g ->
                  let g_ty = infer_expr_ctx ctx g in
                  unify ctx g.expr_span g_ty t_bool);
              let body_ty = infer_expr_ctx ctx c.case_body in
              unify ctx c.case_span body_ty result))
        cases;
      result
  | Ast.Expr_bin (op, a, b) -> infer_binop ctx span op a b
  | Ast.Expr_un (op, e) -> infer_unop ctx span op e
  | Ast.Expr_tuple es -> tuple (List.map (infer_expr_ctx ctx) es)
  | Ast.Expr_record fields ->
      TRecord
        (List.map
           (fun (name, e) ->
             (Ident.name name, infer_expr_ctx ctx e, false))
           fields)
  | Ast.Expr_field (e, name) -> (
      let e_ty = infer_expr_ctx ctx e in
      match
        try Ok (Unify.project_field ~span e_ty name)
        with Error.Type_error err -> Error err
      with
      | Ok ty -> ty
      | Error err ->
          Diagnostic.Bag.add ctx.diags (Error.to_diagnostic err);
          fresh_var ())
  | Ast.Expr_block es -> (
      match es with
      | [] -> t_unit
      | _ ->
          let rec go = function
            | [] -> t_unit
            | [ e ] -> infer_expr_ctx ctx e
            | e :: rest ->
                ignore (infer_expr_ctx ctx e);
                go rest
          in
          go es)
  | Ast.Expr_annotate (e, ty) ->
      let ann = translate_ty0 ctx ty in
      let e_ty = infer_expr_ctx ctx e in
      unify ctx span e_ty ann;
      ann
  | Ast.Expr_pipe (a, b) ->
      let a_ty = infer_expr_ctx ctx a in
      let b_ty = infer_expr_ctx ctx b in
      (match
         try Ok (Unify.apply ~span ~fun_ty:b_ty ~arg_ty:a_ty)
         with Error.Type_error e -> Error e
       with
      | Ok ret -> ret
      | Error e ->
          Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e);
          fresh_var ())

and infer_binop ctx span op a b =
  let a_ty = infer_expr_ctx ctx a in
  let b_ty = infer_expr_ctx ctx b in
  match Env.find_binop ctx.env op with
  | Some scheme -> (
      let op_ty = instantiate scheme in
      match
        try Ok (Unify.apply_many ~span ~fun_ty:op_ty ~arg_tys:[ a_ty; b_ty ])
        with Error.Type_error e -> Error e
      with
      | Ok ret -> ret
      | Error e ->
          Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e);
          fresh_var ())
  | None -> (
      match op with
      | Token.Op_eq | Token.Op_neq ->
          unify ctx span a_ty b_ty;
          t_bool
      | Token.Op_and | Token.Op_or ->
          unify ctx a.expr_span a_ty t_bool;
          unify ctx b.expr_span b_ty t_bool;
          t_bool
      | Token.Op_cons ->
          let elem = fresh_var () in
          unify ctx a.expr_span a_ty elem;
          unify ctx b.expr_span b_ty (t_list elem);
          t_list elem
      | Token.Op_pipe -> (
          match
            try Ok (Unify.apply ~span ~fun_ty:b_ty ~arg_ty:a_ty)
            with Error.Type_error e -> Error e
          with
          | Ok ret -> ret
          | Error e ->
              Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e);
              fresh_var ())
      | Token.Op_add | Token.Op_sub | Token.Op_mul | Token.Op_div | Token.Op_mod
        ->
          unify ctx a.expr_span a_ty t_int;
          unify ctx b.expr_span b_ty t_int;
          t_int
      | Token.Op_lt | Token.Op_le | Token.Op_gt | Token.Op_ge ->
          unify ctx span a_ty b_ty;
          t_bool)

and infer_unop ctx span op e =
  let e_ty = infer_expr_ctx ctx e in
  match Env.find_unop ctx.env op with
  | Some scheme -> (
      match
        try
          Ok (Unify.apply ~span ~fun_ty:(instantiate scheme) ~arg_ty:e_ty)
        with Error.Type_error err -> Error err
      with
      | Ok ret -> ret
      | Error err ->
          Diagnostic.Bag.add ctx.diags (Error.to_diagnostic err);
          fresh_var ())
  | None -> (
      match op with
      | Token.Op_neg ->
          unify ctx span e_ty t_int;
          t_int
      | Token.Op_not ->
          unify ctx span e_ty t_bool;
          t_bool)

and infer_binding_rhs ctx (lb : Ast.let_binding) : ty =
  match lb.lb_params with
  | [] ->
      let body_ty = infer_expr_ctx ctx lb.lb_body in
      (match lb.lb_ty with
      | None -> body_ty
      | Some ty ->
          let ann = translate_ty0 ctx ty in
          unify ctx lb.lb_span body_ty ann;
          ann)
  | params ->
      with_env_snapshot ctx (fun () ->
          let param_tys =
            List.map
              (fun (p : Ast.param) ->
                let tv =
                  match p.param_ty with
                  | None -> fresh_var ~name:(Ident.name p.param_name) ()
                  | Some ty -> translate_ty0 ctx ty
                in
                ctx.env <- Env.extend_mono ctx.env p.param_name tv;
                tv)
              params
          in
          let body_ty = infer_expr_ctx ctx lb.lb_body in
          let fun_ty = arrows param_tys body_ty in
          match lb.lb_ty with
          | None -> fun_ty
          | Some ty ->
              let ann = translate_ty0 ctx ty in
              unify ctx lb.lb_span fun_ty ann;
              ann)

and infer_let ctx (lb : Ast.let_binding) body =
  enter_level ();
  let rhs_ty = infer_binding_rhs ctx lb in
  leave_level ();
  let scheme = generalize rhs_ty in
  with_env_snapshot ctx (fun () ->
      ctx.env <- Env.extend ctx.env lb.lb_name scheme;
      infer_expr_ctx ctx body)

and infer_let_rec ctx (lbs : Ast.let_binding list) body =
  enter_level ();
  let placeholders =
    List.map
      (fun (lb : Ast.let_binding) ->
        let tv = fresh_var ~name:(Ident.name lb.lb_name) () in
        ctx.env <- Env.extend_mono ctx.env lb.lb_name tv;
        (lb, tv))
      lbs
  in
  List.iter
    (fun (lb, tv) ->
      let rhs_ty = infer_binding_rhs ctx lb in
      unify ctx lb.lb_span tv rhs_ty)
    placeholders;
  leave_level ();
  let schemes =
    List.map (fun (lb, tv) -> (lb.lb_name, generalize tv)) placeholders
  in
  with_env_snapshot ctx (fun () ->
      List.iter
        (fun (name, scheme) -> ctx.env <- Env.extend ctx.env name scheme)
        schemes;
      infer_expr_ctx ctx body)

(* -------------------------------------------------------------------------- *)
(* Top-level                                                                  *)
(* -------------------------------------------------------------------------- *)

let infer_let_binding_item ctx (lb : Ast.let_binding) =
  if lb.lb_rec || lb.lb_params <> [] then (
    enter_level ();
    let tv = fresh_var ~name:(Ident.name lb.lb_name) () in
    ctx.env <- Env.extend_mono ctx.env lb.lb_name tv;
    let rhs_ty = infer_binding_rhs ctx lb in
    unify ctx lb.lb_span tv rhs_ty;
    leave_level ();
    let scheme = generalize tv in
    ctx.env <- Env.extend ctx.env lb.lb_name scheme)
  else (
    enter_level ();
    let rhs_ty = infer_binding_rhs ctx lb in
    leave_level ();
    let scheme = generalize rhs_ty in
    ctx.env <- Env.extend ctx.env lb.lb_name scheme)

let infer_item ctx = function
  | Ast.Item_fn lb | Ast.Item_let lb -> infer_let_binding_item ctx lb
  | Ast.Item_type td -> ctx.env <- Env.add_type_def ctx.env td
  | Ast.Item_extern ext ->
      let param_tys = List.map (translate_ty0 ctx) ext.ext_params in
      let ret_ty = translate_ty0 ctx ext.ext_ret in
      ctx.env <-
        Env.extend ctx.env ext.ext_name (mono (arrows param_tys ret_ty))

let finish ctx env =
  let diags = Diagnostic.Bag.to_list ctx.diags in
  if Diagnostic.Bag.has_errors ctx.diags then Error diags else Ok env

let infer_program ?(env = Env.prelude ()) (prog : Ast.program) =
  reset_level ();
  let ctx = create_ctx ~env () in
  try
    List.iter (infer_item ctx) prog.Ast.items;
    finish ctx ctx.env
  with Error.Type_error e ->
    Diagnostic.Bag.add ctx.diags (Error.to_diagnostic e);
    Error (Diagnostic.Bag.to_list ctx.diags)

let infer_expr ?(env = Env.prelude ()) (e : Ast.expr) =
  reset_level ();
  let ctx = create_ctx ~env () in
  try
    let ty = infer_expr_ctx ctx e in
    let diags = Diagnostic.Bag.to_list ctx.diags in
    if Diagnostic.Bag.has_errors ctx.diags then Error diags else Ok (zonk ty)
  with Error.Type_error err ->
    Diagnostic.Bag.add ctx.diags (Error.to_diagnostic err);
    Error (Diagnostic.Bag.to_list ctx.diags)

let infer_pat ?(env = Env.prelude ()) ~expected (p : Ast.pat) =
  let ctx = create_ctx ~env () in
  try
    let binds = infer_pat_ctx ctx expected p in
    let diags = Diagnostic.Bag.to_list ctx.diags in
    if Diagnostic.Bag.has_errors ctx.diags then Error diags else Ok binds
  with Error.Type_error err ->
    Diagnostic.Bag.add ctx.diags (Error.to_diagnostic err);
    Error (Diagnostic.Bag.to_list ctx.diags)
