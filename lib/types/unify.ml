(** Unification for Glyph's Hindley–Milner types (matches [Ty]). *)

open Ty

type error = string * Span.t option

let err msg = Error (msg, None)
let err_span span msg = Error (msg, Some span)

let mismatch expected actual =
  Printf.sprintf "type mismatch: expected %s but got %s" (to_string expected)
    (to_string actual)

let rec unify (a : ty) (b : ty) : (unit, error) result =
  let a = repr a in
  let b = repr b in
  match (a, b) with
  | TVar r1, TVar r2 when r1 == r2 -> Ok ()
  | ( TVar ({ contents = Unbound tv1 } as r1),
      TVar ({ contents = Unbound tv2 } as r2) ) ->
      let lvl = min tv1.level tv2.level in
      tv1.level <- lvl;
      tv2.level <- lvl;
      if tv1.id < tv2.id then (
        r2 := Link a;
        Ok ())
      else (
        r1 := Link b;
        Ok ())
  | TVar ({ contents = Unbound tv } as r), t | t, TVar ({ contents = Unbound tv } as r)
    ->
      if occurs tv t then
        err
          (Printf.sprintf "occurs check failed: variable occurs in %s"
             (to_string t))
      else (
        update_level tv.level t;
        r := Link t;
        Ok ())
  | TVar { contents = Generic _ }, _ | _, TVar { contents = Generic _ } ->
      err "internal: unification involving generic variable"
  | TVar { contents = Link _ }, _ | _, TVar { contents = Link _ } ->
      unify (repr a) (repr b)
  | TUnit, TUnit
  | TInt, TInt
  | TFloat, TFloat
  | TBool, TBool
  | TString, TString
  | TChar, TChar ->
      Ok ()
  | TCon n1, TCon n2 when String.equal n1 n2 -> Ok ()
  | TApp (f1, a1), TApp (f2, a2) -> (
      match unify f1 f2 with Error _ as e -> e | Ok () -> unify a1 a2)
  | TArrow (a1, b1), TArrow (a2, b2) -> (
      match unify a1 a2 with Error _ as e -> e | Ok () -> unify b1 b2)
  | TTuple xs, TTuple ys ->
      if List.length xs <> List.length ys then
        err
          (Printf.sprintf "tuple arity mismatch: %d vs %d" (List.length xs)
             (List.length ys))
      else unify_list xs ys
  | TArray a, TArray b | TRef a, TRef b -> unify a b
  | TRecord f1, TRecord f2 -> unify_records f1 f2
  | _ -> err (mismatch a b)

and unify_list xs ys =
  match (xs, ys) with
  | [], [] -> Ok ()
  | x :: xs, y :: ys -> (
      match unify x y with Error _ as e -> e | Ok () -> unify_list xs ys)
  | _ -> err "internal: list length mismatch in unify_list"

and unify_records f1 f2 =
  let sort fs =
    List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) fs
  in
  let f1 = sort f1 in
  let f2 = sort f2 in
  let rec go a b =
    match (a, b) with
    | [], [] -> Ok ()
    | (n1, t1, m1) :: xs, (n2, t2, m2) :: ys ->
        if n1 <> n2 then
          err (Printf.sprintf "record field mismatch: %s vs %s" n1 n2)
        else if m1 <> m2 then
          err (Printf.sprintf "record field mutability mismatch: %s" n1)
        else (
          match unify t1 t2 with Error _ as e -> e | Ok () -> go xs ys)
    | (n, _, _) :: _, [] -> err (Printf.sprintf "missing record field %s" n)
    | [], (n, _, _) :: _ -> err (Printf.sprintf "unexpected record field %s" n)
  in
  go f1 f2

let unify_span ~span a b =
  match unify a b with
  | Ok () -> Ok ()
  | Error (msg, _) -> Error (msg, Some span)

let require_arrow t =
  match repr t with
  | TArrow (a, b) -> Ok (a, b)
  | TVar ({ contents = Unbound _ } as r) ->
      let a = fresh_var () in
      let b = fresh_var () in
      r := Link (TArrow (a, b));
      Ok (a, b)
  | _ -> err (Printf.sprintf "expected a function type, got %s" (to_string t))

let require_tuple ~arity t =
  match repr t with
  | TTuple ts when List.length ts = arity -> Ok ts
  | TVar ({ contents = Unbound _ } as r) ->
      let ts = List.init arity (fun _ -> fresh_var ()) in
      r := Link (TTuple ts);
      Ok ts
  | _ ->
      err
        (Printf.sprintf "expected a tuple of arity %d, got %s" arity
           (to_string t))

let try_unify ~span a b : (unit, Error.t) result =
  match unify_span ~span a b with
  | Ok () -> Ok ()
  | Error (msg, sp) ->
      let span = Option.value sp ~default:span in
      Error
        (Error.make ~kind:Error.Unify_mismatch ~expected:a
           ~actual:b span msg)

let apply ~span ~fun_ty ~arg_ty =
  match require_arrow fun_ty with
  | Error (msg, _) ->
      raise
        (Error.Type_error (Error.make ~kind:Error.Not_a_function span msg))
  | Ok (domain, codomain) -> (
      match unify_span ~span domain arg_ty with
      | Ok () -> codomain
      | Error (msg, _) ->
          raise
            (Error.Type_error
               (Error.make ~kind:Error.Unify_mismatch ~expected:domain
                  ~actual:arg_ty span msg)))

let apply_many ~span ~fun_ty ~arg_tys =
  List.fold_left
    (fun fty arg -> apply ~span ~fun_ty:fty ~arg_ty:arg)
    fun_ty arg_tys

let project_field ~span record_ty name =
  let fname = Ident.name name in
  match repr record_ty with
  | TRecord fields -> (
      match List.find_opt (fun (n, _, _) -> String.equal n fname) fields with
      | Some (_, ty, _) -> ty
      | None ->
          raise
            (Error.Type_error
               (Error.make ~kind:Error.Unbound_field span
                  (Printf.sprintf "unbound record field %s" fname))))
  | TVar ({ contents = Unbound _ } as r) ->
      let field_ty = fresh_var () in
      r := Link (TRecord [ (fname, field_ty, false) ]);
      field_ty
  | _ ->
      raise
        (Error.Type_error
           (Error.make ~kind:Error.Not_a_record span
              (Printf.sprintf "expected a record, got %s" (to_string record_ty))))
