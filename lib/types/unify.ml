(** Unification for Glyph's Hindley–Milner type system.

    Implements Rémy-style unification with occurs-check and level updates.
    Errors are returned as [(message, optional span)] without raising. *)

open Ty

type error = string * Span.t option

type 'a res = ('a, error) result

let err msg = Error (msg, None)
let err_span span msg = Error (msg, Some span)

(* -------------------------------------------------------------------------- *)
(* Error formatting                                                            *)
(* -------------------------------------------------------------------------- *)

let mismatch_msg expected actual =
  Printf.sprintf "type mismatch: expected %s but got %s"
    (ty_to_string expected) (ty_to_string actual)

let arity_msg name expected actual =
  Printf.sprintf "type constructor %s expects %d argument(s) but got %d"
    (Ident.to_string name) expected actual

let occurs_msg tv t =
  Printf.sprintf "occurs check failed: '%s occurs in %s"
    (match !tv.name with Some n -> n | None -> string_of_int !tv.id)
    (ty_to_string t)

let record_field_msg missing =
  Printf.sprintf "missing record field %s" (Ident.to_string missing)

let extra_field_msg extra =
  Printf.sprintf "unknown record field %s" (Ident.to_string extra)

(* -------------------------------------------------------------------------- *)
(* Core unification                                                            *)
(* -------------------------------------------------------------------------- *)

let rec unify_ty (a : ty) (b : ty) : unit res =
  let a = repr a in
  let b = repr b in
  match (a, b) with
  | Var tv1, Var tv2 when tv1 == tv2 || !tv1.id = !tv2.id -> Ok ()
  | Var tv, t | t, Var tv -> unify_var tv t
  | QVar s1, QVar s2 when String.equal s1 s2 -> Ok ()
  | QVar s, t | t, QVar s ->
      (* Treat free QVar as rigid — should have been instantiated. *)
      err
        (Printf.sprintf
           "unification involving uninstantiated quantified variable '%s \
            (against %s)"
           s (ty_to_string t))
  | Con (n1, args1), Con (n2, args2) ->
      if not (Ident.equal n1 n2) then err (mismatch_msg a b)
      else if List.length args1 <> List.length args2 then
        err (arity_msg n1 (List.length args1) (List.length args2))
      else unify_list args1 args2
  | Arrow (a1, b1), Arrow (a2, b2) -> (
      match unify_ty a1 a2 with
      | Error _ as e -> e
      | Ok () -> unify_ty b1 b2)
  | Tuple t1, Tuple t2 ->
      if List.length t1 <> List.length t2 then
        err
          (Printf.sprintf "tuple arity mismatch: expected %d elements, got %d"
             (List.length t1) (List.length t2))
      else unify_list t1 t2
  | Tuple [], Con (n, []) when Ident.equal n Builtin.unit_id -> Ok ()
  | Con (n, []), Tuple [] when Ident.equal n Builtin.unit_id -> Ok ()
  | Record f1, Record f2 -> unify_records f1 f2
  | _ -> err (mismatch_msg a b)

and unify_var (tv : tvar ref) (t : ty) : unit res =
  let t = repr t in
  match t with
  | Var tv' when tv == tv' || !tv'.id = !tv.id -> Ok ()
  | _ ->
      if occur_check tv t then err (occurs_msg tv t)
      else (
        (* Level update before linking. *)
        update_level !tv.level t;
        (* If both are vars, keep the lower level on the surviving root. *)
        (match t with
        | Var tv' ->
            let lvl = min !tv.level !tv'.level in
            !tv.level <- lvl;
            !tv'.level <- lvl
        | _ -> ());
        !tv.link <- Some t;
        Ok ())

and unify_list xs ys : unit res =
  match (xs, ys) with
  | [], [] -> Ok ()
  | x :: xs, y :: ys -> (
      match unify_ty x y with
      | Error _ as e -> e
      | Ok () -> unify_list xs ys)
  | _ -> err "internal: unify_list length mismatch"

and unify_records f1 f2 : unit res =
  let sort fs =
    List.sort (fun (a, _) (b, _) -> Ident.compare a b) fs
  in
  let f1 = sort f1 in
  let f2 = sort f2 in
  let rec go a b =
    match (a, b) with
    | [], [] -> Ok ()
    | (n1, t1) :: a, (n2, t2) :: b when Ident.equal n1 n2 -> (
        match unify_ty t1 t2 with
        | Error _ as e -> e
        | Ok () -> go a b)
    | (n1, _) :: _, (n2, _) :: _ ->
        if Ident.compare n1 n2 < 0 then err (record_field_msg n1)
        else err (extra_field_msg n2)
    | (n, _) :: _, [] -> err (record_field_msg n)
    | [], (n, _) :: _ -> err (extra_field_msg n)
  in
  go f1 f2

(* -------------------------------------------------------------------------- *)
(* Public unify API                                                            *)
(* -------------------------------------------------------------------------- *)

let unify a b = unify_ty a b

let unify_span ~span a b =
  match unify_ty a b with
  | Ok () -> Ok ()
  | Error (msg, _) -> Error (msg, Some span)

let unify_many pairs =
  let rec go = function
    | [] -> Ok ()
    | (a, b) :: rest -> (
        match unify_ty a b with
        | Error _ as e -> e
        | Ok () -> go rest)
  in
  go pairs

(** Unify and return the representative of [a] (after linking). *)
let unify_to a b =
  match unify_ty a b with
  | Ok () -> Ok (repr a)
  | Error _ as e -> e

(* -------------------------------------------------------------------------- *)
(* Occurs-aware helpers                                                        *)
(* -------------------------------------------------------------------------- *)

let ensure_no_occur tv t =
  if occur_check tv t then err (occurs_msg tv t) else Ok ()

(** Bind [tv] to [t] after occurs + level checks. *)
let bind tv t = unify_var tv (repr t)

(* -------------------------------------------------------------------------- *)
(* Instantiation / generalization (shared with Infer)                          *)
(* -------------------------------------------------------------------------- *)

let instantiate = instantiate_scheme

let generalize = Ty.generalize

let generalize_ty t = Ty.generalize t

(** Enter a let level, run [f], then exit. *)
let with_level f =
  enter_level ();
  let result =
    try
      let v = f () in
      exit_level ();
      v
    with exn ->
      exit_level ();
      raise exn
  in
  result

(* -------------------------------------------------------------------------- *)
(* Function / arrow helpers                                                    *)
(* -------------------------------------------------------------------------- *)

(** Unify [t] with [arg -> ret], synthesizing fresh vars as needed. *)
let unify_arrow t =
  match repr t with
  | Arrow (a, b) -> Ok (a, b)
  | Var _ ->
      let a = fresh () in
      let b = fresh () in
      (match unify_ty t (Arrow (a, b)) with
      | Ok () -> Ok (a, b)
      | Error _ as e -> e)
  | _ ->
      err
        (Printf.sprintf "expected a function type, got %s" (ty_to_string t))

(** Apply function type [ft] to argument type [arg], returning result type. *)
let apply_fun ft arg =
  match unify_arrow ft with
  | Error _ as e -> e
  | Ok (param, ret) -> (
      match unify_ty param arg with
      | Error _ as e -> e
      | Ok () -> Ok ret)

(** Build [arg1 -> arg2 -> ... -> ret] and unify with [t]. *)
let unify_fun_type t args ret =
  let expected = arrows args ret in
  unify_ty t expected

(* -------------------------------------------------------------------------- *)
(* Constructor / algebraic helpers                                             *)
(* -------------------------------------------------------------------------- *)

let unify_con name args t =
  unify_ty t (Con (name, args))

let unify_tuple elems t =
  match (elems, repr t) with
  | [], _ when is_unit t -> Ok ()
  | _, Tuple ts when List.length ts = List.length elems ->
      unify_list elems ts
  | _, Var _ -> unify_ty t (tuple elems)
  | _ ->
      err
        (Printf.sprintf "expected tuple of %d elements, got %s"
           (List.length elems) (ty_to_string t))

(* -------------------------------------------------------------------------- *)
(* Record helpers                                                              *)
(* -------------------------------------------------------------------------- *)

(** Unify [t] with a record containing at least [fields] (row polymorphism
    is not supported — exact match only). *)
let unify_record fields t =
  match repr t with
  | Record existing -> unify_records fields existing
  | Var _ -> unify_ty t (Record fields)
  | _ ->
      err
        (Printf.sprintf "expected a record type, got %s" (ty_to_string t))

(** Project field [name] from [t], unifying with a fresh record if needed. *)
let project_field t name =
  match repr t with
  | Record fields -> (
      match List.find_opt (fun (n, _) -> Ident.equal n name) fields with
      | Some (_, ty) -> Ok ty
      | None ->
          err
            (Printf.sprintf "record has no field %s" (Ident.to_string name)))
  | Var _ ->
      let field_ty = fresh () in
      (* Without row variables we cannot open records; fail soft by binding
         an exact singleton record — callers that need multi-field records
         should unify the full record first. *)
      (match unify_ty t (Record [ (name, field_ty) ]) with
      | Ok () -> Ok field_ty
      | Error _ as e -> e)
  | _ ->
      err
        (Printf.sprintf "cannot project field %s from %s"
           (Ident.to_string name) (ty_to_string t))

(* -------------------------------------------------------------------------- *)
(* Subsumption (instance check)                                                *)
(* -------------------------------------------------------------------------- *)

(** Check that [specific] is an instance of scheme [general].
    Instantiates [general] and unifies with [specific]. *)
let subsume ~(general : scheme) ~(specific : ty) =
  let g = instantiate general in
  unify_ty g specific

(** Check two types are equal under unification (may bind vars). *)
let force_equal a b = unify_ty a b

(* -------------------------------------------------------------------------- *)
(* Solving constraint lists                                                    *)
(* -------------------------------------------------------------------------- *)

type constraint_ =
  | Eq of ty * ty * Span.t option
  | Inst of scheme * ty * Span.t option

let solve_constraints (cs : constraint_ list) : unit res =
  let rec go = function
    | [] -> Ok ()
    | Eq (a, b, span) :: rest -> (
        match unify_ty a b with
        | Ok () -> go rest
        | Error (msg, sp) ->
            Error (msg, match span with Some _ as s -> s | None -> sp))
    | Inst (sch, t, span) :: rest -> (
        let inst = instantiate sch in
        match unify_ty inst t with
        | Ok () -> go rest
        | Error (msg, sp) ->
            Error (msg, match span with Some _ as s -> s | None -> sp))
  in
  go cs

(* -------------------------------------------------------------------------- *)
(* Diagnostic conversion                                                       *)
(* -------------------------------------------------------------------------- *)

let to_diagnostic ?(default_span = Span.dummy) (msg, span) =
  Diagnostic.error (Option.value span ~default:default_span) msg

let unify_or_diag ~span a b =
  match unify_span ~span a b with
  | Ok () -> Ok ()
  | Error e -> Error (to_diagnostic ~default_span:span e)

(* -------------------------------------------------------------------------- *)
(* Rigid / skolem handling                                                     *)
(* -------------------------------------------------------------------------- *)

(** Unify treating variables in [rigid] as non-unifiable skolem constants.
    Implemented by temporarily marking them with a sentinel link to a unique
    constructor — actually we check membership before binding. *)
let unify_rigid (rigid : TVarSet.t) a b =
  let rec go a b =
    let a = repr a in
    let b = repr b in
    match (a, b) with
    | Var tv1, Var tv2 when tv1 == tv2 || !tv1.id = !tv2.id -> Ok ()
    | Var tv, t when TVarSet.mem rigid tv -> (
        match t with
        | Var tv' when TVarSet.mem rigid tv' && !tv.id = !tv'.id -> Ok ()
        | Var tv' when not (TVarSet.mem rigid tv') -> go t (Var tv)
        | _ ->
            err
              (Printf.sprintf
                 "cannot unify rigid type variable '%s with %s"
                 (match !tv.name with
                 | Some n -> n
                 | None -> string_of_int !tv.id)
                 (ty_to_string t)))
    | t, Var tv when TVarSet.mem rigid tv -> go (Var tv) t
    | Var tv, t | t, Var tv -> unify_var tv t
    | Con (n1, a1), Con (n2, a2)
      when Ident.equal n1 n2 && List.length a1 = List.length a2 ->
        List.fold_left
          (fun acc (x, y) ->
            match acc with Error _ as e -> e | Ok () -> go x y)
          (Ok ()) (List.combine a1 a2)
    | Arrow (a1, b1), Arrow (a2, b2) -> (
        match go a1 a2 with Ok () -> go b1 b2 | Error _ as e -> e)
    | Tuple t1, Tuple t2 when List.length t1 = List.length t2 ->
        List.fold_left
          (fun acc (x, y) ->
            match acc with Error _ as e -> e | Ok () -> go x y)
          (Ok ()) (List.combine t1 t2)
    | Record f1, Record f2 -> unify_records f1 f2
    | _ when equal a b -> Ok ()
    | _ -> err (mismatch_msg a b)
  in
  go a b

(* -------------------------------------------------------------------------- *)
(* Debugging                                                                   *)
(* -------------------------------------------------------------------------- *)

let pp_error fmt (msg, span) =
  match span with
  | None -> Format.pp_print_string fmt msg
  | Some sp -> Format.fprintf fmt "%s at %a" msg Span.pp sp

let error_to_string e =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_error fmt e;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Attempt unification without mutating; uses a speculative copy.
    Note: because links are mutable, we save/restore var links. *)
let can_unify a b =
  let saved : (tvar ref * ty option * level) list ref = ref [] in
  let remember tv =
    if not (List.exists (fun (tv', _, _) -> !tv'.id = !tv.id) !saved) then
      saved := (tv, !tv.link, !tv.level) :: !saved
  in
  let rec collect t =
    match repr t with
    | Var tv -> remember tv
    | Con (_, args) -> List.iter collect args
    | Arrow (x, y) ->
        collect x;
        collect y
    | Tuple ts -> List.iter collect ts
    | Record fs -> List.iter (fun (_, ty) -> collect ty) fs
    | QVar _ -> ()
  in
  collect a;
  collect b;
  let result = unify_ty a b in
  List.iter
    (fun (tv, link, level) ->
      !tv.link <- link;
      !tv.level <- level)
    !saved;
  match result with Ok () -> true | Error _ -> false
