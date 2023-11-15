(** Unification with occurs-check and Rémy-style level updates. *)

let rec unify_var span tv_ref t =
  let t = Ty.repr t in
  match t with
  | Ty.TVar other when other == tv_ref -> ()
  | Ty.TVar { contents = Ty.Unbound tv' } as t' -> (
      match !tv_ref with
      | Ty.Unbound tv ->
          if tv.id = tv'.id then ()
          else if Ty.occurs tv t' then Error.occurs_error span ~tv ~ty:t'
          else (
            let lvl = if tv.level < tv'.level then tv.level else tv'.level in
            tv.level <- lvl;
            tv'.level <- lvl;
            tv_ref := Ty.Link t')
      | Ty.Link t2 -> unify ~span (Ty.repr t2) t'
      | Ty.Generic _ ->
          Error.raise_error ~kind:Error.Other ~actual:t' span
            "Cannot unify a quantified type variable")
  | t' -> (
      match !tv_ref with
      | Ty.Unbound tv ->
          if Ty.occurs tv t' then Error.occurs_error span ~tv ~ty:t'
          else (
            Ty.update_level tv.level t';
            tv_ref := Ty.Link t')
      | Ty.Link t2 -> unify ~span (Ty.repr t2) t'
      | Ty.Generic tv ->
          Error.raise_error ~kind:Error.Other ~actual:t' span
            (Printf.sprintf "Cannot unify quantified type variable '%s"
               (match tv.namehint with Some n -> n | None -> string_of_int tv.id)))

and unify ~span a b =
  let a = Ty.repr a in
  let b = Ty.repr b in
  match (a, b) with
  | Ty.TVar r1, Ty.TVar r2 when r1 == r2 -> ()
  | Ty.TVar r, t | t, Ty.TVar r -> unify_var span r t
  | Ty.TUnit, Ty.TUnit | Ty.TInt, Ty.TInt | Ty.TFloat, Ty.TFloat | Ty.TBool, Ty.TBool
  | Ty.TString, Ty.TString | Ty.TChar, Ty.TChar -> ()
  | Ty.TCon n1, Ty.TCon n2 when String.equal n1 n2 -> ()
  | Ty.TApp (f1, a1), Ty.TApp (f2, a2) -> unify ~span f1 f2; unify ~span a1 a2
  | Ty.TArrow (a1, b1), Ty.TArrow (a2, b2) -> unify ~span a1 a2; unify ~span b1 b2
  | Ty.TTuple xs, Ty.TTuple ys ->
      if List.length xs <> List.length ys then Error.mismatch span ~expected:a ~actual:b
      else List.iter2 (fun x y -> unify ~span x y) xs ys
  | Ty.TArray a1, Ty.TArray a2 | Ty.TRef a1, Ty.TRef a2 -> unify ~span a1 a2
  | Ty.TRecord fs, Ty.TRecord gs -> unify_records ~span ~expected:a ~actual:b fs gs
  | Ty.TUnit, Ty.TTuple [] | Ty.TTuple [], Ty.TUnit -> ()
  | Ty.TUnit, Ty.TCon "Unit" | Ty.TCon "Unit", Ty.TUnit -> ()
  | Ty.TInt, Ty.TCon "Int" | Ty.TCon "Int", Ty.TInt -> ()
  | Ty.TFloat, Ty.TCon "Float" | Ty.TCon "Float", Ty.TFloat -> ()
  | Ty.TBool, Ty.TCon "Bool" | Ty.TCon "Bool", Ty.TBool -> ()
  | Ty.TString, Ty.TCon "String" | Ty.TCon "String", Ty.TString -> ()
  | Ty.TChar, Ty.TCon "Char" | Ty.TCon "Char", Ty.TChar -> ()
  | _ -> Error.mismatch span ~expected:a ~actual:b

and unify_records ~span ~expected ~actual fs gs =
  let sorted = List.sort (fun (n1, _, _) (n2, _, _) -> String.compare n1 n2) in
  let fs = sorted fs and gs = sorted gs in
  if List.length fs <> List.length gs then Error.mismatch span ~expected ~actual
  else
    List.iter2
      (fun (n1, t1, m1) (n2, t2, m2) ->
        if n1 <> n2 || m1 <> m2 then Error.mismatch span ~expected ~actual
        else unify ~span t1 t2)
      fs gs

let try_unify ~span a b =
  try unify ~span a b; Ok () with Error.Type_error e -> Error e

let unify_list ~span pairs = List.iter (fun (a, b) -> unify ~span a b) pairs

let as_function ~span ty =
  match Ty.repr ty with
  | Ty.TArrow (a, b) -> (a, b)
  | Ty.TVar ({ contents = Ty.Unbound _ } as r) ->
      let a = Ty.fresh_var () in
      let b = Ty.fresh_var () in
      r := Ty.Link (Ty.TArrow (a, b));
      (a, b)
  | _ -> Error.not_a_function span ty

let apply ~span ~fun_ty ~arg_ty =
  let a, b = as_function ~span fun_ty in
  unify ~span a arg_ty;
  b

let apply_many ~span ~fun_ty ~arg_tys =
  List.fold_left (fun fty arg -> apply ~span ~fun_ty:fty ~arg_ty:arg) fun_ty arg_tys

let as_tuple ~span ~arity ty =
  match Ty.repr ty with
  | Ty.TTuple ts when List.length ts = arity -> ts
  | Ty.TVar ({ contents = Ty.Unbound _ } as r) ->
      let ts = List.init arity (fun _ -> Ty.fresh_var ()) in
      r := Ty.Link (Ty.TTuple ts);
      ts
  | Ty.TUnit when arity = 0 -> []
  | other ->
      Error.raise_error ~kind:Error.Arity_mismatch ~actual:other span
        (Printf.sprintf "Expected a tuple of arity %d, got %s" arity (Ty.to_string other))

let project_field ~span ty name =
  let name_s = Ident.name name in
  match Ty.repr ty with
  | Ty.TRecord fields -> (
      match List.find_opt (fun (n, _, _) -> String.equal n name_s) fields with
      | Some (_, fty, _) -> fty
      | None -> Error.unbound_field span name)
  | Ty.TVar ({ contents = Ty.Unbound _ } as r) ->
      let fty = Ty.fresh_var () in
      r := Ty.Link (Ty.TRecord [ (name_s, fty, false) ]);
      fty
  | other ->
      Error.raise_error ~kind:Error.Not_a_record ~actual:other span
        (Printf.sprintf "Cannot project field %s from %s" name_s (Ty.to_string other))

let with_level f =
  Ty.enter_level ();
  match f () with
  | exception exn -> Ty.leave_level (); raise exn
  | v -> Ty.leave_level (); v

let can_unify a b =
  let saved = ref [] in
  let seen = Hashtbl.create 16 in
  let remember r tv =
    if not (Hashtbl.mem seen tv.Ty.id) then (
      Hashtbl.add seen tv.id ();
      saved := (r, !r, tv.level) :: !saved)
  in
  let rec collect t =
    match Ty.repr t with
    | Ty.TVar ({ contents = Ty.Unbound tv } as r)
    | Ty.TVar ({ contents = Ty.Generic tv } as r) -> remember r tv
    | Ty.TApp (a, b) | Ty.TArrow (a, b) -> collect a; collect b
    | Ty.TTuple ts -> List.iter collect ts
    | Ty.TArray t | Ty.TRef t -> collect t
    | Ty.TRecord fs -> List.iter (fun (_, ty, _) -> collect ty) fs
    | _ -> ()
  in
  collect a; collect b;
  let ok = match try_unify ~span:Span.dummy a b with Ok () -> true | Error _ -> false in
  List.iter
    (fun (r, state, level) ->
      r := state;
      match state with Ty.Unbound tv | Ty.Generic tv -> tv.level <- level | Ty.Link _ -> ())
    !saved;
  ok
