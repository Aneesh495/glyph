(** Type substitutions, free-variable analysis, and scheme utilities.

    A substitution maps type-variable ids (or quantified names) to types.
    Composition and application are eager; path compression in [Ty.repr]
    remains the source of truth for unification links. *)

open Ty

(* -------------------------------------------------------------------------- *)
(* Substitution type                                                           *)
(* -------------------------------------------------------------------------- *)

type t = {
  by_id : (int, ty) Hashtbl.t;
  by_name : (string, ty) Hashtbl.t;
}

let empty () : t =
  { by_id = Hashtbl.create 16; by_name = Hashtbl.create 8 }

let is_empty s =
  Hashtbl.length s.by_id = 0 && Hashtbl.length s.by_name = 0

let singleton_id id ty =
  let s = empty () in
  Hashtbl.add s.by_id id ty;
  s

let singleton_name name ty =
  let s = empty () in
  Hashtbl.add s.by_name name ty;
  s

let of_id_list (pairs : (int * ty) list) =
  let s = empty () in
  List.iter (fun (id, ty) -> Hashtbl.replace s.by_id id ty) pairs;
  s

let of_name_list (pairs : (string * ty) list) =
  let s = empty () in
  List.iter (fun (name, ty) -> Hashtbl.replace s.by_name name ty) pairs;
  s

let of_tvar_list (pairs : (tvar ref * ty) list) =
  let s = empty () in
  List.iter (fun (tv, ty) -> Hashtbl.replace s.by_id !tv.id ty) pairs;
  s

let add_id s id ty =
  Hashtbl.replace s.by_id id ty;
  s

let add_name s name ty =
  Hashtbl.replace s.by_name name ty;
  s

let add_tvar s (tv : tvar ref) ty =
  Hashtbl.replace s.by_id !tv.id ty;
  s

let find_id s id = Hashtbl.find_opt s.by_id id
let find_name s name = Hashtbl.find_opt s.by_name name

let mem_id s id = Hashtbl.mem s.by_id id
let mem_name s name = Hashtbl.mem s.by_name name

let remove_id s id =
  Hashtbl.remove s.by_id id;
  s

let remove_name s name =
  Hashtbl.remove s.by_name name;
  s

let copy s =
  let s' = empty () in
  Hashtbl.iter (fun k v -> Hashtbl.add s'.by_id k v) s.by_id;
  Hashtbl.iter (fun k v -> Hashtbl.add s'.by_name k v) s.by_name;
  s'

let merge a b =
  (* Prefer bindings from [b] on conflict. *)
  let s = copy a in
  Hashtbl.iter (fun k v -> Hashtbl.replace s.by_id k v) b.by_id;
  Hashtbl.iter (fun k v -> Hashtbl.replace s.by_name k v) b.by_name;
  s

let domain_ids s =
  Hashtbl.fold (fun id _ acc -> id :: acc) s.by_id []

let domain_names s =
  Hashtbl.fold (fun name _ acc -> name :: acc) s.by_name []

let bindings_id s =
  Hashtbl.fold (fun id ty acc -> (id, ty) :: acc) s.by_id []

let bindings_name s =
  Hashtbl.fold (fun name ty acc -> (name, ty) :: acc) s.by_name []

let cardinal s = Hashtbl.length s.by_id + Hashtbl.length s.by_name

(* -------------------------------------------------------------------------- *)
(* Application                                                                 *)
(* -------------------------------------------------------------------------- *)

(** Apply substitution [s] to type [t], following [Ty.repr] first. *)
let rec apply (s : t) (t : ty) : ty =
  match repr t with
  | Var tv -> (
      match Hashtbl.find_opt s.by_id !tv.id with
      | Some t' -> apply s t'
      | None -> (
          match !tv.name with
          | Some name -> (
              match Hashtbl.find_opt s.by_name name with
              | Some t' -> apply s t'
              | None -> Var tv)
          | None -> Var tv))
  | QVar name -> (
      match Hashtbl.find_opt s.by_name name with
      | Some t' -> apply s t'
      | None -> QVar name)
  | Con (n, args) -> Con (n, List.map (apply s) args)
  | Arrow (a, b) -> Arrow (apply s a, apply s b)
  | Tuple ts -> Tuple (List.map (apply s) ts)
  | Record fields ->
      Record (List.map (fun (n, ty) -> (n, apply s ty)) fields)

let apply_list s ts = List.map (apply s) ts

let apply_scheme s (Forall (qs, body)) =
  (* Do not substitute for quantified names. *)
  let s' = copy s in
  List.iter (fun q -> Hashtbl.remove s'.by_name q) qs;
  Forall (qs, apply s' body)

(** Compose [s2] after [s1]: [apply (compose s1 s2) t = apply s2 (apply s1 t)]. *)
let compose (s1 : t) (s2 : t) : t =
  let s = empty () in
  Hashtbl.iter
    (fun id ty -> Hashtbl.add s.by_id id (apply s2 ty))
    s1.by_id;
  Hashtbl.iter
    (fun name ty -> Hashtbl.add s.by_name name (apply s2 ty))
    s1.by_name;
  Hashtbl.iter
    (fun id ty ->
      if not (Hashtbl.mem s.by_id id) then Hashtbl.add s.by_id id ty)
    s2.by_id;
  Hashtbl.iter
    (fun name ty ->
      if not (Hashtbl.mem s.by_name name) then Hashtbl.add s.by_name name ty)
    s2.by_name;
  s

(** Restrict substitution domain to variables free in [t]. *)
let restrict s t =
  let free = free_tvars t in
  let s' = empty () in
  Hashtbl.iter
    (fun id ty ->
      if
        TVarSet.fold (fun tv acc -> acc || !tv.id = id) free false
      then Hashtbl.add s'.by_id id ty)
    s.by_id;
  Hashtbl.iter
    (fun name ty ->
      let needed =
        List.exists (String.equal name) (free_qvars t)
        || TVarSet.fold
             (fun tv acc ->
               acc
               ||
               match !tv.name with
               | Some n -> String.equal n name
               | None -> false)
             free false
      in
      if needed then Hashtbl.add s'.by_name name ty)
    s.by_name;
  s'

(* -------------------------------------------------------------------------- *)
(* Free type variables (extended helpers)                                      *)
(* -------------------------------------------------------------------------- *)

let ftv = free_tvars
let ftv_list = free_tvars_list

let ftv_scheme = free_tvars_scheme

let ftv_list_of_tys ts =
  let s = TVarSet.create () in
  List.iter (collect_free_tvars s) ts;
  s

let ftv_env_bindings (schemes : scheme list) =
  let s = TVarSet.create () in
  List.iter
    (fun sch ->
      let s' = free_tvars_scheme sch in
      TVarSet.iter (TVarSet.add s) s')
    schemes;
  s

(** Difference: variables in [a] that are not in [b]. *)
let tvars_diff a b =
  let s = TVarSet.create () in
  TVarSet.iter
    (fun tv -> if not (TVarSet.mem b tv) then TVarSet.add s tv)
    a;
  s

(** Union of two tvar sets. *)
let tvars_union a b =
  let s = TVarSet.copy a in
  TVarSet.iter (TVarSet.add s) b;
  s

(** Intersection. *)
let tvars_inter a b =
  let s = TVarSet.create () in
  TVarSet.iter
    (fun tv -> if TVarSet.mem b tv then TVarSet.add s tv)
    a;
  s

(** Variables free in [t] but not bound by the current environment set. *)
let free_in_context env_ftv t = tvars_diff (free_tvars t) env_ftv

(* -------------------------------------------------------------------------- *)
(* Occurs / safety                                                             *)
(* -------------------------------------------------------------------------- *)

let occurs_in_subst (tv : tvar ref) (s : t) =
  Hashtbl.fold
    (fun _ ty acc -> acc || occur_check tv ty)
    s.by_id false
  || Hashtbl.fold
       (fun _ ty acc -> acc || occur_check tv ty)
       s.by_name false

(** Build a substitution from a list of equations by binding vars.
    Does not perform full unification — only direct [Var = ty] pairs. *)
let from_equations (eqs : (ty * ty) list) : (t, string) result =
  let s = empty () in
  let rec go = function
    | [] -> Ok s
    | (a, b) :: rest -> (
        let a = repr (apply s a) in
        let b = repr (apply s b) in
        match (a, b) with
        | Var tv, t | t, Var tv ->
            if occur_check tv t then
              Error
                (Printf.sprintf "occurs check in substitution for '%d" !tv.id)
            else (
              Hashtbl.replace s.by_id !tv.id t;
              go rest)
        | QVar n, t | t, QVar n ->
            Hashtbl.replace s.by_name n t;
            go rest
        | _ when equal a b -> go rest
        | _ ->
            Error
              (Printf.sprintf "cannot form substitution: %s vs %s"
                 (ty_to_string a) (ty_to_string b)))
  in
  go eqs

(* -------------------------------------------------------------------------- *)
(* Instantiation helpers                                                       *)
(* -------------------------------------------------------------------------- *)

(** Explicit instantiation: map quantified names to given types. *)
let instantiate_with (Forall (qs, body)) (args : ty list) =
  if List.length qs <> List.length args then
    Error
      (Printf.sprintf
         "instantiate_with: expected %d type arguments, got %d"
         (List.length qs) (List.length args))
  else
    let s = of_name_list (List.combine qs args) in
    Ok (apply s body)

(** Fresh instantiation returning both the type and the substitution used. *)
let instantiate_ex (Forall (qs, body)) =
  let pairs =
    List.map
      (fun q ->
        let tv = new_var ~name:q () in
        (q, tv))
      qs
  in
  let s = of_name_list pairs in
  (apply s body, s)

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                             *)
(* -------------------------------------------------------------------------- *)

let pp fmt s =
  let id_bindings = bindings_id s in
  let name_bindings = bindings_name s in
  Format.fprintf fmt "[";
  let first = ref true in
  List.iter
    (fun (id, ty) ->
      if not !first then Format.fprintf fmt "; ";
      first := false;
      Format.fprintf fmt "'%d ↦ %a" id pp_ty ty)
    id_bindings;
  List.iter
    (fun (name, ty) ->
      if not !first then Format.fprintf fmt "; ";
      first := false;
      Format.fprintf fmt "'%s ↦ %a" name pp_ty ty)
    name_bindings;
  Format.fprintf fmt "]"

let to_string s =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt s;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* -------------------------------------------------------------------------- *)
(* Alpha renaming of schemes                                                   *)
(* -------------------------------------------------------------------------- *)

let refresh_scheme (Forall (qs, body)) =
  let pairs =
    List.map
      (fun q ->
        let q' =
          let tv = new_tvar ~name:q () in
          match !tv.name with
          | Some n -> n ^ "_" ^ string_of_int !tv.id
          | None -> q ^ "_" ^ string_of_int !tv.id
        in
        (q, q'))
      qs
  in
  let s =
    of_name_list (List.map (fun (q, q') -> (q, QVar q')) pairs)
  in
  Forall (List.map snd pairs, apply s body)

(** Strip all QVars by replacing with fresh Vars (partial instantiate). *)
let open_scheme (Forall (qs, body)) =
  let pairs = List.map (fun q -> (q, new_var ~name:q ())) qs in
  apply (of_name_list pairs) body

(** Close a type by quantifying all free vars. *)
let close_over t = generalize t

(** Close quantifying only vars not free in [avoid]. *)
let close_over_excluding avoid t =
  let avoid_set = free_tvars avoid in
  let level = get_level () in
  let quantified : (tvar ref * string) list ref = ref [] in
  let next_name =
    let n = ref 0 in
    fun () ->
      let i = !n in
      incr n;
      let letter = Char.chr (Char.code 'a' + (i mod 26)) in
      if i < 26 then String.make 1 letter
      else Printf.sprintf "%c%d" letter (i / 26)
  in
  let rec go t =
    match repr t with
    | Var tv ->
        if !tv.level > level && not (TVarSet.mem avoid_set tv) then (
          match
            List.find_opt (fun (tv', _) -> !tv'.id = !tv.id) !quantified
          with
          | Some (_, name) -> QVar name
          | None ->
              let name =
                match !tv.name with Some n -> n | None -> next_name ()
              in
              quantified := (tv, name) :: !quantified;
              QVar name)
        else Var tv
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields ->
        Record (List.map (fun (n, ty) -> (n, go ty)) fields)
    | QVar _ as q -> q
  in
  let body = go t in
  Forall (List.rev_map snd !quantified, body)

(* -------------------------------------------------------------------------- *)
(* Type matching (one-way unification / pattern)                               *)
(* -------------------------------------------------------------------------- *)

(** Match [pattern] against [subject], binding QVars / Vars in pattern.
    Does not mutate unification links. *)
let match_ty ~(pattern : ty) ~(subject : ty) : (t, string) result =
  let s = empty () in
  let rec go p subj =
    let p = repr (apply s p) in
    let subj = repr (apply s subj) in
    match (p, subj) with
    | Var tv, t ->
        if occur_check tv t && not (equal (Var tv) t) then
          Error "occurs check in match"
        else (
          Hashtbl.replace s.by_id !tv.id t;
          Ok ())
    | QVar name, t ->
        Hashtbl.replace s.by_name name t;
        Ok ()
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
    | Record f1, Record f2 ->
        let sort fs =
          List.sort (fun (a, _) (b, _) -> Ident.compare a b) fs
        in
        let f1 = sort f1 and f2 = sort f2 in
        if List.length f1 <> List.length f2 then
          Error "record arity mismatch"
        else
          List.fold_left
            (fun acc ((n1, t1), (n2, t2)) ->
              match acc with
              | Error _ as e -> e
              | Ok () ->
                  if not (Ident.equal n1 n2) then
                    Error
                      (Printf.sprintf "record field mismatch: %s vs %s"
                         (Ident.to_string n1) (Ident.to_string n2))
                  else go t1 t2)
            (Ok ())
            (List.combine f1 f2)
    | _ when equal p subj -> Ok ()
    | _ ->
        Error
          (Printf.sprintf "type match failed: %s vs %s" (ty_to_string p)
             (ty_to_string subj))
  in
  match go pattern subject with Ok () -> Ok s | Error e -> Error e

(* -------------------------------------------------------------------------- *)
(* Normalization                                                               *)
(* -------------------------------------------------------------------------- *)

(** Fully chase links and rewrite the tree. *)
let normalize t =
  let rec go t =
    match repr t with
    | Var _ as v -> v
    | QVar _ as q -> q
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields ->
        Record (List.map (fun (n, ty) -> (n, go ty)) fields)
  in
  go t

let normalize_scheme (Forall (qs, body)) = Forall (qs, normalize body)

(** Collect all constructor names appearing in a type. *)
let constructors_in t =
  let acc = ref Ident.Set.empty in
  iter
    (function
      | Con (n, _) -> acc := Ident.Set.add n !acc
      | _ -> ())
    t;
  !acc
