(** Internal type representation for Glyph's Hindley–Milner type system.

    Types use mutable levels and union-find links (Rémy-style) so that
    generalization and unification share a single representation. *)

(* -------------------------------------------------------------------------- *)
(* Type variables                                                              *)
(* -------------------------------------------------------------------------- *)

type level = int

type tvar = {
  id : int;
  mutable level : level;
  mutable name : string option;
  mutable link : ty option;
}

and ty =
  | Var of tvar ref
  | Con of Ident.t * ty list
  | Arrow of ty * ty
  | Tuple of ty list
  | Record of (Ident.t * ty) list
  | QVar of string
(** [QVar] appears only inside quantified schemes before instantiation. *)

type scheme = Forall of string list * ty
(** [Forall (qs, body)] binds the quantified names [qs] which appear as
    [QVar] nodes in [body]. After generalization we may also keep unbound
    [Var] nodes whose level is generic (see [generic_level]). *)

(* -------------------------------------------------------------------------- *)
(* Levels                                                                      *)
(* -------------------------------------------------------------------------- *)

(** Current nesting level for let-generalization. *)
let current_level : level ref = ref 1

(** Marker level meaning "generalized / quantified". *)
let generic_level = 1_000_000_000

(** Marker level meaning "not yet assigned / unbound sentinel". *)
let no_level = -1

let enter_level () = incr current_level
let exit_level () = decr current_level
let get_level () = !current_level
let reset_level () = current_level := 1
let set_level n = current_level := n

(* -------------------------------------------------------------------------- *)
(* Fresh variables                                                             *)
(* -------------------------------------------------------------------------- *)

let tvar_counter = ref 0

let reset_tvar_counter () = tvar_counter := 0

let new_tvar ?name ?(level = !current_level) () : tvar ref =
  incr tvar_counter;
  ref { id = !tvar_counter; level; name; link = None }

let new_var ?name ?(level = !current_level) () : ty =
  Var (new_tvar ?name ~level ())

let fresh () = new_var ()

let fresh_named name = new_var ~name ()

let tvar_id (tv : tvar ref) = !tv.id
let tvar_level (tv : tvar ref) = !tv.level
let tvar_name (tv : tvar ref) = !tv.name

let set_tvar_level (tv : tvar ref) level = !tv.level <- level

(* -------------------------------------------------------------------------- *)
(* Path compression / forced representative                                    *)
(* -------------------------------------------------------------------------- *)

let rec repr (t : ty) : ty =
  match t with
  | Var tv -> (
      match !tv.link with
      | None -> t
      | Some t' ->
          let r = repr t' in
          !tv.link <- Some r;
          r)
  | _ -> t

let force = repr

(** Follow links without allocating; returns the same as [repr]. *)
let rec unlink t =
  match t with
  | Var tv -> (
      match !tv.link with
      | Some t' ->
          let r = unlink t' in
          !tv.link <- Some r;
          r
      | None -> t)
  | _ -> t

(* -------------------------------------------------------------------------- *)
(* Occurs check                                                                *)
(* -------------------------------------------------------------------------- *)

let rec occur_check (tv : tvar ref) (t : ty) : bool =
  match repr t with
  | Var tv' -> tv == tv' || !tv'.id = !tv.id
  | Con (_, args) -> List.exists (occur_check tv) args
  | Arrow (a, b) -> occur_check tv a || occur_check tv b
  | Tuple ts -> List.exists (occur_check tv) ts
  | Record fields -> List.exists (fun (_, ty) -> occur_check tv ty) fields
  | QVar _ -> false

let occurs tv t = occur_check tv t

(* -------------------------------------------------------------------------- *)
(* Level adjustment (min-level update on binding)                              *)
(* -------------------------------------------------------------------------- *)

let rec update_level (min_level : level) (t : ty) : unit =
  match repr t with
  | Var tv -> if !tv.level > min_level then !tv.level <- min_level
  | Con (_, args) -> List.iter (update_level min_level) args
  | Arrow (a, b) ->
      update_level min_level a;
      update_level min_level b
  | Tuple ts -> List.iter (update_level min_level) ts
  | Record fields -> List.iter (fun (_, ty) -> update_level min_level ty) fields
  | QVar _ -> ()

(** Lower levels of free vars in [t] to at most [level]. *)
let adjust_levels level t = update_level level t

(* -------------------------------------------------------------------------- *)
(* Binding a type variable                                                     *)
(* -------------------------------------------------------------------------- *)

let bind_tvar (tv : tvar ref) (t : ty) : (unit, string) result =
  let t = repr t in
  match t with
  | Var tv' when tv == tv' || !tv'.id = !tv.id -> Ok ()
  | _ ->
      if occur_check tv t then
        Error
          (Printf.sprintf "occurs check failed: type variable '%s occurs in type"
             (match !tv.name with
             | Some n -> n
             | None -> string_of_int !tv.id))
      else (
        update_level !tv.level t;
        !tv.link <- Some t;
        Ok ())

let link_tvar tv t =
  match bind_tvar tv t with
  | Ok () -> ()
  | Error msg -> invalid_arg msg

(* -------------------------------------------------------------------------- *)
(* Builtin type constructors                                                   *)
(* -------------------------------------------------------------------------- *)

module Builtin = struct
  let int_id = Ident.Intern.intern "int"
  let float_id = Ident.Intern.intern "float"
  let bool_id = Ident.Intern.intern "bool"
  let string_id = Ident.Intern.intern "string"
  let char_id = Ident.Intern.intern "char"
  let unit_id = Ident.Intern.intern "unit"
  let list_id = Ident.Intern.intern "list"
  let option_id = Ident.Intern.intern "option"
  let result_id = Ident.Intern.intern "result"
  let array_id = Ident.Intern.intern "array"
  let ref_id = Ident.Intern.intern "ref"
  let exn_id = Ident.Intern.intern "exn"

  let int = Con (int_id, [])
  let float = Con (float_id, [])
  let bool = Con (bool_id, [])
  let string = Con (string_id, [])
  let char = Con (char_id, [])
  let unit = Con (unit_id, [])

  let list a = Con (list_id, [ a ])
  let option a = Con (option_id, [ a ])
  let result a b = Con (result_id, [ a; b ])
  let array a = Con (array_id, [ a ])
  let ref_ a = Con (ref_id, [ a ])
  let exn = Con (exn_id, [])

  let names =
    [
      int_id;
      float_id;
      bool_id;
      string_id;
      char_id;
      unit_id;
      list_id;
      option_id;
      result_id;
      array_id;
      ref_id;
      exn_id;
    ]
end

let ty_int = Builtin.int
let ty_float = Builtin.float
let ty_bool = Builtin.bool
let ty_string = Builtin.string
let ty_char = Builtin.char
let ty_unit = Builtin.unit
let ty_list = Builtin.list
let ty_option = Builtin.option
let ty_result = Builtin.result
let ty_array = Builtin.array

(* -------------------------------------------------------------------------- *)
(* Constructors                                                                *)
(* -------------------------------------------------------------------------- *)

let arrow a b = Arrow (a, b)

let ( @-> ) = arrow

let arrows args ret =
  List.fold_right (fun a r -> Arrow (a, r)) args ret

let tuple = function
  | [] -> Builtin.unit
  | [ t ] -> t
  | ts -> Tuple ts

let record fields = Record fields

let con name args = Con (name, args)

let qvar name = QVar name

let mono t = Forall ([], t)

let poly qs t = Forall (qs, t)

(* -------------------------------------------------------------------------- *)
(* Equality (structural, after forcing)                                        *)
(* -------------------------------------------------------------------------- *)

let rec equal a b =
  match (repr a, repr b) with
  | Var tv1, Var tv2 -> tv1 == tv2 || !tv1.id = !tv2.id
  | Con (n1, a1), Con (n2, a2) ->
      Ident.equal n1 n2 && List.length a1 = List.length a2
      && List.for_all2 equal a1 a2
  | Arrow (a1, b1), Arrow (a2, b2) -> equal a1 a2 && equal b1 b2
  | Tuple t1, Tuple t2 ->
      List.length t1 = List.length t2 && List.for_all2 equal t1 t2
  | Record f1, Record f2 ->
      let sort fs =
        List.sort (fun (a, _) (b, _) -> Ident.compare a b) fs
      in
      let f1 = sort f1 and f2 = sort f2 in
      List.length f1 = List.length f2
      && List.for_all2
           (fun (n1, t1) (n2, t2) -> Ident.equal n1 n2 && equal t1 t2)
           f1 f2
  | QVar s1, QVar s2 -> String.equal s1 s2
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* Free type variables                                                         *)
(* -------------------------------------------------------------------------- *)

module TVarSet = struct
  type t = (int, tvar ref) Hashtbl.t

  let create () = Hashtbl.create 16
  let mem s tv = Hashtbl.mem s !tv.id
  let add s tv = Hashtbl.replace s !tv.id tv
  let remove s tv = Hashtbl.remove s !tv.id
  let iter f s = Hashtbl.iter (fun _ tv -> f tv) s
  let fold f s acc =
    Hashtbl.fold (fun _ tv acc -> f tv acc) s acc
  let to_list s = fold (fun tv acc -> tv :: acc) s []
  let of_list tvs =
    let s = create () in
    List.iter (add s) tvs;
    s
  let cardinal s = Hashtbl.length s
  let is_empty s = Hashtbl.length s = 0
  let clear s = Hashtbl.clear s
  let copy s =
    let s' = create () in
    Hashtbl.iter (fun k v -> Hashtbl.add s' k v) s;
    s'
end

let rec collect_free_tvars (acc : TVarSet.t) (t : ty) : unit =
  match repr t with
  | Var tv -> TVarSet.add acc tv
  | Con (_, args) -> List.iter (collect_free_tvars acc) args
  | Arrow (a, b) ->
      collect_free_tvars acc a;
      collect_free_tvars acc b
  | Tuple ts -> List.iter (collect_free_tvars acc) ts
  | Record fields ->
      List.iter (fun (_, ty) -> collect_free_tvars acc ty) fields
  | QVar _ -> ()

let free_tvars t =
  let s = TVarSet.create () in
  collect_free_tvars s t;
  s

let free_tvars_list t = TVarSet.to_list (free_tvars t)

let free_qvars t =
  let rec go acc t =
    match repr t with
    | QVar s -> if List.mem s acc then acc else s :: acc
    | Var _ -> acc
    | Con (_, args) -> List.fold_left go acc args
    | Arrow (a, b) -> go (go acc a) b
    | Tuple ts -> List.fold_left go acc ts
    | Record fields -> List.fold_left (fun acc (_, ty) -> go acc ty) acc fields
  in
  List.rev (go [] t)

(* -------------------------------------------------------------------------- *)
(* Instantiation                                                               *)
(* -------------------------------------------------------------------------- *)

(** Replace [QVar] names with fresh [Var]s at the current level. *)
let instantiate (Forall (qs, body) : scheme) : ty =
  let subst =
    List.fold_left
      (fun m q ->
        let tv = new_var ~name:q () in
        (q, tv) :: m)
      [] qs
  in
  let rec go t =
    match repr t with
    | QVar name -> (
        match List.assoc_opt name subst with
        | Some t' -> t'
        | None -> QVar name)
    | Var _ as v -> v
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
  in
  go body

(** Instantiate a scheme that was generalized with mutable levels: every
    variable at [generic_level] is replaced by a fresh variable. *)
let instantiate_levels (t : ty) : ty =
  let memo : (int, ty) Hashtbl.t = Hashtbl.create 16 in
  let rec go t =
    match repr t with
    | Var tv when !tv.level = generic_level -> (
        match Hashtbl.find_opt memo !tv.id with
        | Some t' -> t'
        | None ->
            let t' = new_var ?name:!tv.name () in
            Hashtbl.add memo !tv.id t';
            t')
    | Var _ as v -> v
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
    | QVar _ as q -> q
  in
  go t

let instantiate_scheme (Forall (qs, body)) =
  if qs = [] then instantiate_levels body else instantiate (Forall (qs, body))

(* -------------------------------------------------------------------------- *)
(* Generalization                                                              *)
(* -------------------------------------------------------------------------- *)

(** Generalize free variables whose level is greater than [current_level].
    Returns a scheme using both [QVar] names and level-marked generic vars. *)
let generalize (t : ty) : scheme =
  let level = !current_level in
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
        if !tv.level > level then (
          match List.find_opt (fun (tv', _) -> !tv'.id = !tv.id) !quantified with
          | Some (_, name) -> QVar name
          | None ->
              let name =
                match !tv.name with
                | Some n -> n
                | None -> next_name ()
              in
              !tv.level <- generic_level;
              quantified := (tv, name) :: !quantified;
              QVar name)
        else Var tv
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
    | QVar _ as q -> q
  in
  let body = go t in
  let qs = List.rev_map snd !quantified in
  Forall (qs, body)

(** Generalize in place by bumping levels only (no [QVar] rewrite). *)
let generalize_inplace (t : ty) : ty =
  let level = !current_level in
  let rec go t =
    match repr t with
    | Var tv ->
        if !tv.level > level then !tv.level <- generic_level;
        Var tv
    | Con (n, args) ->
        List.iter go args;
        Con (n, args)
    | Arrow (a, b) ->
        go a;
        go b;
        Arrow (a, b)
    | Tuple ts ->
        List.iter go ts;
        Tuple ts
    | Record fields ->
        List.iter (fun (_, ty) -> go ty) fields;
        Record fields
    | QVar _ as q -> q
  in
  ignore (go t);
  t

let scheme_of_ty t = generalize t

(* -------------------------------------------------------------------------- *)
(* Scheme free variables                                                       *)
(* -------------------------------------------------------------------------- *)

let free_tvars_scheme (Forall (qs, body)) =
  let s = free_tvars body in
  List.iter
    (fun q ->
      TVarSet.iter
        (fun tv ->
          match !tv.name with
          | Some n when String.equal n q -> TVarSet.remove s tv
          | _ -> ())
        s)
    qs;
  (* Also drop QVars by not counting them; free_tvars already ignores QVar. *)
  s

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                             *)
(* -------------------------------------------------------------------------- *)

let is_atomic = function
  | Var _ | QVar _ | Con (_, []) -> true
  | _ -> false

let rec pp_ty fmt t =
  match repr t with
  | Var tv -> (
      match !tv.name with
      | Some n -> Format.fprintf fmt "'%s" n
      | None ->
          if !tv.level = generic_level then
            Format.fprintf fmt "'_%d" !tv.id
          else Format.fprintf fmt "'%d" !tv.id)
  | QVar s -> Format.fprintf fmt "'%s" s
  | Con (name, []) -> Ident.pp fmt name
  | Con (name, [ arg ]) ->
      if is_atomic arg then Format.fprintf fmt "%a %a" pp_ty arg Ident.pp name
      else Format.fprintf fmt "(%a) %a" pp_ty arg Ident.pp name
  | Con (name, args) ->
      Format.fprintf fmt "(%a) %a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           pp_ty)
        args Ident.pp name
  | Arrow (a, b) ->
      (match repr a with
      | Arrow _ -> Format.fprintf fmt "(%a)" pp_ty a
      | _ -> pp_ty fmt a);
      Format.fprintf fmt " -> ";
      pp_ty fmt b
  | Tuple [] -> Format.pp_print_string fmt "unit"
  | Tuple [ t ] -> pp_ty fmt t
  | Tuple ts ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " * ")
        (fun fmt t ->
          match repr t with
          | Arrow _ | Tuple _ -> Format.fprintf fmt "(%a)" pp_ty t
          | _ -> pp_ty fmt t)
        fmt ts
  | Record fields ->
      Format.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           (fun fmt (n, ty) ->
             Format.fprintf fmt "%a : %a" Ident.pp n pp_ty ty))
        fields

let pp_scheme fmt (Forall (qs, body)) =
  match qs with
  | [] -> pp_ty fmt body
  | _ ->
      Format.fprintf fmt "forall %a. %a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
           (fun fmt q -> Format.fprintf fmt "'%s" q))
        qs pp_ty body

let ty_to_string t =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_ty fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let scheme_to_string s =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_scheme fmt s;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* -------------------------------------------------------------------------- *)
(* Deconstructors / helpers                                                    *)
(* -------------------------------------------------------------------------- *)

let as_arrow t =
  match repr t with
  | Arrow (a, b) -> Some (a, b)
  | _ -> None

let as_tuple t =
  match repr t with
  | Tuple ts -> Some ts
  | Con (n, []) when Ident.equal n Builtin.unit_id -> Some []
  | _ -> None

let as_con t =
  match repr t with
  | Con (n, args) -> Some (n, args)
  | _ -> None

let as_record t =
  match repr t with
  | Record fields -> Some fields
  | _ -> None

let is_var t =
  match repr t with
  | Var _ -> true
  | _ -> false

let is_unit t =
  match repr t with
  | Con (n, []) -> Ident.equal n Builtin.unit_id
  | Tuple [] -> true
  | _ -> false

(** Deep copy a type, preserving sharing of unbound vars via a memo table. *)
let copy_ty t =
  let memo : (int, ty) Hashtbl.t = Hashtbl.create 16 in
  let rec go t =
    match repr t with
    | Var tv -> (
        match Hashtbl.find_opt memo !tv.id with
        | Some t' -> t'
        | None ->
            let t' =
              Var
                (ref
                   {
                     id = !tv.id;
                     level = !tv.level;
                     name = !tv.name;
                     link = None;
                   })
            in
            Hashtbl.add memo !tv.id t';
            t')
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
    | QVar s -> QVar s
  in
  go t

(** Substitute QVar names according to an association list. *)
let subst_qvars (mapping : (string * ty) list) t =
  let rec go t =
    match repr t with
    | QVar name -> (
        match List.assoc_opt name mapping with
        | Some t' -> t'
        | None -> QVar name)
    | Var _ as v -> v
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
  in
  go t

(** Build a scheme from quantified tvar refs (level-style). *)
let quantify (tvs : tvar ref list) (body : ty) : scheme =
  let qs =
    List.mapi
      (fun i tv ->
        let name =
          match !tv.name with
          | Some n -> n
          | None ->
              let letter = Char.chr (Char.code 'a' + (i mod 26)) in
              if i < 26 then String.make 1 letter
              else Printf.sprintf "%c%d" letter (i / 26)
        in
        !tv.level <- generic_level;
        !tv.name <- Some name;
        (tv, name))
      tvs
  in
  let mapping =
    List.map (fun (tv, name) -> (name, QVar name)) qs
  in
  (* Rewrite body to use QVar for quantified vars. *)
  let rec go t =
    match repr t with
    | Var tv -> (
        match List.find_opt (fun (tv', _) -> !tv'.id = !tv.id) qs with
        | Some (_, name) -> QVar name
        | None -> Var tv)
    | Con (n, args) -> Con (n, List.map go args)
    | Arrow (a, b) -> Arrow (go a, go b)
    | Tuple ts -> Tuple (List.map go ts)
    | Record fields -> Record (List.map (fun (n, ty) -> (n, go ty)) fields)
    | QVar _ as q -> q
  in
  ignore mapping;
  Forall (List.map snd qs, go body)

(** Reset mutable type-system state (levels + tvar counter). Useful in tests. *)
let reset () =
  reset_level ();
  reset_tvar_counter ()

(** Walk a type applying [f] to every node after [repr]. *)
let iter f t =
  let rec go t =
    let t = repr t in
    f t;
    match t with
    | Var _ | QVar _ -> ()
    | Con (_, args) -> List.iter go args
    | Arrow (a, b) ->
        go a;
        go b
    | Tuple ts -> List.iter go ts
    | Record fields -> List.iter (fun (_, ty) -> go ty) fields
  in
  go t

let map_ty f t =
  let rec go t =
    let t = repr t in
    match t with
    | Var _ | QVar _ -> f t
    | Con (n, args) -> f (Con (n, List.map go args))
    | Arrow (a, b) -> f (Arrow (go a, go b))
    | Tuple ts -> f (Tuple (List.map go ts))
    | Record fields ->
        f (Record (List.map (fun (n, ty) -> (n, go ty)) fields))
  in
  go t

(** Fold over type structure. *)
let fold f acc t =
  let rec go acc t =
    let t = repr t in
    let acc = f acc t in
    match t with
    | Var _ | QVar _ -> acc
    | Con (_, args) -> List.fold_left go acc args
    | Arrow (a, b) -> go (go acc a) b
    | Tuple ts -> List.fold_left go acc ts
    | Record fields -> List.fold_left (fun acc (_, ty) -> go acc ty) acc fields
  in
  go acc t

(** Size of a type tree (nodes). *)
let size t = fold (fun n _ -> n + 1) 0 t

(** Maximum depth of a type tree. *)
let depth t =
  let rec go t =
    match repr t with
    | Var _ | QVar _ -> 1
    | Con (_, args) ->
        1
        + List.fold_left (fun d a -> max d (go a)) 0 args
    | Arrow (a, b) -> 1 + max (go a) (go b)
    | Tuple ts ->
        1 + List.fold_left (fun d a -> max d (go a)) 0 ts
    | Record fields ->
        1
        + List.fold_left (fun d (_, ty) -> max d (go ty)) 0 fields
  in
  go t
