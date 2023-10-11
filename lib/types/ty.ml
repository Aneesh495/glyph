(** Internal type representation and Rémy-style level generalization. *)

type level = int

type tv = {
  id : int;
  mutable level : level;
  mutable namehint : string option;
}

type tv_state =
  | Unbound of tv
  | Link of ty
  | Generic of tv

and ty =
  | TVar of tv_state ref
  | TCon of string
  | TApp of ty * ty
  | TArrow of ty * ty
  | TTuple of ty list
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TString
  | TChar
  | TArray of ty
  | TRef of ty
  | TRecord of (string * ty * bool) list

type scheme = Forall of tv list * ty

(* -------------------------------------------------------------------------- *)
(* Globals                                                                    *)
(* -------------------------------------------------------------------------- *)

let current_level = ref 1
let gensym_counter = ref 0

let enter_level () = incr current_level
let leave_level () = decr current_level
let reset_level () = current_level := 1

let reset_gensym () = gensym_counter := 0

let fresh_tv ?name level =
  incr gensym_counter;
  { id = !gensym_counter; level; namehint = name }

let fresh_var ?name () =
  TVar (ref (Unbound (fresh_tv ?name !current_level)))

let fresh_var_at ?name level =
  TVar (ref (Unbound (fresh_tv ?name level)))

(* -------------------------------------------------------------------------- *)
(* Representation / zonking                                                   *)
(* -------------------------------------------------------------------------- *)

let rec repr = function
  | TVar ({ contents = Link t } as r) ->
      let t' = repr t in
      r := Link t';
      t'
  | t -> t

let rec zonk t =
  match repr t with
  | TVar _ as v -> v
  | TCon _ as c -> c
  | TApp (a, b) -> TApp (zonk a, zonk b)
  | TArrow (a, b) -> TArrow (zonk a, zonk b)
  | TTuple ts -> TTuple (List.map zonk ts)
  | TUnit | TInt | TFloat | TBool | TString | TChar as p -> p
  | TArray t -> TArray (zonk t)
  | TRef t -> TRef (zonk t)
  | TRecord fields ->
      TRecord
        (List.map (fun (n, ty, mut) -> (n, zonk ty, mut)) fields)

let zonk_scheme (Forall (qs, body)) = Forall (qs, zonk body)

(* -------------------------------------------------------------------------- *)
(* Levels / occurs                                                            *)
(* -------------------------------------------------------------------------- *)

let min_level a b = if a < b then a else b

let rec update_level level t =
  match repr t with
  | TVar ({ contents = Unbound tv } as r) ->
      if tv.level > level then tv.level <- level;
      r := Unbound tv
  | TVar { contents = Generic _ } | TVar { contents = Link _ } -> ()
  | TCon _ | TUnit | TInt | TFloat | TBool | TString | TChar -> ()
  | TApp (a, b) | TArrow (a, b) ->
      update_level level a;
      update_level level b
  | TTuple ts -> List.iter (update_level level) ts
  | TArray t | TRef t -> update_level level t
  | TRecord fields ->
      List.iter (fun (_, ty, _) -> update_level level ty) fields

let rec occurs tv t =
  match repr t with
  | TVar { contents = Unbound tv' } ->
      if tv.id = tv'.id then true
      else (
        if tv'.level > tv.level then tv'.level <- tv.level;
        false)
  | TVar { contents = Generic tv' } -> tv.id = tv'.id
  | TVar { contents = Link _ } -> false
  | TCon _ | TUnit | TInt | TFloat | TBool | TString | TChar -> false
  | TApp (a, b) | TArrow (a, b) -> occurs tv a || occurs tv b
  | TTuple ts -> List.exists (occurs tv) ts
  | TArray t | TRef t -> occurs tv t
  | TRecord fields -> List.exists (fun (_, ty, _) -> occurs tv ty) fields

(* -------------------------------------------------------------------------- *)
(* Free variables                                                             *)
(* -------------------------------------------------------------------------- *)

module Tv_set = Set.Make (struct
  type t = tv
  let compare a b = Int.compare a.id b.id
end)

let rec free_vars_acc acc t =
  match repr t with
  | TVar { contents = Unbound tv } -> Tv_set.add tv acc
  | TVar { contents = Generic tv } -> Tv_set.add tv acc
  | TVar { contents = Link _ } -> acc
  | TCon _ | TUnit | TInt | TFloat | TBool | TString | TChar -> acc
  | TApp (a, b) | TArrow (a, b) -> free_vars_acc (free_vars_acc acc a) b
  | TTuple ts -> List.fold_left free_vars_acc acc ts
  | TArray t | TRef t -> free_vars_acc acc t
  | TRecord fields ->
      List.fold_left (fun a (_, ty, _) -> free_vars_acc a ty) acc fields

let free_vars t = Tv_set.elements (free_vars_acc Tv_set.empty t)

let free_vars_scheme (Forall (qs, body)) =
  let qset = List.fold_left (fun s tv -> Tv_set.add tv s) Tv_set.empty qs in
  Tv_set.elements (Tv_set.diff (free_vars_acc Tv_set.empty body) qset)

(* -------------------------------------------------------------------------- *)
(* Generalization / instantiation                                             *)
(* -------------------------------------------------------------------------- *)

let rec generalize_ty t =
  match repr t with
  | TVar ({ contents = Unbound tv } as r) ->
      if tv.level > !current_level then (
        r := Generic tv;
        TVar r)
      else TVar r
  | TVar _ as v -> v
  | TCon _ as c -> c
  | TApp (a, b) -> TApp (generalize_ty a, generalize_ty b)
  | TArrow (a, b) -> TArrow (generalize_ty a, generalize_ty b)
  | TTuple ts -> TTuple (List.map generalize_ty ts)
  | TUnit | TInt | TFloat | TBool | TString | TChar as p -> p
  | TArray t -> TArray (generalize_ty t)
  | TRef t -> TRef (generalize_ty t)
  | TRecord fields ->
      TRecord
        (List.map (fun (n, ty, mut) -> (n, generalize_ty ty, mut)) fields)

let collect_generics t =
  let rec go acc t =
    match repr t with
    | TVar { contents = Generic tv } -> Tv_set.add tv acc
    | TVar _ -> acc
    | TCon _ | TUnit | TInt | TFloat | TBool | TString | TChar -> acc
    | TApp (a, b) | TArrow (a, b) -> go (go acc a) b
    | TTuple ts -> List.fold_left go acc ts
    | TArray t | TRef t -> go acc t
    | TRecord fields ->
        List.fold_left (fun a (_, ty, _) -> go a ty) acc fields
  in
  Tv_set.elements (go Tv_set.empty t)

let generalize t =
  let body = generalize_ty (zonk t) in
  Forall (collect_generics body, body)

let instantiate (Forall (qs, body)) =
  let subst_map =
    List.fold_left
      (fun m tv ->
        let fresh =
          match tv.namehint with
          | Some n -> fresh_var ~name:n ()
          | None -> fresh_var ()
        in
        (tv.id, fresh) :: m)
      [] qs
  in
  let rec go t =
    match repr t with
    | TVar { contents = Generic tv } -> (
        match List.assoc_opt tv.id subst_map with
        | Some t' -> t'
        | None ->
            (* Shouldn't happen for well-formed schemes; leave as-is. *)
            TVar (ref (Generic tv)))
    | TVar _ as v -> v
    | TCon _ as c -> c
    | TApp (a, b) -> TApp (go a, go b)
    | TArrow (a, b) -> TArrow (go a, go b)
    | TTuple ts -> TTuple (List.map go ts)
    | TUnit | TInt | TFloat | TBool | TString | TChar as p -> p
    | TArray t -> TArray (go t)
    | TRef t -> TRef (go t)
    | TRecord fields ->
        TRecord (List.map (fun (n, ty, mut) -> (n, go ty, mut)) fields)
  in
  go body

let mono t = Forall ([], zonk t)

(* -------------------------------------------------------------------------- *)
(* Substitution                                                               *)
(* -------------------------------------------------------------------------- *)

let subst pairs t =
  let id_map = List.map (fun (tv, ty) -> (tv.id, ty)) pairs in
  let rec go t =
    match repr t with
    | TVar { contents = Unbound tv } | TVar { contents = Generic tv } -> (
        match List.assoc_opt tv.id id_map with Some t' -> t' | None -> t)
    | TVar _ as v -> v
    | TCon _ as c -> c
    | TApp (a, b) -> TApp (go a, go b)
    | TArrow (a, b) -> TArrow (go a, go b)
    | TTuple ts -> TTuple (List.map go ts)
    | TUnit | TInt | TFloat | TBool | TString | TChar as p -> p
    | TArray t -> TArray (go t)
    | TRef t -> TRef (go t)
    | TRecord fields ->
        TRecord (List.map (fun (n, ty, mut) -> (n, go ty, mut)) fields)
  in
  go t

(* -------------------------------------------------------------------------- *)
(* Equality                                                                   *)
(* -------------------------------------------------------------------------- *)

let rec equal a b =
  match (repr a, repr b) with
  | TVar { contents = Unbound u }, TVar { contents = Unbound v } ->
      u.id = v.id
  | TVar { contents = Generic u }, TVar { contents = Generic v } ->
      u.id = v.id
  | TCon a, TCon b -> String.equal a b
  | TApp (a1, a2), TApp (b1, b2) -> equal a1 b1 && equal a2 b2
  | TArrow (a1, a2), TArrow (b1, b2) -> equal a1 b1 && equal a2 b2
  | TTuple xs, TTuple ys ->
      List.length xs = List.length ys && List.for_all2 equal xs ys
  | TUnit, TUnit
  | TInt, TInt
  | TFloat, TFloat
  | TBool, TBool
  | TString, TString
  | TChar, TChar ->
      true
  | TArray a, TArray b | TRef a, TRef b -> equal a b
  | TRecord fs, TRecord gs ->
      List.length fs = List.length gs
      && List.for_all2
           (fun (n1, t1, m1) (n2, t2, m2) ->
             String.equal n1 n2 && m1 = m2 && equal t1 t2)
           fs gs
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                            *)
(* -------------------------------------------------------------------------- *)

let tv_name tv =
  match tv.namehint with
  | Some n -> n
  | None ->
      (* a, b, … z, a1, … *)
      let i = tv.id - 1 in
      if i < 26 then String.make 1 (Char.chr (Char.code 'a' + i))
      else Printf.sprintf "t%d" tv.id

let needs_parens_left = function
  | TArrow _ -> true
  | _ -> false

let rec pp_ty_prec prec fmt t =
  let t = repr t in
  match t with
  | TVar { contents = Unbound tv } ->
      Format.fprintf fmt "'%s" (tv_name tv)
  | TVar { contents = Generic tv } ->
      Format.fprintf fmt "'%s" (tv_name tv)
  | TVar { contents = Link t } -> pp_ty_prec prec fmt t
  | TUnit -> Format.pp_print_string fmt "Unit"
  | TInt -> Format.pp_print_string fmt "Int"
  | TFloat -> Format.pp_print_string fmt "Float"
  | TBool -> Format.pp_print_string fmt "Bool"
  | TString -> Format.pp_print_string fmt "String"
  | TChar -> Format.pp_print_string fmt "Char"
  | TCon name -> Format.pp_print_string fmt name
  | TApp (f, arg) ->
      let args, head = peel_apps t in
      (match head with
      | TCon name ->
          Format.fprintf fmt "%s" name;
          List.iter
            (fun a ->
              Format.pp_print_string fmt " ";
              pp_ty_prec 2 fmt a)
            args
      | _ ->
          Format.fprintf fmt "%a %a" (pp_ty_prec 2) f (pp_ty_prec 2) arg)
  | TArrow (a, b) ->
      if prec > 0 then Format.pp_print_string fmt "(";
      pp_ty_prec 1 fmt a;
      Format.pp_print_string fmt " -> ";
      pp_ty_prec 0 fmt b;
      if prec > 0 then Format.pp_print_string fmt ")"
  | TTuple [] -> Format.pp_print_string fmt "Unit"
  | TTuple [ x ] -> pp_ty_prec prec fmt x
  | TTuple ts ->
      if prec > 0 then Format.pp_print_string fmt "(";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " * ")
        (pp_ty_prec 1) fmt ts;
      if prec > 0 then Format.pp_print_string fmt ")"
  | TArray t -> Format.fprintf fmt "%a array" (pp_ty_prec 2) t
  | TRef t -> Format.fprintf fmt "%a ref" (pp_ty_prec 2) t
  | TRecord fields ->
      Format.fprintf fmt "{ ";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
        (fun fmt (n, ty, mut) ->
          if mut then Format.pp_print_string fmt "mutable ";
          Format.fprintf fmt "%s : %a" n pp ty)
        fmt fields;
      Format.pp_print_string fmt " }"

and peel_apps t =
  let rec go acc t =
    match repr t with
    | TApp (f, a) -> go (a :: acc) f
    | head -> (acc, head)
  in
  go [] t

and pp fmt t = pp_ty_prec 0 fmt (zonk t)

let to_string t =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let pp_scheme fmt (Forall (qs, body)) =
  (match qs with
  | [] -> ()
  | _ ->
      Format.pp_print_string fmt "∀";
      List.iter
        (fun tv -> Format.fprintf fmt " '%s" (tv_name tv))
        qs;
      Format.pp_print_string fmt ". ");
  pp fmt body

let scheme_to_string sch =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_scheme fmt sch;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* Silence unused warning for helper used only in pretty-printer comments. *)
let _ = needs_parens_left
let _ = min_level

(* -------------------------------------------------------------------------- *)
(* Constructors                                                               *)
(* -------------------------------------------------------------------------- *)

let t_unit = TUnit
let t_int = TInt
let t_float = TFloat
let t_bool = TBool
let t_string = TString
let t_char = TChar

let arrow a b = TArrow (a, b)

let arrows args ret =
  List.fold_right (fun a r -> TArrow (a, r)) args ret

let tuple = function
  | [] -> TUnit
  | [ t ] -> t
  | ts -> TTuple ts

let t_list elem = TApp (TCon "List", elem)
let t_option elem = TApp (TCon "Option", elem)
let t_ref t = TRef t
let t_array t = TArray t

let apply_constructor name args =
  List.fold_left (fun f a -> TApp (f, a)) (TCon name) args

let app_con = apply_constructor

let as_arrow t =
  match repr t with
  | TArrow (a, b) -> Some (a, b)
  | _ -> None

let peel_arrows t =
  let rec go acc t =
    match repr t with
    | TArrow (a, b) -> go (a :: acc) b
    | r -> (List.rev acc, r)
  in
  go [] t

let rec is_ground t =
  match repr t with
  | TVar { contents = Unbound _ } -> false
  | TVar { contents = Generic _ } -> false
  | TVar { contents = Link _ } -> false
  | TCon _ | TUnit | TInt | TFloat | TBool | TString | TChar -> true
  | TApp (a, b) | TArrow (a, b) -> is_ground a && is_ground b
  | TTuple ts -> List.for_all is_ground ts
  | TArray t | TRef t -> is_ground t
  | TRecord fields -> List.for_all (fun (_, ty, _) -> is_ground ty) fields

let type_constructors t =
  let rec go acc t =
    match repr t with
    | TCon name -> name :: acc
    | TApp (a, b) | TArrow (a, b) -> go (go acc a) b
    | TTuple ts -> List.fold_left go acc ts
    | TArray t | TRef t -> go acc t
    | TRecord fields ->
        List.fold_left (fun a (_, ty, _) -> go a ty) acc fields
    | _ -> acc
  in
  List.sort_uniq String.compare (go [] t)
