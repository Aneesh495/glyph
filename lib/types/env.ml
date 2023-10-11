(** Typing environment: values, type constructors, and data constructors.

    Levels for let-generalization are managed via [Ty.enter_level] /
    [Ty.exit_level]; this module mirrors those calls for API convenience. *)

open Ty

(* -------------------------------------------------------------------------- *)
(* Type constructor info                                                       *)
(* -------------------------------------------------------------------------- *)

type type_ctor_info = {
  name : Ident.t;
  arity : int;
  params : string list;
  constructors : Ident.t list;
  (** Names of data constructors belonging to this type. *)
  is_abstract : bool;
  is_builtin : bool;
}

type data_ctor_info = {
  name : Ident.t;
  type_name : Ident.t;
  (** Owning algebraic type. *)
  arg_tys : ty list;
  (** Argument types; may contain [QVar] for type params. *)
  result : ty;
  (** Fully applied result type, e.g. [list 'a]. *)
  scheme : scheme;
  (** Quantified constructor type: [arg1 -> ... -> result]. *)
  index : int;
  (** Tag index among siblings. *)
}

(* -------------------------------------------------------------------------- *)
(* Environment                                                                 *)
(* -------------------------------------------------------------------------- *)

type t = {
  values : scheme Ident.Map.t;
  types : type_ctor_info Ident.Map.t;
  constructors : data_ctor_info Ident.Map.t;
  (** Optional lexical parent for nested scopes (not required for HM). *)
  parent : t option;
  level_snapshot : int;
}

let empty =
  {
    values = Ident.Map.empty;
    types = Ident.Map.empty;
    constructors = Ident.Map.empty;
    parent = None;
    level_snapshot = 1;
  }

let level env = env.level_snapshot

let enter_level env =
  Ty.enter_level ();
  { env with level_snapshot = Ty.get_level () }

let exit_level env =
  Ty.exit_level ();
  { env with level_snapshot = Ty.get_level () }

let with_level env f =
  let env = enter_level env in
  let env', result = f env in
  let env' = exit_level env' in
  (env', result)

(* -------------------------------------------------------------------------- *)
(* Value environment                                                           *)
(* -------------------------------------------------------------------------- *)

let extend name scheme env =
  { env with values = Ident.Map.add name scheme env.values }

let extend_mono name ty env = extend name (mono ty) env

let extend_poly name qs ty env = extend name (Forall (qs, ty)) env

let extend_many bindings env =
  List.fold_left (fun env (name, scheme) -> extend name scheme env) env bindings

let lookup_value name env =
  match Ident.Map.find_opt name env.values with
  | Some _ as s -> s
  | None -> (
      match env.parent with
      | Some p -> lookup_value name p
      | None -> None)

let lookup_value_exn name env =
  match lookup_value name env with
  | Some s -> s
  | None ->
      invalid_arg
        (Printf.sprintf "unbound value %s" (Ident.to_string name))

let mem_value name env =
  match lookup_value name env with Some _ -> true | None -> false

let find_value name env = lookup_value name env

(** Instantiate a looked-up scheme at the current level. *)
let lookup_value_inst name env =
  Option.map instantiate_scheme (lookup_value name env)

(* -------------------------------------------------------------------------- *)
(* Type constructor environment                                                *)
(* -------------------------------------------------------------------------- *)

let add_type_ctor info env =
  { env with types = Ident.Map.add info.name info env.types }

let lookup_type name env =
  match Ident.Map.find_opt name env.types with
  | Some _ as s -> s
  | None -> (
      match env.parent with
      | Some p -> lookup_type name p
      | None -> None)

let mem_type name env =
  match lookup_type name env with Some _ -> true | None -> false

let type_arity name env =
  Option.map (fun info -> info.arity) (lookup_type name env)

(* -------------------------------------------------------------------------- *)
(* Data constructor environment                                                *)
(* -------------------------------------------------------------------------- *)

let add_constructor info env =
  { env with constructors = Ident.Map.add info.name info env.constructors }

let lookup_constructor name env =
  match Ident.Map.find_opt name env.constructors with
  | Some _ as s -> s
  | None -> (
      match env.parent with
      | Some p -> lookup_constructor name p
      | None -> None)

let mem_constructor name env =
  match lookup_constructor name env with Some _ -> true | None -> false

let constructor_scheme name env =
  Option.map (fun info -> info.scheme) (lookup_constructor name env)

let constructor_type_name name env =
  Option.map (fun info -> info.type_name) (lookup_constructor name env)

(* -------------------------------------------------------------------------- *)
(* Registering algebraic data types                                            *)
(* -------------------------------------------------------------------------- *)

(** Register a variant type and its constructors.
    [params] are the type parameter names; each constructor's [arg_tys] may
    mention them as [QVar]s. *)
let add_variant ~name ~params ~(ctors : (Ident.t * ty list) list) env =
  let param_tys = List.map qvar params in
  let result_ty = Con (name, param_tys) in
  let type_info =
    {
      name;
      arity = List.length params;
      params;
      constructors = List.map fst ctors;
      is_abstract = false;
      is_builtin = false;
    }
  in
  let env = add_type_ctor type_info env in
  List.mapi
    (fun index (ctor_name, arg_tys) ->
      let fun_ty = arrows arg_tys result_ty in
      let scheme = Forall (params, fun_ty) in
      let info =
        {
          name = ctor_name;
          type_name = name;
          arg_tys;
          result = result_ty;
          scheme;
          index;
        }
      in
      (ctor_name, info))
    ctors
  |> List.fold_left
       (fun env (_name, info) -> add_constructor info env)
       env

let add_alias ~name ~params ~body env =
  let info =
    {
      name;
      arity = List.length params;
      params;
      constructors = [];
      is_abstract = true;
      is_builtin = false;
    }
  in
  (* Aliases are stored as types; expansion is left to the caller via [body]. *)
  ignore body;
  add_type_ctor info env

let add_abstract ~name ~arity ?(params = []) env =
  let params =
    if params = [] then
      List.init arity (fun i ->
          let letter = Char.chr (Char.code 'a' + i) in
          String.make 1 letter)
    else params
  in
  add_type_ctor
    {
      name;
      arity;
      params;
      constructors = [];
      is_abstract = true;
      is_builtin = false;
    }
    env

(* -------------------------------------------------------------------------- *)
(* Free variables of the value environment                                     *)
(* -------------------------------------------------------------------------- *)

let free_tvars env =
  let s = TVarSet.create () in
  Ident.Map.iter
    (fun _ scheme ->
      let s' = free_tvars_scheme scheme in
      TVarSet.iter (TVarSet.add s) s')
    env.values;
  s

let generalize_in env ty =
  (* Standard HM: generalize variables not free in the environment.
     With levels, Ty.generalize already uses current_level; ensure we
     entered a level before inferring the RHS. *)
  ignore env;
  Ty.generalize ty

(* -------------------------------------------------------------------------- *)
(* Nested scope                                                                *)
(* -------------------------------------------------------------------------- *)

let push env =
  {
    values = Ident.Map.empty;
    types = Ident.Map.empty;
    constructors = Ident.Map.empty;
    parent = Some env;
    level_snapshot = env.level_snapshot;
  }

let pop env =
  match env.parent with
  | Some p -> p
  | None -> env

let merge_child ~parent ~child =
  {
    parent with
    values =
      Ident.Map.fold Ident.Map.add child.values parent.values;
    types = Ident.Map.fold Ident.Map.add child.types parent.types;
    constructors =
      Ident.Map.fold Ident.Map.add child.constructors parent.constructors;
  }

(* -------------------------------------------------------------------------- *)
(* Iteration / pretty-printing                                                 *)
(* -------------------------------------------------------------------------- *)

let fold_values f acc env = Ident.Map.fold f env.values acc
let fold_types f acc env = Ident.Map.fold f env.types acc
let fold_constructors f acc env = Ident.Map.fold f env.constructors acc

let value_names env =
  Ident.Map.fold (fun k _ acc -> k :: acc) env.values []

let pp fmt env =
  Format.fprintf fmt "@[<v>values:@,";
  Ident.Map.iter
    (fun name scheme ->
      Format.fprintf fmt "  %a : %a@," Ident.pp name pp_scheme scheme)
    env.values;
  Format.fprintf fmt "types:@,";
  Ident.Map.iter
    (fun name info ->
      Format.fprintf fmt "  %a/%d@," Ident.pp name info.arity)
    env.types;
  Format.fprintf fmt "constructors:@,";
  Ident.Map.iter
    (fun name info ->
      Format.fprintf fmt "  %a : %a@," Ident.pp name pp_scheme info.scheme)
    env.constructors;
  Format.fprintf fmt "@]"

let to_string env =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt env;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(* -------------------------------------------------------------------------- *)
(* Builtin helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let id s = Ident.Intern.intern s

let forall1 name body_fn =
  let a = name in
  Forall ([ a ], body_fn (qvar a))

let forall2 n1 n2 body_fn =
  Forall ([ n1; n2 ], body_fn (qvar n1) (qvar n2))

let binop_ty a = a @-> a @-> a
let cmp_ty a = a @-> a @-> Builtin.bool
let pred_ty a = a @-> Builtin.bool

let add_builtin_value name scheme env = extend (id name) scheme env

let add_binop name ty env = add_builtin_value name (mono ty) env

let add_poly_binop name scheme env = add_builtin_value name scheme env

(* -------------------------------------------------------------------------- *)
(* Initial environment                                                         *)
(* -------------------------------------------------------------------------- *)

let add_builtin_types env =
  let env =
    List.fold_left
      (fun env (name, arity) ->
        add_type_ctor
          {
            name = id name;
            arity;
            params =
              List.init arity (fun i ->
                  String.make 1 (Char.chr (Char.code 'a' + i)));
            constructors = [];
            is_abstract = true;
            is_builtin = true;
          }
          env)
      env
      [
        ("int", 0);
        ("float", 0);
        ("bool", 0);
        ("string", 0);
        ("char", 0);
        ("unit", 0);
        ("list", 1);
        ("option", 1);
        ("result", 2);
        ("array", 1);
        ("ref", 1);
        ("exn", 0);
      ]
  in
  env

let add_option_ctors env =
  add_variant ~name:(id "option") ~params:[ "a" ]
    ~ctors:
      [
        (id "None", []);
        (id "Some", [ qvar "a" ]);
      ]
    env

let add_result_ctors env =
  add_variant ~name:(id "result") ~params:[ "a"; "b" ]
    ~ctors:
      [
        (id "Ok", [ qvar "a" ]);
        (id "Error", [ qvar "b" ]);
      ]
    env

let add_list_ctors env =
  add_variant ~name:(id "list") ~params:[ "a" ]
    ~ctors:
      [
        (id "[]", []);
        (* Cons is also available as operator :: *)
        (id "::", [ qvar "a"; Builtin.list (qvar "a") ]);
      ]
    env

let add_bool_ctors env =
  (* true/false as constructors of bool for pattern matching. *)
  let name = id "bool" in
  let env =
    add_type_ctor
      {
        name;
        arity = 0;
        params = [];
        constructors = [ id "true"; id "false" ];
        is_abstract = false;
        is_builtin = true;
      }
      env
  in
  let add_ctor ctor_name index env =
    let scheme = mono Builtin.bool in
    add_constructor
      {
        name = id ctor_name;
        type_name = name;
        arg_tys = [];
        result = Builtin.bool;
        scheme;
        index;
      }
      env
  in
  env |> add_ctor "true" 0 |> add_ctor "false" 1

let add_arithmetic env =
  let i = Builtin.int in
  let f = Builtin.float in
  env
  |> add_binop "+" (binop_ty i)
  |> add_binop "-" (binop_ty i)
  |> add_binop "*" (binop_ty i)
  |> add_binop "/" (binop_ty i)
  |> add_binop "%" (binop_ty i)
  |> add_binop "+." (binop_ty f)
  |> add_binop "-." (binop_ty f)
  |> add_binop "*." (binop_ty f)
  |> add_binop "/." (binop_ty f)
  |> add_builtin_value "~-" (mono (i @-> i))
  |> add_builtin_value "~-." (mono (f @-> f))
  |> add_builtin_value "mod" (mono (binop_ty i))
  |> add_builtin_value "abs" (mono (i @-> i))
  |> add_builtin_value "succ" (mono (i @-> i))
  |> add_builtin_value "pred" (mono (i @-> i))
  |> add_builtin_value "float_of_int" (mono (i @-> f))
  |> add_builtin_value "int_of_float" (mono (f @-> i))

let add_comparisons env =
  (* Polymorphic equality at this stage; a later pass may restrict it. *)
  let eq = forall1 "a" (fun a -> cmp_ty a) in
  env
  |> add_poly_binop "=" eq
  |> add_poly_binop "<>" eq
  |> add_poly_binop "==" eq
  |> add_poly_binop "!=" eq
  |> add_binop "<" (cmp_ty Builtin.int)
  |> add_binop "<=" (cmp_ty Builtin.int)
  |> add_binop ">" (cmp_ty Builtin.int)
  |> add_binop ">=" (cmp_ty Builtin.int)
  |> add_binop "<." (cmp_ty Builtin.float)
  |> add_binop "<=." (cmp_ty Builtin.float)
  |> add_binop ">." (cmp_ty Builtin.float)
  |> add_binop ">=." (cmp_ty Builtin.float)
  |> add_builtin_value "compare"
       (forall1 "a" (fun a -> a @-> a @-> Builtin.int))
  |> add_builtin_value "min" (forall1 "a" (fun a -> a @-> a @-> a))
  |> add_builtin_value "max" (forall1 "a" (fun a -> a @-> a @-> a))

let add_boolean env =
  env
  |> add_binop "&&" (binop_ty Builtin.bool)
  |> add_binop "||" (binop_ty Builtin.bool)
  |> add_builtin_value "not" (mono (Builtin.bool @-> Builtin.bool))

let add_list_ops env =
  let cons =
    forall1 "a" (fun a -> a @-> Builtin.list a @-> Builtin.list a)
  in
  let append =
    forall1 "a" (fun a -> Builtin.list a @-> Builtin.list a @-> Builtin.list a)
  in
  env
  |> add_poly_binop "::" cons
  |> add_poly_binop "@" append
  |> add_builtin_value "List.hd"
       (forall1 "a" (fun a -> Builtin.list a @-> a))
  |> add_builtin_value "List.tl"
       (forall1 "a" (fun a -> Builtin.list a @-> Builtin.list a))
  |> add_builtin_value "List.length"
       (forall1 "a" (fun a -> Builtin.list a @-> Builtin.int))
  |> add_builtin_value "List.rev"
       (forall1 "a" (fun a -> Builtin.list a @-> Builtin.list a))
  |> add_builtin_value "List.map"
       (forall2 "a" "b" (fun a b ->
            (a @-> b) @-> Builtin.list a @-> Builtin.list b))
  |> add_builtin_value "List.filter"
       (forall1 "a" (fun a ->
            (a @-> Builtin.bool) @-> Builtin.list a @-> Builtin.list a))
  |> add_builtin_value "List.fold_left"
       (forall2 "a" "b" (fun a b ->
            (a @-> b @-> a) @-> a @-> Builtin.list b @-> a))
  |> add_builtin_value "List.fold_right"
       (forall2 "a" "b" (fun a b ->
            (a @-> b @-> b) @-> Builtin.list a @-> b @-> b))
  |> add_builtin_value "List.append" append
  |> add_builtin_value "List.cons" cons
  |> add_builtin_value "List.nil" (forall1 "a" (fun a -> Builtin.list a))

let add_option_ops env =
  env
  |> add_builtin_value "Option.map"
       (forall2 "a" "b" (fun a b ->
            (a @-> b) @-> Builtin.option a @-> Builtin.option b))
  |> add_builtin_value "Option.bind"
       (forall2 "a" "b" (fun a b ->
            Builtin.option a @-> (a @-> Builtin.option b)
            @-> Builtin.option b))
  |> add_builtin_value "Option.value"
       (forall1 "a" (fun a -> Builtin.option a @-> a @-> a))
  |> add_builtin_value "Option.is_some"
       (forall1 "a" (fun a -> Builtin.option a @-> Builtin.bool))
  |> add_builtin_value "Option.is_none"
       (forall1 "a" (fun a -> Builtin.option a @-> Builtin.bool))

let add_tuple_ops env =
  env
  |> add_builtin_value "fst"
       (forall2 "a" "b" (fun a b -> Tuple [ a; b ] @-> a))
  |> add_builtin_value "snd"
       (forall2 "a" "b" (fun a b -> Tuple [ a; b ] @-> b))
  |> add_builtin_value "fst3"
       (Forall
          ( [ "a"; "b"; "c" ],
            Tuple [ qvar "a"; qvar "b"; qvar "c" ] @-> qvar "a" ))
  |> add_builtin_value "snd3"
       (Forall
          ( [ "a"; "b"; "c" ],
            Tuple [ qvar "a"; qvar "b"; qvar "c" ] @-> qvar "b" ))
  |> add_builtin_value "thd3"
       (Forall
          ( [ "a"; "b"; "c" ],
            Tuple [ qvar "a"; qvar "b"; qvar "c" ] @-> qvar "c" ))
  |> add_builtin_value "ignore" (forall1 "a" (fun a -> a @-> Builtin.unit))
  |> add_builtin_value "identity" (forall1 "a" (fun a -> a @-> a))
  |> add_builtin_value "id" (forall1 "a" (fun a -> a @-> a))
  |> add_builtin_value "@@"
       (forall2 "a" "b" (fun a b -> (a @-> b) @-> a @-> b))
  |> add_builtin_value "|>"
       (forall2 "a" "b" (fun a b -> a @-> (a @-> b) @-> b))

let add_string_ops env =
  env
  |> add_binop "^" (binop_ty Builtin.string)
  |> add_builtin_value "String.length" (mono (Builtin.string @-> Builtin.int))
  |> add_builtin_value "String.concat"
       (mono (Builtin.string @-> Builtin.list Builtin.string @-> Builtin.string))
  |> add_builtin_value "String.sub"
       (mono
          (Builtin.string @-> Builtin.int @-> Builtin.int @-> Builtin.string))
  |> add_builtin_value "String.make"
       (mono (Builtin.int @-> Builtin.char @-> Builtin.string))
  |> add_builtin_value "String.get"
       (mono (Builtin.string @-> Builtin.int @-> Builtin.char))
  |> add_builtin_value "string_of_int" (mono (Builtin.int @-> Builtin.string))
  |> add_builtin_value "int_of_string" (mono (Builtin.string @-> Builtin.int))
  |> add_builtin_value "string_of_float"
       (mono (Builtin.float @-> Builtin.string))
  |> add_builtin_value "float_of_string"
       (mono (Builtin.string @-> Builtin.float))
  |> add_builtin_value "string_of_bool"
       (mono (Builtin.bool @-> Builtin.string))
  |> add_builtin_value "bool_of_string"
       (mono (Builtin.string @-> Builtin.bool))
  |> add_builtin_value "Char.code" (mono (Builtin.char @-> Builtin.int))
  |> add_builtin_value "Char.chr" (mono (Builtin.int @-> Builtin.char))
  |> add_builtin_value "Char.escaped"
       (mono (Builtin.char @-> Builtin.string))

let add_io env =
  env
  |> add_builtin_value "print_int" (mono (Builtin.int @-> Builtin.unit))
  |> add_builtin_value "print_string"
       (mono (Builtin.string @-> Builtin.unit))
  |> add_builtin_value "print_float"
       (mono (Builtin.float @-> Builtin.unit))
  |> add_builtin_value "print_char" (mono (Builtin.char @-> Builtin.unit))
  |> add_builtin_value "print_bool" (mono (Builtin.bool @-> Builtin.unit))
  |> add_builtin_value "print_endline"
       (mono (Builtin.string @-> Builtin.unit))
  |> add_builtin_value "print_newline" (mono (Builtin.unit @-> Builtin.unit))
  |> add_builtin_value "prerr_string"
       (mono (Builtin.string @-> Builtin.unit))
  |> add_builtin_value "prerr_endline"
       (mono (Builtin.string @-> Builtin.unit))
  |> add_builtin_value "read_line" (mono (Builtin.unit @-> Builtin.string))
  |> add_builtin_value "read_int" (mono (Builtin.unit @-> Builtin.int))
  |> add_builtin_value "flush_all" (mono (Builtin.unit @-> Builtin.unit))

let add_array_ops env =
  env
  |> add_builtin_value "Array.length"
       (forall1 "a" (fun a -> Builtin.array a @-> Builtin.int))
  |> add_builtin_value "Array.get"
       (forall1 "a" (fun a -> Builtin.array a @-> Builtin.int @-> a))
  |> add_builtin_value "Array.set"
       (forall1 "a" (fun a ->
            Builtin.array a @-> Builtin.int @-> a @-> Builtin.unit))
  |> add_builtin_value "Array.make"
       (forall1 "a" (fun a -> Builtin.int @-> a @-> Builtin.array a))
  |> add_builtin_value "Array.init"
       (forall1 "a" (fun a ->
            Builtin.int @-> (Builtin.int @-> a) @-> Builtin.array a))
  |> add_builtin_value "Array.to_list"
       (forall1 "a" (fun a -> Builtin.array a @-> Builtin.list a))
  |> add_builtin_value "Array.of_list"
       (forall1 "a" (fun a -> Builtin.list a @-> Builtin.array a))

let add_ref_ops env =
  env
  |> add_builtin_value "ref"
       (forall1 "a" (fun a -> a @-> Builtin.ref_ a))
  |> add_builtin_value "!"
       (forall1 "a" (fun a -> Builtin.ref_ a @-> a))
  |> add_builtin_value ":="
       (forall1 "a" (fun a -> Builtin.ref_ a @-> a @-> Builtin.unit))

let add_exn_ops env =
  env
  |> add_builtin_value "raise" (forall1 "a" (fun a -> Builtin.exn @-> a))
  |> add_builtin_value "failwith"
       (forall1 "a" (fun a -> Builtin.string @-> a))
  |> add_builtin_value "invalid_arg"
       (forall1 "a" (fun a -> Builtin.string @-> a))

(** The initial typing environment with all prelude bindings. *)
let initial () =
  empty
  |> add_builtin_types
  |> add_bool_ctors
  |> add_option_ctors
  |> add_result_ctors
  |> add_list_ctors
  |> add_arithmetic
  |> add_comparisons
  |> add_boolean
  |> add_list_ops
  |> add_option_ops
  |> add_tuple_ops
  |> add_string_ops
  |> add_io
  |> add_array_ops
  |> add_ref_ops
  |> add_exn_ops

(** Snapshot of initial env (lazily built once). *)
let prelude =
  let cache = ref None in
  fun () ->
    match !cache with
    | Some e -> e
    | None ->
        let e = initial () in
        cache := Some e;
        e

(** Reset prelude cache (after [Ident.Intern.reset] / [Ty.reset]). *)
let reset_prelude () = ()
