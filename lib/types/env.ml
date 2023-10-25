(** Typing environment seeded with the Glyph prelude. *)

open Ty

type type_ctor_info = {
  name : string;
  arity : int;
  constructors : string list;
}

type data_ctor_info = {
  name : string;
  parent : string;
  arity : int;
  scheme : scheme;
  tag : int;
}

type t = {
  values : scheme Ident.Map.t;
  types : type_ctor_info Ident.Map.t;
  constructors : data_ctor_info Ident.Map.t;
}

let empty =
  {
    values = Ident.Map.empty;
    types = Ident.Map.empty;
    constructors = Ident.Map.empty;
  }

let extend name scheme env =
  { env with values = Ident.Map.add name scheme env.values }

let extend_many bindings env =
  List.fold_left (fun env (n, s) -> extend n s env) env bindings

let lookup_value name env = Ident.Map.find_opt name env.values

let find_value name env = lookup_value name env

let find_value_exn name env span =
  match lookup_value name env with
  | Some s -> s
  | None ->
      raise
        (Diagnostic.Error
           (Diagnostic.error span
              (Printf.sprintf "unbound value %s" (Ident.to_string name))))

let add_type info env =
  let id = Ident.Intern.intern info.name in
  { env with types = Ident.Map.add id info env.types }

let lookup_type name env =
  Ident.Map.find_opt name env.types
  |> fun x ->
  match x with
  | Some _ as s -> s
  | None -> Ident.Map.find_opt (Ident.Intern.intern (Ident.name name)) env.types

let add_constructor info env =
  let id = Ident.Intern.intern info.name in
  { env with constructors = Ident.Map.add id info env.constructors }

let lookup_constructor name env =
  match Ident.Map.find_opt name env.constructors with
  | Some _ as s -> s
  | None ->
      Ident.Map.find_opt (Ident.Intern.intern (Ident.name name)) env.constructors

let find_constructor = lookup_constructor

let enter_level env =
  Ty.enter_level ();
  env

let leave_level env =
  Ty.leave_level ();
  env

let ( @-> ) = arrow

let forall1 name f =
  let a = fresh_var ~name () in
  match repr a with
  | TVar { contents = Unbound tv } -> Forall ([ tv ], f a)
  | _ -> mono (f a)

let add_val env name scheme =
  extend (Ident.Intern.intern name) scheme env

let add_binop env name ty = add_val env name (mono ty)

let register_variant env ~type_name ~params ctors =
  let arity = List.length params in
  let env =
    add_type
      {
        name = type_name;
        arity;
        constructors = List.map (fun (n, _, _) -> n) ctors;
      }
      env
  in
  let param_tys =
    List.map (fun p -> fresh_var ~name:p ()) params
  in
  let qs =
    List.filter_map
      (fun t ->
        match repr t with
        | TVar { contents = Unbound tv } -> Some tv
        | _ -> None)
      param_tys
  in
  let result =
    List.fold_left (fun acc t -> TApp (acc, t)) (TCon type_name) param_tys
  in
  let env, _ =
    List.fold_left
      (fun (env, tag) (cname, arg_builders, _) ->
        let args = List.map (fun f -> f param_tys) arg_builders in
        let ty =
          List.fold_right (fun a r -> a @-> r) args result
        in
        let scheme = Forall (qs, ty) in
        let info =
          {
            name = cname;
            parent = type_name;
            arity = List.length args;
            scheme;
            tag;
          }
        in
        (add_constructor info env, tag + 1))
      (env, 0) ctors
  in
  env

let prelude () =
  let env = empty in
  let env =
    List.fold_left
      (fun env (name, arity) ->
        add_type { name; arity; constructors = [] } env)
      env
      [
        ("Unit", 0);
        ("Int", 0);
        ("Float", 0);
        ("Bool", 0);
        ("String", 0);
        ("Char", 0);
        ("Array", 1);
        ("Ref", 1);
      ]
  in
  (* Option *)
  let env =
    register_variant env ~type_name:"Option" ~params:[ "a" ]
      [
        ("None", [], ());
        ( "Some",
          [ (fun ps -> List.hd ps) ],
          () );
      ]
  in
  (* List *)
  let env =
    register_variant env ~type_name:"List" ~params:[ "a" ]
      [
        ("Nil", [], ());
        ( "Cons",
          [
            (fun ps -> List.hd ps);
            (fun ps ->
               List.fold_left
                 (fun acc t -> TApp (acc, t))
                 (TCon "List") ps);
          ],
          () );
      ]
  in
  let i = t_int in
  let f = t_float in
  let b = t_bool in
  let s = t_string in
  let env =
    env
    |> fun e -> add_binop e "+" (i @-> i @-> i)
    |> fun e -> add_binop e "-" (i @-> i @-> i)
    |> fun e -> add_binop e "*" (i @-> i @-> i)
    |> fun e -> add_binop e "/" (i @-> i @-> i)
    |> fun e -> add_binop e "%" (i @-> i @-> i)
    |> fun e -> add_binop e "+." (f @-> f @-> f)
    |> fun e -> add_binop e "-." (f @-> f @-> f)
    |> fun e -> add_binop e "*." (f @-> f @-> f)
    |> fun e -> add_binop e "/." (f @-> f @-> f)
    |> fun e -> add_binop e "=" (forall1 "a" (fun a -> a @-> a @-> b) |> fun s -> match s with Forall (_, t) -> t | _ -> b)
  in
  (* Fix polymorphic ops properly *)
  let env =
    empty
    |> fun e ->
    List.fold_left
      (fun env (name, arity) ->
        add_type { name; arity; constructors = [] } env)
      e
      [
        ("Unit", 0);
        ("Int", 0);
        ("Float", 0);
        ("Bool", 0);
        ("String", 0);
        ("Char", 0);
        ("Array", 1);
        ("Ref", 1);
      ]
  in
  let env =
    register_variant env ~type_name:"Option" ~params:[ "a" ]
      [
        ("None", [], ());
        ("Some", [ (fun ps -> List.hd ps) ], ());
      ]
  in
  let env =
    register_variant env ~type_name:"List" ~params:[ "a" ]
      [
        ("Nil", [], ());
        ( "Cons",
          [
            (fun ps -> List.hd ps);
            (fun ps -> apply_constructor "List" ps);
          ],
          () );
      ]
  in
  let env =
    [
      ("+", mono (i @-> i @-> i));
      ("-", mono (i @-> i @-> i));
      ("*", mono (i @-> i @-> i));
      ("/", mono (i @-> i @-> i));
      ("%", mono (i @-> i @-> i));
      ("+.", mono (f @-> f @-> f));
      ("-.", mono (f @-> f @-> f));
      ("*.", mono (f @-> f @-> f));
      ("/.", mono (f @-> f @-> f));
      ("<", mono (i @-> i @-> b));
      ("<=", mono (i @-> i @-> b));
      (">", mono (i @-> i @-> b));
      (">=", mono (i @-> i @-> b));
      ("&&", mono (b @-> b @-> b));
      ("||", mono (b @-> b @-> b));
      ("not", mono (b @-> b));
      ("~-", mono (i @-> i));
      ("print_int", mono (i @-> t_unit));
      ("print_string", mono (s @-> t_unit));
      ("print_float", mono (f @-> t_unit));
      ("print_bool", mono (b @-> t_unit));
      ("string_of_int", mono (i @-> s));
      ("int_of_string", mono (s @-> i));
      ("=", forall1 "a" (fun a -> a @-> a @-> b));
      ("<>", forall1 "a" (fun a -> a @-> a @-> b));
      ("::", forall1 "a" (fun a -> a @-> t_list a @-> t_list a));
      ("@", forall1 "a" (fun a -> t_list a @-> t_list a @-> t_list a));
      ( "|>",
        forall1 "a" (fun a ->
            match fresh_var ~name:"b" () with
            | b -> a @-> (a @-> b) @-> b) );
    ]
    |> List.fold_left (fun env (n, sch) -> add_val env n sch) env
  in
  (* true/false as constructors-as-values *)
  let env =
    env
    |> add_val "true" (mono b)
    |> add_val "false" (mono b)
  in
  env
