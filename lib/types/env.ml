(** Typing environment with prelude bindings for Glyph. *)

open Ty

type type_kind =
  | Abstract
  | Alias of ty
  | Variant of constructor_info list

and constructor_info = {
  name : Ident.t;
  parent : Ident.t;
  arity : int;
  scheme : scheme;
  tag : int;
}

type type_info = {
  name : Ident.t;
  params : Ident.t list;
  kind : type_kind;
}

type t = {
  values : scheme Ident.Map.t;
  types : type_info Ident.Map.t;
  constructors : constructor_info Ident.Map.t;
  binops : scheme Ident.Map.t;
  unops : scheme Ident.Map.t;
}

let empty =
  {
    values = Ident.Map.empty;
    types = Ident.Map.empty;
    constructors = Ident.Map.empty;
    binops = Ident.Map.empty;
    unops = Ident.Map.empty;
  }

let extend env name scheme =
  { env with values = Ident.Map.add name scheme env.values }

let extend_mono env name ty = extend env name (mono ty)

let find_value env name = Ident.Map.find_opt name env.values

let find_constructor env name = Ident.Map.find_opt name env.constructors

let find_type env name = Ident.Map.find_opt name env.types

let binop_name = function
  | Token.Op_add -> "+"
  | Token.Op_sub -> "-"
  | Token.Op_mul -> "*"
  | Token.Op_div -> "/"
  | Token.Op_mod -> "%"
  | Token.Op_eq -> "="
  | Token.Op_neq -> "<>"
  | Token.Op_lt -> "<"
  | Token.Op_le -> "<="
  | Token.Op_gt -> ">"
  | Token.Op_ge -> ">="
  | Token.Op_and -> "&&"
  | Token.Op_or -> "||"
  | Token.Op_cons -> "::"
  | Token.Op_pipe -> "|>"

let unop_name = function
  | Token.Op_neg -> "~-"
  | Token.Op_not -> "not"

let find_binop env op =
  let id = Ident.Intern.intern (binop_name op) in
  match Ident.Map.find_opt id env.binops with
  | Some _ as s -> s
  | None -> Ident.Map.find_opt id env.values

let find_unop env op =
  let id = Ident.Intern.intern (unop_name op) in
  match Ident.Map.find_opt id env.unops with
  | Some _ as s -> s
  | None -> Ident.Map.find_opt id env.values

let add_val env name scheme =
  let id = Ident.Intern.intern name in
  extend env id scheme

let add_binop env name scheme =
  let id = Ident.Intern.intern name in
  { env with binops = Ident.Map.add id scheme env.binops }
  |> fun env -> extend env id scheme

let add_unop env name scheme =
  let id = Ident.Intern.intern name in
  { env with unops = Ident.Map.add id scheme env.unops }
  |> fun env -> extend env id scheme

let add_type env info =
  { env with types = Ident.Map.add info.name info env.types }

let add_constructor (env : t) (info : constructor_info) : t =
  {
    env with
    constructors =
      (Ident.Map.add info.name info env.constructors
        : constructor_info Ident.Map.t);
  }

let ( @-> ) = arrow

let forall1 name body =
  let a = fresh_var ~name () in
  match repr a with
  | TVar { contents = Unbound tv } -> Forall ([ tv ], body a)
  | _ -> mono (body a)

let register_variant env ~type_name ~params ctors =
  let type_id = Ident.Intern.intern type_name in
  let param_ids = List.map Ident.Intern.intern params in
  let param_tys = List.map (fun p -> fresh_var ~name:p ()) params in
  let qs =
    List.filter_map
      (fun t ->
        match repr t with
        | TVar { contents = Unbound tv } -> Some tv
        | _ -> None)
      param_tys
  in
  let result = apply_constructor type_name param_tys in
  let env, ctor_infos, _ =
    List.fold_left
      (fun (env, infos, tag) (cname, arg_builders) ->
        let args = List.map (fun f -> f param_tys) arg_builders in
        let ty = List.fold_right (fun a r -> a @-> r) args result in
        let scheme = Forall (qs, ty) in
        let info =
          {
            name = Ident.Intern.intern cname;
            parent = type_id;
            arity = List.length args;
            scheme;
            tag;
          }
        in
        (add_constructor env info, info :: infos, tag + 1))
      (env, [], 0) ctors
  in
  let type_info =
    {
      name = type_id;
      params = param_ids;
      kind = Variant (List.rev ctor_infos);
    }
  in
  add_type env type_info

let add_type_def env (td : Ast.type_def) =
  let params = td.td_params in
  let param_tys =
    List.map (fun p -> fresh_var ~name:(Ident.name p) ()) params
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
    apply_constructor (Ident.name td.td_name)
      (List.map (fun _ -> fresh_var ()) params)
  in
  (* Rebuild result with the actual param vars. *)
  let result = apply_constructor (Ident.name td.td_name) param_tys in
  let env, ctor_infos, _ =
    List.fold_left
      (fun (env, infos, tag) (cd : Ast.ctor_decl) ->
        (* Translate ctor args as fresh for now; Infer's translate is preferred
           but Env is defined before full inference context. Treat args as opaque
           type constructors by name when simple. *)
        let arg_tys =
          List.map
            (fun (ty : Ast.ty) ->
              match ty.ty_desc with
              | Ast.Ty_named (n, args) -> (
                  match (String.lowercase_ascii (Ident.name n), args) with
                  | "int", [] -> t_int
                  | "float", [] -> t_float
                  | "bool", [] -> t_bool
                  | "string", [] -> t_string
                  | "char", [] -> t_char
                  | "unit", [] -> t_unit
                  | _, _ ->
                      apply_constructor (Ident.name n)
                        (List.map (fun _ -> fresh_var ()) args))
              | Ast.Ty_var id -> (
                  match
                    List.find_opt
                      (fun (p, _) -> Ident.equal p id)
                      (List.combine params param_tys)
                  with
                  | Some (_, t) -> t
                  | None -> fresh_var ~name:(Ident.name id) ())
              | Ast.Ty_unit -> t_unit
              | Ast.Ty_hole -> fresh_var ()
              | Ast.Ty_arrow (_a, _b) -> fresh_var ()
              | Ast.Ty_tuple ts ->
                  tuple (List.map (fun _ -> fresh_var ()) ts))
            cd.ctor_args
        in
        let ty = List.fold_right (fun a r -> a @-> r) arg_tys result in
        let scheme = Forall (qs, ty) in
        let info =
          {
            name = cd.ctor_name;
            parent = td.td_name;
            arity = List.length arg_tys;
            scheme;
            tag;
          }
        in
        (add_constructor env info, info :: infos, tag + 1))
      (env, [], 0) td.td_ctors
  in
  let type_info =
    {
      name = td.td_name;
      params;
      kind = Variant (List.rev ctor_infos);
    }
  in
  add_type env type_info

let prelude () =
  let env = empty in
  let env =
    List.fold_left
      (fun env (name, arity) ->
        let id = Ident.Intern.intern name in
        let params =
          List.init arity (fun i -> Ident.Intern.intern (Printf.sprintf "a%d" i))
        in
        add_type env { name = id; params; kind = Abstract })
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
  let env =
    register_variant env ~type_name:"Option" ~params:[ "a" ]
      [ ("None", []); ("Some", [ (fun ps -> List.hd ps) ]) ]
  in
  let env =
    register_variant env ~type_name:"List" ~params:[ "a" ]
      [
        ("Nil", []);
        ( "Cons",
          [
            (fun ps -> List.hd ps);
            (fun ps -> apply_constructor "List" ps);
          ] );
      ]
  in
  let i = t_int in
  let f = t_float in
  let b = t_bool in
  let s = t_string in
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
    ]
    |> List.fold_left
         (fun env (n, sch) ->
           if
             List.mem n
               [ "+"; "-"; "*"; "/"; "%"; "<"; "<="; ">"; ">="; "&&"; "||"; "="; "<>"; "::"; "@" ]
           then add_binop env n sch
           else if List.mem n [ "not"; "~-" ] then add_unop env n sch
           else add_val env n sch)
         env
  in
  let env = add_val env "true" (mono b) in
  add_val env "false" (mono b)
