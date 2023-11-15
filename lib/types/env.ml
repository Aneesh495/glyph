(** Typing environments for values, type constructors, and ADT constructors. *)

type type_ctor = {
  name : Ident.t;
  params : Ident.t list;
  kind : type_ctor_kind;
  span : Span.t;
}

and type_ctor_kind =
  | Variant of constructor_info list
  | Record of (Ident.t * Ty.ty * bool) list
  | Abbrev of Ty.ty
  | Abstract

and constructor_info = {
  cname : Ident.t;
  scheme : Ty.scheme;
  arity : int;
  parent : Ident.t;
  span : Span.t;
}

type field_info = {
  fname : Ident.t;
  scheme : Ty.scheme;
  mutable_ : bool;
  parent : Ident.t;
  span : Span.t;
}

type t = {
  values : Ty.scheme Ident.Map.t;
  types : type_ctor Ident.Map.t;
  constructors : constructor_info Ident.Map.t;
  fields : field_info Ident.Map.t;
  parent : t option;
}

let empty =
  {
    values = Ident.Map.empty;
    types = Ident.Map.empty;
    constructors = Ident.Map.empty;
    fields = Ident.Map.empty;
    parent = None;
  }

let extend env id scheme =
  { env with values = Ident.Map.add id scheme env.values }

let extend_mono env id ty = extend env id (Ty.mono ty)

let extend_many env pairs =
  List.fold_left (fun e (id, sch) -> extend e id sch) env pairs

let find_by_name map id =
  match Ident.Map.find_opt id map with
  | Some v -> Some v
  | None ->
      let name = Ident.name id in
      Ident.Map.fold
        (fun k v acc ->
          match acc with
          | Some _ -> acc
          | None -> if String.equal (Ident.name k) name then Some v else None)
        map None

let rec find_value env id =
  match find_by_name env.values id with
  | Some _ as s -> s
  | None -> (
      match env.parent with Some p -> find_value p id | None -> None)

let find_value_exn env id span =
  match find_value env id with
  | Some s -> s
  | None -> Error.unbound_value span id

let add_type env tc = { env with types = Ident.Map.add tc.name tc env.types }

let rec find_type env id =
  match find_by_name env.types id with
  | Some _ as s -> s
  | None -> (
      match env.parent with Some p -> find_type p id | None -> None)

let add_constructor env c =
  { env with constructors = Ident.Map.add c.cname c env.constructors }

let rec find_constructor env id =
  match find_by_name env.constructors id with
  | Some _ as s -> s
  | None -> (
      match env.parent with
      | Some p -> find_constructor p id
      | None -> None)

let find_constructor_exn env id span =
  match find_constructor env id with
  | Some c -> c
  | None -> Error.unbound_constructor span id

let add_field env f = { env with fields = Ident.Map.add f.fname f env.fields }

let rec find_field env id =
  match find_by_name env.fields id with
  | Some _ as s -> s
  | None -> (
      match env.parent with Some p -> find_field p id | None -> None)

module Tv_set = Set.Make (struct
  type t = Ty.tv
  let compare a b = Int.compare a.id b.id
end)

let free_vars env =
  Ident.Map.fold
    (fun _ sch set ->
      List.fold_left (fun s tv -> Tv_set.add tv s) set
        (Ty.free_vars_scheme sch))
    env.values Tv_set.empty
  |> Tv_set.elements

let make_generic_var ?name () =
  let t = Ty.fresh_var_at ?name 0 in
  match Ty.repr t with
  | Ty.TVar ({ contents = Ty.Unbound tv } as r) ->
      r := Ty.Generic tv;
      (tv, Ty.TVar r)
  | _ -> assert false

let rec translate_ast_ty (param_map : (Ident.t * Ty.ty) list) (aty : Ast.ty) :
    Ty.ty =
  match aty.Ast.ty_desc with
  | Ast.Ty_hole -> Ty.fresh_var ()
  | Ast.Ty_unit -> Ty.t_unit
  | Ast.Ty_var id -> (
      match List.find_opt (fun (p, _) -> Ident.equal p id) param_map with
      | Some (_, t) -> t
      | None -> Ty.TCon (Ident.name id))
  | Ast.Ty_named (name, args) ->
      let args = List.map (translate_ast_ty param_map) args in
      (match String.lowercase_ascii (Ident.name name) with
      | "int" -> Ty.t_int
      | "float" -> Ty.t_float
      | "bool" -> Ty.t_bool
      | "string" -> Ty.t_string
      | "char" -> Ty.t_char
      | "unit" -> Ty.t_unit
      | "list" -> (
          match args with [ a ] -> Ty.t_list a | _ -> Ty.apply_constructor "List" args)
      | "option" -> (
          match args with
          | [ a ] -> Ty.t_option a
          | _ -> Ty.apply_constructor "Option" args)
      | "array" -> (
          match args with
          | [ a ] -> Ty.t_array a
          | _ -> Ty.apply_constructor "Array" args)
      | "ref" -> (
          match args with [ a ] -> Ty.t_ref a | _ -> Ty.apply_constructor "Ref" args)
      | _ -> Ty.apply_constructor (Ident.name name) args)
  | Ast.Ty_arrow (a, b) ->
      Ty.arrow (translate_ast_ty param_map a) (translate_ast_ty param_map b)
  | Ast.Ty_tuple ts -> Ty.tuple (List.map (translate_ast_ty param_map) ts)

let add_type_def env (td : Ast.type_def) =
  let params =
    List.map
      (fun id ->
        let tv, ty = make_generic_var ~name:(Ident.name id) () in
        (id, tv, ty))
      td.Ast.td_params
  in
  let param_tvs = List.map (fun (_, tv, _) -> tv) params in
  let param_ty_list = List.map (fun (_, _, ty) -> ty) params in
  let param_map = List.map (fun (id, _, ty) -> (id, ty)) params in
  let result = Ty.apply_constructor (Ident.name td.td_name) param_ty_list in
  let ctor_infos =
    List.map
      (fun (cd : Ast.ctor_decl) ->
        let arg_tys = List.map (translate_ast_ty param_map) cd.ctor_args in
        {
          cname = cd.ctor_name;
          scheme = Ty.Forall (param_tvs, Ty.arrows arg_tys result);
          arity = List.length cd.ctor_args;
          parent = td.td_name;
          span = cd.ctor_span;
        })
      td.td_ctors
  in
  let env =
    add_type env
      {
        name = td.td_name;
        params = td.td_params;
        kind = Variant ctor_infos;
        span = td.td_span;
      }
  in
  List.fold_left add_constructor env ctor_infos

let add_list_type env =
  let list_id = Ident.Predef.list in
  let tv, a = make_generic_var ~name:"a" () in
  let list_a = Ty.t_list a in
  let nil_info =
    {
      cname = Ident.Predef.nil;
      scheme = Ty.Forall ([ tv ], list_a);
      arity = 0;
      parent = list_id;
      span = Span.dummy;
    }
  in
  let cons_info =
    {
      cname = Ident.Predef.cons;
      scheme = Ty.Forall ([ tv ], Ty.arrows [ a; list_a ] list_a);
      arity = 2;
      parent = list_id;
      span = Span.dummy;
    }
  in
  env
  |> fun e ->
  add_type e
    {
      name = list_id;
      params = [ Ident.Intern.intern "a" ];
      kind = Variant [ nil_info; cons_info ];
      span = Span.dummy;
    }
  |> add_constructor nil_info
  |> add_constructor cons_info
  |> add_constructor { cons_info with cname = Ident.Intern.intern "::" }

let add_option_type env =
  let opt_id = Ident.Intern.intern "Option" in
  let tv, a = make_generic_var ~name:"a" () in
  let opt_a = Ty.t_option a in
  let none_info =
    {
      cname = Ident.Intern.intern "None";
      scheme = Ty.Forall ([ tv ], opt_a);
      arity = 0;
      parent = opt_id;
      span = Span.dummy;
    }
  in
  let some_info =
    {
      cname = Ident.Intern.intern "Some";
      scheme = Ty.Forall ([ tv ], Ty.arrow a opt_a);
      arity = 1;
      parent = opt_id;
      span = Span.dummy;
    }
  in
  env
  |> fun e ->
  add_type e
    {
      name = opt_id;
      params = [ Ident.Intern.intern "a" ];
      kind = Variant [ none_info; some_info ];
      span = Span.dummy;
    }
  |> add_constructor none_info
  |> add_constructor some_info

let add_result_type env =
  let res_id = Ident.Intern.intern "Result" in
  let tv_a, a = make_generic_var ~name:"a" () in
  let tv_b, b = make_generic_var ~name:"b" () in
  let res = Ty.apply_constructor "Result" [ a; b ] in
  let ok_info =
    {
      cname = Ident.Intern.intern "Ok";
      scheme = Ty.Forall ([ tv_a; tv_b ], Ty.arrow a res);
      arity = 1;
      parent = res_id;
      span = Span.dummy;
    }
  in
  let err_info =
    {
      cname = Ident.Intern.intern "Error";
      scheme = Ty.Forall ([ tv_a; tv_b ], Ty.arrow b res);
      arity = 1;
      parent = res_id;
      span = Span.dummy;
    }
  in
  env
  |> fun e ->
  add_type e
    {
      name = res_id;
      params = [ Ident.Intern.intern "a"; Ident.Intern.intern "b" ];
      kind = Variant [ ok_info; err_info ];
      span = Span.dummy;
    }
  |> add_constructor ok_info
  |> add_constructor err_info

let add_builtin_abstract env =
  List.fold_left
    (fun env (name, params) ->
      add_type env
        {
          name = Ident.Intern.intern name;
          params;
          kind = Abstract;
          span = Span.dummy;
        })
    env
    [
      ("Unit", []);
      ("Int", []);
      ("Float", []);
      ("Bool", []);
      ("String", []);
      ("Char", []);
      ("Array", [ Ident.Intern.intern "a" ]);
      ("Ref", [ Ident.Intern.intern "a" ]);
      ("Exn", []);
    ]

let mono_prim name ty = (Ident.Intern.intern name, Ty.mono ty)

let poly1 name f =
  let tv, a = make_generic_var ~name:"a" () in
  (Ident.Intern.intern name, Ty.Forall ([ tv ], f a))

let poly2 name f =
  let tv1, a = make_generic_var ~name:"a" () in
  let tv2, b = make_generic_var ~name:"b" () in
  (Ident.Intern.intern name, Ty.Forall ([ tv1; tv2 ], f a b))

let prelude () =
  let env =
    empty |> add_builtin_abstract |> add_list_type |> add_option_type
    |> add_result_type
  in
  let i = Ty.t_int and f = Ty.t_float and b = Ty.t_bool and s = Ty.t_string in
  let bin_int op = mono_prim op (Ty.arrows [ i; i ] i) in
  let bin_float op = mono_prim op (Ty.arrows [ f; f ] f) in
  let poly_cmp name = poly1 name (fun a -> Ty.arrows [ a; a ] b) in
  let all =
    [
      bin_int "+";
      bin_int "-";
      bin_int "*";
      bin_int "/";
      bin_int "%";
      bin_float "+.";
      bin_float "-.";
      bin_float "*.";
      bin_float "/.";
      mono_prim "~-" (Ty.arrow i i);
      mono_prim "~-." (Ty.arrow f f);
      mono_prim "&&" (Ty.arrows [ b; b ] b);
      mono_prim "||" (Ty.arrows [ b; b ] b);
      mono_prim "not" (Ty.arrow b b);
      mono_prim "print_int" (Ty.arrow i Ty.t_unit);
      mono_prim "print_string" (Ty.arrow s Ty.t_unit);
      mono_prim "print_endline" (Ty.arrow s Ty.t_unit);
      mono_prim "print_float" (Ty.arrow f Ty.t_unit);
      mono_prim "print_bool" (Ty.arrow b Ty.t_unit);
      mono_prim "print_char" (Ty.arrow Ty.t_char Ty.t_unit);
      mono_prim "string_of_int" (Ty.arrow i s);
      mono_prim "int_of_string" (Ty.arrow s i);
      mono_prim "^" (Ty.arrows [ s; s ] s);
      poly_cmp "=";
      poly_cmp "==";
      poly_cmp "<>";
      poly_cmp "!=";
      poly_cmp "<";
      poly_cmp "<=";
      poly_cmp ">";
      poly_cmp ">=";
      poly1 "ignore" (fun a -> Ty.arrow a Ty.t_unit);
      poly1 "id" (fun a -> Ty.arrow a a);
      poly1 "ref" (fun a -> Ty.arrow a (Ty.t_ref a));
      poly1 "!" (fun a -> Ty.arrow (Ty.t_ref a) a);
      poly1 ":=" (fun a -> Ty.arrows [ Ty.t_ref a; a ] Ty.t_unit);
      poly1 "::" (fun a -> Ty.arrows [ a; Ty.t_list a ] (Ty.t_list a));
      poly1 "@" (fun a -> Ty.arrows [ Ty.t_list a; Ty.t_list a ] (Ty.t_list a));
      poly2 "|>" (fun a b -> Ty.arrows [ a; Ty.arrow a b ] b);
      poly2 "@@" (fun a b -> Ty.arrows [ Ty.arrow a b; a ] b);
      poly2 "fst" (fun a b -> Ty.arrow (Ty.tuple [ a; b ]) a);
      poly2 "snd" (fun a b -> Ty.arrow (Ty.tuple [ a; b ]) b);
      poly1 "List.hd" (fun a -> Ty.arrow (Ty.t_list a) a);
      poly1 "List.tl" (fun a -> Ty.arrow (Ty.t_list a) (Ty.t_list a));
      poly1 "List.length" (fun a -> Ty.arrow (Ty.t_list a) i);
      poly1 "failwith" (fun a -> Ty.arrow s a);
      mono_prim "true" b;
      mono_prim "false" b;
    ]
  in
  extend_many env all

let binop_name : Token.binop -> string = function
  | Token.Op_add -> "+"
  | Token.Op_sub -> "-"
  | Token.Op_mul -> "*"
  | Token.Op_div -> "/"
  | Token.Op_mod -> "%"
  | Token.Op_eq -> "=="
  | Token.Op_neq -> "!="
  | Token.Op_lt -> "<"
  | Token.Op_le -> "<="
  | Token.Op_gt -> ">"
  | Token.Op_ge -> ">="
  | Token.Op_and -> "&&"
  | Token.Op_or -> "||"
  | Token.Op_cons -> "::"
  | Token.Op_pipe -> "|>"

let unop_name : Token.unop -> string = function
  | Token.Op_neg -> "~-"
  | Token.Op_not -> "not"

let find_binop env op = find_value env (Ident.Intern.intern (binop_name op))
let find_unop env op = find_value env (Ident.Intern.intern (unop_name op))

let bindings env =
  let rec go acc e =
    let acc = Ident.Map.fold (fun k v a -> (k, v) :: a) e.values acc in
    match e.parent with Some p -> go acc p | None -> acc
  in
  go [] env

let pp fmt env =
  Format.fprintf fmt "@[<v>Environment:@,";
  Ident.Map.iter
    (fun id sch ->
      Format.fprintf fmt "  val %a : %a@," Ident.pp id Ty.pp_scheme sch)
    env.values;
  Ident.Map.iter
    (fun id _ -> Format.fprintf fmt "  type %a@," Ident.pp id)
    env.types;
  Ident.Map.iter
    (fun id c ->
      Format.fprintf fmt "  constr %a : %a@," Ident.pp id Ty.pp_scheme c.scheme)
    env.constructors;
  Format.fprintf fmt "@]"

let to_string env =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt env;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
