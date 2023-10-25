(** Built-in runtime functions for the Glyph VM.

    Builtins are invoked by well-known proto ids reserved in the negative
    range, or by name through the interpreter's builtin table. They operate
    on [Value.t] arguments and may allocate on the heap (triggering GC via
    the provided callback).
*)

type alloc_fn = Object_.t -> int
(** Allocate an object; the VM wires this to [Gc.alloc_with_gc]. *)

type context = {
  alloc : alloc_fn;
  heap : Heap.t;
  mutable out : out_channel;
  mutable err : out_channel;
}

let default_context heap alloc =
  { alloc; heap; out = stdout; err = stderr }

type builtin = {
  name : string;
  arity : int;
  fn : context -> Value.t list -> Value.t;
}

let fail name msg =
  failwith (Printf.sprintf "builtin %s: %s" name msg)

(* -------------------------------------------------------------------------- *)
(* Individual builtins                                                        *)
(* -------------------------------------------------------------------------- *)

let print_int ctx args =
  match args with
  | [ Value.ImmInt i ] ->
      Printf.fprintf ctx.out "%d" i;
      flush ctx.out;
      Value.unit
  | [ v ] ->
      Printf.fprintf ctx.out "%s" (Value.to_string v);
      flush ctx.out;
      Value.unit
  | _ -> fail "print_int" "arity"

let print_float ctx args =
  match args with
  | [ Value.ImmFloat f ] ->
      Printf.fprintf ctx.out "%g" f;
      flush ctx.out;
      Value.unit
  | [ v ] ->
      Printf.fprintf ctx.out "%s" (Value.to_string v);
      flush ctx.out;
      Value.unit
  | _ -> fail "print_float" "arity"

let print_bool ctx args =
  match args with
  | [ Value.ImmBool b ] ->
      Printf.fprintf ctx.out "%b" b;
      flush ctx.out;
      Value.unit
  | _ -> fail "print_bool" "arity"

let print_char ctx args =
  match args with
  | [ Value.ImmChar c ] ->
      output_char ctx.out c;
      flush ctx.out;
      Value.unit
  | _ -> fail "print_char" "arity"

let print_string ctx args =
  match args with
  | [ Value.Ptr addr ] ->
      let obj = Heap.get ctx.heap addr in
      let s = Object_.as_string obj in
      output_string ctx.out s;
      flush ctx.out;
      Value.unit
  | [ Value.ImmInt i ] ->
      Printf.fprintf ctx.out "%d" i;
      flush ctx.out;
      Value.unit
  | [ v ] ->
      output_string ctx.out (Value.to_string v);
      flush ctx.out;
      Value.unit
  | _ -> fail "print_string" "arity"

let print_endline ctx args =
  let _ = print_string ctx args in
  output_char ctx.out '\n';
  flush ctx.out;
  Value.unit

let print_value ctx args =
  match args with
  | [ v ] ->
      output_string ctx.out (Value.to_string v);
      flush ctx.out;
      Value.unit
  | _ -> fail "print_value" "arity"

let string_length ctx args =
  match args with
  | [ Value.Ptr addr ] ->
      let s = Object_.as_string (Heap.get ctx.heap addr) in
      Value.ImmInt (String.length s)
  | _ -> fail "string_length" "expected string"

let string_get ctx args =
  match args with
  | [ Value.Ptr addr; Value.ImmInt i ] ->
      let s = Object_.as_string (Heap.get ctx.heap addr) in
      if i < 0 || i >= String.length s then fail "string_get" "index"
      else Value.ImmChar s.[i]
  | _ -> fail "string_get" "arity/types"

let string_append ctx args =
  match args with
  | [ Value.Ptr a; Value.Ptr b ] ->
      let sa = Object_.as_string (Heap.get ctx.heap a) in
      let sb = Object_.as_string (Heap.get ctx.heap b) in
      let addr = ctx.alloc (Object_.string (sa ^ sb)) in
      Value.Ptr addr
  | _ -> fail "string_append" "expected two strings"

let int_to_string ctx args =
  match args with
  | [ Value.ImmInt i ] ->
      Value.Ptr (ctx.alloc (Object_.string (string_of_int i)))
  | _ -> fail "int_to_string" "expected int"

let float_to_string ctx args =
  match args with
  | [ Value.ImmFloat f ] ->
      Value.Ptr (ctx.alloc (Object_.string (string_of_float f)))
  | _ -> fail "float_to_string" "expected float"

let bool_to_string ctx args =
  match args with
  | [ Value.ImmBool b ] ->
      Value.Ptr (ctx.alloc (Object_.string (string_of_bool b)))
  | _ -> fail "bool_to_string" "expected bool"

let string_of_value ctx args =
  match args with
  | [ v ] -> Value.Ptr (ctx.alloc (Object_.string (Value.to_string v)))
  | _ -> fail "string_of_value" "arity"

let array_length ctx args =
  match args with
  | [ Value.Ptr addr ] -> (
      match Heap.get ctx.heap addr with
      | Object_.Array xs | Object_.Tuple xs ->
          Value.ImmInt (Array.length xs)
      | _ -> fail "array_length" "not an array")
  | _ -> fail "array_length" "arity"

let array_get ctx args =
  match args with
  | [ Value.Ptr addr; Value.ImmInt i ] -> (
      match Heap.get ctx.heap addr with
      | Object_.Array xs | Object_.Tuple xs ->
          if i < 0 || i >= Array.length xs then
            fail "array_get" "index"
          else xs.(i)
      | _ -> fail "array_get" "not an array")
  | _ -> fail "array_get" "arity"

let array_set ctx args =
  match args with
  | [ Value.Ptr addr; Value.ImmInt i; v ] -> (
      match Heap.get ctx.heap addr with
      | Object_.Array xs ->
          if i < 0 || i >= Array.length xs then
            fail "array_set" "index"
          else (
            xs.(i) <- v;
            Value.unit)
      | _ -> fail "array_set" "not a mutable array")
  | _ -> fail "array_set" "arity"

let make_array ctx args =
  match args with
  | [ Value.ImmInt n; init ] ->
      if n < 0 then fail "make_array" "negative length";
      let xs = Array.make n init in
      Value.Ptr (ctx.alloc (Object_.array xs))
  | _ -> fail "make_array" "arity"

let is_nil ctx args =
  match args with
  | [ Value.Ptr addr ] -> (
      match Heap.get ctx.heap addr with
      | Object_.Adt { tag; fields } when tag = Object_.tag_nil
        && Array.length fields = 0 ->
          Value.ImmBool true
      | _ -> Value.ImmBool false)
  | [ _ ] -> Value.ImmBool false
  | _ -> fail "is_nil" "arity"

let runtime_error _ctx args =
  let msg =
    match args with
    | [ Value.Ptr addr ] -> (
        try Object_.as_string (Heap.get _ctx.heap addr)
        with _ -> "error")
    | [ v ] -> Value.to_string v
    | _ -> "runtime error"
  in
  failwith ("Glyph runtime error: " ^ msg)

let identity _ctx args =
  match args with
  | [ v ] -> v
  | _ -> fail "identity" "arity"

let ignore_ _ctx args =
  match args with
  | [ _ ] -> Value.unit
  | _ -> fail "ignore" "arity"

let not_bool _ctx args =
  match args with
  | [ Value.ImmBool b ] -> Value.ImmBool (not b)
  | _ -> fail "not" "expected bool"

let exit_code _ctx args =
  match args with
  | [ Value.ImmInt c ] -> exit c
  | [] -> exit 0
  | _ -> fail "exit" "arity"

(* -------------------------------------------------------------------------- *)
(* Registry                                                                   *)
(* -------------------------------------------------------------------------- *)

let all : builtin list =
  [
    { name = "print_int"; arity = 1; fn = print_int };
    { name = "print_float"; arity = 1; fn = print_float };
    { name = "print_bool"; arity = 1; fn = print_bool };
    { name = "print_char"; arity = 1; fn = print_char };
    { name = "print_string"; arity = 1; fn = print_string };
    { name = "print_endline"; arity = 1; fn = print_endline };
    { name = "print"; arity = 1; fn = print_value };
    { name = "string_length"; arity = 1; fn = string_length };
    { name = "string_get"; arity = 2; fn = string_get };
    { name = "string_append"; arity = 2; fn = string_append };
    { name = "int_to_string"; arity = 1; fn = int_to_string };
    { name = "float_to_string"; arity = 1; fn = float_to_string };
    { name = "bool_to_string"; arity = 1; fn = bool_to_string };
    { name = "string_of"; arity = 1; fn = string_of_value };
    { name = "array_length"; arity = 1; fn = array_length };
    { name = "array_get"; arity = 2; fn = array_get };
    { name = "array_set"; arity = 3; fn = array_set };
    { name = "make_array"; arity = 2; fn = make_array };
    { name = "is_nil"; arity = 1; fn = is_nil };
    { name = "error"; arity = 1; fn = runtime_error };
    { name = "identity"; arity = 1; fn = identity };
    { name = "ignore"; arity = 1; fn = ignore_ };
    { name = "not"; arity = 1; fn = not_bool };
    { name = "exit"; arity = 1; fn = exit_code };
  ]

let by_name : (string, builtin) Hashtbl.t =
  let t = Hashtbl.create 32 in
  List.iter (fun b -> Hashtbl.replace t b.name b) all;
  t

let find name = Hashtbl.find_opt by_name name

let find_exn name =
  match find name with
  | Some b -> b
  | None -> failwith ("unknown builtin: " ^ name)

let call ctx name args =
  let b = find_exn name in
  if List.length args <> b.arity then
    fail name
      (Printf.sprintf "arity: expected %d got %d" b.arity
         (List.length args));
  b.fn ctx args

let names () = List.map (fun b -> b.name) all

(** Resolve a proto name to a builtin if it matches. *)
let of_proto_name name = find name
