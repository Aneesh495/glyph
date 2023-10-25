(** Built-in native functions. *)

let print_int = function
  | [ Value.Int i ] ->
      print_int i;
      print_newline ();
      Value.Unit
  | [ v ] ->
      print_int (Value.as_int v);
      print_newline ();
      Value.Unit
  | _ -> failwith "print_int: arity"

let print_string = function
  | [ Value.String s ] ->
      print_string s;
      print_newline ();
      Value.Unit
  | [ v ] ->
      print_string (Value.to_string v);
      print_newline ();
      Value.Unit
  | _ -> failwith "print_string: arity"

let print_bool = function
  | [ Value.Bool b ] ->
      print_string (string_of_bool b);
      print_newline ();
      Value.Unit
  | [ v ] ->
      print_string (string_of_bool (Value.as_bool v));
      print_newline ();
      Value.Unit
  | _ -> failwith "print_bool: arity"

let print_any = function
  | [ v ] ->
      print_endline (Value.to_string v);
      Value.Unit
  | vs ->
      List.iter (fun v -> print_string (Value.to_string v)) vs;
      print_newline ();
      Value.Unit

let string_of_int = function
  | [ Value.Int i ] -> Value.String (Stdlib.string_of_int i)
  | [ v ] -> Value.String (Stdlib.string_of_int (Value.as_int v))
  | _ -> failwith "string_of_int: arity"

let string_concat = function
  | [ Value.String a; Value.String b ] -> Value.String (a ^ b)
  | [ a; b ] -> Value.String (Value.to_string a ^ Value.to_string b)
  | _ -> failwith "string_concat: arity"

let abort = function
  | [] -> failwith "abort"
  | vs ->
      failwith
        ("abort: " ^ String.concat " " (List.map Value.to_string vs))

let table : (string, Value.native) Hashtbl.t = Hashtbl.create 32

let register name fn = Hashtbl.replace table name fn

let lookup name = Hashtbl.find_opt table name

let all () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table []

let () =
  register "print_int" print_int;
  register "print_string" print_string;
  register "print_bool" print_bool;
  register "print" print_any;
  register "string_of_int" string_of_int;
  register "string_concat" string_concat;
  register "abort" abort
