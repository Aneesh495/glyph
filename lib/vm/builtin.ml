(** Built-in native functions. *)

let print_int = function
  | [ Value.Int i ] ->
      Stdlib.print_int i;
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | [ v ] ->
      Stdlib.print_int (Value.as_int v);
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | _ -> failwith "print_int: arity"

let print_string = function
  | [ Value.String s ] ->
      Stdlib.print_string s;
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | [ v ] ->
      Stdlib.print_string (Value.to_string v);
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | _ -> failwith "print_string: arity"

let print_bool = function
  | [ Value.Bool b ] ->
      Stdlib.print_string (string_of_bool b);
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | [ v ] ->
      Stdlib.print_string (string_of_bool (Value.as_bool v));
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | _ -> failwith "print_bool: arity"

let print_float = function
  | [ Value.Float f ] ->
      Stdlib.print_float f;
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | [ v ] ->
      Stdlib.print_float (Value.as_float v);
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit
  | _ -> failwith "print_float: arity"

let print_char = function
  | [ Value.Int i ] ->
      Stdlib.print_char (Char.chr i);
      flush stdout;
      Value.Unit
  | [ v ] ->
      Stdlib.print_char (Char.chr (Value.as_int v));
      flush stdout;
      Value.Unit
  | _ -> failwith "print_char: arity"

let print_any = function
  | [ v ] ->
      Stdlib.print_endline (Value.to_string v);
      flush stdout;
      Value.Unit
  | vs ->
      List.iter (fun v -> Stdlib.print_string (Value.to_string v)) vs;
      Stdlib.print_newline ();
      flush stdout;
      Value.Unit

let string_of_int = function
  | [ Value.Int i ] -> Value.String (Stdlib.string_of_int i)
  | [ v ] -> Value.String (Stdlib.string_of_int (Value.as_int v))
  | _ -> failwith "string_of_int: arity"

let string_concat = function
  | [ Value.String a; Value.String b ] -> Value.String (a ^ b)
  | [ a; b ] -> Value.String (Value.to_string a ^ Value.to_string b)
  | _ -> failwith "string_concat: arity"

let string_length = function
  | [ Value.String s ] -> Value.Int (String.length s)
  | [ v ] -> Value.Int (String.length (Value.to_string v))
  | _ -> failwith "string_length: arity"

let exit_code = function
  | [ Value.Int c ] -> exit c
  | [] -> exit 0
  | _ -> failwith "exit: arity"

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
  register "print_float" print_float;
  register "print_char" print_char;
  register "print" print_any;
  register "string_of_int" string_of_int;
  register "int_to_string" string_of_int;
  register "string_concat" string_concat;
  register "string_length" string_length;
  register "abort" abort;
  register "exit" exit_code
