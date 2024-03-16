(** Built-in native functions. *)

let output_sink : (string -> unit) option ref = ref None
let set_output_sink f = output_sink := f

let emit_out s =
  match !output_sink with
  | Some f -> f s
  | None ->
      Stdlib.print_string s;
      flush stdout

let print_int = function
  | [ Value.Int i ] ->
      emit_out (Stdlib.string_of_int i ^ "\n");
      Value.Unit
  | [ v ] ->
      emit_out (Stdlib.string_of_int (Value.as_int v) ^ "\n");
      Value.Unit
  | _ -> failwith "print_int: arity"

let print_string = function
  | [ Value.String s ] ->
      emit_out (s ^ "\n");
      Value.Unit
  | [ v ] ->
      emit_out (Value.to_string v ^ "\n");
      Value.Unit
  | _ -> failwith "print_string: arity"

let print_bool = function
  | [ Value.Bool b ] ->
      emit_out (string_of_bool b ^ "\n");
      Value.Unit
  | [ v ] ->
      emit_out (string_of_bool (Value.as_bool v) ^ "\n");
      Value.Unit
  | _ -> failwith "print_bool: arity"

let print_float = function
  | [ Value.Float f ] ->
      emit_out (Stdlib.string_of_float f ^ "\n");
      Value.Unit
  | [ v ] ->
      emit_out (Stdlib.string_of_float (Value.as_float v) ^ "\n");
      Value.Unit
  | _ -> failwith "print_float: arity"

let print_char = function
  | [ Value.Int i ] ->
      emit_out (String.make 1 (Char.chr i));
      Value.Unit
  | [ v ] ->
      emit_out (String.make 1 (Char.chr (Value.as_int v)));
      Value.Unit
  | _ -> failwith "print_char: arity"

let print_any = function
  | [ v ] ->
      emit_out (Value.to_string v ^ "\n");
      Value.Unit
  | vs ->
      List.iter (fun v -> emit_out (Value.to_string v)) vs;
      emit_out "\n";
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
