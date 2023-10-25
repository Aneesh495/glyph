(** Native builtins callable from bytecode / the prelude. *)

val print_int : Value.t list -> Value.t
val print_string : Value.t list -> Value.t
val print_bool : Value.t list -> Value.t
val print_any : Value.t list -> Value.t
val string_of_int : Value.t list -> Value.t
val string_concat : Value.t list -> Value.t
val abort : Value.t list -> Value.t

val lookup : string -> Value.native option
val register : string -> Value.native -> unit
val all : unit -> (string * Value.native) list
