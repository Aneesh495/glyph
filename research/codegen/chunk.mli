(** Bytecode chunk: code, constant pool, and function metadata. *)

type const =
  | CInt of int
  | CFloat of float
  | CBool of bool
  | CChar of char
  | CUnit
  | CString of string

type func = {
  name : string;
  fn_id : int;
  arity : int;
  nregs : int;
  (** Number of local registers (including parameters). *)
  entry : int;
  (** Instruction pointer of the entry block. *)
  is_main : bool;
}

type t = {
  mutable code : Opcode.instr array;
  mutable consts : const array;
  mutable funcs : func array;
  mutable globals : string array;
  main : int;
  (** [fn_id] of the program entry point. *)
}

val empty : unit -> t
val create :
  code:Opcode.instr array ->
  consts:const array ->
  funcs:func array ->
  ?globals:string array ->
  main:int ->
  unit ->
  t

val add_const : t -> const -> int
val const_equal : const -> const -> bool
val pp_const : Format.formatter -> const -> unit

val find_func : t -> int -> func
val find_func_by_name : t -> string -> func option
val func_count : t -> int
val code_length : t -> int

(** Serialize / deserialize [.gbc] files. *)
val magic : string
val to_bytes : t -> bytes
val of_bytes : bytes -> t
val write_file : t -> string -> unit
val read_file : string -> t

val pp : Format.formatter -> t -> unit
