(** End-to-end Glyph compilation pipeline. *)

type options = {
  optimize : bool;
  dump_ast : bool;
  dump_types : bool;
}

val default_options : options

type result = {
  program : Ast.program;
  typed : Infer.result;
  chunk : Chunk.t option;
}

val compile_string : ?file:string -> ?options:options -> string -> (result, Diagnostic.t list) result
val compile_file : ?options:options -> string -> (result, Diagnostic.t list) result
val run_file : ?options:options -> string -> (Value.t, string) result
