(** End-to-end Glyph compilation pipeline. *)

type options = {
  optimize : bool;
  opt_level : int;
  dump_ast : bool;
  dump_types : bool;
}

val default_options : options

type result = {
  program : Ast.program;
  env : Env.t;
  hir : Hir.program option;
  mir : Mir.program option;
  chunk : Chunk.t option;
}

val parse : ?file:string -> string -> (Ast.program, Diagnostic.t list) Stdlib.result
val typecheck : Ast.program -> (Env.t, Diagnostic.t list) Stdlib.result
val compile_string : ?file:string -> ?options:options -> string -> (result, Diagnostic.t list) Stdlib.result
val compile_file : ?options:options -> string -> (result, Diagnostic.t list) Stdlib.result
val run_file : ?options:options -> string -> (Value.t, string) Stdlib.result
