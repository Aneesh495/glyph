(** Pass manager interface. *)

type level =
  | O0
  | O1
  | O2
  | O3

type stats = {
  mutable rewritten : int;
  mutable removed : int;
  mutable inlined : int;
}

type context = {
  level : level;
  mutable stats : stats;
  verbose : bool;
}

val empty_stats : unit -> stats
val make_context : ?verbose:bool -> level -> context

type pass = {
  name : string;
  run_func : context -> Mir.func -> Mir.func;
  run_program : context -> Mir.program -> Mir.program;
}

val make_func_pass :
  name:string -> (context -> Mir.func -> Mir.func) -> pass

val make_prog_pass :
  name:string -> (context -> Mir.program -> Mir.program) -> pass

val run_pass : context -> pass -> Mir.program -> Mir.program
val run_pipeline : context -> pass list -> Mir.program -> Mir.program
val level_to_string : level -> string
