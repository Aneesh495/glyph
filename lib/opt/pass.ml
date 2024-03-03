(** Core optimization pass infrastructure. *)

open Mir

type stats = {
  mutable iterations : int;
  mutable pass_runs : (string * int) list;
  mutable rewritten : int;
  mutable removed : int;
}

let empty_stats () =
  { iterations = 0; pass_runs = []; rewritten = 0; removed = 0 }

type context = {
  stats : stats;
}

let make_context ?(stats = empty_stats ()) () = { stats }

type pass = {
  name : string;
  run_func : context -> func -> func;
  run_program : context -> program -> program;
}

let make_func_pass ~name run_func =
  {
    name;
    run_func;
    run_program =
      (fun ctx prog ->
        let functions = List.map (run_func ctx) prog.functions in
        { prog with functions });
  }

let make_program_pass ~name run_program =
  {
    name;
    run_func = (fun _ fn -> fn);
    run_program;
  }
