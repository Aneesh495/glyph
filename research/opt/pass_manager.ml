(** Optimization pass manager.

    A pass is a named transformation over a [Mir.program] (or per-function).
    Pipelines compose passes with optional fixed-point iteration and
    per-function vs whole-program scope. *)

open Mir

type pass_level =
  | Function_level
  | Module_level

type pass = {
  name : string;
  level : pass_level;
  run_func : func -> bool;
  (** Returns [true] if the function changed. *)
  run_program : program -> bool;
  (** Returns [true] if the program changed. Unused for function-level. *)
}

type pipeline = {
  name : string;
  passes : pass list;
  max_iterations : int;
}

type stats = {
  mutable iterations : int;
  mutable pass_runs : (string * int) list;
  (** pass name × times it reported a change *)
}

let empty_stats () = { iterations = 0; pass_runs = [] }

let make_func_pass name run_func =
  {
    name;
    level = Function_level;
    run_func;
    run_program = (fun _ -> false);
  }

let make_program_pass name run_program =
  {
    name;
    level = Module_level;
    run_func = (fun _ -> false);
    run_program;
  }

let record_change stats name =
  let rec bump = function
    | [] -> [ (name, 1) ]
    | (n, c) :: rest when String.equal n name -> (n, c + 1) :: rest
    | x :: rest -> x :: bump rest
  in
  stats.pass_runs <- bump stats.pass_runs

let run_pass_on_func pass f =
  match pass.level with
  | Function_level -> pass.run_func f
  | Module_level -> false

let run_pass_on_program pass prog =
  match pass.level with
  | Module_level -> pass.run_program prog
  | Function_level ->
      let changed = ref false in
      Ident.Map.iter
        (fun _ f -> if pass.run_func f then changed := true)
        prog.funcs;
      !changed

let run_pipeline ?(stats = empty_stats ()) (pipe : pipeline) (prog : program)
    : program =
  let rec loop iter =
    if iter >= pipe.max_iterations then ()
    else
      let any = ref false in
      List.iter
        (fun pass ->
          let changed = run_pass_on_program pass prog in
          if changed then (
            any := true;
            record_change stats pass.name))
        pipe.passes;
      stats.iterations <- stats.iterations + 1;
      if !any then loop (iter + 1)
  in
  loop 0;
  prog

let run = run_pipeline

(** Single sweep — no fixed point. *)
let run_once ?(stats = empty_stats ()) passes prog =
  run_pipeline ~stats
    { name = "once"; passes; max_iterations = 1 }
    prog

(* -------------------------------------------------------------------------- *)
(* Standard pipelines                                                         *)
(* -------------------------------------------------------------------------- *)

(** Placeholders filled by [register_standard] once pass modules are linked.
    Avoids circular init issues by late binding. *)
let standard_passes : pass list ref = ref []

let register_standard passes = standard_passes := passes

let o0_pipeline =
  { name = "O0"; passes = []; max_iterations = 1 }

let default_pipeline () =
  {
    name = "O2";
    passes = !standard_passes;
    max_iterations = 8;
  }

let run_default prog =
  let stats = empty_stats () in
  let prog = run_pipeline ~stats (default_pipeline ()) prog in
  (prog, stats)

let pp_stats fmt stats =
  Format.fprintf fmt "iterations: %d\n" stats.iterations;
  List.iter
    (fun (n, c) -> Format.fprintf fmt "  %s changed × %d\n" n c)
    stats.pass_runs

(** Helper: wrap a unit-returning rewriter that we approximate as "changed"
    by comparing instruction counts. *)
let changed_by_count (f : func) (rewrite : func -> unit) : bool =
  let before = func_instr_count f in
  rewrite f;
  func_instr_count f <> before

(** Helper: track an explicit changed flag. *)
let with_flag (f : func) (rewrite : func -> bool ref -> unit) : bool =
  let flag = ref false in
  rewrite f flag;
  !flag
