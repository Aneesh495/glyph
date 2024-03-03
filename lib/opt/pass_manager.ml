(** Optimization pass manager.

    A pass is a named transformation over a [Mir.program] (or per-function).
    Pipelines compose passes with optional fixed-point iteration. *)

open Mir

type pass = Pass.pass
type stats = Pass.stats

let empty_stats = Pass.empty_stats

type pipeline = {
  name : string;
  passes : pass list;
  max_iterations : int;
}

let run_pipeline ?(stats = empty_stats ()) (pipe : pipeline) (prog : program)
    : program =
  let ctx = Pass.make_context ~stats () in
  let rec loop iter current_prog =
    if iter >= pipe.max_iterations then current_prog
    else
      let before_rewritten = stats.rewritten in
      let before_removed = stats.removed in
      let next_prog =
        List.fold_left
          (fun p pass -> pass.Pass.run_program ctx p)
          current_prog pipe.passes
      in
      stats.iterations <- stats.iterations + 1;
      let changed =
        stats.rewritten <> before_rewritten || stats.removed <> before_removed
      in
      if changed then loop (iter + 1) next_prog else next_prog
  in
  loop 0 prog

let run = run_pipeline

let run_once ?(stats = empty_stats ()) passes prog =
  run_pipeline ~stats { name = "once"; passes; max_iterations = 1 } prog

let standard_passes : pass list ref = ref []

let register_standard passes = standard_passes := passes

let o0_pipeline = { name = "O0"; passes = []; max_iterations = 1 }

let o1_pipeline () =
  {
    name = "O1";
    passes = [ Const_prop.pass; Copy_prop.pass; Dce.pass ];
    max_iterations = 4;
  }

let o2_pipeline () =
  {
    name = "O2";
    passes =
      if !standard_passes <> [] then !standard_passes
      else
        [
          Simplify_cfg.pass;
          Const_prop.pass;
          Copy_prop.pass;
          Cse.pass;
          Dce.pass;
          Simplify_cfg.pass;
        ];
    max_iterations = 8;
  }

let default_pipeline = o2_pipeline

let run_default prog =
  let stats = empty_stats () in
  let prog = run_pipeline ~stats (default_pipeline ()) prog in
  (prog, stats)

let pp_stats fmt (stats : stats) =
  Format.fprintf fmt "iterations: %d, rewritten: %d, removed: %d\n"
    stats.iterations stats.rewritten stats.removed
