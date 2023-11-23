(** CFG simplification: unreachable elimination, branch folding. *)

open Mir

let run_func (fn : func) : func =
  let fn = Cfg.prune_unreachable fn in
  let blocks =
    List.map
      (fun (b : block) ->
        let term =
          match b.term with
          | TBranch (_, t, e) when t = e -> TJump t
          | TSwitch (_, cases, d)
            when List.for_all (fun (_, l) -> l = d) cases ->
              TJump d
          | t -> t
        in
        { b with term })
      fn.blocks
  in
  Cfg.prune_unreachable { fn with blocks }

let pass = Pass_manager.make_func_pass "simplify_cfg" run_func
let run = run_func
