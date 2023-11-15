(** Sparse conditional constant propagation (simplified). *)

open Mir

let run_func (f : func) : bool =
  let changed = Const_prop.run_func f in
  let c2 = Simplify_cfg.run_func f in
  changed || c2

let pass = Pass_manager.make_func_pass "sccp" run_func
let run = run_func
