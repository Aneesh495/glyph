(** Sparse Conditional Constant Propagation (composed). *)

open Mir

let fold_constant_branches (f : func) : bool =
  let changed = ref false in
  Label.Map.iter
    (fun _ (b : block) ->
      match b.terminator with
      | Branch { cond = VConst (CBool true); then_; span; _ }
      | Branch { cond = VConst (CInt n); then_; span; _ } when n <> 0 ->
          b.terminator <- Jump (then_, span);
          changed := true
      | Branch { cond = VConst (CBool false); else_; span; _ }
      | Branch { cond = VConst (CInt 0); else_; span; _ } ->
          b.terminator <- Jump (else_, span);
          changed := true
      | _ -> ())
    f.blocks;
  !changed

let run_func (f : func) : bool =
  let a = Const_prop.run_func f in
  let b = fold_constant_branches f in
  let c = Simplify_cfg.run_func f in
  a || b || c

let pass = Pass_manager.make_func_pass "sccp" run_func
let run = run_func
