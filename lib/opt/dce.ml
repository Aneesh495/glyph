(** Dead code elimination on SSA MIR. *)

open Mir

let effectful = function
  | Call _ | Store _ | SetField _ | Alloc _ -> true
  | _ -> false

let run_func (f : func) : bool =
  let live = Vreg.Tbl.create 64 in
  let q = Queue.create () in
  let mark v =
    if not (Vreg.Tbl.mem live v) then (
      Vreg.Tbl.add live v ();
      Queue.push v q)
  in
  Label.Map.iter
    (fun _ (b : block) ->
      List.iter mark (terminator_uses b.terminator);
      List.iter
        (fun i ->
          if effectful i then (
            List.iter mark (instr_uses i);
            List.iter mark (instr_defs i)))
        (b.phis @ b.instrs))
    f.blocks;
  let def_of = Vreg.Tbl.create 64 in
  Label.Map.iter
    (fun _ (b : block) ->
      List.iter
        (fun i ->
          List.iter (fun d -> Vreg.Tbl.replace def_of d i) (instr_defs i))
        (b.phis @ b.instrs))
    f.blocks;
  while not (Queue.is_empty q) do
    match Vreg.Tbl.find_opt def_of (Queue.pop q) with
    | Some i -> List.iter mark (instr_uses i)
    | None -> ()
  done;
  let changed = ref false in
  let keep i =
    match instr_defs i with
    | [] -> true
    | ds -> List.exists (Vreg.Tbl.mem live) ds || effectful i
  in
  Label.Map.iter
    (fun _ (b : block) ->
      let phis = List.filter keep b.phis in
      let instrs = List.filter keep b.instrs in
      if
        List.length phis <> List.length b.phis
        || List.length instrs <> List.length b.instrs
      then changed := true;
      b.phis <- phis;
      b.instrs <- instrs)
    f.blocks;
  !changed

let pass = Pass_manager.make_func_pass "dce" run_func
let run = run_func
