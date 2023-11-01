(** Dead code elimination. *)

open Mir

let has_side_effect = function
  | Store _ | SetField _ | Call _ -> true
  | _ -> false

let run_func (f : func) : bool =
  ignore (Cfg.ensure_cfg f);
  let live = Vreg.Tbl.create 128 in
  let work = Queue.create () in
  let mark r =
    if not (Vreg.Tbl.mem live r) then (
      Vreg.Tbl.replace live r true;
      Queue.add r work)
  in
  Label.Map.iter
    (fun _ b ->
      List.iter mark (terminator_uses b.terminator);
      List.iter
        (fun instr ->
          if has_side_effect instr then List.iter mark (instr_uses instr))
        (block_all_instrs b))
    f.blocks;
  let def_uses = Vreg.Tbl.create 128 in
  Label.Map.iter
    (fun _ b ->
      List.iter
        (fun instr ->
          List.iter
            (fun d -> Vreg.Tbl.replace def_uses d (instr_uses instr))
            (instr_defs instr))
        (block_all_instrs b))
    f.blocks;
  while not (Queue.is_empty work) do
    let r = Queue.take work in
    match Vreg.Tbl.find_opt def_uses r with
    | None -> ()
    | Some uses -> List.iter mark uses
  done;
  let changed = ref false in
  Label.Map.iter
    (fun _ b ->
      let filter instrs =
        List.filter
          (fun instr ->
            if has_side_effect instr then true
            else
              match instr_defs instr with
              | [] -> true
              | ds ->
                  let keep = List.exists (Vreg.Tbl.mem live) ds in
                  if not keep then changed := true;
                  keep)
          instrs
      in
      b.phis <- filter b.phis;
      b.instrs <- filter b.instrs)
    f.blocks;
  !changed

let pass = Pass_manager.make_func_pass "dce" run_func
let run = run_func
