(** Copy propagation on SSA MIR. *)

open Mir

let run_func (f : func) : bool =
  let alias = Hashtbl.create 64 in
  let rec resolve = function
    | VReg r as v -> (
        match Hashtbl.find_opt alias r with
        | Some v' when not (value_equal v v') -> resolve v'
        | Some v' -> v'
        | None -> v)
    | v -> v
  in
  Label.Map.iter
    (fun _ (b : block) ->
      List.iter
        (function
          | Assign { dst; src; _ } ->
              Hashtbl.replace alias dst (resolve src)
          | Phi { dst; incoming; _ } -> (
              let ops = List.map (fun (_, v) -> resolve v) incoming in
              match ops with
              | v :: rest when List.for_all (value_equal v) rest ->
                  Hashtbl.replace alias dst v
              | _ -> ())
          | _ -> ())
        (b.phis @ b.instrs))
    f.blocks;
  let changed = ref false in
  let rw v =
    let v' = resolve v in
    if not (value_equal v v') then changed := true;
    v'
  in
  Label.Map.iter
    (fun _ (b : block) ->
      b.phis <-
        List.map (map_instr_values ~on_use:rw ~on_def:Fun.id) b.phis;
      b.instrs <-
        List.map (map_instr_values ~on_use:rw ~on_def:Fun.id) b.instrs;
      b.terminator <- map_terminator_values rw b.terminator)
    f.blocks;
  !changed

let pass = Pass_manager.make_func_pass "copy_prop" run_func
let run = run_func
