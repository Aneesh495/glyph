(** Copy propagation on SSA MIR. *)

open Mir

type subst = value Vreg.Map.t

let resolve (s : subst) v =
  let rec go v seen =
    match v with
    | VReg r -> (
        if Vreg.Set.mem r seen then v
        else
          match Vreg.Map.find_opt r s with
          | Some (VReg _ as v') -> go v' (Vreg.Set.add r seen)
          | Some v' -> v'
          | None -> v)
    | _ -> v
  in
  go v Vreg.Set.empty

let add_copy s dst = function
  | (VReg _ | VConst _) as v -> Vreg.Map.add dst v s
  | _ -> s

let run_func (f : func) : bool =
  let subst = ref Vreg.Map.empty in
  let changed = ref false in
  List.iter
    (fun l ->
      match find_block f l with
      | None -> ()
      | Some b ->
          List.iter
            (function
              | Phi { dst; incoming; _ } -> (
                  match List.map snd incoming with
                  | v :: rest when List.for_all (value_equal v) rest ->
                      subst := add_copy !subst dst (resolve !subst v)
                  | _ -> ())
              | Assign { dst; src; _ } ->
                  subst := add_copy !subst dst (resolve !subst src)
              | _ -> ())
            (block_all_instrs b))
    (Cfg.reverse_postorder f);
  if Vreg.Map.is_empty !subst then false
  else (
    let rewrite v =
      let v' = resolve !subst v in
      if not (value_equal v v') then changed := true;
      v'
    in
    Label.Map.iter
      (fun _ b ->
        b.phis <-
          List.map (map_instr_values ~on_use:rewrite ~on_def:Fun.id) b.phis;
        b.instrs <-
          List.map (map_instr_values ~on_use:rewrite ~on_def:Fun.id) b.instrs;
        b.terminator <- map_terminator_values rewrite b.terminator)
      f.blocks;
    ignore (Ssa.eliminate_trivial_phis f);
    !changed)

let pass = Pass_manager.make_func_pass "copy_prop" run_func
let run = run_func
