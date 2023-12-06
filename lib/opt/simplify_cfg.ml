(** CFG simplification: unreachable elimination, jump threading, merges. *)

open Mir

let thread_jumps (f : func) : bool =
  ignore (Cfg.ensure_cfg f);
  let changed = ref false in
  let rec ultimate l seen =
    if Label.Set.mem l seen then l
    else
      match find_block f l with
      | Some b
        when b.phis = [] && b.instrs = []
             &&
             match b.terminator with Jump _ -> true | _ -> false -> (
          match b.terminator with
          | Jump (dst, _) -> ultimate dst (Label.Set.add l seen)
          | _ -> l)
      | _ -> l
  in
  Label.Map.iter
    (fun _ b ->
      let map_l l =
        let l' = ultimate l Label.Set.empty in
        if not (Label.equal l l') then changed := true;
        l'
      in
      b.terminator <-
        (match b.terminator with
        | Jump (l, sp) -> Jump (map_l l, sp)
        | Branch ({ then_; else_; _ } as br) ->
            let then_' = map_l then_ in
            let else_' = map_l else_ in
            if Label.equal then_' else_' then (
              changed := true;
              Jump (then_', br.span))
            else Branch { br with then_ = then_'; else_ = else_' }
        | Switch ({ cases; default; _ } as sw) ->
            Switch
              {
                sw with
                cases = List.map (fun (t, l) -> (t, map_l l)) cases;
                default = map_l default;
              }
        | t -> t))
    f.blocks;
  if !changed then ignore (Cfg.ensure_cfg f);
  !changed

let remove_unreachable (f : func) : bool =
  match Cfg.prune_unreachable f with
  | f' ->
      (* prune_unreachable may return same ref; count via size change *)
      let before = Label.Map.cardinal f.blocks in
      ignore f';
      let after = Label.Map.cardinal f'.blocks in
      before <> after

let collapse_same_target_branches (f : func) : bool =
  let changed = ref false in
  Label.Map.iter
    (fun _ b ->
      match b.terminator with
      | Branch { then_; else_; span; _ } when Label.equal then_ else_ ->
          b.terminator <- Jump (then_, span);
          changed := true
      | Switch { cases; default; span; _ }
        when List.for_all (fun (_, l) -> Label.equal l default) cases ->
          b.terminator <- Jump (default, span);
          changed := true
      | _ -> ())
    f.blocks;
  if !changed then ignore (Cfg.ensure_cfg f);
  !changed

let merge_blocks (f : func) : bool =
  Cfg.simplify_trivial_merges f > 0

let run_func (f : func) : bool =
  let changed = ref false in
  let bump b = if b then changed := true in
  bump (remove_unreachable f);
  bump (thread_jumps f);
  bump (collapse_same_target_branches f);
  bump (merge_blocks f);
  bump (remove_unreachable f);
  bump (thread_jumps f);
  ignore (Ssa.cleanup f);
  ignore (Cfg.ensure_cfg f);
  !changed

let pass = Pass_manager.make_func_pass "simplify_cfg" run_func
let run = run_func
