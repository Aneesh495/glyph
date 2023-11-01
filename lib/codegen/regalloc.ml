(** Linear-scan register allocation for Ident-based SSA MIR. *)

type preg = int

type interval = {
  vreg : Mir.vreg;
  mutable start : int;
  mutable end_ : int;
}

type result = {
  mapping : preg Mir.Vreg.Map.t;
  n_regs : int;
  intervals : interval list;
}

let successors (fn : Mir.func) (lbl : Mir.label) =
  match Mir.find_block fn lbl with
  | None -> []
  | Some b -> Mir.terminator_succs b.terminator

let rpo_labels (fn : Mir.func) =
  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec dfs lbl =
    if Hashtbl.mem visited lbl then ()
    else (
      Hashtbl.add visited lbl ();
      List.iter dfs (successors fn lbl);
      order := lbl :: !order)
  in
  dfs fn.entry;
  Mir.iter_blocks
    (fun (b : Mir.block) ->
      if not (Hashtbl.mem visited b.label) then (
        Hashtbl.add visited b.label ();
        order := !order @ [ b.label ]))
    fn;
  List.rev !order

type numbered = {
  block_start : (Mir.label, int) Hashtbl.t;
  phi_points : (Mir.label, int list) Hashtbl.t;
  instr_points : (Mir.label, int list) Hashtbl.t;
  term_point : (Mir.label, int) Hashtbl.t;
  block_end : (Mir.label, int) Hashtbl.t;
  mutable next : int;
}

let number_func fn =
  let n =
    {
      block_start = Hashtbl.create 16;
      phi_points = Hashtbl.create 16;
      instr_points = Hashtbl.create 16;
      term_point = Hashtbl.create 16;
      block_end = Hashtbl.create 16;
      next = 0;
    }
  in
  let alloc () =
    let p = n.next in
    n.next <- p + 1;
    p
  in
  List.iter
    (fun lbl ->
      match Mir.find_block fn lbl with
      | None -> ()
      | Some b ->
          Hashtbl.replace n.block_start lbl (alloc ());
          Hashtbl.replace n.phi_points lbl (List.map (fun _ -> alloc ()) b.phis);
          Hashtbl.replace n.instr_points lbl
            (List.map (fun _ -> alloc ()) b.instrs);
          let tp = alloc () in
          Hashtbl.replace n.term_point lbl tp;
          Hashtbl.replace n.block_end lbl tp)
    (rpo_labels fn);
  n

module VSet = Mir.Vreg.Set

let compute_intervals fn num =
  let labels = Mir.func_labels fn in
  let label_index = Hashtbl.create 16 in
  List.iteri (fun i l -> Hashtbl.replace label_index l i) labels;
  let idx_of l = Hashtbl.find label_index l in
  let nblocks = List.length labels in
  let live_in = Array.make nblocks VSet.empty in
  let live_out = Array.make nblocks VSet.empty in
  let block_use = Array.make nblocks VSet.empty in
  let block_def = Array.make nblocks VSet.empty in
  List.iter
    (fun lbl ->
      match Mir.find_block fn lbl with
      | None -> ()
      | Some b ->
          let i = idx_of lbl in
          let use = ref VSet.empty in
          let def = ref VSet.empty in
          let add_use v =
            if not (VSet.mem v !def) then use := VSet.add v !use
          in
          let add_def v = def := VSet.add v !def in
          List.iter
            (fun instr ->
              List.iter add_use (Mir.instr_uses instr);
              List.iter add_def (Mir.instr_defs instr))
            (Mir.block_all_instrs b);
          List.iter add_use (Mir.terminator_uses b.terminator);
          block_use.(i) <- !use;
          block_def.(i) <- !def)
    labels;
  let pred_phi_uses : (Mir.label, VSet.t) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace pred_phi_uses l VSet.empty) labels;
  List.iter
    (fun lbl ->
      match Mir.find_block fn lbl with
      | None -> ()
      | Some b ->
          List.iter
            (function
              | Mir.Phi { incoming; _ } ->
                  List.iter
                    (fun (pred, v) ->
                      List.iter
                        (fun u ->
                          let s =
                            try Hashtbl.find pred_phi_uses pred
                            with Not_found -> VSet.empty
                          in
                          Hashtbl.replace pred_phi_uses pred (VSet.add u s))
                        (Mir.value_uses v))
                    incoming
              | _ -> ())
            b.phis)
    labels;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun lbl ->
        let i = idx_of lbl in
        let out = ref VSet.empty in
        List.iter
          (fun succ -> out := VSet.union !out live_in.(idx_of succ))
          (successors fn lbl);
        out :=
          VSet.union !out
            (try Hashtbl.find pred_phi_uses lbl with Not_found -> VSet.empty);
        if not (VSet.equal !out live_out.(i)) then (
          live_out.(i) <- !out;
          changed := true);
        let new_in =
          VSet.union block_use.(i) (VSet.diff live_out.(i) block_def.(i))
        in
        if not (VSet.equal new_in live_in.(i)) then (
          live_in.(i) <- new_in;
          changed := true))
      (List.rev labels)
  done;
  let intervals : (Mir.vreg, interval) Hashtbl.t = Hashtbl.create 64 in
  let touch v point =
    match Hashtbl.find_opt intervals v with
    | Some iv ->
        if point < iv.start then iv.start <- point;
        if point > iv.end_ then iv.end_ <- point
    | None ->
        Hashtbl.add intervals v { vreg = v; start = point; end_ = point }
  in
  List.iter
    (fun (v, _) ->
      touch v (try Hashtbl.find num.block_start fn.entry with Not_found -> 0))
    fn.params;
  List.iter
    (fun lbl ->
      match Mir.find_block fn lbl with
      | None -> ()
      | Some b ->
          let phi_ps =
            try Hashtbl.find num.phi_points lbl with Not_found -> []
          in
          List.iter2
            (fun pt instr ->
              List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
              List.iter (fun d -> touch d pt) (Mir.instr_defs instr))
            phi_ps b.phis;
          let ips =
            try Hashtbl.find num.instr_points lbl with Not_found -> []
          in
          List.iter2
            (fun pt instr ->
              List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
              List.iter (fun d -> touch d pt) (Mir.instr_defs instr))
            ips b.instrs;
          let tp = Hashtbl.find num.term_point lbl in
          List.iter (fun u -> touch u tp) (Mir.terminator_uses b.terminator);
          let bi = idx_of lbl in
          let bs = Hashtbl.find num.block_start lbl in
          let be = Hashtbl.find num.block_end lbl in
          VSet.iter
            (fun v ->
              touch v bs;
              touch v be)
            (VSet.union live_in.(bi) live_out.(bi)))
    labels;
  Hashtbl.fold (fun _ iv acc -> iv :: acc) intervals []

type active_entry = {
  preg : preg;
  interval : interval;
}

let linear_scan intervals =
  let sorted =
    List.sort
      (fun a b ->
        match Int.compare a.start b.start with
        | 0 -> Mir.Vreg.compare a.vreg b.vreg
        | c -> c)
      intervals
  in
  let mapping = ref Mir.Vreg.Map.empty in
  let active = ref [] in
  let free_pool = ref [] in
  let next_preg = ref 0 in
  let expire_old start =
    let kept, expired =
      List.partition (fun e -> e.interval.end_ >= start) !active
    in
    active :=
      List.sort
        (fun a b -> Int.compare a.interval.end_ b.interval.end_)
        kept;
    List.iter (fun e -> free_pool := e.preg :: !free_pool) expired
  in
  let alloc_preg () =
    match !free_pool with
    | p :: rest ->
        free_pool := rest;
        p
    | [] ->
        let p = !next_preg in
        incr next_preg;
        p
  in
  List.iter
    (fun iv ->
      expire_old iv.start;
      let p = alloc_preg () in
      mapping := Mir.Vreg.Map.add iv.vreg p !mapping;
      active :=
        List.sort
          (fun a b -> Int.compare a.interval.end_ b.interval.end_)
          ({ preg = p; interval = iv } :: !active))
    sorted;
  (!mapping, !next_preg)

let allocate (fn : Mir.func) : result =
  let num = number_func fn in
  let intervals = compute_intervals fn num in
  let mapping, n_regs = linear_scan intervals in
  let mapping = ref mapping in
  let n_regs = ref n_regs in
  List.iter
    (fun (v, _) ->
      if not (Mir.Vreg.Map.mem v !mapping) then (
        mapping := Mir.Vreg.Map.add v !n_regs !mapping;
        incr n_regs))
    fn.params;
  { mapping = !mapping; n_regs = max !n_regs 1; intervals }

let lookup (r : result) (v : Mir.vreg) : preg =
  match Mir.Vreg.Map.find_opt v r.mapping with
  | Some p -> p
  | None -> 0

let lookup_opt r v = Mir.Vreg.Map.find_opt v r.mapping

let identity (fn : Mir.func) : result =
  let mapping = ref Mir.Vreg.Map.empty in
  let next = ref 0 in
  let add v =
    if not (Mir.Vreg.Map.mem v !mapping) then (
      mapping := Mir.Vreg.Map.add v !next !mapping;
      incr next)
  in
  List.iter (fun (v, _) -> add v) fn.params;
  Mir.iter_blocks
    (fun b ->
      List.iter
        (fun i ->
          List.iter add (Mir.instr_defs i);
          List.iter add (Mir.instr_uses i))
        (Mir.block_all_instrs b);
      List.iter add (Mir.terminator_uses b.terminator))
    fn;
  { mapping = !mapping; n_regs = max !next 1; intervals = [] }

let validate fn r =
  let errs = ref [] in
  let check v =
    if not (Mir.Vreg.Map.mem v r.mapping) then
      errs :=
        Printf.sprintf "unmapped %s" (Mir.Vreg.to_string v) :: !errs
  in
  List.iter (fun (v, _) -> check v) fn.params;
  Mir.iter_blocks
    (fun b ->
      List.iter
        (fun i ->
          List.iter check (Mir.instr_defs i);
          List.iter check (Mir.instr_uses i))
        (Mir.block_all_instrs b);
      List.iter check (Mir.terminator_uses b.terminator))
    fn;
  List.rev !errs

let pp_result fmt r =
  Format.fprintf fmt "regalloc: %d regs\n" r.n_regs;
  Mir.Vreg.Map.iter
    (fun v p -> Format.fprintf fmt "  %a -> r%d\n" Mir.Vreg.pp v p)
    r.mapping
