(** Linear-scan register allocation for int-vreg MIR. *)

type preg = int
type vreg = Mir.vreg

type interval = {
  vreg : vreg;
  mutable start : int;
  mutable end_ : int;
}

type result = {
  mapping : preg array;
  n_regs : int;
  intervals : interval list;
}

let rpo_blocks (fn : Mir.func) =
  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec dfs lbl =
    if Hashtbl.mem visited lbl then ()
    else (
      Hashtbl.add visited lbl ();
      List.iter dfs (Mir.successors_of fn lbl);
      match Mir.find_block_opt fn lbl with
      | Some b -> order := b :: !order
      | None -> ())
  in
  dfs fn.entry;
  List.iter
    (fun (b : Mir.block) ->
      if not (Hashtbl.mem visited b.label) then order := !order @ [ b ])
    fn.blocks;
  List.rev !order

let number_func fn =
  let next = ref 0 in
  let alloc () =
    let p = !next in
    incr next;
    p
  in
  let block_start = Hashtbl.create 16 in
  let block_end = Hashtbl.create 16 in
  let phi_points = Hashtbl.create 16 in
  let instr_points = Hashtbl.create 16 in
  let term_point = Hashtbl.create 16 in
  List.iter
    (fun (b : Mir.block) ->
      Hashtbl.replace block_start b.label (alloc ());
      Hashtbl.replace phi_points b.label (List.map (fun _ -> alloc ()) b.phis);
      Hashtbl.replace instr_points b.label
        (List.map (fun _ -> alloc ()) b.instrs);
      let tp = alloc () in
      Hashtbl.replace term_point b.label tp;
      Hashtbl.replace block_end b.label tp)
    (rpo_blocks fn);
  (block_start, block_end, phi_points, instr_points, term_point, !next)

module VSet = Set.Make (Int)

let compute_intervals fn =
  let block_start, block_end, phi_points, instr_points, term_point, _ =
    number_func fn
  in
  let labels = List.map (fun (b : Mir.block) -> b.label) fn.blocks in
  let label_index = Hashtbl.create 16 in
  List.iteri (fun i l -> Hashtbl.replace label_index l i) labels;
  let idx_of l = Hashtbl.find label_index l in
  let n = List.length labels in
  let live_in = Array.make n VSet.empty in
  let live_out = Array.make n VSet.empty in
  let block_use = Array.make n VSet.empty in
  let block_def = Array.make n VSet.empty in
  List.iter
    (fun (b : Mir.block) ->
      let i = idx_of b.label in
      let use = ref VSet.empty in
      let def = ref VSet.empty in
      let add_use v =
        if not (VSet.mem v !def) then use := VSet.add v !use
      in
      let add_def v = def := VSet.add v !def in
      List.iter
        (fun instr ->
          List.iter add_use (Mir.instr_uses instr);
          Option.iter add_def (Mir.instr_def instr))
        (b.phis @ b.instrs);
      List.iter add_use (Mir.term_uses b.term);
      block_use.(i) <- !use;
      block_def.(i) <- !def)
    fn.blocks;
  let pred_phi = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace pred_phi l VSet.empty) labels;
  List.iter
    (fun (b : Mir.block) ->
      List.iter
        (function
          | Mir.IPhi (_, incoming) ->
              List.iter
                (fun (pred, v) ->
                  let s =
                    try Hashtbl.find pred_phi pred with Not_found -> VSet.empty
                  in
                  Hashtbl.replace pred_phi pred (VSet.add v s))
                incoming
          | _ -> ())
        b.phis)
    fn.blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (b : Mir.block) ->
        let i = idx_of b.label in
        let out = ref VSet.empty in
        List.iter
          (fun s -> out := VSet.union !out live_in.(idx_of s))
          (Mir.term_successors b.term);
        out :=
          VSet.union !out
            (try Hashtbl.find pred_phi b.label with Not_found -> VSet.empty);
        if not (VSet.equal !out live_out.(i)) then (
          live_out.(i) <- !out;
          changed := true);
        let nin =
          VSet.union block_use.(i) (VSet.diff live_out.(i) block_def.(i))
        in
        if not (VSet.equal nin live_in.(i)) then (
          live_in.(i) <- nin;
          changed := true))
      (List.rev fn.blocks)
  done;
  let intervals = Hashtbl.create 64 in
  let touch v p =
    match Hashtbl.find_opt intervals v with
    | Some iv ->
        if p < iv.start then iv.start <- p;
        if p > iv.end_ then iv.end_ <- p
    | None -> Hashtbl.add intervals v { vreg = v; start = p; end_ = p }
  in
  let entry_pt =
    try Hashtbl.find block_start fn.entry with Not_found -> 0
  in
  List.iter (fun v -> touch v entry_pt) fn.params;
  List.iter
    (fun (b : Mir.block) ->
      let pps = try Hashtbl.find phi_points b.label with Not_found -> [] in
      List.iter2
        (fun pt instr ->
          List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
          Option.iter (fun d -> touch d pt) (Mir.instr_def instr))
        pps b.phis;
      let ips = try Hashtbl.find instr_points b.label with Not_found -> [] in
      List.iter2
        (fun pt instr ->
          List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
          Option.iter (fun d -> touch d pt) (Mir.instr_def instr))
        ips b.instrs;
      let tp = Hashtbl.find term_point b.label in
      List.iter (fun u -> touch u tp) (Mir.term_uses b.term);
      let bi = idx_of b.label in
      let bs = Hashtbl.find block_start b.label in
      let be = Hashtbl.find block_end b.label in
      VSet.iter
        (fun v ->
          touch v bs;
          touch v be)
        (VSet.union live_in.(bi) live_out.(bi)))
    fn.blocks;
  List.iter
    (fun v -> if not (Hashtbl.mem intervals v) then touch v 0)
    (Mir.all_vregs fn);
  Hashtbl.fold (fun _ iv acc -> iv :: acc) intervals []

let linear_scan intervals =
  let sorted =
    List.sort
      (fun a b ->
        match Int.compare a.start b.start with
        | 0 -> Int.compare a.vreg b.vreg
        | c -> c)
      intervals
  in
  let max_v = List.fold_left (fun m iv -> max m iv.vreg) (-1) intervals in
  let mapping = Array.make (max_v + 1) (-1) in
  let active = ref [] in
  let free = ref [] in
  let next = ref 0 in
  let expire start =
    let kept, expired =
      List.partition (fun (_p, iv) -> iv.end_ >= start) !active
    in
    active :=
      List.sort (fun (_, a) (_, b) -> Int.compare a.end_ b.end_) kept;
    List.iter (fun (p, _) -> free := p :: !free) expired
  in
  let alloc () =
    match !free with
    | p :: r ->
        free := r;
        p
    | [] ->
        let p = !next in
        incr next;
        p
  in
  List.iter
    (fun iv ->
      expire iv.start;
      let p = alloc () in
      mapping.(iv.vreg) <- p;
      active :=
        List.sort
          (fun (_, a) (_, b) -> Int.compare a.end_ b.end_)
          ((p, iv) :: !active))
    sorted;
  (mapping, !next)

let allocate fn =
  let intervals = compute_intervals fn in
  let mapping, n_regs = linear_scan intervals in
  let n_regs = ref n_regs in
  List.iter
    (fun v ->
      if v < Array.length mapping && mapping.(v) < 0 then (
        mapping.(v) <- !n_regs;
        incr n_regs))
    fn.params;
  { mapping; n_regs = max !n_regs 1; intervals }

let lookup r v =
  if v < 0 || v >= Array.length r.mapping || r.mapping.(v) < 0 then 0
  else r.mapping.(v)

let identity fn =
  let n = max 1 fn.n_vregs in
  {
    mapping = Array.init n (fun i -> i);
    n_regs = n;
    intervals = [];
  }

let validate fn r =
  let errs = ref [] in
  let check v =
    if v < 0 || v >= Array.length r.mapping || r.mapping.(v) < 0 then
      errs := Printf.sprintf "unmapped %%%d" v :: !errs
  in
  List.iter check fn.params;
  Mir.iter_instrs fn (fun _ i ->
      Option.iter check (Mir.instr_def i);
      List.iter check (Mir.instr_uses i));
  List.iter
    (fun (b : Mir.block) -> List.iter check (Mir.term_uses b.term))
    fn.blocks;
  List.rev !errs
