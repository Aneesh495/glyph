(** Linear-scan register allocation for Glyph MIR.

    Strategy (Poletto & Sarkar style, adapted for an unbounded bytecode
    register file):

    1. Number instructions in a reverse-postorder walk of the CFG so each
       definition/use has a discrete program point.
    2. Compute live intervals [start, end] per SSA vreg via a backward dataflow
       liveness pass, then take the min def / max use as the interval.
    3. Sort intervals by start point; assign the lowest free physical register
       whose current occupants have expired; otherwise allocate a fresh preg.

    Because the VM register file is soft-bounded ([proto.max_regs]), we do not
    spill to memory — we simply grow the register count. Linear scan still
    reuses registers aggressively across non-overlapping live ranges, which
    keeps frame sizes small.

    φ-nodes are handled by assigning the φ destination a register and emitting
    Moves in predecessor blocks during codegen (see Emit); here we treat φ
    destinations as normal defs and φ arguments as uses at the end of the
    corresponding predecessor.
*)

type preg = int
type vreg = Mir.vreg

type interval = {
  vreg : vreg;
  mutable start : int;
  mutable end_ : int;
}

type result = {
  mapping : preg array;
      (** [mapping.(v)] = physical register for virtual register [v],
          or [-1] if unused. *)
  n_regs : int;
  intervals : interval list;
}

(* -------------------------------------------------------------------------- *)
(* Instruction numbering                                                      *)
(* -------------------------------------------------------------------------- *)

type point_map = {
  (** Map (block_label, kind, index) → point. We expose helpers instead. *)
  block_start : (Mir.label, int) Hashtbl.t;
  block_end : (Mir.label, int) Hashtbl.t;
  mutable next_point : int;
}

let rpo_blocks (fn : Mir.func) : Mir.block list =
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
  (* Any unreachable blocks still get numbered deterministically. *)
  List.iter
    (fun (b : Mir.block) ->
      if not (Hashtbl.mem visited b.label) then (
        Hashtbl.add visited b.label ();
        order := !order @ [ b ]))
    fn.blocks;
  List.rev !order

type numbered = {
  points : point_map;
  (** For each block: list of (point, instr option) — None marks terminator. *)
  block_instr_points : (Mir.label, int list) Hashtbl.t;
  term_point : (Mir.label, int) Hashtbl.t;
  phi_points : (Mir.label, int list) Hashtbl.t;
}

let number_func (fn : Mir.func) : numbered =
  let points =
    {
      block_start = Hashtbl.create 16;
      block_end = Hashtbl.create 16;
      next_point = 0;
    }
  in
  let block_instr_points = Hashtbl.create 16 in
  let term_point = Hashtbl.create 16 in
  let phi_points = Hashtbl.create 16 in
  let alloc () =
    let p = points.next_point in
    points.next_point <- p + 1;
    p
  in
  List.iter
    (fun (b : Mir.block) ->
      let start = alloc () in
      Hashtbl.replace points.block_start b.label start;
      let phi_ps =
        List.map
          (fun _ -> alloc ())
          b.phis
      in
      Hashtbl.replace phi_points b.label phi_ps;
      let ips =
        List.map
          (fun _ -> alloc ())
          b.instrs
      in
      Hashtbl.replace block_instr_points b.label ips;
      let tp = alloc () in
      Hashtbl.replace term_point b.label tp;
      Hashtbl.replace points.block_end b.label tp)
    (rpo_blocks fn);
  { points; block_instr_points; term_point; phi_points }

(* -------------------------------------------------------------------------- *)
(* Liveness                                                                   *)
(* -------------------------------------------------------------------------- *)

module VSet = Set.Make (Int)

let compute_live_intervals (fn : Mir.func) (num : numbered) : interval list =
  let npoints = num.points.next_point in
  let live_in : VSet.t array = Array.make (List.length fn.blocks) VSet.empty in
  let label_index = Hashtbl.create 16 in
  List.iteri
    (fun i (b : Mir.block) -> Hashtbl.replace label_index b.label i)
    fn.blocks;
  let idx_of lbl = Hashtbl.find label_index lbl in

  (* Build use/def sets per block (excluding φ uses — those are attributed to
     predecessors). *)
  let block_use = Array.make (List.length fn.blocks) VSet.empty in
  let block_def = Array.make (List.length fn.blocks) VSet.empty in
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

  (* φ operand uses belong to the predecessor edge. *)
  let pred_phi_uses : (Mir.label, VSet.t) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (b : Mir.block) -> Hashtbl.replace pred_phi_uses b.label VSet.empty)
    fn.blocks;
  List.iter
    (fun (b : Mir.block) ->
      List.iter
        (function
          | Mir.IPhi (_, incoming) ->
              List.iter
                (fun (pred, v) ->
                  let s =
                    try Hashtbl.find pred_phi_uses pred
                    with Not_found -> VSet.empty
                  in
                  Hashtbl.replace pred_phi_uses pred (VSet.add v s))
                incoming
          | _ -> ())
        b.phis)
    fn.blocks;

  (* Iterative liveness. *)
  let live_out = Array.make (List.length fn.blocks) VSet.empty in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (b : Mir.block) ->
        let i = idx_of b.label in
        let out = ref VSet.empty in
        List.iter
          (fun succ ->
            out := VSet.union !out live_in.(idx_of succ))
          (Mir.term_successors b.term);
        (* Include φ uses on this block as a predecessor. *)
        out :=
          VSet.union !out
            (try Hashtbl.find pred_phi_uses b.label
             with Not_found -> VSet.empty);
        if not (VSet.equal !out live_out.(i)) then (
          live_out.(i) <- !out;
          changed := true);
        let new_in =
          VSet.union block_use.(i)
            (VSet.diff live_out.(i) block_def.(i))
        in
        if not (VSet.equal new_in live_in.(i)) then (
          live_in.(i) <- new_in;
          changed := true))
      (List.rev fn.blocks)
  done;

  (* Build intervals from defs/uses at concrete points. *)
  let intervals : (vreg, interval) Hashtbl.t = Hashtbl.create fn.n_vregs in
  let touch v point =
    match Hashtbl.find_opt intervals v with
    | Some iv ->
        if point < iv.start then iv.start <- point;
        if point > iv.end_ then iv.end_ <- point
    | None ->
        Hashtbl.add intervals v { vreg = v; start = point; end_ = point }
  in

  (* Parameters live at function entry. *)
  let entry_pt =
    try Hashtbl.find num.points.block_start fn.entry with Not_found -> 0
  in
  List.iter (fun v -> touch v entry_pt) fn.params;

  List.iter
    (fun (b : Mir.block) ->
      let phi_ps =
        try Hashtbl.find num.phi_points b.label with Not_found -> []
      in
      List.iter2
        (fun pt instr ->
          List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
          Option.iter (fun d -> touch d pt) (Mir.instr_def instr))
        phi_ps b.phis;
      let ips =
        try Hashtbl.find num.block_instr_points b.label
        with Not_found -> []
      in
      List.iter2
        (fun pt instr ->
          List.iter (fun u -> touch u pt) (Mir.instr_uses instr);
          Option.iter (fun d -> touch d pt) (Mir.instr_def instr))
        ips b.instrs;
      let tp = Hashtbl.find num.term_point b.label in
      List.iter (fun u -> touch u tp) (Mir.term_uses b.term);
      (* Extend intervals for variables live across the block. *)
      let bi = idx_of b.label in
      let bs = Hashtbl.find num.points.block_start b.label in
      let be = Hashtbl.find num.points.block_end b.label in
      VSet.iter
        (fun v ->
          touch v bs;
          touch v be)
        (VSet.union live_in.(bi) live_out.(bi)))
    fn.blocks;

  (* Ensure every vreg that appears has at least a trivial interval. *)
  List.iter
    (fun v -> if not (Hashtbl.mem intervals v) then touch v 0)
    (Mir.all_vregs fn);

  ignore npoints;
  Hashtbl.fold (fun _ iv acc -> iv :: acc) intervals []

(* -------------------------------------------------------------------------- *)
(* Linear scan                                                                *)
(* -------------------------------------------------------------------------- *)

type active_entry = {
  preg : preg;
  interval : interval;
}

let linear_scan (intervals : interval list) : preg array * int =
  let sorted =
    List.sort
      (fun a b ->
        match Int.compare a.start b.start with
        | 0 -> Int.compare a.vreg b.vreg
        | c -> c)
      intervals
  in
  let max_vreg =
    List.fold_left (fun m iv -> max m iv.vreg) (-1) intervals
  in
  let mapping = Array.make (max_vreg + 1) (-1) in
  let active : active_entry list ref = ref [] in
  let free_pool : preg list ref = ref [] in
  let next_preg = ref 0 in

  let expire_old start =
    let kept, expired =
      List.partition (fun (e : active_entry) -> e.interval.end_ >= start) !active
    in
    active :=
      List.sort
        (fun a b -> Int.compare a.interval.end_ b.interval.end_)
        kept;
    List.iter
      (fun (e : active_entry) -> free_pool := e.preg :: !free_pool)
      expired
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
    (fun (iv : interval) ->
      expire_old iv.start;
      let p = alloc_preg () in
      mapping.(iv.vreg) <- p;
      active :=
        List.sort
          (fun a b -> Int.compare a.interval.end_ b.interval.end_)
          ({ preg = p; interval = iv } :: !active))
    sorted;
  (mapping, !next_preg)

(* -------------------------------------------------------------------------- *)
(* Public API                                                                 *)
(* -------------------------------------------------------------------------- *)

let allocate (fn : Mir.func) : result =
  let num = number_func fn in
  let intervals = compute_live_intervals fn num in
  let mapping, n_regs = linear_scan intervals in
  (* Guarantee parameters get distinct registers if somehow missed. *)
  let n_regs = ref n_regs in
  List.iter
    (fun v ->
      if v >= Array.length mapping then ()
      else if mapping.(v) < 0 then (
        mapping.(v) <- !n_regs;
        incr n_regs))
    fn.params;
  { mapping; n_regs = max !n_regs 1; intervals }

let lookup (r : result) (v : vreg) : preg =
  if v < 0 || v >= Array.length r.mapping then
    invalid_arg (Printf.sprintf "Regalloc.lookup: bad vreg %d" v)
  else
    let p = r.mapping.(v) in
    if p < 0 then
      (* Unused vreg — give it a scratch by extending conceptually; callers
         should not look up dead vregs, but be defensive. *)
      0
    else p

let lookup_opt (r : result) (v : vreg) : preg option =
  if v < 0 || v >= Array.length r.mapping then None
  else
    let p = r.mapping.(v) in
    if p < 0 then None else Some p

let pp_result fmt (r : result) =
  Format.fprintf fmt "regalloc: %d physical regs\n" r.n_regs;
  Array.iteri
    (fun v p ->
      if p >= 0 then Format.fprintf fmt "  %%%d -> r%d\n" v p)
    r.mapping;
  Format.fprintf fmt "intervals:\n";
  List.iter
    (fun (iv : interval) ->
      Format.fprintf fmt "  %%%d: [%d, %d]\n" iv.vreg iv.start iv.end_)
    (List.sort
       (fun a b -> Int.compare a.start b.start)
       r.intervals)

(** Identity allocation: preg = vreg. Useful for debugging. *)
let identity (fn : Mir.func) : result =
  let n = max 1 fn.n_vregs in
  let mapping = Array.init n (fun i -> i) in
  let intervals =
    List.map
      (fun v -> { vreg = v; start = 0; end_ = 0 })
      (Mir.all_vregs fn)
  in
  { mapping; n_regs = n; intervals }

(** Validate that the mapping covers all defs/uses. *)
let validate (fn : Mir.func) (r : result) : string list =
  let errs = ref [] in
  let check v =
    if v < 0 || v >= Array.length r.mapping || r.mapping.(v) < 0 then
      errs := Printf.sprintf "unmapped vreg %%%d" v :: !errs
  in
  List.iter check fn.params;
  Mir.iter_instrs fn (fun _ i ->
      Option.iter check (Mir.instr_def i);
      List.iter check (Mir.instr_uses i));
  List.iter
    (fun (b : Mir.block) -> List.iter check (Mir.term_uses b.term))
    fn.blocks;
  List.rev !errs
