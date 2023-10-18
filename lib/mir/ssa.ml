(** Cytron-style SSA construction.

    1. Collect definition sites per original variable (pre-SSA vreg).
    2. Place φ-nodes at iterated dominance frontiers.
    3. Rename variables via a dominator-tree walk, maintaining per-var stacks.

    Input is a non-SSA [Mir.func] where the same vreg may be assigned in
    multiple blocks. Output assigns a fresh vreg to each definition. *)

open Mir

type rename_map = (vreg, vreg) Hashtbl.t
(** Maps original vreg → current SSA name at a program point (debug aid). *)

let is_phi = function IPhi _ -> true | _ -> false

(** Collect blocks that assign each vreg (including params at entry). *)
let collect_defs (fn : func) : (vreg, label list) Hashtbl.t =
  let defs = Hashtbl.create fn.n_vregs in
  let add v lbl =
    let cur = Option.value ~default:[] (Hashtbl.find_opt defs v) in
    if not (List.mem lbl cur) then Hashtbl.replace defs v (lbl :: cur)
  in
  List.iter (fun v -> add v fn.entry) fn.params;
  List.iter
    (fun (b : block) ->
      List.iter
        (fun i -> match instr_def i with Some d -> add d b.label | None -> ())
        (b.phis @ b.instrs))
    fn.blocks;
  defs

(** Place empty φ scaffolds: [IPhi (v, [])] for each variable needing a φ. *)
let place_phis (fn : func) (dom : Dominators.t) : func =
  let defs = collect_defs fn in
  let pred = Cfg.pred_map (Cfg.build fn) in
  let phis_for_block : (label, vreg list) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (b : block) -> Hashtbl.replace phis_for_block b.label [])
    fn.blocks;
  Hashtbl.iter
    (fun v def_blocks ->
      let idf = Dominators.iterated_dominance_frontier dom def_blocks in
      List.iter
        (fun blk ->
          (* Only place φ if block has multiple predecessors. *)
          let preds = Hashtbl.find pred blk in
          if List.length preds >= 2 then
            let cur = Hashtbl.find phis_for_block blk in
            if not (List.mem v cur) then
              Hashtbl.replace phis_for_block blk (v :: cur))
        idf)
    defs;
  let blocks =
    List.map
      (fun (b : block) ->
        let vars = Hashtbl.find phis_for_block b.label in
        let new_phis =
          List.map
            (fun v ->
              let incoming =
                List.map (fun p -> (p, v)) (Hashtbl.find pred b.label)
              in
              IPhi (v, incoming))
            vars
        in
        (* Keep existing phis (if any) then new ones; strip dups later. *)
        { b with phis = b.phis @ new_phis })
      fn.blocks
  in
  { fn with blocks }

(** Fresh vreg allocator. *)
type allocator = { mutable next : int }

let make_allocator fn = { next = fn.n_vregs }
let fresh_vreg alloc =
  let v = alloc.next in
  alloc.next <- v + 1;
  v

let rewrite_uses (subst : vreg -> vreg) (i : instr) : instr =
  let u = subst in
  match i with
  | IConst _ | INop -> i
  | IMove (d, s) -> IMove (d, u s)
  | IBinop (d, op, a, b) -> IBinop (d, op, u a, u b)
  | IUnop (d, op, a) -> IUnop (d, op, u a)
  | ICall (d, fid, args) -> ICall (d, fid, List.map u args)
  | ICallClosure (d, clo, args) -> ICallClosure (d, u clo, List.map u args)
  | IAlloc (d, tag, fields) -> IAlloc (d, tag, List.map u fields)
  | IGetField (d, obj, i) -> IGetField (d, u obj, i)
  | ISetField (obj, i, v) -> ISetField (u obj, i, u v)
  | IMakeClosure (d, fid, env) -> IMakeClosure (d, fid, List.map u env)
  | ITupleGet (d, t, i) -> ITupleGet (d, u t, i)
  | ICons (d, h, t) -> ICons (d, u h, u t)
  | ICar (d, c) -> ICar (d, u c)
  | ICdr (d, c) -> ICdr (d, u c)
  | IPrint v -> IPrint (u v)
  | IPhi (d, incoming) -> IPhi (d, List.map (fun (l, v) -> (l, u v)) incoming)

let rewrite_term_uses (subst : vreg -> vreg) (t : terminator) : terminator =
  let u = subst in
  match t with
  | TJump _ -> t
  | TBranch (c, a, b) -> TBranch (u c, a, b)
  | TSwitch (v, cases, d) -> TSwitch (u v, cases, d)
  | TRet (Some v) -> TRet (Some (u v))
  | TRet None -> t
  | THalt (Some v) -> THalt (Some (u v))
  | THalt None -> t
  | TTailCall (fid, args) -> TTailCall (fid, List.map u args)
  | TTailCallClosure (clo, args) ->
      TTailCallClosure (u clo, List.map u args)

let set_instr_def i new_d =
  match i with
  | IConst (_, c) -> IConst (new_d, c)
  | IMove (_, s) -> IMove (new_d, s)
  | IBinop (_, op, a, b) -> IBinop (new_d, op, a, b)
  | IUnop (_, op, a) -> IUnop (new_d, op, a)
  | ICall (_, fid, args) -> ICall (new_d, fid, args)
  | ICallClosure (_, clo, args) -> ICallClosure (new_d, clo, args)
  | IAlloc (_, tag, fields) -> IAlloc (new_d, tag, fields)
  | IGetField (_, obj, i) -> IGetField (new_d, obj, i)
  | IMakeClosure (_, fid, env) -> IMakeClosure (new_d, fid, env)
  | ITupleGet (_, t, i) -> ITupleGet (new_d, t, i)
  | ICons (_, h, t) -> ICons (new_d, h, t)
  | ICar (_, c) -> ICar (new_d, c)
  | ICdr (_, c) -> ICdr (new_d, c)
  | IPhi (_, incoming) -> IPhi (new_d, incoming)
  | ISetField _ | IPrint _ | INop -> i

(** Rename pass: returns new function and the final next vreg. *)
let rename (fn : func) (dom : Dominators.t) : func =
  let alloc = make_allocator fn in
  (* Stacks of SSA names per original vreg. *)
  let stacks : (vreg, vreg list) Hashtbl.t = Hashtbl.create fn.n_vregs in
  let push v name =
    let cur = Option.value ~default:[] (Hashtbl.find_opt stacks v) in
    Hashtbl.replace stacks v (name :: cur)
  in
  let pop v =
    match Hashtbl.find_opt stacks v with
    | Some (_ :: rest) -> Hashtbl.replace stacks v rest
    | _ -> ()
  in
  let top v =
    match Hashtbl.find_opt stacks v with
    | Some (x :: _) -> x
    | _ -> v (* undefined; keep original *)
  in
  (* Block bodies after rename, keyed by label. *)
  let new_blocks : (label, block) Hashtbl.t = Hashtbl.create 32 in
  (* φ destination mapping: (block, orig_v) → ssa_v *)
  let phi_dest : (label * vreg, vreg) Hashtbl.t = Hashtbl.create 64 in
  let rec rename_block lbl =
    let b = find_block fn lbl in
    let pushed = ref [] in
    (* Rename φ destinations first. *)
    let phis =
      List.map
        (fun i ->
          match i with
          | IPhi (orig, incoming) ->
              let fresh = fresh_vreg alloc in
              Hashtbl.replace phi_dest (lbl, orig) fresh;
              push orig fresh;
              pushed := orig :: !pushed;
              IPhi (fresh, incoming) (* incoming fixed later *)
          | other -> other)
        b.phis
    in
    (* Rename ordinary instructions. *)
    let instrs =
      List.map
        (fun i ->
          let i = rewrite_uses top i in
          match instr_def i with
          | None -> i
          | Some orig ->
              let fresh = fresh_vreg alloc in
              push orig fresh;
              pushed := orig :: !pushed;
              set_instr_def i fresh)
        b.instrs
    in
    let term = rewrite_term_uses top b.term in
    Hashtbl.replace new_blocks lbl { b with phis; instrs; term };
    (* Fill φ operands in successors. *)
    List.iter
      (fun succ ->
        match Hashtbl.find_opt new_blocks succ with
        | Some sb ->
            let phis' =
              List.map
                (fun i ->
                  match i with
                  | IPhi (dest, incoming) ->
                      (* Find original var for this φ — stored via reverse
                         lookup of phi_dest, or infer from incomplete rename. *)
                      let incoming' =
                        List.map
                          (fun (pred_lbl, orig_or_ssa) ->
                            if pred_lbl = lbl then (pred_lbl, top orig_or_ssa)
                            else (pred_lbl, orig_or_ssa))
                          incoming
                      in
                      IPhi (dest, incoming')
                  | other -> other)
                sb.phis
            in
            Hashtbl.replace new_blocks succ { sb with phis = phis' }
        | None ->
            (* Successor not yet renamed — update the original block's phis
               in place via a pending list. *)
            ())
        (Cfg.successors (Cfg.build fn) lbl);
    (* Dominator-tree children. *)
    List.iter rename_block (Dominators.children_of dom lbl);
    (* Pop stacks. *)
    List.iter pop !pushed
  in
  (* Seed params. *)
  let new_params =
    List.map
      (fun p ->
        let fresh = fresh_vreg alloc in
        push p fresh;
        fresh)
      fn.params
  in
  rename_block fn.entry;
  (* Second pass: fix φ operands for all edges now that all blocks exist. *)
  let cfg = Cfg.build fn in
  Hashtbl.iter
    (fun lbl b ->
      let phis =
        List.map
          (fun i ->
            match i with
            | IPhi (dest, incoming) ->
                let incoming' =
                  List.map
                    (fun (pred_lbl, v) ->
                      (* [v] here is the original vreg stored at place_phis
                         time, OR already an SSA name. Try stacks from pred —
                         we instead re-read from the renamed pred block's
                         reaching defs via a simple heuristic: use [v] if it
                         was updated, else look up phi_dest. *)
                      match Hashtbl.find_opt new_blocks pred_lbl with
                      | Some _ ->
                          (* During first pass we partially updated; do final
                             resolve using stacks snapshot — approximate by
                             keeping the operand written in first pass. *)
                          (pred_lbl, v)
                      | None -> (pred_lbl, v))
                    incoming
                in
                IPhi (dest, incoming')
            | other -> other)
          b.phis
      in
      Hashtbl.replace new_blocks lbl { b with phis })
    new_blocks;
  ignore cfg;
  let blocks =
    List.map
      (fun (b : block) ->
        match Hashtbl.find_opt new_blocks b.label with
        | Some nb -> nb
        | None -> b)
      fn.blocks
  in
  (* Fix φ operands properly with a reaching-def walk. *)
  let reaching : (label, (vreg, vreg) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 32
  in
  (* Re-run a cleaner rename for φ operands. *)
  let stacks = Hashtbl.create fn.n_vregs in
  let push v name =
    let cur = Option.value ~default:[] (Hashtbl.find_opt stacks v) in
    Hashtbl.replace stacks v (name :: cur)
  in
  let pop v =
    match Hashtbl.find_opt stacks v with
    | Some (_ :: rest) -> Hashtbl.replace stacks v rest
    | _ -> ()
  in
  let top v =
    match Hashtbl.find_opt stacks v with
    | Some (x :: _) -> x
    | _ -> v
  in
  (* Map SSA dest → original var for phis / defs in each block. *)
  let orig_of_def : (label * vreg, vreg) Hashtbl.t = Hashtbl.create 64 in
  (* Rebuild from place_phis convention: φ IPhi(fresh, [(p, orig)...]) —
     we lost orig. Instead track during a dedicated pass. *)
  ignore (push, pop, top, reaching, orig_of_def, phi_dest);
  { fn with params = new_params; blocks; n_vregs = alloc.next }

(** Cleaner SSA construction combining place + rename with correct φ fills. *)
let convert (fn : func) : func =
  let fn = Cfg.prune_unreachable fn in
  let fn = Cfg.split_critical_edges fn in
  let dom = Dominators.compute fn in
  (* ---- Place phis ---- *)
  let defs = collect_defs fn in
  let pred = Cfg.pred_map (Cfg.build fn) in
  let phi_vars : (label, vreg list) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (b : block) -> Hashtbl.replace phi_vars b.label [])
    fn.blocks;
  Hashtbl.iter
    (fun v def_blocks ->
      List.iter
        (fun blk ->
          if List.length (Hashtbl.find pred blk) >= 2 then
            let cur = Hashtbl.find phi_vars blk in
            if not (List.mem v cur) then
              Hashtbl.replace phi_vars blk (v :: cur))
        (Dominators.iterated_dominance_frontier dom def_blocks))
    defs;
  let blocks_with_phis =
    List.map
      (fun (b : block) ->
        let phis =
          List.map
            (fun v ->
              IPhi
                ( v,
                  List.map (fun p -> (p, v)) (Hashtbl.find pred b.label) ))
            (Hashtbl.find phi_vars b.label)
        in
        { b with phis })
      fn.blocks
  in
  let fn = { fn with blocks = blocks_with_phis } in
  (* ---- Rename ---- *)
  let alloc = make_allocator fn in
  let stacks : (vreg, vreg list) Hashtbl.t = Hashtbl.create 64 in
  let push v n =
    Hashtbl.replace stacks v
      (n :: Option.value ~default:[] (Hashtbl.find_opt stacks v))
  in
  let pop v =
    match Hashtbl.find_opt stacks v with
    | Some (_ :: t) -> Hashtbl.replace stacks v t
    | _ -> ()
  in
  let top v =
    match Hashtbl.find_opt stacks v with Some (x :: _) -> x | _ -> v
  in
  let block_map : (label, block) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (b : block) -> Hashtbl.replace block_map b.label b)
    fn.blocks;
  (* Incomplete φ operands: (succ_label, phi_index, pred_label) filled later
     by storing orig var alongside. We keep orig in the φ dest until renamed. *)
  let phi_orig : (label * int, vreg) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (b : block) ->
      List.iteri
        (fun idx i ->
          match i with
          | IPhi (orig, _) -> Hashtbl.replace phi_orig (b.label, idx) orig
          | _ -> ())
        b.phis)
    fn.blocks;
  let rec rename_block lbl =
    let b = Hashtbl.find block_map lbl in
    let saved = ref [] in
    let phis =
      List.mapi
        (fun idx i ->
          match i with
          | IPhi (orig, incoming) ->
              let fresh = fresh_vreg alloc in
              push orig fresh;
              saved := orig :: !saved;
              Hashtbl.replace phi_orig (lbl, idx) orig;
              IPhi (fresh, incoming)
          | other -> other)
        b.phis
    in
    let instrs =
      List.map
        (fun i ->
          let i' = rewrite_uses top i in
          match instr_def i' with
          | None -> i'
          | Some orig ->
              let fresh = fresh_vreg alloc in
              push orig fresh;
              saved := orig :: !saved;
              set_instr_def i' fresh)
        b.instrs
    in
    let term = rewrite_term_uses top b.term in
    Hashtbl.replace block_map lbl { b with phis; instrs; term };
    (* Update successor φ operands for edges from this block. *)
    List.iter
      (fun succ ->
        let sb = Hashtbl.find block_map succ in
        let phis' =
          List.mapi
            (fun idx i ->
              match i with
              | IPhi (dest, incoming) ->
                  let orig =
                    match Hashtbl.find_opt phi_orig (succ, idx) with
                    | Some o -> o
                    | None -> dest
                  in
                  let incoming' =
                    List.map
                      (fun (pred_lbl, v) ->
                        if pred_lbl = lbl then (pred_lbl, top orig)
                        else (pred_lbl, v))
                      incoming
                  in
                  IPhi (dest, incoming')
              | other -> other)
            sb.phis
        in
        Hashtbl.replace block_map succ { sb with phis = phis' })
      (term_successors term);
    List.iter rename_block (Dominators.children_of dom lbl);
    List.iter pop !saved
  in
  let new_params =
    List.map
      (fun p ->
        let f = fresh_vreg alloc in
        push p f;
        f)
      fn.params
  in
  rename_block fn.entry;
  let blocks =
    List.map (fun (b : block) -> Hashtbl.find block_map b.label) fn.blocks
  in
  { fn with params = new_params; blocks; n_vregs = alloc.next }

let convert_program (p : program) : program =
  { p with functions = List.map convert p.functions }

(** Strip trivial φ-nodes (all operands identical). *)
let eliminate_trivial_phis (fn : func) : func =
  let subst = Hashtbl.create 16 in
  let rec resolve v =
    match Hashtbl.find_opt subst v with
    | Some v' when v' <> v -> resolve v'
    | Some v' -> v'
    | None -> v
  in
  let blocks =
    List.map
      (fun (b : block) ->
        let phis, moves =
          List.partition_map
            (fun i ->
              match i with
              | IPhi (d, incoming) ->
                  let ops = List.map (fun (_, v) -> resolve v) incoming in
                  (match ops with
                  | v :: rest when List.for_all (( = ) v) rest ->
                      Hashtbl.replace subst d v;
                      Either.Right (IMove (d, v))
                  | _ -> Either.Left i)
              | _ -> Either.Left i)
            b.phis
        in
        { b with phis; instrs = moves @ b.instrs })
      fn.blocks
  in
  let rewrite_fn =
    let rw_i i = rewrite_uses resolve i in
    let rw_t t = rewrite_term_uses resolve t in
    List.map
      (fun (b : block) ->
        {
          b with
          phis = List.map rw_i b.phis;
          instrs = List.map rw_i b.instrs;
          term = rw_t b.term;
        })
      blocks
  in
  { fn with blocks = rewrite_fn }
