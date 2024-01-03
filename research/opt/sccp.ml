(** Sparse Conditional Constant Propagation (Wegman & Zadeck).

    Lattice: Bottom ⊑ Const(c) ⊑ Top
    Dual worklists: SSA edge worklist + CFG flow worklist.
    Only executable edges contribute to φ meets; this simultaneously
    folds constants and deletes unreachable branches. *)

open Mir

type lattice =
  | Bottom
  | Const of const
  | Top

let lattice_equal a b =
  match (a, b) with
  | Bottom, Bottom | Top, Top -> true
  | Const x, Const y -> x = y
  | _ -> false

let meet a b =
  match (a, b) with
  | Bottom, x | x, Bottom -> x
  | Const x, Const y when x = y -> Const x
  | Const _, Const _ -> Top
  | Top, _ | _, Top -> Top

let eval_binop = Const_prop.eval_binop
let eval_unop = Const_prop.eval_unop

type state = {
  values : (vreg, lattice) Hashtbl.t;
  (** Executable CFG edges (pred, succ). *)
  exec_edge : (label * label, unit) Hashtbl.t;
  (** Executable blocks. *)
  exec_block : (label, unit) Hashtbl.t;
  cfg_work : (label * label) Queue.t;
  ssa_work : vreg Queue.t;
  ssa_pending : (vreg, unit) Hashtbl.t;
  def_site : (vreg, label * instr) Hashtbl.t;
  uses : (vreg, (label * [ `Instr of instr | `Term of terminator ])) Hashtbl.t;
  fn : func;
}

let get st v = Option.value ~default:Bottom (Hashtbl.find_opt st.values v)

let set_value st v lat =
  match Hashtbl.find_opt st.values v with
  | Some old when lattice_equal old lat -> false
  | _ ->
      Hashtbl.replace st.values v lat;
      true

let enqueue_ssa st v =
  if not (Hashtbl.mem st.ssa_pending v) then (
    Hashtbl.replace st.ssa_pending v ();
    Queue.push v st.ssa_work)

let enqueue_cfg st pred succ =
  let key = (pred, succ) in
  if not (Hashtbl.mem st.exec_edge key) then (
    Hashtbl.replace st.exec_edge key ();
    Queue.push key st.cfg_work)

let build_use_def (fn : func) =
  let def_site = Hashtbl.create fn.n_vregs in
  let uses = Hashtbl.create fn.n_vregs in
  let add_use v site =
    let cur = Option.value ~default:[] (Hashtbl.find_opt uses v) in
    Hashtbl.replace uses v (site :: cur)
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun i ->
          (match instr_def i with
          | Some d -> Hashtbl.replace def_site d (b.label, i)
          | None -> ());
          List.iter (fun u -> add_use u (b.label, `Instr i)) (instr_uses i))
        (b.phis @ b.instrs);
      List.iter
        (fun u -> add_use u (b.label, `Term b.term))
        (term_uses b.term))
    fn.blocks;
  (* Params are defs at entry with unknown value → Top. *)
  List.iter
    (fun p ->
      Hashtbl.replace def_site p (fn.entry, INop))
    fn.params;
  (def_site, uses)

let visit_instr st i =
  match i with
  | IConst (d, c) ->
      if set_value st d (Const c) then enqueue_ssa st d
  | IMove (d, s) ->
      if set_value st d (get st s) then enqueue_ssa st d
  | IBinop (d, op, a, b) ->
      let lat =
        match (get st a, get st b) with
        | Bottom, _ | _, Bottom -> Bottom
        | Const ca, Const cb -> (
            match eval_binop op ca cb with
            | Some c -> Const c
            | None -> Top)
        | _ -> Top
      in
      if set_value st d lat then enqueue_ssa st d
  | IUnop (d, op, a) ->
      let lat =
        match get st a with
        | Bottom -> Bottom
        | Const ca -> (
            match eval_unop op ca with Some c -> Const c | None -> Top)
        | Top -> Top
      in
      if set_value st d lat then enqueue_ssa st d
  | IPhi (d, incoming) ->
      (* Prefer [visit_phi_fixed] which knows the block label; fall back. *)
      let phi_block =
        match Hashtbl.find_opt st.def_site d with
        | Some (lbl, _) -> lbl
        | None -> -1
      in
      let lat =
        List.fold_left
          (fun acc (pred, v) ->
            if Hashtbl.mem st.exec_edge (pred, phi_block) then
              meet acc (get st v)
            else acc)
          Bottom incoming
      in
      if set_value st d lat then enqueue_ssa st d
  | ICall (d, _, _)
  | ICallClosure (d, _, _)
  | IAlloc (d, _, _)
  | IGetField (d, _, _)
  | IMakeClosure (d, _, _)
  | ITupleGet (d, _, _)
  | ICons (d, _, _)
  | ICar (d, _)
  | ICdr (d, _) ->
      if set_value st d Top then enqueue_ssa st d
  | ISetField _ | IPrint _ | INop -> ()

let visit_phi_fixed st phi_block i =
  match i with
  | IPhi (d, incoming) ->
      let lat =
        List.fold_left
          (fun acc (pred, v) ->
            if Hashtbl.mem st.exec_edge (pred, phi_block) then
              meet acc (get st v)
            else acc)
          Bottom incoming
      in
      if set_value st d lat then enqueue_ssa st d
  | other -> visit_instr st other

let visit_term st blk_lbl term =
  match term with
  | TJump l -> enqueue_cfg st blk_lbl l
  | TBranch (c, t, e) -> (
      match get st c with
      | Const (CBool true) -> enqueue_cfg st blk_lbl t
      | Const (CBool false) -> enqueue_cfg st blk_lbl e
      | Const (CInt 0) -> enqueue_cfg st blk_lbl e
      | Const (CInt _) -> enqueue_cfg st blk_lbl t
      | Bottom -> ()
      | _ ->
          enqueue_cfg st blk_lbl t;
          enqueue_cfg st blk_lbl e)
  | TSwitch (v, cases, d) -> (
      match get st v with
      | Const (CInt n) ->
          (match List.assoc_opt n cases with
          | Some l -> enqueue_cfg st blk_lbl l
          | None -> enqueue_cfg st blk_lbl d)
      | Bottom -> ()
      | _ ->
          List.iter (fun (_, l) -> enqueue_cfg st blk_lbl l) cases;
          enqueue_cfg st blk_lbl d)
  | TRet _ | THalt _ | TTailCall _ | TTailCallClosure _ -> ()

let process_cfg_edge st (pred, succ) =
  ignore pred;
  let first = not (Hashtbl.mem st.exec_block succ) in
  Hashtbl.replace st.exec_block succ ();
  match find_block_opt st.fn succ with
  | None -> ()
  | Some b ->
      List.iter (visit_phi_fixed st succ) b.phis;
      if first then (
        List.iter (visit_instr st) b.instrs;
        visit_term st succ b.term)

let process_ssa st v =
  Hashtbl.remove st.ssa_pending v;
  match Hashtbl.find_opt st.uses v with
  | None -> ()
  | Some sites ->
      List.iter
        (fun (lbl, site) ->
          if Hashtbl.mem st.exec_block lbl then
            match site with
            | `Instr i -> (
                match i with
                | IPhi _ -> visit_phi_fixed st lbl i
                | _ -> visit_instr st i)
            | `Term t -> visit_term st lbl t)
        sites

let run_func (ctx : Pass.context) (fn : func) : func =
  let def_site, uses = build_use_def fn in
  let st =
    {
      values = Hashtbl.create fn.n_vregs;
      exec_edge = Hashtbl.create 64;
      exec_block = Hashtbl.create 32;
      cfg_work = Queue.create ();
      ssa_work = Queue.create ();
      ssa_pending = Hashtbl.create 64;
      def_site;
      uses;
      fn;
    }
  in
  (* Params are Top (unknown). *)
  List.iter
    (fun p ->
      Hashtbl.replace st.values p Top;
      enqueue_ssa st p)
    fn.params;
  (* Entry is executable. *)
  Hashtbl.replace st.exec_block fn.entry ();
  (match find_block_opt fn fn.entry with
  | Some b ->
      List.iter (visit_instr st) b.phis;
      List.iter (visit_instr st) b.instrs;
      visit_term st fn.entry b.term
  | None -> ());
  let steps = ref 0 in
  while
    (not (Queue.is_empty st.cfg_work) || not (Queue.is_empty st.ssa_work))
    && !steps < 100_000
  do
    incr steps;
    if not (Queue.is_empty st.cfg_work) then
      process_cfg_edge st (Queue.pop st.cfg_work)
    else if not (Queue.is_empty st.ssa_work) then
      process_ssa st (Queue.pop st.ssa_work)
  done;
  (* Rewrite: fold constants; neutralize dead branches. *)
  let blocks =
    List.map
      (fun (b : block) ->
        if not (Hashtbl.mem st.exec_block b.label) then
          (* Unreachable: replace with halt. *)
          {
            b with
            phis = [];
            instrs = [];
            term = THalt None;
          }
        else
          let rewrite i =
            match instr_def i with
            | Some d -> (
                match get st d with
                | Const c ->
                    ctx.stats.rewritten <- ctx.stats.rewritten + 1;
                    IConst (d, c)
                | _ -> i)
            | None -> i
          in
          let term =
            match b.term with
            | TBranch (c, t, e) -> (
                match get st c with
                | Const (CBool true) | Const (CInt n) when n <> 0 -> TJump t
                | Const (CBool false) | Const (CInt 0) -> TJump e
                | _ -> b.term)
            | TSwitch (v, cases, d) -> (
                match get st v with
                | Const (CInt n) -> (
                    match List.assoc_opt n cases with
                    | Some l -> TJump l
                    | None -> TJump d)
                | _ -> b.term)
            | other -> other
          in
          {
            b with
            phis = List.map rewrite b.phis;
            instrs = List.map rewrite b.instrs;
            term;
          })
      fn.blocks
  in
  Cfg.prune_unreachable { fn with blocks }

let pass = Pass.make_func_pass ~name:"sccp" run_func
