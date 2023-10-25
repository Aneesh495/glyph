(** SSA construction: φ-insertion (Cytron) + rename.

    [construct] takes a non-SSA CFG function (assignments may redefine the
    same [vreg] across blocks) and returns an SSA function where each name is
    defined exactly once. *)

open Mir

(* -------------------------------------------------------------------------- *)
(* Collect variables & definition sites                                       *)
(* -------------------------------------------------------------------------- *)

let instr_def_opt = function
  | Assign { dst; _ } | Binop { dst; _ } | Unop { dst; _ }
  | Alloc { dst; _ } | Load { dst; _ } | GetField { dst; _ }
  | Cast { dst; _ } | Phi { dst; _ } ->
      Some dst
  | Call { dst; _ } -> dst
  | Store _ | SetField _ -> None

let collect_defs (f : func) : (vreg, Label.Set.t) Hashtbl.t =
  let defs = Hashtbl.create 64 in
  let add v l =
    let set = try Hashtbl.find defs v with Not_found -> Label.Set.empty in
    Hashtbl.replace defs v (Label.Set.add l set)
  in
  List.iter (fun (p, _) -> add p f.entry) f.params;
  iter_blocks
    (fun (b : block) ->
      List.iter
        (fun i -> Option.iter (fun d -> add d b.label) (instr_def_opt i))
        (b.phis @ b.instrs))
    f;
  defs

let all_vars (f : func) : Vreg.Set.t =
  let s = ref Vreg.Set.empty in
  List.iter (fun (p, _) -> s := Vreg.Set.add p !s) f.params;
  iter_blocks
    (fun (b : block) ->
      List.iter
        (fun i ->
          List.iter (fun d -> s := Vreg.Set.add d !s) (instr_defs i);
          List.iter (fun u -> s := Vreg.Set.add u !s) (instr_uses i))
        (b.phis @ b.instrs);
      List.iter (fun u -> s := Vreg.Set.add u !s) (terminator_uses b.terminator))
    f;
  !s

(* -------------------------------------------------------------------------- *)
(* φ insertion                                                                *)
(* -------------------------------------------------------------------------- *)

let has_phi_for (b : block) (v : vreg) =
  List.exists
    (function Phi { dst; _ } -> Vreg.equal dst v | _ -> false)
    b.phis

let insert_phis (f : func) (dom : Dominators.tree) : unit =
  let defs = collect_defs f in
  Hashtbl.iter
    (fun v def_blocks ->
      let work =
        Queue.create ()
      in
      let in_work = Hashtbl.create 16 in
      let phi_placed = Hashtbl.create 16 in
      Label.Set.iter
        (fun l ->
          Queue.push l work;
          Hashtbl.replace in_work l true)
        def_blocks;
      while not (Queue.is_empty work) do
        let b = Queue.pop work in
        Hashtbl.remove in_work b;
        let frontier = Dominators.dominance_frontier dom b in
        Label.Set.iter
          (fun df_block ->
            if not (Hashtbl.mem phi_placed df_block) then (
              Hashtbl.replace phi_placed df_block true;
              match find_block f df_block with
              | None -> ()
              | Some blk ->
                  if not (has_phi_for blk v) then (
                    let preds = blk.preds in
                    let incoming =
                      List.map (fun p -> (p, VReg v)) preds
                    in
                    let phi =
                      Phi
                        {
                          dst = v;
                          ty = Ty_any;
                          incoming;
                          span = Span.dummy;
                        }
                    in
                    blk.phis <- phi :: blk.phis;
                    (* A φ is a new definition site. *)
                    if not (Label.Set.mem df_block def_blocks) then (
                      if not (Hashtbl.mem in_work df_block) then (
                        Queue.push df_block work;
                        Hashtbl.replace in_work df_block true)))))
          frontier
      done)
    defs

(* -------------------------------------------------------------------------- *)
(* Rename                                                                     *)
(* -------------------------------------------------------------------------- *)

type stacks = (vreg, vreg list) Hashtbl.t

let fresh_version (orig : vreg) = Vreg.fresh (Ident.name orig)

let push_stack (st : stacks) v version =
  let stack = try Hashtbl.find st v with Not_found -> [] in
  Hashtbl.replace st v (version :: stack)

let pop_stack (st : stacks) v =
  match Hashtbl.find_opt st v with
  | Some (_ :: rest) -> Hashtbl.replace st v rest
  | _ -> ()

let top_stack (st : stacks) v =
  match Hashtbl.find_opt st v with
  | Some (x :: _) -> x
  | _ -> v

let rename_value st = function
  | VReg r -> VReg (top_stack st r)
  | v -> v

let rename_instr_uses st instr =
  map_instr_values ~on_use:(rename_value st) ~on_def:Fun.id instr

let rename_def st dst =
  let version = fresh_version dst in
  push_stack st dst version;
  version

let construct (f : func) : func =
  if f.is_ssa then f
  else
    let f = Cfg.ensure_cfg f in
    let dom = Dominators.compute f in
    insert_phis f dom;

    let stacks : stacks = Hashtbl.create 64 in
    (* Seed parameters. *)
    let new_params =
      List.map
        (fun (p, ty) ->
          let v = fresh_version p in
          push_stack stacks p v;
          (v, ty))
        f.params
    in
    (* Also push original names so early uses work before first rename...
       Actually params are renamed — map original param idents pushed. *)
    List.iter2
      (fun (orig, _) (fresh, _) ->
        (* stacks already has fresh on top from push above via new_params loop;
           ensure orig key points at fresh. *)
        Hashtbl.replace stacks orig [ fresh ])
      f.params new_params;
    f.params <- new_params;

    let rec rename_block (l : label) =
      match find_block f l with
      | None -> ()
      | Some b ->
          let pushed = ref [] in
          (* Rename φ destinations. *)
          b.phis <-
            List.map
              (function
                | Phi ({ dst; incoming; _ } as p) ->
                    let dst' = rename_def stacks dst in
                    pushed := dst :: !pushed;
                    Phi { p with dst = dst'; incoming }
                | i -> i)
              b.phis;
          (* Rename ordinary instructions. *)
          b.instrs <-
            List.map
              (fun instr ->
                let instr = rename_instr_uses stacks instr in
                match instr_def_opt instr with
                | None -> instr
                | Some dst ->
                    let dst' = rename_def stacks dst in
                    pushed := dst :: !pushed;
                    map_instr_values ~on_use:Fun.id ~on_def:(fun d ->
                        if Vreg.equal d dst then dst' else d)
                      instr)
              b.instrs;
          b.terminator <-
            map_terminator_values (rename_value stacks) b.terminator;

          (* Fill φ operands in successors. *)
          List.iter
            (fun succ ->
              match find_block f succ with
              | None -> ()
              | Some sb ->
                  sb.phis <-
                    List.map
                      (function
                        | Phi ({ incoming; _ } as p) ->
                            let incoming =
                              List.map
                                (fun (pred, v) ->
                                  if Label.equal pred l then
                                    (pred, rename_value stacks v)
                                  else (pred, v))
                                incoming
                            in
                            Phi { p with incoming }
                        | i -> i)
                      sb.phis)
            b.succs;

          (* Recurse on dominator-tree children. *)
          List.iter rename_block (Dominators.children_of dom l);

          (* Pop stacks. *)
          List.iter (fun v -> pop_stack stacks v) !pushed
    in
    rename_block f.entry;
    f.is_ssa <- true;
    Cfg.recompute_edges f;
    f

(** Destroy SSA by replacing φ-nodes with moves in predecessors.
    Useful before non-SSA-aware codegen paths. *)
let destroy (f : func) : func =
  let f = Cfg.ensure_cfg f in
  iter_blocks
    (fun (b : block) ->
      List.iter
        (function
          | Phi { dst; incoming; ty; span; _ } ->
              List.iter
                (fun (pred, v) ->
                  match find_block f pred with
                  | None -> ()
                  | Some pb ->
                      let mv = Assign { dst; src = v; ty; span } in
                      (* Insert before terminator. *)
                      pb.instrs <- pb.instrs @ [ mv ])
                incoming
          | _ -> ())
        b.phis;
      b.phis <- [])
    f;
  f.is_ssa <- false;
  f

let is_ssa (f : func) = f.is_ssa

(** Construct SSA for every function in a program. *)
let construct_program (prog : program) : program =
  Ident.Map.iter
    (fun _ f -> ignore (construct f))
    prog.funcs;
  prog
