(** Dead code elimination on SSA MIR.

    Marks live vregs from side-effecting instructions and terminators,
    then walks use-def chains backwards. Deletes instructions whose
    destination is never live (except effectful ops). *)

open Mir

let is_effectful = function
  | ICall _ | ICallClosure _ | ISetField _ | IPrint _ | IAlloc _
  | IMakeClosure _ ->
      true
  | _ -> false

let run_func (ctx : Pass.context) (fn : func) : func =
  let live = Hashtbl.create fn.n_vregs in
  let work = Queue.create () in
  let mark v =
    if not (Hashtbl.mem live v) then (
      Hashtbl.replace live v ();
      Queue.push v work)
  in
  (* Seed: terminator uses + effectful instr uses/defs. *)
  List.iter
    (fun (b : block) ->
      List.iter mark (term_uses b.term);
      List.iter
        (fun i ->
          if is_effectful i then (
            List.iter mark (instr_uses i);
            match instr_def i with Some d -> mark d | None -> ()))
        (b.phis @ b.instrs))
    fn.blocks;
  (* Def map *)
  let def_of : (vreg, instr) Hashtbl.t = Hashtbl.create fn.n_vregs in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun i ->
          match instr_def i with
          | Some d -> Hashtbl.replace def_of d i
          | None -> ())
        (b.phis @ b.instrs))
    fn.blocks;
  while not (Queue.is_empty work) do
    let v = Queue.pop work in
    match Hashtbl.find_opt def_of v with
    | Some i -> List.iter mark (instr_uses i)
    | None -> ()
  done;
  let keep i =
    match instr_def i with
    | None -> is_effectful i || (match i with INop -> false | _ -> true)
    | Some d -> Hashtbl.mem live d || is_effectful i
  in
  let blocks =
    List.map
      (fun (b : block) ->
        let phis =
          List.filter
            (fun i ->
              let k = keep i in
              if not k then ctx.stats.removed <- ctx.stats.removed + 1;
              k)
            b.phis
        in
        let instrs =
          List.filter
            (fun i ->
              let k = keep i in
              if not k then ctx.stats.removed <- ctx.stats.removed + 1;
              k)
            b.instrs
        in
        { b with phis; instrs })
      fn.blocks
  in
  { fn with blocks }

let pass = Pass.make_func_pass ~name:"dce" run_func
