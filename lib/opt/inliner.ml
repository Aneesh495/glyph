(** Simple function inlining (single-block callees only). *)

open Mir

let func_size (fn : func) =
  List.fold_left
    (fun n (b : block) -> n + List.length b.phis + List.length b.instrs + 1)
    0 fn.blocks

let is_candidate ~budget (fn : func) =
  (not fn.is_main) && func_size fn <= budget && List.length fn.blocks = 1

let run_program ?(budget = 24) (prog : program) : program =
  let by_id =
    List.fold_left
      (fun m (f : func) -> Hashtbl.replace m f.id f; m)
      (Hashtbl.create 16) prog.functions
  in
  let next_vreg = ref 0 in
  let bump n = if n >= !next_vreg then next_vreg := n + 1 in
  List.iter
    (fun (f : func) ->
      bump (f.n_vregs - 1);
      List.iter bump f.params)
    prog.functions;
  let fresh () =
    let v = !next_vreg in
    incr next_vreg;
    v
  in
  let functions =
    List.map
      (fun (caller : func) ->
        let blocks =
          List.map
            (fun (b : block) ->
              let instrs = ref [] in
              List.iter
                (fun i ->
                  match i with
                  | ICall (dst, fid, args) -> (
                      match Hashtbl.find_opt by_id fid with
                      | Some callee
                        when is_candidate ~budget callee
                             && List.length callee.blocks = 1 -> (
                          match callee.blocks with
                          | [ cb ] ->
                              let map_v =
                                let tbl = Hashtbl.create 16 in
                                List.iter2
                                  (fun p a -> Hashtbl.replace tbl p a)
                                  callee.params args;
                              List.iter
                                (fun (b2 : block) ->
                                  List.iter
                                    (fun i2 ->
                                      Option.iter
                                        (fun d ->
                                          if not (Hashtbl.mem tbl d) then
                                            Hashtbl.replace tbl d (fresh ()))
                                        (instr_def i2);
                                      List.iter
                                        (fun u ->
                                          if not (Hashtbl.mem tbl u) then
                                            Hashtbl.replace tbl u (fresh ()))
                                        (instr_uses i2))
                                    (b2.phis @ b2.instrs))
                                  callee.blocks;
                                fun v ->
                                  try Hashtbl.find tbl v with Not_found -> v
                              in
                              List.iter2
                                (fun p a ->
                                  instrs := IMove (map_v p, a) :: !instrs)
                                callee.params args;
                              List.iter
                                (fun i2 ->
                                  let i2 =
                                    match i2 with
                                    | IConst (d, c) -> IConst (map_v d, c)
                                    | IMove (d, s) -> IMove (map_v d, map_v s)
                                    | IBinop (d, op, a, b) ->
                                        IBinop
                                          (map_v d, op, map_v a, map_v b)
                                    | IUnop (d, op, s) ->
                                        IUnop (map_v d, op, map_v s)
                                    | IAlloc (d, tag, fs) ->
                                        IAlloc
                                          (map_v d, tag, List.map map_v fs)
                                    | IGetField (d, o, ix) ->
                                        IGetField (map_v d, map_v o, ix)
                                    | ITupleGet (d, t, ix) ->
                                        ITupleGet (map_v d, map_v t, ix)
                                    | other -> other
                                  in
                                  instrs := i2 :: !instrs)
                                cb.instrs;
                              (match cb.term with
                              | TRet (Some v) ->
                                  instrs := IMove (dst, map_v v) :: !instrs
                              | TRet None ->
                                  instrs := IConst (dst, CUnit) :: !instrs
                              | _ -> instrs := i :: !instrs)
                          | _ -> instrs := i :: !instrs)
                      | _ -> instrs := i :: !instrs)
                  | i -> instrs := i :: !instrs)
                b.instrs;
              { b with instrs = List.rev !instrs })
            caller.blocks
        in
        { caller with blocks; n_vregs = max caller.n_vregs !next_vreg })
      prog.functions
  in
  { prog with functions }

let pass =
  Pass_manager.make_program_pass "inline" (run_program ?budget:None)

let run = run_program
