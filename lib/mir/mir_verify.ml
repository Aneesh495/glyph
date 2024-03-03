(** MIR Verifier: validates CFG consistency, terminator targets, and register invariants. *)

open Mir

let verify_func (prog : program) (fn : func) : (unit, string list) result =
  let errs = ref [] in
  let add_err msg = errs := msg :: !errs in

  (* 1. Entry block exists *)
  let block_labels = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      if Hashtbl.mem block_labels b.label then
        add_err (Printf.sprintf "fn %s (@%d): duplicate block label L%d" (Ident.name fn.name) fn.id b.label)
      else
        Hashtbl.add block_labels b.label b)
    fn.blocks;

  if not (Hashtbl.mem block_labels fn.entry) then
    add_err (Printf.sprintf "fn %s (@%d): entry block L%d missing" (Ident.name fn.name) fn.id fn.entry);

  (* 2. Predecessors for phi check *)
  let preds = predecessors fn in

  (* 3. Validate each block *)
  List.iter
    (fun (b : block) ->
      (* Successor labels must exist *)
      List.iter
        (fun succ_lbl ->
          if not (Hashtbl.mem block_labels succ_lbl) then
            add_err
              (Printf.sprintf "fn %s (@%d): block L%d targets nonexistent successor L%d"
                 (Ident.name fn.name) fn.id b.label succ_lbl))
        (term_successors b.term);

      (* Phi verification *)
      List.iter
        (function
          | IPhi (dst, incoming) ->
              if dst < 0 || dst >= fn.n_vregs then
                add_err
                  (Printf.sprintf "fn %s (@%d): phi dst %%%d out of range [0, %d)"
                     (Ident.name fn.name) fn.id dst fn.n_vregs);
              let block_preds = try Hashtbl.find preds b.label with Not_found -> [] in
              List.iter
                (fun (pred_lbl, src_vreg) ->
                  if not (List.mem pred_lbl block_preds) then
                    add_err
                      (Printf.sprintf "fn %s (@%d): phi in L%d specifies incoming from non-predecessor L%d"
                         (Ident.name fn.name) fn.id b.label pred_lbl);
                  if src_vreg < 0 || src_vreg >= fn.n_vregs then
                    add_err
                      (Printf.sprintf "fn %s (@%d): phi in L%d uses out of range vreg %%%d"
                         (Ident.name fn.name) fn.id b.label src_vreg))
                incoming
          | _ -> ())
        b.phis;

      (* Instructions verification *)
      List.iter
        (fun instr ->
          Option.iter
            (fun d ->
              if d < 0 || d >= fn.n_vregs then
                add_err
                  (Printf.sprintf "fn %s (@%d): instr def %%%d out of range [0, %d)"
                     (Ident.name fn.name) fn.id d fn.n_vregs))
            (instr_def instr);
          List.iter
            (fun u ->
              if u < 0 || u >= fn.n_vregs then
                add_err
                  (Printf.sprintf "fn %s (@%d): instr use %%%d out of range [0, %d)"
                     (Ident.name fn.name) fn.id u fn.n_vregs))
            (instr_uses instr);
          match instr with
          | ICall (_, target_id, _) | IMakeClosure (_, target_id, _) ->
              if find_func_opt prog target_id = None then
                add_err
                  (Printf.sprintf "fn %s (@%d): references nonexistent function @%d"
                     (Ident.name fn.name) fn.id target_id)
          | IGetField (_, _, idx) | ISetField (_, idx, _) | ITupleGet (_, _, idx) ->
              if idx < 0 then
                add_err
                  (Printf.sprintf "fn %s (@%d): negative field index %d"
                     (Ident.name fn.name) fn.id idx)
          | _ -> ())
        b.instrs;

      (* Terminator verification *)
      List.iter
        (fun u ->
          if u < 0 || u >= fn.n_vregs then
            add_err
              (Printf.sprintf "fn %s (@%d): term use %%%d out of range [0, %d)"
                 (Ident.name fn.name) fn.id u fn.n_vregs))
        (term_uses b.term);
      match b.term with
      | TTailCall (target_id, _) ->
          if find_func_opt prog target_id = None then
            add_err
              (Printf.sprintf "fn %s (@%d): tail call references nonexistent function @%d"
                 (Ident.name fn.name) fn.id target_id)
      | _ -> ())
    fn.blocks;

  if !errs = [] then Ok () else Error (List.rev !errs)

let verify_program (prog : program) : (unit, string list) result =
  let errs = ref [] in
  let add_err msg = errs := msg :: !errs in

  (* Main function exists *)
  if find_func_opt prog prog.main = None then
    add_err (Printf.sprintf "program main function @%d does not exist" prog.main);

  (* Unique function IDs *)
  let fn_ids = Hashtbl.create 16 in
  List.iter
    (fun (f : func) ->
      if Hashtbl.mem fn_ids f.id then
        add_err (Printf.sprintf "duplicate function ID @%d" f.id)
      else
        Hashtbl.add fn_ids f.id ())
    prog.functions;

  (* Verify each function *)
  List.iter
    (fun f ->
      match verify_func prog f with
      | Ok () -> ()
      | Error es -> errs := es @ !errs)
    prog.functions;

  if !errs = [] then Ok () else Error (List.rev !errs)

let check_program (prog : program) : unit =
  match verify_program prog with
  | Ok () -> ()
  | Error es ->
      failwith ("MIR verification failed:\n" ^ String.concat "\n" es)
