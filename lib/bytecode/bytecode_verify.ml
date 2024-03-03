(** Bytecode Verifier: validates chunk integrity, register bounds, constant references, and branch targets. *)

open Chunk

let verify_chunk (chunk : t) : (unit, string list) result =
  let errs = ref [] in
  let add_err msg = errs := msg :: !errs in

  let n_code = Array.length chunk.code in
  let n_consts = Array.length chunk.consts in
  let n_globals = Array.length chunk.globals in

  (* 1. Verify main exists *)
  let main_found = Array.exists (fun f -> f.fn_id = chunk.main) chunk.funcs in
  if not main_found then
    add_err (Printf.sprintf "main function fn%d not found in chunk funcs" chunk.main);

  (* 2. Verify functions *)
  let fn_ids = Hashtbl.create 16 in
  Array.iter
    (fun f ->
      if Hashtbl.mem fn_ids f.fn_id then
        add_err (Printf.sprintf "duplicate function ID fn%d (%s)" f.fn_id f.name)
      else
        Hashtbl.add fn_ids f.fn_id f;
      if f.arity < 0 then
        add_err (Printf.sprintf "function fn%d has negative arity %d" f.fn_id f.arity);
      if f.nregs < f.arity then
        add_err
          (Printf.sprintf "function fn%d: nregs %d < arity %d" f.fn_id f.nregs f.arity);
      if f.entry < 0 || (n_code > 0 && f.entry >= n_code) then
        add_err
          (Printf.sprintf "function fn%d entry %d out of code bounds [0, %d)"
             f.fn_id f.entry n_code))
    chunk.funcs;

  (* 3. Map code offset to enclosing function *)
  (* Sort functions by entry to find range *)
  let sorted_funcs =
    Array.copy chunk.funcs
    |> Array.to_list
    |> List.sort (fun a b -> Int.compare a.entry b.entry)
  in
  let func_for_ip ip =
    let rec find = function
      | [] -> None
      | [ f ] -> if ip >= f.entry then Some f else None
      | f1 :: (f2 :: _ as rest) ->
          if ip >= f1.entry && ip < f2.entry then Some f1
          else find rest
    in
    find sorted_funcs
  in

  (* 4. Verify instructions *)
  Array.iteri
    (fun ip (instr : Opcode.instr) ->
      let current_fn = func_for_ip ip in
      let max_reg =
        match current_fn with
        | Some f -> f.nregs
        | None -> 256 (* fallback if outside standard func range *)
      in

      let check_reg r name =
        if r < 0 || r >= max_reg then
          add_err
            (Printf.sprintf "ip %d: register %s=%d out of bounds [0, %d)"
               ip name r max_reg)
      in

      let check_target target name =
        if target < 0 || target >= n_code then
          add_err
            (Printf.sprintf "ip %d: branch target %s=%d out of bounds [0, %d)"
               ip name target n_code)
      in

      let open Opcode in
      match instr.op with
      | Op_nop | Op_gc_safepoint | Op_ret_void -> ()

      | Op_load_const ->
          check_reg instr.a "a";
          if instr.b < 0 || instr.b >= n_consts then
            add_err
              (Printf.sprintf "ip %d: const index %d out of bounds [0, %d)"
                 ip instr.b n_consts)

      | Op_move | Op_neg | Op_neg_f | Op_not ->
          check_reg instr.a "a";
          check_reg instr.b "b"

      | Op_load_global ->
          check_reg instr.a "a";
          if instr.b < 0 || instr.b >= n_globals then
            add_err
              (Printf.sprintf "ip %d: load_global index %d out of bounds [0, %d)"
                 ip instr.b n_globals)

      | Op_store_global ->
          if instr.a < 0 || instr.a >= n_globals then
            add_err
              (Printf.sprintf "ip %d: store_global index %d out of bounds [0, %d)"
                 ip instr.a n_globals);
          check_reg instr.b "b"

      | Op_add | Op_sub | Op_mul | Op_div | Op_mod
      | Op_add_f | Op_sub_f | Op_mul_f | Op_div_f
      | Op_eq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
      | Op_eq_f | Op_ne_f | Op_lt_f | Op_le_f | Op_gt_f | Op_ge_f
      | Op_and | Op_or ->
          check_reg instr.a "a";
          check_reg instr.b "b";
          check_reg instr.c "c"

      | Op_jump ->
          check_target instr.a "jump target"

      | Op_jump_if | Op_jump_if_not ->
          check_reg instr.a "cond";
          check_target instr.b "jump target"

      | Op_switch ->
          check_reg instr.a "scrutinee";
          check_target instr.c "default target";
          let num_cases = instr.b in
          if Array.length instr.extra < 2 * num_cases then
            add_err
              (Printf.sprintf "ip %d: switch extra length %d < 2 * %d"
                 ip (Array.length instr.extra) num_cases)
          else
            for i = 0 to num_cases - 1 do
              let target = instr.extra.((2 * i) + 1) in
              check_target target (Printf.sprintf "case %d target" i)
            done

      | Op_call ->
          check_reg instr.a "dst";
          if not (Hashtbl.mem fn_ids instr.b) then
            add_err (Printf.sprintf "ip %d: call nonexistent fn%d" ip instr.b);
          Array.iter (fun r -> check_reg r "arg") instr.extra

      | Op_call_closure ->
          check_reg instr.a "dst";
          check_reg instr.b "clo";
          Array.iter (fun r -> check_reg r "arg") instr.extra

      | Op_tail_call ->
          if not (Hashtbl.mem fn_ids instr.a) then
            add_err (Printf.sprintf "ip %d: tail_call nonexistent fn%d" ip instr.a);
          Array.iter (fun r -> check_reg r "arg") instr.extra

      | Op_tail_call_closure ->
          check_reg instr.a "clo";
          Array.iter (fun r -> check_reg r "arg") instr.extra

      | Op_ret ->
          check_reg instr.a "ret_val"

      | Op_alloc_tuple | Op_alloc_adt ->
          check_reg instr.a "dst";
          Array.iter (fun r -> check_reg r "field") instr.extra

      | Op_alloc_closure ->
          check_reg instr.a "dst";
          if not (Hashtbl.mem fn_ids instr.b) then
            add_err (Printf.sprintf "ip %d: alloc_closure nonexistent fn%d" ip instr.b);
          Array.iter (fun r -> check_reg r "env") instr.extra

      | Op_get_field | Op_tuple_get ->
          check_reg instr.a "dst";
          check_reg instr.b "obj";
          if instr.c < 0 then
            add_err (Printf.sprintf "ip %d: negative field index %d" ip instr.c)

      | Op_set_field ->
          check_reg instr.a "obj";
          if instr.b < 0 then
            add_err (Printf.sprintf "ip %d: negative field index %d" ip instr.b);
          check_reg instr.c "src"

      | Op_get_tag ->
          check_reg instr.a "dst";
          check_reg instr.b "obj"

      | Op_cons ->
          check_reg instr.a "dst";
          check_reg instr.b "head";
          check_reg instr.c "tail"

      | Op_car | Op_cdr ->
          check_reg instr.a "dst";
          check_reg instr.b "cell"

      | Op_print | Op_print_int | Op_print_string | Op_print_bool ->
          check_reg instr.a "val"

      | Op_halt ->
          if instr.b <> 0 then check_reg instr.a "exit_val")
    chunk.code;

  if !errs = [] then Ok () else Error (List.rev !errs)

let check_chunk (chunk : t) : unit =
  match verify_chunk chunk with
  | Ok () -> ()
  | Error es ->
      failwith ("Bytecode verification failed:\n" ^ String.concat "\n" es)
