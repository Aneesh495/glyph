(** MIR → bytecode emitter. *)



(** Growable instruction buffer. *)
module Resize = struct
  type 'a t = {
    mutable arr : 'a array;
    mutable len : int;
  }

  let create () = { arr = [||]; len = 0 }

  let push t x =
    if t.len = Array.length t.arr then (
      let n = max 8 (Array.length t.arr * 2) in
      let arr' =
        if Array.length t.arr = 0 then Array.make n x
        else
          let a = Array.make n t.arr.(0) in
          Array.blit t.arr 0 a 0 t.len;
          a
      in
      t.arr <- arr');
    t.arr.(t.len) <- x;
    t.len <- t.len + 1

  let to_array t = Array.sub t.arr 0 t.len
  let get t i = t.arr.(i)
  let set t i x = t.arr.(i) <- x
  let length t = t.len
end

type pending_jump =
  | JAbs of int * int
  | JIf of int * int * bool
  | JSwitchCase of int * int * int

type emit_state = {
  chunk : Chunk.t;
  code : Opcode.instr Resize.t;
  label_ip : (int, int) Hashtbl.t;
  pending : pending_jump list ref;
  phi_moves : (int, (int * int) list) Hashtbl.t;
  reg_of : int array;
  mutable nregs : int;
}

let mir_const_to_chunk = function
  | Mir.CInt i -> Chunk.CInt i
  | Mir.CFloat f -> Chunk.CFloat f
  | Mir.CBool b -> Chunk.CBool b
  | Mir.CChar c -> Chunk.CChar c
  | Mir.CUnit -> Chunk.CUnit
  | Mir.CString s -> Chunk.CString s

let binop_op = function
  | Mir.Add -> Opcode.Op_add
  | Mir.Sub -> Opcode.Op_sub
  | Mir.Mul -> Opcode.Op_mul
  | Mir.Div -> Opcode.Op_div
  | Mir.Mod -> Opcode.Op_mod
  | Mir.AddF -> Opcode.Op_add_f
  | Mir.SubF -> Opcode.Op_sub_f
  | Mir.MulF -> Opcode.Op_mul_f
  | Mir.DivF -> Opcode.Op_div_f
  | Mir.Eq -> Opcode.Op_eq
  | Mir.Ne -> Opcode.Op_ne
  | Mir.Lt -> Opcode.Op_lt
  | Mir.Le -> Opcode.Op_le
  | Mir.Gt -> Opcode.Op_gt
  | Mir.Ge -> Opcode.Op_ge
  | Mir.EqF -> Opcode.Op_eq_f
  | Mir.NeF -> Opcode.Op_ne_f
  | Mir.LtF -> Opcode.Op_lt_f
  | Mir.LeF -> Opcode.Op_le_f
  | Mir.GtF -> Opcode.Op_gt_f
  | Mir.GeF -> Opcode.Op_ge_f
  | Mir.And -> Opcode.Op_and
  | Mir.Or -> Opcode.Op_or

let unop_op = function
  | Mir.Neg -> Opcode.Op_neg
  | Mir.NegF -> Opcode.Op_neg_f
  | Mir.Not -> Opcode.Op_not

let r st v =
  if v < 0 || v >= Array.length st.reg_of then
    failwith (Printf.sprintf "Emit: vreg %%%d out of range" v);
  st.reg_of.(v)

let emit_instr st instr = Resize.push st.code instr

let current_ip st = Resize.length st.code

let build_regmap (fn : Mir.func) =
  (* Identity mapping: SSA vreg index == VM register. Compact only if sparse. *)
  let used = Mir.all_vregs fn in
  let max_v =
    List.fold_left max (-1) used |> fun m -> max m (fn.n_vregs - 1)
  in
  let dense = List.length used < max_v / 2 && max_v > 32 in
  if not dense then (
    let arr = Array.init (max_v + 1) (fun i -> i) in
    (arr, max_v + 1))
  else
    let arr = Array.make (max_v + 1) (-1) in
    let next = ref 0 in
    List.iter
      (fun v ->
        arr.(v) <- !next;
        incr next)
      used;
    (arr, !next)

let collect_phi_moves st (fn : Mir.func) =
  Hashtbl.clear st.phi_moves;
  List.iter
    (fun (b : Mir.block) ->
      List.iter
        (function
          | Mir.IPhi (dst, incoming) ->
              let d = r st dst in
              List.iter
                (fun (pred, src) ->
                  let s = r st src in
                  if d <> s then
                    let moves =
                      Option.value
                        (Hashtbl.find_opt st.phi_moves pred)
                        ~default:[]
                    in
                    Hashtbl.replace st.phi_moves pred ((d, s) :: moves))
                incoming
          | _ -> ())
        b.phis)
    fn.blocks

let emit_phi_moves_for st pred_label =
  match Hashtbl.find_opt st.phi_moves pred_label with
  | None -> ()
  | Some moves ->
      (* Parallel moves: use a scratch register at nregs when needed. *)
      let scratch = st.nregs in
      if List.length moves > 1 then st.nregs <- max st.nregs (scratch + 1);
      (* Simple sequential moves — break cycles via scratch when dst is live. *)
      let pending = ref moves in
      let assigned = Hashtbl.create 8 in
      while !pending <> [] do
        let progress = ref false in
        let rest = ref [] in
        List.iter
          (fun (d, s) ->
            let s' =
              match Hashtbl.find_opt assigned s with Some t -> t | None -> s
            in
            let blocked =
              List.exists
                (fun (d2, _) -> d2 = s' && d2 <> d)
                (List.filter (fun p -> p <> (d, s)) !pending)
            in
            if blocked then rest := (d, s) :: !rest
            else (
              emit_instr st (Opcode.make Opcode.Op_move ~a:d ~b:s');
              Hashtbl.replace assigned d s';
              progress := true))
          !pending;
        if not !progress then (
          (* Cycle: break with scratch *)
          match !pending with
          | (d, s) :: tl ->
              let s' =
                match Hashtbl.find_opt assigned s with Some t -> t | None -> s
              in
              emit_instr st (Opcode.make Opcode.Op_move ~a:scratch ~b:s');
              emit_instr st (Opcode.make Opcode.Op_move ~a:d ~b:scratch);
              Hashtbl.replace assigned d scratch;
              pending := tl
          | [] -> ())
        else pending := List.rev !rest
      done

let regs_of st vs = Array.of_list (List.map (r st) vs)

let emit_mir_instr st (i : Mir.instr) =
  match i with
  | Mir.IPhi _ -> () (* handled via predecessor moves *)
  | Mir.INop -> emit_instr st (Opcode.make Opcode.Op_nop)
  | Mir.IConst (dst, c) ->
      let idx = Chunk.add_const st.chunk (mir_const_to_chunk c) in
      emit_instr st (Opcode.make Opcode.Op_load_const ~a:(r st dst) ~b:idx)
  | Mir.IMove (dst, src) ->
      emit_instr st (Opcode.make Opcode.Op_move ~a:(r st dst) ~b:(r st src))
  | Mir.IBinop (dst, op, a, b) ->
      emit_instr st
        (Opcode.make (binop_op op) ~a:(r st dst) ~b:(r st a) ~c:(r st b))
  | Mir.IUnop (dst, op, a) ->
      emit_instr st (Opcode.make (unop_op op) ~a:(r st dst) ~b:(r st a))
  | Mir.ICall (dst, fid, args) ->
      let extra = regs_of st args in
      emit_instr st
        (Opcode.make Opcode.Op_call ~a:(r st dst) ~b:fid
           ~c:(List.length args) ~extra)
  | Mir.ICallClosure (dst, clo, args) ->
      let extra = regs_of st args in
      emit_instr st
        (Opcode.make Opcode.Op_call_closure ~a:(r st dst) ~b:(r st clo)
           ~c:(List.length args) ~extra)
  | Mir.IAlloc (dst, tag, fields) ->
      let extra = regs_of st fields in
      let n = List.length fields in
      if tag = 0 && true then
        (* Convention: tag 0 with no ADT metadata → tuple; otherwise ADT.
           MIR uses IAlloc for both; tag distinguishes constructors. Tuples
           lowered from HIR typically use tag 0. *)
        emit_instr st
          (Opcode.make Opcode.Op_alloc_adt ~a:(r st dst) ~b:tag ~c:n ~extra)
      else
        emit_instr st
          (Opcode.make Opcode.Op_alloc_adt ~a:(r st dst) ~b:tag ~c:n ~extra)
  | Mir.IGetField (dst, obj, idx) ->
      emit_instr st
        (Opcode.make Opcode.Op_get_field ~a:(r st dst) ~b:(r st obj) ~c:idx)
  | Mir.ISetField (obj, idx, src) ->
      emit_instr st
        (Opcode.make Opcode.Op_set_field ~a:(r st obj) ~b:idx ~c:(r st src))
  | Mir.IMakeClosure (dst, fid, env) ->
      let extra = regs_of st env in
      emit_instr st
        (Opcode.make Opcode.Op_alloc_closure ~a:(r st dst) ~b:fid
           ~c:(List.length env) ~extra)
  | Mir.ITupleGet (dst, tup, idx) ->
      emit_instr st
        (Opcode.make Opcode.Op_tuple_get ~a:(r st dst) ~b:(r st tup) ~c:idx)
  | Mir.ICons (dst, h, t) ->
      emit_instr st
        (Opcode.make Opcode.Op_cons ~a:(r st dst) ~b:(r st h) ~c:(r st t))
  | Mir.ICar (dst, cell) ->
      emit_instr st (Opcode.make Opcode.Op_car ~a:(r st dst) ~b:(r st cell))
  | Mir.ICdr (dst, cell) ->
      emit_instr st (Opcode.make Opcode.Op_cdr ~a:(r st dst) ~b:(r st cell))
  | Mir.IPrint v ->
      emit_instr st (Opcode.make Opcode.Op_print ~a:(r st v))

let patch_label st lbl =
  match Hashtbl.find_opt st.label_ip lbl with
  | Some ip -> ip
  | None -> failwith (Printf.sprintf "Emit: unresolved label L%d" lbl)

let emit_term st (fn : Mir.func) (b : Mir.block) =
  (* φ moves for successors are emitted in the predecessor, i.e. here. *)
  emit_phi_moves_for st b.label;
  match b.term with
  | Mir.TJump lbl ->
      let idx = current_ip st in
      emit_instr st (Opcode.make Opcode.Op_jump ~a:0);
      st.pending := JAbs (idx, lbl) :: !(st.pending)
  | Mir.TBranch (cond, t, e) ->
      (* jump_if_not cond, else; jump then *)
      let i_else = current_ip st in
      emit_instr st (Opcode.make Opcode.Op_jump_if_not ~a:(r st cond) ~b:0);
      st.pending := JIf (i_else, e, false) :: !(st.pending);
      let i_then = current_ip st in
      emit_instr st (Opcode.make Opcode.Op_jump ~a:0);
      st.pending := JAbs (i_then, t) :: !(st.pending)
  | Mir.TSwitch (scrut, cases, default) ->
      let flat =
        Array.of_list
          (List.flatten (List.map (fun (tag, _) -> [ tag; 0 ]) cases))
      in
      let idx = current_ip st in
      emit_instr st
        (Opcode.make Opcode.Op_switch ~a:(r st scrut)
           ~b:(List.length cases) ~c:0 ~extra:flat);
      (* default stored in c after patch — use pending for each case + default *)
      List.iteri
        (fun i (_, lbl) ->
          st.pending := JSwitchCase (idx, (2 * i) + 1, lbl) :: !(st.pending))
        cases;
      (* store default in instr.c via a fake JAbs on a side channel: reuse c *)
      st.pending := JAbs (idx, default) :: !(st.pending)
      (* NOTE: resolve_pending special-cases switch default into .c *)
  | Mir.TRet None -> emit_instr st (Opcode.make Opcode.Op_ret_void)
  | Mir.TRet (Some v) ->
      emit_instr st (Opcode.make Opcode.Op_ret ~a:(r st v))
  | Mir.TTailCall (fid, args) ->
      let extra = regs_of st args in
      emit_instr st
        (Opcode.make Opcode.Op_tail_call ~a:fid ~b:(List.length args) ~extra)
  | Mir.TTailCallClosure (clo, args) ->
      let extra = regs_of st args in
      emit_instr st
        (Opcode.make Opcode.Op_tail_call_closure ~a:(r st clo)
           ~b:(List.length args) ~extra)
  | Mir.THalt None -> emit_instr st (Opcode.make Opcode.Op_halt ~a:0 ~b:0)
  | Mir.THalt (Some v) ->
      emit_instr st (Opcode.make Opcode.Op_halt ~a:(r st v) ~b:1)

let resolve_pending st =
  List.iter
    (function
      | JAbs (idx, lbl) ->
          let ip = patch_label st lbl in
          let instr = Resize.get st.code idx in
          if instr.Opcode.op = Opcode.Op_switch then
            Resize.set st.code idx { instr with c = ip }
          else Resize.set st.code idx { instr with a = ip }
      | JIf (idx, lbl, _is_if) ->
          let ip = patch_label st lbl in
          let instr = Resize.get st.code idx in
          Resize.set st.code idx { instr with b = ip }
      | JSwitchCase (idx, slot, lbl) ->
          let ip = patch_label st lbl in
          let instr = Resize.get st.code idx in
          let extra = Array.copy instr.extra in
          extra.(slot) <- ip;
          Resize.set st.code idx { instr with extra })
    (List.rev !(st.pending));
  st.pending := []

let rpo_blocks (fn : Mir.func) =
  (* Reverse postorder from entry; fall back to source order for leftovers. *)
  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec dfs lbl =
    if Hashtbl.mem visited lbl then ()
    else (
      Hashtbl.add visited lbl ();
      List.iter dfs (Mir.successors_of fn lbl);
      order := lbl :: !order)
  in
  dfs fn.entry;
  let rpo = !order in
  let missing =
    List.filter
      (fun (b : Mir.block) -> not (Hashtbl.mem visited b.label))
      fn.blocks
    |> List.map (fun b -> b.label)
  in
  rpo @ missing

let emit_func chunk (fn : Mir.func) =
  let reg_of, nregs0 = build_regmap fn in
  let st =
    {
      chunk;
      code = Resize.create ();
      label_ip = Hashtbl.create 32;
      pending = ref [];
      phi_moves = Hashtbl.create 32;
      reg_of;
      nregs = max nregs0 (List.length fn.params);
    }
  in
  collect_phi_moves st fn;
  (* Seed code buffer start offset relative to chunk *)
  let base = Array.length chunk.code in
  let blocks_by_label =
    let t = Hashtbl.create 16 in
    List.iter (fun (b : Mir.block) -> Hashtbl.replace t b.label b) fn.blocks;
    t
  in
  List.iter
    (fun lbl ->
      let b = Hashtbl.find blocks_by_label lbl in
      Hashtbl.replace st.label_ip b.label (base + current_ip st);
      List.iter (emit_mir_instr st) b.instrs;
      emit_term st fn b;
      emit_instr st (Opcode.make Opcode.Op_gc_safepoint))
    (rpo_blocks fn);
  resolve_pending st;
  let entry =
    match Hashtbl.find_opt st.label_ip fn.entry with
    | Some ip -> ip
    | None -> base
  in
  let new_code = Resize.to_array st.code in
  chunk.code <- Array.append chunk.code new_code;
  let fmeta : Chunk.func =
    {
      name = Ident.to_string fn.name;
      fn_id = fn.id;
      arity = List.length fn.params;
      nregs = st.nregs;
      entry;
      is_main = fn.is_main;
    }
  in
  chunk.funcs <- Array.append chunk.funcs [| fmeta |]

let emit (prog : Mir.program) =
  let chunk = Chunk.empty () in
  (* Pre-seed string table constants *)
  List.iter
    (fun s -> ignore (Chunk.add_const chunk (Chunk.CString s)))
    prog.string_table;
  List.iter (emit_func chunk) prog.functions;
  (* Fix main field — Chunk.t.main is immutable, recreate *)
  {
    chunk with
    main = prog.main;
  }
