(** Emit Ident-based SSA MIR into bytecode [Chunk.t].

    Public API: [emit_program]. *)

open Mir

exception Emit_error of string

module Buf = struct
  type 'a t = { mutable arr : 'a array; mutable len : int }
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
  let set t i x = t.arr.(i) <- x
  let get t i = t.arr.(i)
  let length t = t.len
end

type reloc =
  | Rel_jump of int * label
  | Rel_jump_if of int * label
  | Rel_switch of int * (int * label) list * label

type ctx = {
  chunk : Chunk.t;
  code : Opcode.instr Buf.t;
  mutable alloc : Regalloc.result;
  fn_ids : (Ident.t, int) Hashtbl.t;
  label_ip : (label, int) Hashtbl.t;
  mutable relocs : reloc list;
  mutable scratch : int;
}

let mir_const = function
  | CInt i -> Chunk.CInt i
  | CFloat f -> Chunk.CFloat f
  | CBool b -> Chunk.CBool b
  | CChar c -> Chunk.CChar c
  | CUnit | CNull -> Chunk.CUnit
  | CString s -> Chunk.CString s

let binop_op = function
  | Add -> Opcode.Op_add | Sub -> Opcode.Op_sub | Mul -> Opcode.Op_mul
  | Div -> Opcode.Op_div | Mod -> Opcode.Op_mod
  | FAdd -> Opcode.Op_add_f | FSub -> Opcode.Op_sub_f
  | FMul -> Opcode.Op_mul_f | FDiv -> Opcode.Op_div_f
  | Eq -> Opcode.Op_eq | Ne -> Opcode.Op_ne | Lt -> Opcode.Op_lt
  | Le -> Opcode.Op_le | Gt -> Opcode.Op_gt | Ge -> Opcode.Op_ge
  | And -> Opcode.Op_and | Or -> Opcode.Op_or
  | Xor | Shl | Shr -> Opcode.Op_add

let push ctx op = Buf.push ctx.code op

let next_scratch ctx =
  let s = ctx.scratch in
  ctx.scratch <- s + 1;
  s

let materialize ctx = function
  | VReg r -> Regalloc.lookup ctx.alloc r
  | VConst c ->
      let dst = next_scratch ctx in
      let idx = Chunk.add_const ctx.chunk (mir_const c) in
      push ctx (Opcode.make Opcode.Op_load_const ~a:dst ~b:idx ());
      dst
  | VGlobal g ->
      let dst = next_scratch ctx in
      (match Hashtbl.find_opt ctx.fn_ids g with
      | Some fid ->
          let idx = Chunk.add_const ctx.chunk (Chunk.CInt fid) in
          push ctx (Opcode.make Opcode.Op_load_const ~a:dst ~b:idx ())
      | None ->
          let idx =
            Chunk.add_const ctx.chunk (Chunk.CString (Ident.name g))
          in
          push ctx (Opcode.make Opcode.Op_load_const ~a:dst ~b:idx ()));
      dst
  | VUndef ->
      let dst = next_scratch ctx in
      let idx = Chunk.add_const ctx.chunk Chunk.CUnit in
      push ctx (Opcode.make Opcode.Op_load_const ~a:dst ~b:idx ());
      dst

let move_to ctx dst v =
  match v with
  | VReg r ->
      let src = Regalloc.lookup ctx.alloc r in
      if src <> dst then push ctx (Opcode.make Opcode.Op_move ~a:dst ~b:src ())
  | _ ->
      let src = materialize ctx v in
      if src <> dst then push ctx (Opcode.make Opcode.Op_move ~a:dst ~b:src ())

let emit_phi_moves ctx pred (succ : block) =
  List.iter
    (function
      | Phi { dst; incoming; _ } -> (
          match List.assoc_opt pred incoming with
          | None -> ()
          | Some v -> move_to ctx (Regalloc.lookup ctx.alloc dst) v)
      | _ -> ())
    succ.phis

let emit_instr ctx = function
  | Assign { dst; src; _ } -> move_to ctx (Regalloc.lookup ctx.alloc dst) src
  | Binop { dst; op; lhs; rhs; _ } ->
      push ctx
        (Opcode.make (binop_op op)
           ~a:(Regalloc.lookup ctx.alloc dst)
           ~b:(materialize ctx lhs) ~c:(materialize ctx rhs) ())
  | Unop { dst; op; arg; _ } ->
      let d = Regalloc.lookup ctx.alloc dst in
      let a = materialize ctx arg in
      (match op with
      | Neg -> push ctx (Opcode.make Opcode.Op_neg ~a:d ~b:a ())
      | FNeg -> push ctx (Opcode.make Opcode.Op_neg_f ~a:d ~b:a ())
      | Not | BitNot | IsNull ->
          push ctx (Opcode.make Opcode.Op_not ~a:d ~b:a ())
      | TagOf -> push ctx (Opcode.make Opcode.Op_get_tag ~a:d ~b:a ())
      | Box | Unbox -> push ctx (Opcode.make Opcode.Op_move ~a:d ~b:a ()))
  | Call { dst; callee; args; _ } ->
      let arg_regs =
        Array.of_list (List.map (fun a -> materialize ctx a) args)
      in
      let d =
        match dst with
        | Some r -> Regalloc.lookup ctx.alloc r
        | None -> next_scratch ctx
      in
      (match callee with
      | VGlobal g -> (
          match Hashtbl.find_opt ctx.fn_ids g with
          | Some fid ->
              push ctx
                (Opcode.make Opcode.Op_call ~a:d ~b:fid
                   ~c:(Array.length arg_regs) ~extra:arg_regs ())
          | None -> (
              match (Ident.name g, args) with
              | "print_int", [ a ] ->
                  push ctx
                    (Opcode.make Opcode.Op_print_int ~a:(materialize ctx a) ())
              | "print_bool", [ a ] ->
                  push ctx
                    (Opcode.make Opcode.Op_print_bool ~a:(materialize ctx a) ())
              | ("print" | "print_string"), [ a ] ->
                  push ctx
                    (Opcode.make Opcode.Op_print_string
                       ~a:(materialize ctx a) ())
              | _ -> ()))
      | v ->
          push ctx
            (Opcode.make Opcode.Op_call_closure ~a:d ~b:(materialize ctx v)
               ~c:(Array.length arg_regs) ~extra:arg_regs ()))
  | Alloc { dst; tag; fields; _ } ->
      let d = Regalloc.lookup ctx.alloc dst in
      let regs =
        Array.of_list (List.map (fun f -> materialize ctx f) fields)
      in
      if tag = -2 then
        push ctx
          (Opcode.make Opcode.Op_alloc_tuple ~a:d ~b:(Array.length regs)
             ~extra:regs ())
      else if tag < 0 then
        push ctx
          (Opcode.make Opcode.Op_alloc_closure ~a:d ~b:0
             ~c:(Array.length regs) ~extra:regs ())
      else
        push ctx
          (Opcode.make Opcode.Op_alloc_adt ~a:d ~b:tag ~c:(Array.length regs)
             ~extra:regs ())
  | GetField { dst; obj; index; _ } ->
      push ctx
        (Opcode.make Opcode.Op_get_field
           ~a:(Regalloc.lookup ctx.alloc dst)
           ~b:(materialize ctx obj) ~c:index ())
  | SetField { obj; index; value; _ } ->
      push ctx
        (Opcode.make Opcode.Op_set_field ~a:(materialize ctx obj) ~b:index
           ~c:(materialize ctx value) ())
  | Load { dst; ptr; _ } | Cast { dst; src = ptr; _ } ->
      move_to ctx (Regalloc.lookup ctx.alloc dst) ptr
  | Store _ | Phi _ -> ()

let emit_term ctx (fn : func) (b : block) =
  List.iter
    (fun succ ->
      match find_block fn succ with
      | None -> ()
      | Some sb -> emit_phi_moves ctx b.label sb)
    b.succs;
  match b.terminator with
  | Return (None, _) -> push ctx (Opcode.make Opcode.Op_ret_void ())
  | Return (Some v, _) ->
      push ctx (Opcode.make Opcode.Op_ret ~a:(materialize ctx v) ())
  | Jump (l, _) ->
      let ip = Buf.length ctx.code in
      push ctx (Opcode.make Opcode.Op_jump ~a:0 ());
      ctx.relocs <- Rel_jump (ip, l) :: ctx.relocs
  | Branch { cond; then_; else_; _ } ->
      let c = materialize ctx cond in
      let ip1 = Buf.length ctx.code in
      push ctx (Opcode.make Opcode.Op_jump_if ~a:c ~b:0 ());
      ctx.relocs <- Rel_jump_if (ip1, then_) :: ctx.relocs;
      let ip2 = Buf.length ctx.code in
      push ctx (Opcode.make Opcode.Op_jump ~a:0 ());
      ctx.relocs <- Rel_jump (ip2, else_) :: ctx.relocs
  | Switch { scrut; cases; default; _ } ->
      let s = materialize ctx scrut in
      let extra =
        Array.of_list
          (List.concat_map (fun (tag, _) -> [ tag; 0 ]) cases @ [ 0 ])
      in
      let ip = Buf.length ctx.code in
      push ctx
        (Opcode.make Opcode.Op_switch ~a:s ~b:(List.length cases) ~extra ());
      ctx.relocs <- Rel_switch (ip, cases, default) :: ctx.relocs
  | TailCall { callee; args; _ } -> (
      let arg_regs =
        Array.of_list (List.map (fun a -> materialize ctx a) args)
      in
      match callee with
      | VGlobal g -> (
          match Hashtbl.find_opt ctx.fn_ids g with
          | Some fid ->
              push ctx
                (Opcode.make Opcode.Op_tail_call ~a:fid
                   ~b:(Array.length arg_regs) ~extra:arg_regs ())
          | None -> push ctx (Opcode.make Opcode.Op_halt ()))
      | v ->
          push ctx
            (Opcode.make Opcode.Op_tail_call_closure ~a:(materialize ctx v)
               ~b:(Array.length arg_regs) ~extra:arg_regs ()))
  | Unreachable _ -> push ctx (Opcode.make Opcode.Op_halt ())

let patch_relocs ctx =
  List.iter
    (function
      | Rel_jump (ip, l) -> (
          match Hashtbl.find_opt ctx.label_ip l with
          | Some target ->
              let instr = Buf.get ctx.code ip in
              Buf.set ctx.code ip { instr with a = target }
          | None ->
              raise (Emit_error ("unresolved jump " ^ Label.to_string l)))
      | Rel_jump_if (ip, l) -> (
          match Hashtbl.find_opt ctx.label_ip l with
          | Some target ->
              let instr = Buf.get ctx.code ip in
              Buf.set ctx.code ip { instr with b = target }
          | None ->
              raise (Emit_error ("unresolved jump_if " ^ Label.to_string l)))
      | Rel_switch (ip, cases, default) -> (
          match Hashtbl.find_opt ctx.label_ip default with
          | None ->
              raise
                (Emit_error ("unresolved switch default " ^ Label.to_string default))
          | Some def_ip ->
              let instr = Buf.get ctx.code ip in
              let extra = Array.copy instr.extra in
              List.iteri
                (fun i (_, l) ->
                  match Hashtbl.find_opt ctx.label_ip l with
                  | Some tip -> extra.((i * 2) + 1) <- tip
                  | None ->
                      raise
                        (Emit_error
                           ("unresolved switch case " ^ Label.to_string l)))
                cases;
              let def_slot = Array.length cases * 2 in
              if def_slot < Array.length extra then extra.(def_slot) <- def_ip;
              Buf.set ctx.code ip { instr with extra }))
    (List.rev ctx.relocs);
  ctx.relocs <- []

let emit_func ctx (fn : func) =
  Cfg.prepare fn;
  ctx.alloc <- Regalloc.allocate fn;
  ctx.scratch <- ctx.alloc.n_regs;
  Hashtbl.clear ctx.label_ip;
  List.iter
    (fun lbl ->
      match find_block fn lbl with
      | None -> ()
      | Some b ->
          Hashtbl.replace ctx.label_ip b.label (Buf.length ctx.code);
          List.iter (emit_instr ctx) b.instrs;
          emit_term ctx fn b)
    (Cfg.reverse_postorder fn);
  patch_relocs ctx;
  let entry_ip =
    match Hashtbl.find_opt ctx.label_ip fn.entry with Some ip -> ip | None -> 0
  in
  (entry_ip, max ctx.scratch (ctx.alloc.n_regs + 4))

let emit_program (prog : program) : Chunk.t =
  let chunk = Chunk.empty () in
  let fn_ids = Hashtbl.create 32 in
  let i = ref 0 in
  Ident.Map.iter
    (fun name _ ->
      Hashtbl.replace fn_ids name !i;
      incr i)
    prog.funcs;
  let dummy =
    Mir.make_func ~name:(Ident.of_string "_") ~params:[]
      ~entry:(Label.of_string "e") ~blocks:Label.Map.empty ~return_ty:Ty_unit ()
  in
  let ctx =
    {
      chunk;
      code = Buf.create ();
      alloc = Regalloc.identity dummy;
      fn_ids;
      label_ip = Hashtbl.create 32;
      relocs = [];
      scratch = 0;
    }
  in
  let metas = ref [] in
  Ident.Map.iter
    (fun name fn ->
      let fid = Hashtbl.find fn_ids name in
      let entry_ip, nregs = emit_func ctx fn in
      metas :=
        {
          Chunk.name = Ident.name name;
          fn_id = fid;
          arity = List.length fn.params;
          nregs;
          entry = entry_ip;
          is_main =
            String.equal (Ident.name name) "main"
            || String.equal (Ident.name name) "_main";
        }
        :: !metas)
    prog.funcs;
  let funcs = Array.of_list (List.rev !metas) in
  let main =
    match
      Array.find_opt (fun (f : Chunk.func) -> f.is_main) funcs
    with
    | Some f -> f.fn_id
    | None -> if Array.length funcs > 0 then funcs.(0).fn_id else 0
  in
  chunk.code <- Buf.to_array ctx.code;
  chunk.funcs <- funcs;
  { chunk with main }

let emit = emit_program
