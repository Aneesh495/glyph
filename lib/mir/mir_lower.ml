(** Lowering from High-level IR (HIR) to Mid-level IR (MIR). *)

open Mir

type global_fn = {
  fn_id : fn_id;
  arity : int;
}

type ctx = {
  global_fns : (Ident.t, global_fn) Hashtbl.t;
  globals : (Ident.t, int) Hashtbl.t;
  mutable global_names : string list;
  mutable extra_funcs : func list;
  mutable next_fn_id : int;
  string_table : (string, int) Hashtbl.t;
  mutable string_list : string list;
}

type block_builder = {
  label : label;
  mutable phis : (vreg * (label * vreg) list) list;
  mutable instrs : instr list;
  mutable term : terminator option;
  span : Span.t;
}

type fn_ctx = {
  fn_id : fn_id;
  fn_name : Ident.t;
  mutable next_vreg : int;
  mutable next_label : int;
  mutable blocks : block_builder list;
  block_map : (label, block_builder) Hashtbl.t;
  vars : (Ident.t, vreg) Hashtbl.t;
  span : Span.t;
}

let create_ctx () =
  {
    global_fns = Hashtbl.create 32;
    globals = Hashtbl.create 32;
    global_names = [];
    extra_funcs = [];
    next_fn_id = 0;
    string_table = Hashtbl.create 32;
    string_list = [];
  }

let alloc_fn_id ctx =
  let id = ctx.next_fn_id in
  ctx.next_fn_id <- ctx.next_fn_id + 1;
  id

let intern_global ctx (name : Ident.t) =
  match Hashtbl.find_opt ctx.globals name with
  | Some idx -> idx
  | None ->
      let idx = List.length ctx.global_names in
      let s = Ident.name name in
      ctx.global_names <- ctx.global_names @ [ s ];
      Hashtbl.replace ctx.globals name idx;
      idx

let intern_string ctx s =
  match Hashtbl.find_opt ctx.string_table s with
  | Some idx -> idx
  | None ->
      let idx = List.length ctx.string_list in
      ctx.string_list <- ctx.string_list @ [ s ];
      Hashtbl.replace ctx.string_table s idx;
      idx

let create_fn_ctx ~fn_id ~name ~span =
  let fn_ctx =
    {
      fn_id;
      fn_name = name;
      next_vreg = 0;
      next_label = 0;
      blocks = [];
      block_map = Hashtbl.create 16;
      vars = Hashtbl.create 32;
      span;
    }
  in
  fn_ctx

let fresh_vreg fn_ctx =
  let v = fn_ctx.next_vreg in
  fn_ctx.next_vreg <- fn_ctx.next_vreg + 1;
  v

let fresh_label fn_ctx =
  let l = fn_ctx.next_label in
  fn_ctx.next_label <- fn_ctx.next_label + 1;
  l

let new_block fn_ctx ?(span = Span.dummy) () =
  let label = fresh_label fn_ctx in
  let b = { label; phis = []; instrs = []; term = None; span } in
  fn_ctx.blocks <- b :: fn_ctx.blocks;
  Hashtbl.replace fn_ctx.block_map label b;
  b

let emit fn_ctx lbl instr =
  match Hashtbl.find_opt fn_ctx.block_map lbl with
  | Some b -> b.instrs <- instr :: b.instrs
  | None -> failwith (Printf.sprintf "emit to unknown block L%d" lbl)

let set_term fn_ctx lbl term =
  match Hashtbl.find_opt fn_ctx.block_map lbl with
  | Some b ->
      if b.term = None then b.term <- Some term
  | None -> failwith (Printf.sprintf "set_term on unknown block L%d" lbl)

let is_terminated fn_ctx lbl =
  match Hashtbl.find_opt fn_ctx.block_map lbl with
  | Some b -> b.term <> None
  | None -> false

let add_phi fn_ctx lbl dst incoming =
  match Hashtbl.find_opt fn_ctx.block_map lbl with
  | Some b -> b.phis <- (dst, incoming) :: b.phis
  | None -> failwith (Printf.sprintf "add_phi to unknown block L%d" lbl)

let mir_const_of_lit = function
  | Hir.Lit_unit -> CUnit
  | Hir.Lit_bool b -> CBool b
  | Hir.Lit_int i -> CInt i
  | Hir.Lit_float f -> CFloat f
  | Hir.Lit_char c -> CChar c
  | Hir.Lit_string s -> CString s

let int_of_lit = function
  | Hir.Lit_int i -> Some i
  | Hir.Lit_bool b -> Some (if b then 1 else 0)
  | Hir.Lit_char c -> Some (Char.code c)
  | Hir.Lit_unit -> Some 0
  | _ -> None

let is_builtin = function
  | "print_int" | "print_string" | "print_bool" | "print_float" | "print_char" | "print"
  | "string_of_int" | "int_of_string" | "string_concat" | "string_length" | "abort" | "exit" -> true
  | _ -> false

let lower_atom ctx fn_ctx lbl = function
  | Hir.Atom_lit lit ->
      let v = fresh_vreg fn_ctx in
      (match lit with
      | Hir.Lit_string s ->
          let _ = intern_string ctx s in
          emit fn_ctx lbl (IConst (v, CString s))
      | other -> emit fn_ctx lbl (IConst (v, mir_const_of_lit other)));
      v
  | Hir.Atom_var id -> (
      match Hashtbl.find_opt fn_ctx.vars id with
      | Some v -> v
      | None -> (
          match Hashtbl.find_opt ctx.global_fns id with
          | Some gfn ->
              (* Used as a first-class value: create closure with empty env *)
              let v = fresh_vreg fn_ctx in
              emit fn_ctx lbl (IMakeClosure (v, gfn.fn_id, []));
              v
          | None -> (
              let s = Ident.name id in
              if is_builtin s then
                let g_idx = intern_global ctx id in
                let v = fresh_vreg fn_ctx in
                emit fn_ctx lbl (ILoadGlobal (v, g_idx));
                v
              else
                match Hashtbl.find_opt ctx.globals id with
                | Some g_idx ->
                    let v = fresh_vreg fn_ctx in
                    emit fn_ctx lbl (ILoadGlobal (v, g_idx));
                    v
                | None ->
                    failwith
                      (Printf.sprintf "Mir_lower: unbound variable '%s' in function '%s'"
                         s (Ident.name fn_ctx.fn_name)))))

let rec lower_expr ctx fn_ctx lbl (e : Hir.expr) : vreg * label =
  match e with
  | Hir.Atom (a, _sp) ->
      let v = lower_atom ctx fn_ctx lbl a in
      (v, lbl)

  | Hir.Let (x, rhs, body, _sp) ->
      let vr, lbl' = lower_expr ctx fn_ctx lbl rhs in
      Hashtbl.replace fn_ctx.vars x vr;
      lower_expr ctx fn_ctx lbl' body

  | Hir.Seq (e1, e2, _sp) ->
      let _, lbl' = lower_expr ctx fn_ctx lbl e1 in
      lower_expr ctx fn_ctx lbl' e2

  | Hir.If (cond, t, f, sp) ->
      let vc = lower_atom ctx fn_ctx lbl cond in
      let l_then = new_block fn_ctx ~span:(Hir.expr_span t) () in
      let l_else = new_block fn_ctx ~span:(Hir.expr_span f) () in
      let l_join = new_block fn_ctx ~span:sp () in
      set_term fn_ctx lbl (TBranch (vc, l_then.label, l_else.label));
      let vt, lt_end = lower_expr ctx fn_ctx l_then.label t in
      if not (is_terminated fn_ctx lt_end) then
        set_term fn_ctx lt_end (TJump l_join.label);
      let vf, lf_end = lower_expr ctx fn_ctx l_else.label f in
      if not (is_terminated fn_ctx lf_end) then
        set_term fn_ctx lf_end (TJump l_join.label);
      let dst = fresh_vreg fn_ctx in
      let incoming = ref [] in
      if not (is_terminated fn_ctx lt_end) || true then
        incoming := (lt_end, vt) :: !incoming;
      if not (is_terminated fn_ctx lf_end) || true then
        incoming := (lf_end, vf) :: !incoming;
      add_phi fn_ctx l_join.label dst (List.rev !incoming);
      (dst, l_join.label)

  | Hir.Prim (op, args, _sp) ->
      let vs = List.map (lower_atom ctx fn_ctx lbl) args in
      let dst = fresh_vreg fn_ctx in
      lower_prim fn_ctx lbl dst op vs;
      (dst, lbl)

  | Hir.App (f_atom, args, _sp) ->
      let arg_vregs = List.map (lower_atom ctx fn_ctx lbl) args in
      let dst = fresh_vreg fn_ctx in
      (match f_atom with
      | Hir.Atom_var id -> (
          let s = Ident.name id in
          if (s = "print_int" || s = "print_string" || s = "print_bool" || s = "print")
             && List.length arg_vregs = 1 then (
            emit fn_ctx lbl (IPrint (List.hd arg_vregs));
            emit fn_ctx lbl (IConst (dst, CUnit)))
          else
            match Hashtbl.find_opt ctx.global_fns id with
            | Some gfn when gfn.arity = List.length arg_vregs ->
                emit fn_ctx lbl (ICall (dst, gfn.fn_id, arg_vregs))
            | _ ->
                let clo_vreg = lower_atom ctx fn_ctx lbl f_atom in
                emit fn_ctx lbl (ICallClosure (dst, clo_vreg, arg_vregs)))
      | _ ->
          let clo_vreg = lower_atom ctx fn_ctx lbl f_atom in
          emit fn_ctx lbl (ICallClosure (dst, clo_vreg, arg_vregs)));
      (dst, lbl)

  | Hir.Ctor (info, args, _sp) ->
      let vs = List.map (lower_atom ctx fn_ctx lbl) args in
      let dst = fresh_vreg fn_ctx in
      emit fn_ctx lbl (IAlloc (dst, info.ctor_tag, vs));
      (dst, lbl)

  | Hir.Tuple (args, _sp) ->
      let vs = List.map (lower_atom ctx fn_ctx lbl) args in
      let dst = fresh_vreg fn_ctx in
      emit fn_ctx lbl (IAlloc (dst, 0, vs));
      (dst, lbl)

  | Hir.Project (atom, idx, _sp) ->
      let v = lower_atom ctx fn_ctx lbl atom in
      let dst = fresh_vreg fn_ctx in
      emit fn_ctx lbl (IGetField (dst, v, idx));
      (dst, lbl)

  | Hir.Fun (params, body, sp) ->
      let dst = fresh_vreg fn_ctx in
      lower_closure ctx fn_ctx lbl dst params body sp;
      (dst, lbl)

  | Hir.Let_rec (bindings, body, sp) ->
      lower_let_rec ctx fn_ctx lbl bindings body sp

  | Hir.Switch_ctor (scrut, cases, default_opt, sp) ->
      let vs = lower_atom ctx fn_ctx lbl scrut in
      lower_switch_ctor ctx fn_ctx lbl vs cases default_opt sp

  | Hir.Switch_lit (scrut, cases, default_opt, sp) ->
      let vs = lower_atom ctx fn_ctx lbl scrut in
      lower_switch_lit ctx fn_ctx lbl vs cases default_opt sp

  | Hir.Raise (a, _sp) ->
      let _v = lower_atom ctx fn_ctx lbl a in
      set_term fn_ctx lbl (THalt None);
      let l_unreach = new_block fn_ctx () in
      let dst = fresh_vreg fn_ctx in
      (dst, l_unreach.label)

  | Hir.Fail_match _sp ->
      set_term fn_ctx lbl (THalt None);
      let l_unreach = new_block fn_ctx () in
      let dst = fresh_vreg fn_ctx in
      (dst, l_unreach.label)

  | Hir.Match _ ->
      failwith "Mir_lower: uncompiled Match encountered; pattern compiler must run before MIR lowering"

and lower_prim fn_ctx lbl dst op vs =
  match (op, vs) with
  | Hir.Prim_add, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Add, a, b))
  | Hir.Prim_sub, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Sub, a, b))
  | Hir.Prim_mul, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Mul, a, b))
  | Hir.Prim_div, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Div, a, b))
  | Hir.Prim_mod, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Mod, a, b))
  | Hir.Prim_fadd, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, AddF, a, b))
  | Hir.Prim_fsub, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, SubF, a, b))
  | Hir.Prim_fmul, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, MulF, a, b))
  | Hir.Prim_fdiv, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, DivF, a, b))
  | Hir.Prim_eq, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Eq, a, b))
  | Hir.Prim_ne, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Ne, a, b))
  | Hir.Prim_lt, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Lt, a, b))
  | Hir.Prim_le, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Le, a, b))
  | Hir.Prim_gt, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Gt, a, b))
  | Hir.Prim_ge, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Ge, a, b))
  | Hir.Prim_and, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, And, a, b))
  | Hir.Prim_or, [ a; b ] -> emit fn_ctx lbl (IBinop (dst, Or, a, b))
  | Hir.Prim_not, [ a ] -> emit fn_ctx lbl (IUnop (dst, Not, a))
  | Hir.Prim_neg, [ a ] -> emit fn_ctx lbl (IUnop (dst, Neg, a))
  | (Hir.Prim_print | Hir.Prim_print_int | Hir.Prim_print_bool), [ a ] ->
      emit fn_ctx lbl (IPrint a);
      emit fn_ctx lbl (IConst (dst, CUnit))
  | Hir.Prim_is_unit, [ _ ] ->
      emit fn_ctx lbl (IConst (dst, CBool true))
  | (Hir.Prim_box | Hir.Prim_unbox), [ a ] ->
      emit fn_ctx lbl (IMove (dst, a))
  | Hir.Prim_abort, _ ->
      set_term fn_ctx lbl (THalt None)
  | _ -> failwith (Printf.sprintf "Mir_lower: unsupported primop or arity mismatch: %s" (Hir.primop_to_string op))

and lower_tail ctx fn_ctx lbl (e : Hir.expr) : unit =
  match e with
  | Hir.Atom (a, _sp) ->
      let v = lower_atom ctx fn_ctx lbl a in
      set_term fn_ctx lbl (TRet (Some v))

  | Hir.Let (x, rhs, body, _sp) ->
      let vr, lbl' = lower_expr ctx fn_ctx lbl rhs in
      Hashtbl.replace fn_ctx.vars x vr;
      lower_tail ctx fn_ctx lbl' body

  | Hir.Seq (e1, e2, _sp) ->
      let _, lbl' = lower_expr ctx fn_ctx lbl e1 in
      lower_tail ctx fn_ctx lbl' e2

  | Hir.If (cond, t, f, _sp) ->
      let vc = lower_atom ctx fn_ctx lbl cond in
      let l_then = new_block fn_ctx ~span:(Hir.expr_span t) () in
      let l_else = new_block fn_ctx ~span:(Hir.expr_span f) () in
      set_term fn_ctx lbl (TBranch (vc, l_then.label, l_else.label));
      lower_tail ctx fn_ctx l_then.label t;
      lower_tail ctx fn_ctx l_else.label f

  | Hir.App (f_atom, args, _sp) ->
      let arg_vregs = List.map (lower_atom ctx fn_ctx lbl) args in
      (match f_atom with
      | Hir.Atom_var id -> (
          let s = Ident.name id in
          if (s = "print_int" || s = "print_string" || s = "print_bool" || s = "print")
             && List.length arg_vregs = 1 then (
            emit fn_ctx lbl (IPrint (List.hd arg_vregs));
            let dst = fresh_vreg fn_ctx in
            emit fn_ctx lbl (IConst (dst, CUnit));
            set_term fn_ctx lbl (TRet (Some dst)))
          else
            match Hashtbl.find_opt ctx.global_fns id with
            | Some gfn when gfn.arity = List.length arg_vregs ->
                set_term fn_ctx lbl (TTailCall (gfn.fn_id, arg_vregs))
            | _ ->
                let clo_vreg = lower_atom ctx fn_ctx lbl f_atom in
                set_term fn_ctx lbl (TTailCallClosure (clo_vreg, arg_vregs)))
      | _ ->
          let clo_vreg = lower_atom ctx fn_ctx lbl f_atom in
          set_term fn_ctx lbl (TTailCallClosure (clo_vreg, arg_vregs)))

  | Hir.Switch_ctor (scrut, cases, default_opt, sp) ->
      let vs = lower_atom ctx fn_ctx lbl scrut in
      lower_switch_ctor_tail ctx fn_ctx lbl vs cases default_opt sp

  | Hir.Switch_lit (scrut, cases, default_opt, sp) ->
      let vs = lower_atom ctx fn_ctx lbl scrut in
      lower_switch_lit_tail ctx fn_ctx lbl vs cases default_opt sp

  | Hir.Fail_match _ | Hir.Raise _ ->
      set_term fn_ctx lbl (THalt None)

  | other ->
      let v, lbl' = lower_expr ctx fn_ctx lbl other in
      if not (is_terminated fn_ctx lbl') then
        set_term fn_ctx lbl' (TRet (Some v))

and lower_switch_ctor ctx fn_ctx lbl vs cases default_opt sp : vreg * label =
  let l_join = new_block fn_ctx ~span:sp () in
  let dst = fresh_vreg fn_ctx in
  let l_default = new_block fn_ctx ~span:sp () in
  let incoming = ref [] in
  let case_tags_labels =
    List.map
      (fun (c, binds, body) ->
        let l_case = new_block fn_ctx ~span:(Hir.expr_span body) () in
        List.iteri
          (fun i b ->
            let vb = fresh_vreg fn_ctx in
            emit fn_ctx l_case.label (IGetField (vb, vs, i));
            Hashtbl.replace fn_ctx.vars b vb)
          binds;
        let v_case, l_case_end = lower_expr ctx fn_ctx l_case.label body in
        if not (is_terminated fn_ctx l_case_end) then (
          set_term fn_ctx l_case_end (TJump l_join.label);
          incoming := (l_case_end, v_case) :: !incoming);
        (c.Hir.ctor_tag, l_case.label))
      cases
  in
  (match default_opt with
  | Some def ->
      let v_def, l_def_end = lower_expr ctx fn_ctx l_default.label def in
      if not (is_terminated fn_ctx l_def_end) then (
        set_term fn_ctx l_def_end (TJump l_join.label);
        incoming := (l_def_end, v_def) :: !incoming)
  | None ->
      set_term fn_ctx l_default.label (THalt None));
  set_term fn_ctx lbl (TSwitch (vs, case_tags_labels, l_default.label));
  add_phi fn_ctx l_join.label dst (List.rev !incoming);
  (dst, l_join.label)

and lower_switch_ctor_tail ctx fn_ctx lbl vs cases default_opt _sp : unit =
  let l_default = new_block fn_ctx () in
  let case_tags_labels =
    List.map
      (fun (c, binds, body) ->
        let l_case = new_block fn_ctx ~span:(Hir.expr_span body) () in
        List.iteri
          (fun i b ->
            let vb = fresh_vreg fn_ctx in
            emit fn_ctx l_case.label (IGetField (vb, vs, i));
            Hashtbl.replace fn_ctx.vars b vb)
          binds;
        lower_tail ctx fn_ctx l_case.label body;
        (c.Hir.ctor_tag, l_case.label))
      cases
  in
  (match default_opt with
  | Some def -> lower_tail ctx fn_ctx l_default.label def
  | None -> set_term fn_ctx l_default.label (THalt None));
  set_term fn_ctx lbl (TSwitch (vs, case_tags_labels, l_default.label))

and lower_switch_lit ctx fn_ctx lbl vs cases default_opt sp : vreg * label =
  let all_ints = List.for_all (fun (l, _) -> int_of_lit l <> None) cases in
  if all_ints then (
    let l_join = new_block fn_ctx ~span:sp () in
    let dst = fresh_vreg fn_ctx in
    let l_default = new_block fn_ctx ~span:sp () in
    let incoming = ref [] in
    let case_tags_labels =
      List.map
        (fun (lit, body) ->
          let tag = Option.get (int_of_lit lit) in
          let l_case = new_block fn_ctx ~span:(Hir.expr_span body) () in
          let v_case, l_case_end = lower_expr ctx fn_ctx l_case.label body in
          if not (is_terminated fn_ctx l_case_end) then (
            set_term fn_ctx l_case_end (TJump l_join.label);
            incoming := (l_case_end, v_case) :: !incoming);
          (tag, l_case.label))
        cases
    in
    (match default_opt with
    | Some def ->
        let v_def, l_def_end = lower_expr ctx fn_ctx l_default.label def in
        if not (is_terminated fn_ctx l_def_end) then (
          set_term fn_ctx l_def_end (TJump l_join.label);
          incoming := (l_def_end, v_def) :: !incoming)
    | None ->
        set_term fn_ctx l_default.label (THalt None));
    set_term fn_ctx lbl (TSwitch (vs, case_tags_labels, l_default.label));
    add_phi fn_ctx l_join.label dst (List.rev !incoming);
    (dst, l_join.label))
  else
    (* Fall back to sequential equality tests for strings / floats *)
    lower_lit_cascade ctx fn_ctx lbl vs cases default_opt sp

and lower_switch_lit_tail ctx fn_ctx lbl vs cases default_opt _sp : unit =
  let all_ints = List.for_all (fun (l, _) -> int_of_lit l <> None) cases in
  if all_ints then (
    let l_default = new_block fn_ctx () in
    let case_tags_labels =
      List.map
        (fun (lit, body) ->
          let tag = Option.get (int_of_lit lit) in
          let l_case = new_block fn_ctx ~span:(Hir.expr_span body) () in
          lower_tail ctx fn_ctx l_case.label body;
          (tag, l_case.label))
        cases
    in
    (match default_opt with
    | Some def -> lower_tail ctx fn_ctx l_default.label def
    | None -> set_term fn_ctx l_default.label (THalt None));
    set_term fn_ctx lbl (TSwitch (vs, case_tags_labels, l_default.label)))
  else
    let v, lbl' = lower_lit_cascade ctx fn_ctx lbl vs cases default_opt _sp in
    if not (is_terminated fn_ctx lbl') then set_term fn_ctx lbl' (TRet (Some v))

and lower_lit_cascade ctx fn_ctx lbl vs cases default_opt sp : vreg * label =
  match cases with
  | [] -> (
      match default_opt with
      | Some def -> lower_expr ctx fn_ctx lbl def
      | None ->
          set_term fn_ctx lbl (THalt None);
          let l_unreach = new_block fn_ctx () in
          (fresh_vreg fn_ctx, l_unreach.label))
  | (lit, body) :: rest ->
      let v_lit = fresh_vreg fn_ctx in
      emit fn_ctx lbl (IConst (v_lit, mir_const_of_lit lit));
      let v_cond = fresh_vreg fn_ctx in
      emit fn_ctx lbl (IBinop (v_cond, Eq, vs, v_lit));
      let l_then = new_block fn_ctx ~span:(Hir.expr_span body) () in
      let l_else = new_block fn_ctx ~span:sp () in
      let l_join = new_block fn_ctx ~span:sp () in
      set_term fn_ctx lbl (TBranch (v_cond, l_then.label, l_else.label));
      let v_t, lt_end = lower_expr ctx fn_ctx l_then.label body in
      if not (is_terminated fn_ctx lt_end) then set_term fn_ctx lt_end (TJump l_join.label);
      let v_f, lf_end = lower_lit_cascade ctx fn_ctx l_else.label vs rest default_opt sp in
      if not (is_terminated fn_ctx lf_end) then set_term fn_ctx lf_end (TJump l_join.label);
      let dst = fresh_vreg fn_ctx in
      add_phi fn_ctx l_join.label dst [ (lt_end, v_t); (lf_end, v_f) ];
      (dst, l_join.label)

and lower_closure ctx enclosing_fn_ctx lbl dst params body sp =
  let fvs = Hir.free_vars body in
  let bound_params = List.fold_left (fun s p -> Ident.Set.add p s) Ident.Set.empty params in
  let captured = Ident.Set.diff fvs bound_params |> Ident.Set.elements in
  let captured_vregs_in_caller =
    List.map
      (fun id ->
        match Hashtbl.find_opt enclosing_fn_ctx.vars id with
        | Some v -> v
        | None ->
            failwith
              (Printf.sprintf "Mir_lower: capture of unbound variable '%s' in closure"
                 (Ident.name id)))
      captured
  in
  let clo_id = alloc_fn_id ctx in
  let clo_name = Ident.fresh (Printf.sprintf "%s_clo%d" (Ident.name enclosing_fn_ctx.fn_name) clo_id) in
  let clo_ctx = create_fn_ctx ~fn_id:clo_id ~name:clo_name ~span:sp in
  let entry_block = new_block clo_ctx ~span:sp () in
  let cap_params =
    List.map
      (fun id ->
        let v = fresh_vreg clo_ctx in
        Hashtbl.replace clo_ctx.vars id v;
        v)
      captured
  in
  let arg_params =
    List.map
      (fun p ->
        let v = fresh_vreg clo_ctx in
        Hashtbl.replace clo_ctx.vars p v;
        v)
      params
  in
  lower_tail ctx clo_ctx entry_block.label body;
  finish_fn_ctx clo_ctx ~params:(cap_params @ arg_params) ~is_main:false;
  ctx.extra_funcs <- build_func clo_ctx :: ctx.extra_funcs;
  emit enclosing_fn_ctx lbl (IMakeClosure (dst, clo_id, captured_vregs_in_caller))

and lower_let_rec ctx fn_ctx lbl bindings body _sp =
  List.iter
    (fun (name, rhs) ->
      match rhs with
      | Hir.Fun (params, f_body, sp) ->
          let dst = fresh_vreg fn_ctx in
          Hashtbl.replace fn_ctx.vars name dst;
          lower_closure ctx fn_ctx lbl dst params f_body sp
      | _ ->
          let vr, _ = lower_expr ctx fn_ctx lbl rhs in
          Hashtbl.replace fn_ctx.vars name vr)
    bindings;
  lower_expr ctx fn_ctx lbl body

and finish_fn_ctx fn_ctx ~params ~is_main =
  List.iter
    (fun (b : block_builder) ->
      if b.term = None then (
        let unit_reg = fresh_vreg fn_ctx in
        b.instrs <- IConst (unit_reg, CUnit) :: b.instrs;
        b.term <- Some (TRet (Some unit_reg))))
    fn_ctx.blocks;
  ignore (params, is_main)

and build_func fn_ctx =
  let blocks =
    List.rev_map
      (fun (b : block_builder) ->
        let phis =
          List.map (fun (d, incoming) -> IPhi (d, incoming)) b.phis
        in
        let instrs = List.rev b.instrs in
        let term =
          match b.term with
          | Some t -> t
          | None -> TRet None
        in
        make_block ~phis ~span:b.span b.label instrs term)
      fn_ctx.blocks
  in
  let entry =
    match blocks with
    | b :: _ -> b.label
    | [] -> 0
  in
  make_func ~id:fn_ctx.fn_id ~name:fn_ctx.fn_name
    ~params:(Hashtbl.fold (fun _ v acc -> v :: acc) fn_ctx.vars [] |> List.sort_uniq Int.compare)
    ~blocks ~entry ~n_vregs:fn_ctx.next_vreg ~span:fn_ctx.span ()

let lower_function ctx (tf : Hir.toplevel) : func =
  match tf with
  | Hir.Toplevel_fun { name; params; body; span; _ } ->
      let gfn = Hashtbl.find ctx.global_fns name in
      let fn_ctx = create_fn_ctx ~fn_id:gfn.fn_id ~name ~span in
      let entry_block = new_block fn_ctx ~span () in
      let param_vregs =
        List.map
          (fun p ->
            let v = fresh_vreg fn_ctx in
            Hashtbl.replace fn_ctx.vars p v;
            v)
          params
      in
      lower_tail ctx fn_ctx entry_block.label body;
      finish_fn_ctx fn_ctx ~params:param_vregs ~is_main:false;
      let blocks =
        List.rev_map
          (fun (b : block_builder) ->
            let phis =
              List.map (fun (d, incoming) -> IPhi (d, incoming)) b.phis
            in
            let instrs = List.rev b.instrs in
            let term = Option.value b.term ~default:(TRet None) in
            make_block ~phis ~span:b.span b.label instrs term)
          fn_ctx.blocks
      in
      make_func ~id:fn_ctx.fn_id ~name:fn_ctx.fn_name ~params:param_vregs
        ~blocks ~entry:entry_block.label ~n_vregs:fn_ctx.next_vreg ~span:fn_ctx.span ()
  | _ -> failwith "Mir_lower: expected Toplevel_fun"

let lower_program (prog : Hir.program) : Mir.program =
  let ctx = create_ctx () in
  (* Register standard builtins in globals *)
  List.iter
    (fun name -> ignore (intern_global ctx (Ident.of_string name)))
    [ "print_int"; "print_string"; "print_bool"; "print_float"; "print_char"; "print";
      "string_of_int"; "int_of_string"; "string_concat"; "string_length"; "abort"; "exit" ];

  (* Pass 1: register all functions and global values *)
  List.iter
    (function
      | Hir.Toplevel_fun { name; params; _ } ->
          let fn_id = alloc_fn_id ctx in
          Hashtbl.replace ctx.global_fns name
            { fn_id; arity = List.length params }
      | Hir.Toplevel_val { name; _ } ->
          ignore (intern_global ctx name)
      | _ -> ())
    prog.items;

  (* Pass 2: lower all top-level functions *)
  let funcs = ref [] in
  List.iter
    (function
      | Hir.Toplevel_fun _ as tf ->
          let f = lower_function ctx tf in
          funcs := f :: !funcs
      | _ -> ())
    prog.items;

  (* Pass 3: synthesize main function evaluating top-level vals in sequence *)
  let main_id = alloc_fn_id ctx in
  let main_name = Ident.of_string "main" in
  let main_ctx = create_fn_ctx ~fn_id:main_id ~name:main_name ~span:prog.span in
  let main_entry = new_block main_ctx ~span:prog.span () in
  let cur_lbl = ref main_entry.label in
  let exit_val = ref None in

  List.iter
    (function
      | Hir.Toplevel_val { name; body; _ } ->
          let v, next_lbl = lower_expr ctx main_ctx !cur_lbl body in
          cur_lbl := next_lbl;
          let g_idx = Hashtbl.find ctx.globals name in
          emit main_ctx !cur_lbl (IStoreGlobal (g_idx, v));
          if Ident.name name = "main" then exit_val := Some v
      | _ -> ())
    prog.items;

  (* If there's a Toplevel_fun named "main", call it *)
  (match !exit_val with
  | Some v ->
      if not (is_terminated main_ctx !cur_lbl) then
        set_term main_ctx !cur_lbl (TRet (Some v))
  | None -> (
      let main_ident = Ident.of_string "main" in
      match Hashtbl.find_opt ctx.global_fns main_ident with
      | Some gfn when gfn.arity = 0 ->
          let r = fresh_vreg main_ctx in
          emit main_ctx !cur_lbl (ICall (r, gfn.fn_id, []));
          set_term main_ctx !cur_lbl (TRet (Some r))
      | _ ->
          let u = fresh_vreg main_ctx in
          emit main_ctx !cur_lbl (IConst (u, CUnit));
          set_term main_ctx !cur_lbl (TRet (Some u))));

  finish_fn_ctx main_ctx ~params:[] ~is_main:true;
  let main_blocks =
    List.rev_map
      (fun (b : block_builder) ->
        let phis =
          List.map (fun (d, incoming) -> IPhi (d, incoming)) b.phis
        in
        let instrs = List.rev b.instrs in
        let term = Option.value b.term ~default:(TRet None) in
        make_block ~phis ~span:b.span b.label instrs term)
      main_ctx.blocks
  in
  let main_func =
    make_func ~id:main_id ~name:main_name ~params:[]
      ~blocks:main_blocks ~entry:main_entry.label ~n_vregs:main_ctx.next_vreg
      ~is_main:true ~span:prog.span ()
  in
  let all_functions = List.rev (!funcs @ ctx.extra_funcs @ [ main_func ]) in
  make_program ~string_table:ctx.string_list ~globals:ctx.global_names
    ~functions:all_functions ~main:main_id ()

let lower_expr_standalone (e : Hir.expr) : Mir.program =
  let item =
    Hir.Toplevel_val
      {
        name = Ident.of_string "main";
        body = e;
        span = Hir.expr_span e;
      }
  in
  lower_program { items = [ item ]; span = Hir.expr_span e }
