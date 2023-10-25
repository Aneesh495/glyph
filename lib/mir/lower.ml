(** Lower [Hir] to non-SSA [Mir], then convert to SSA. *)

open Hir

type env = (Ident.t, Mir.vreg) Hashtbl.t

type builder = {
  mutable blocks : Mir.block list;
  mutable current_label : Mir.label;
  mutable current_instrs : Mir.instr list;
  mutable current_span : Span.t;
  mutable next_label : int;
  mutable next_vreg : int;
  fn_table : (Ident.t, Mir.fn_id) Hashtbl.t;
  mutable string_table : string list;
  env : env;
}

let create_builder () =
  {
    blocks = [];
    current_label = 0;
    current_instrs = [];
    current_span = Span.dummy;
    next_label = 1;
    next_vreg = 0;
    fn_table = Hashtbl.create 32;
    string_table = [];
    env = Hashtbl.create 64;
  }

let fresh_vreg b =
  let v = b.next_vreg in
  b.next_vreg <- v + 1;
  v

let fresh_label b =
  let l = b.next_label in
  b.next_label <- l + 1;
  l

let bind b id v = Hashtbl.replace b.env id v

let lookup b id =
  match Hashtbl.find_opt b.env id with
  | Some v -> v
  | None ->
      let v = fresh_vreg b in
      bind b id v;
      v

let emit b instr = b.current_instrs <- b.current_instrs @ [ instr ]

let start_block b label span =
  b.current_label <- label;
  b.current_instrs <- [];
  b.current_span <- span

let end_block b term =
  let blk =
    Mir.make_block b.current_label b.current_instrs term ~span:b.current_span
  in
  b.blocks <- b.blocks @ [ blk ]

let lit_const = function
  | Lit_unit -> Mir.CUnit
  | Lit_bool x -> Mir.CBool x
  | Lit_int n -> Mir.CInt n
  | Lit_float f -> Mir.CFloat f
  | Lit_string s -> Mir.CString s
  | Lit_char c -> Mir.CChar c

let intern_string b s =
  let rec find i = function
    | [] -> None
    | x :: xs -> if String.equal x s then Some i else find (i + 1) xs
  in
  match find 0 b.string_table with
  | Some i -> i
  | None ->
      b.string_table <- b.string_table @ [ s ];
      List.length b.string_table - 1

let lower_atom b = function
  | Atom_var id -> lookup b id
  | Atom_lit lit ->
      let v = fresh_vreg b in
      emit b (Mir.IConst (v, lit_const lit));
      (match lit with Lit_string s -> ignore (intern_string b s) | _ -> ());
      v

let prim_binop = function
  | Prim_add -> Some Mir.Add
  | Prim_sub -> Some Mir.Sub
  | Prim_mul -> Some Mir.Mul
  | Prim_div -> Some Mir.Div
  | Prim_mod -> Some Mir.Mod
  | Prim_eq -> Some Mir.Eq
  | Prim_ne -> Some Mir.Ne
  | Prim_lt -> Some Mir.Lt
  | Prim_le -> Some Mir.Le
  | Prim_gt -> Some Mir.Gt
  | Prim_ge -> Some Mir.Ge
  | Prim_and -> Some Mir.And
  | Prim_or -> Some Mir.Or
  | Prim_fadd -> Some Mir.AddF
  | Prim_fsub -> Some Mir.SubF
  | Prim_fmul -> Some Mir.MulF
  | Prim_fdiv -> Some Mir.DivF
  | _ -> None

let prim_unop = function
  | Prim_neg -> Some Mir.Neg
  | Prim_not -> Some Mir.Not
  | _ -> None

let rec lower_expr b (e : expr) : Mir.vreg =
  let sp = expr_span e in
  b.current_span <- sp;
  match e with
  | Atom (a, _) -> lower_atom b a
  | Prim (p, args, _) ->
      let vs = List.map (lower_atom b) args in
      let dst = fresh_vreg b in
      (match (prim_binop p, vs) with
      | Some op, [ x; y ] ->
          emit b (Mir.IBinop (dst, op, x, y));
          dst
      | _ -> (
          match (prim_unop p, vs) with
          | Some op, [ x ] ->
              emit b (Mir.IUnop (dst, op, x));
              dst
          | _ -> (
              match (p, vs) with
              | (Prim_print | Prim_print_int | Prim_print_bool), [ x ] ->
                  emit b (Mir.IPrint x);
                  emit b (Mir.IConst (dst, Mir.CUnit));
                  dst
              | Prim_abort, _ ->
                  emit b (Mir.IConst (dst, Mir.CUnit));
                  end_block b (Mir.THalt (Some dst));
                  start_block b (fresh_label b) sp;
                  dst
              | Prim_tag_of, [ x ] ->
                  emit b (Mir.IGetField (dst, x, -1));
                  dst
              | Prim_string_concat, [ x; y ] ->
                  emit b (Mir.IBinop (dst, Mir.Add, x, y));
                  dst
              | _ ->
                  emit b (Mir.IConst (dst, Mir.CUnit));
                  dst)))
  | App (f, args, _) ->
      let fv = lower_atom b f in
      let avs = List.map (lower_atom b) args in
      let dst = fresh_vreg b in
      (match f with
      | Atom_var id when Hashtbl.mem b.fn_table id ->
          emit b (Mir.ICall (dst, Hashtbl.find b.fn_table id, avs))
      | _ -> emit b (Mir.ICallClosure (dst, fv, avs)));
      dst
  | Let (x, rhs, body, _) ->
      let v = lower_expr b rhs in
      bind b x v;
      lower_expr b body
  | Let_rec (bindings, body, _) ->
      List.iter
        (fun (name, _) -> bind b name (fresh_vreg b))
        bindings;
      List.iter
        (fun (name, rhs) ->
          let v = lower_expr b rhs in
          emit b (Mir.IMove (lookup b name, v)))
        bindings;
      lower_expr b body
  | Fun (_params, _body, _) ->
      let dst = fresh_vreg b in
      emit b (Mir.IMakeClosure (dst, -1, []));
      dst
  | If (cond, thn, els, sp) ->
      let cv = lower_atom b cond in
      let then_l = fresh_label b in
      let else_l = fresh_label b in
      let join_l = fresh_label b in
      let result = fresh_vreg b in
      end_block b (Mir.TBranch (cv, then_l, else_l));
      start_block b then_l sp;
      emit b (Mir.IMove (result, lower_expr b thn));
      end_block b (Mir.TJump join_l);
      start_block b else_l sp;
      emit b (Mir.IMove (result, lower_expr b els));
      end_block b (Mir.TJump join_l);
      start_block b join_l sp;
      result
  | Seq (a, body, _) ->
      ignore (lower_expr b a);
      lower_expr b body
  | Ctor (c, args, _) ->
      let dst = fresh_vreg b in
      emit b (Mir.IAlloc (dst, c.ctor_tag, List.map (lower_atom b) args));
      dst
  | Tuple (args, _) ->
      let dst = fresh_vreg b in
      emit b (Mir.IAlloc (dst, 0, List.map (lower_atom b) args));
      dst
  | Project (a, i, _) ->
      let dst = fresh_vreg b in
      emit b (Mir.IGetField (dst, lower_atom b a, i));
      dst
  | Raise (a, _) ->
      let v = lower_atom b a in
      end_block b (Mir.THalt (Some v));
      start_block b (fresh_label b) sp;
      v
  | Fail_match _ ->
      let dst = fresh_vreg b in
      emit b (Mir.IConst (dst, Mir.CInt 0));
      end_block b (Mir.THalt (Some dst));
      start_block b (fresh_label b) sp;
      dst
  | Match _ ->
      let dst = fresh_vreg b in
      emit b (Mir.IConst (dst, Mir.CUnit));
      dst
  | Switch_ctor (scrut, cases, default, sp) ->
      let sv = lower_atom b scrut in
      let tagv = fresh_vreg b in
      emit b (Mir.IGetField (tagv, sv, -1));
      let join_l = fresh_label b in
      let default_l = fresh_label b in
      let case_info =
        List.map
          (fun (c, binds, body) ->
            (c.ctor_tag, fresh_label b, binds, body))
          cases
      in
      end_block b
        (Mir.TSwitch
           ( tagv,
             List.map (fun (tag, lbl, _, _) -> (tag, lbl)) case_info,
             default_l ));
      let result = fresh_vreg b in
      List.iter
        (fun (_tag, lbl, binds, body) ->
          start_block b lbl sp;
          List.iteri
            (fun i name ->
              let v = fresh_vreg b in
              emit b (Mir.IGetField (v, sv, i));
              bind b name v)
            binds;
          emit b (Mir.IMove (result, lower_expr b body));
          end_block b (Mir.TJump join_l))
        case_info;
      start_block b default_l sp;
      (match default with
      | None ->
          let z = fresh_vreg b in
          emit b (Mir.IConst (z, Mir.CInt 0));
          emit b (Mir.IMove (result, z));
          end_block b (Mir.THalt (Some z))
      | Some d ->
          emit b (Mir.IMove (result, lower_expr b d));
          end_block b (Mir.TJump join_l));
      start_block b join_l sp;
      result
  | Switch_lit (scrut, cases, default, sp) ->
      let sv = lower_atom b scrut in
      let join_l = fresh_label b in
      let result = fresh_vreg b in
      let rec emit_cases = function
        | [] -> (
            match default with
            | None ->
                let z = fresh_vreg b in
                emit b (Mir.IConst (z, Mir.CInt 0));
                emit b (Mir.IMove (result, z));
                end_block b (Mir.THalt (Some z))
            | Some d ->
                emit b (Mir.IMove (result, lower_expr b d));
                end_block b (Mir.TJump join_l))
        | (lit, body) :: rest ->
            let lit_v = fresh_vreg b in
            emit b (Mir.IConst (lit_v, lit_const lit));
            let cmp = fresh_vreg b in
            emit b (Mir.IBinop (cmp, Mir.Eq, sv, lit_v));
            let then_l = fresh_label b in
            let else_l = fresh_label b in
            end_block b (Mir.TBranch (cmp, then_l, else_l));
            start_block b then_l sp;
            emit b (Mir.IMove (result, lower_expr b body));
            end_block b (Mir.TJump join_l);
            start_block b else_l sp;
            emit_cases rest
      in
      emit_cases cases;
      start_block b join_l sp;
      result

let lower_function ~id ~name ~params ~body ~span ~fn_table ~is_main :
    Mir.func * string list =
  let b = create_builder () in
  Hashtbl.iter (fun k v -> Hashtbl.replace b.fn_table k v) fn_table;
  start_block b 0 span;
  let param_vregs =
    List.map
      (fun p ->
        let v = fresh_vreg b in
        bind b p v;
        v)
      params
  in
  let ret = lower_expr b body in
  end_block b (Mir.TRet (Some ret));
  let fn =
    Mir.make_func ~id ~name ~params:param_vregs ~blocks:b.blocks ~entry:0
      ~n_vregs:b.next_vreg ~is_main ~span ()
  in
  (fn, b.string_table)

let collect_fn_table items =
  let fn_table = Hashtbl.create 32 in
  let next_id = ref 0 in
  List.iter
    (fun item ->
      match item with
      | Toplevel_fun { name; _ }
      | Toplevel_val { name; _ }
      | Toplevel_extern { name; _ } ->
          if not (Hashtbl.mem fn_table name) then (
            Hashtbl.replace fn_table name !next_id;
            incr next_id)
      | Toplevel_type _ -> ())
    items;
  fn_table

let lower_program ?(ssa = true) (prog : program) : Mir.program =
  let fn_table = collect_fn_table prog.items in
  let functions = ref [] in
  let strings = ref [] in
  let main_id = ref 0 in
  List.iter
    (fun item ->
      match item with
      | Toplevel_fun { name; params; body; span; _ } ->
          let id = Hashtbl.find fn_table name in
          let is_main = Ident.equal name Ident.Predef.main in
          if is_main then main_id := id;
          let fn, strs =
            lower_function ~id ~name ~params ~body ~span ~fn_table ~is_main
          in
          let fn = if ssa then Ssa.convert fn else fn in
          functions := fn :: !functions;
          strings := !strings @ strs
      | Toplevel_val { name; body; span } ->
          let id = Hashtbl.find fn_table name in
          let is_main = Ident.equal name Ident.Predef.main in
          if is_main then main_id := id;
          let fn, strs =
            lower_function ~id ~name ~params:[] ~body ~span ~fn_table ~is_main
          in
          let fn = if ssa then Ssa.convert fn else fn in
          functions := fn :: !functions;
          strings := !strings @ strs
      | Toplevel_extern { name; arity; span } ->
          let id = Hashtbl.find fn_table name in
          let params = List.init arity Fun.id in
          let fn =
            Mir.make_func ~id ~name ~params
              ~blocks:[ Mir.make_block 0 [] (Mir.TRet None) ~span ]
              ~entry:0 ~n_vregs:arity ~span ()
          in
          functions := fn :: !functions
      | Toplevel_type _ -> ())
    prog.items;
  Mir.make_program ~string_table:!strings
    ~functions:(List.rev !functions) ~main:!main_id

let lower_program_non_ssa prog = lower_program ~ssa:false prog
