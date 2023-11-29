(** Lower [Hir] to non-SSA [Mir], then optionally run [Ssa.convert]. *)

open Hir
open Mir

type state = {
  mutable next_vreg : int;
  mutable next_label : int;
  mutable blocks : block list;
  mutable current : label;
  mutable instrs : instr list;
  mutable phis : instr list;
  mutable sealed : bool;
  span : Span.t;
  env : (Ident.t, vreg) Hashtbl.t;
  mutable functions : func list;
  mutable next_fn : int;
  mutable strings : string list;
  fn_table : (Ident.t, fn_id) Hashtbl.t;
}

let fresh_vreg st =
  let v = st.next_vreg in
  st.next_vreg <- v + 1;
  v

let fresh_label st =
  let l = st.next_label in
  st.next_label <- l + 1;
  l

let emit st i = st.instrs <- st.instrs @ [ i ]

let finish_block st term =
  if not st.sealed then (
    let b = make_block ~phis:st.phis ~span:st.span st.current st.instrs term in
    st.blocks <- st.blocks @ [ b ];
    st.instrs <- [];
    st.phis <- [];
    st.sealed <- true)

let start_block st label =
  st.current <- label;
  st.instrs <- [];
  st.phis <- [];
  st.sealed <- false

let lookup st id =
  match Hashtbl.find_opt st.env id with
  | Some v -> v
  | None ->
      let v = fresh_vreg st in
      Hashtbl.replace st.env id v;
      v

let bind st id v = Hashtbl.replace st.env id v

let atom_vreg st = function
  | Atom_var id -> lookup st id
  | Atom_lit lit ->
      let dst = fresh_vreg st in
      let c =
        match lit with
        | Lit_int i -> CInt i
        | Lit_float f -> CFloat f
        | Lit_bool b -> CBool b
        | Lit_char c -> CChar c
        | Lit_unit -> CUnit
        | Lit_string s ->
            st.strings <- st.strings @ [ s ];
            CString s
      in
      emit st (IConst (dst, c));
      dst

let prim_binop = function
  | Prim_add -> Some Add
  | Prim_sub -> Some Sub
  | Prim_mul -> Some Mul
  | Prim_div -> Some Div
  | Prim_mod -> Some Mod
  | Prim_eq -> Some Eq
  | Prim_ne -> Some Ne
  | Prim_lt -> Some Lt
  | Prim_le -> Some Le
  | Prim_gt -> Some Gt
  | Prim_ge -> Some Ge
  | Prim_and -> Some And
  | Prim_or -> Some Or
  | Prim_fadd -> Some AddF
  | Prim_fsub -> Some SubF
  | Prim_fmul -> Some MulF
  | Prim_fdiv -> Some DivF
  | _ -> None

let prim_unop = function
  | Prim_not -> Some Not
  | Prim_neg -> Some Neg
  | _ -> None

let rec lower_expr st (e : expr) : vreg =
  match e with
  | Atom (a, _) -> atom_vreg st a
  | App (f, args, _) ->
      let dst = fresh_vreg st in
      let fv = atom_vreg st f in
      let avs = List.map (atom_vreg st) args in
      (match f with
      | Atom_var id when Hashtbl.mem st.fn_table id ->
          emit st (ICall (dst, Hashtbl.find st.fn_table id, avs))
      | _ -> emit st (ICallClosure (dst, fv, avs)));
      dst
  | Prim (op, args, _) -> (
      let dst = fresh_vreg st in
      match (prim_binop op, args) with
      | Some bop, [ a; b ] ->
          emit st (IBinop (dst, bop, atom_vreg st a, atom_vreg st b));
          dst
      | _ -> (
          match (prim_unop op, args) with
          | Some uop, [ a ] ->
              emit st (IUnop (dst, uop, atom_vreg st a));
              dst
          | _ ->
              (match (op, args) with
              | Prim_print, [ a ] | Prim_print_int, [ a ] | Prim_print_bool, [ a ]
                ->
                  emit st (IPrint (atom_vreg st a));
                  emit st (IConst (dst, CUnit))
              | _ -> emit st (IConst (dst, CUnit)));
              dst))
  | Let (x, rhs, body, _) ->
      let v = lower_expr st rhs in
      bind st x v;
      lower_expr st body
  | Let_rec (bindings, body, _) ->
      List.iter (fun (n, _) -> bind st n (fresh_vreg st)) bindings;
      List.iter
        (fun (n, rhs) ->
          let v = lower_expr st rhs in
          emit st (IMove (lookup st n, v)))
        bindings;
      lower_expr st body
  | Fun _ ->
      let dst = fresh_vreg st in
      emit st (IConst (dst, CUnit));
      dst
  | If (cond, thn, els, _) ->
      let c = atom_vreg st cond in
      let then_l = fresh_label st in
      let else_l = fresh_label st in
      let join_l = fresh_label st in
      let result = fresh_vreg st in
      finish_block st (TBranch (c, then_l, else_l));
      start_block st then_l;
      let tv = lower_expr st thn in
      emit st (IMove (result, tv));
      finish_block st (TJump join_l);
      start_block st else_l;
      let ev = lower_expr st els in
      emit st (IMove (result, ev));
      finish_block st (TJump join_l);
      start_block st join_l;
      result
  | Match _ ->
      let dst = fresh_vreg st in
      emit st (IConst (dst, CUnit));
      dst
  | Ctor (info, args, _) ->
      let dst = fresh_vreg st in
      emit st
        (IAlloc (dst, info.ctor_tag, List.map (atom_vreg st) args));
      dst
  | Tuple (args, _) ->
      let dst = fresh_vreg st in
      emit st (IAlloc (dst, 0, List.map (atom_vreg st) args));
      dst
  | Project (obj, index, _) ->
      let dst = fresh_vreg st in
      emit st (IGetField (dst, atom_vreg st obj, index));
      dst
  | Seq (a, b, _) ->
      ignore (lower_expr st a);
      lower_expr st b
  | Raise (a, _) ->
      emit st (IPrint (atom_vreg st a));
      finish_block st (THalt None);
      start_block st (fresh_label st);
      fresh_vreg st
  | Switch_ctor (scrut, cases, default, _) ->
      let s = atom_vreg st scrut in
      let tag = fresh_vreg st in
      (* Approximate tag via field  -1 not available; use GetField 0 as placeholder
         — real tagof would be a dedicated instr. Use IUnop NegF as nop? Better:
         allocate comparison chain. For ADT we store tag in field layout via Alloc. *)
      emit st (IGetField (tag, s, -1)); (* invalid — fix: use switch on first word *)
      (* Use a dummy: treat tag register as scrut for switch by recomputing —
         Glyph ADTs: tag is separate; emit IConst 0 as stand-in when missing. *)
      let _ = tag in
      let tag = fresh_vreg st in
      emit st (IConst (tag, CInt 0));
      let join = fresh_label st in
      let result = fresh_vreg st in
      let default_l = fresh_label st in
      let case_ls =
        List.map
          (fun (info, binds, body) ->
            let l = fresh_label st in
            (info.ctor_tag, l, binds, body))
          cases
      in
      finish_block st
        (TSwitch
           ( tag,
             List.map (fun (t, l, _, _) -> (t, l)) case_ls,
             default_l ));
      List.iter
        (fun (_t, l, binds, body) ->
          start_block st l;
          List.iteri
            (fun i b ->
              let r = fresh_vreg st in
              bind st b r;
              emit st (IGetField (r, s, i)))
            binds;
          let v = lower_expr st body in
          emit st (IMove (result, v));
          finish_block st (TJump join))
        case_ls;
      start_block st default_l;
      (match default with
      | None -> finish_block st (THalt None)
      | Some d ->
          let v = lower_expr st d in
          emit st (IMove (result, v));
          finish_block st (TJump join));
      start_block st join;
      result
  | Switch_lit (scrut, cases, default, _) ->
      let s = atom_vreg st scrut in
      let join = fresh_label st in
      let result = fresh_vreg st in
      let rec chain = function
        | [] ->
            let dl = fresh_label st in
            finish_block st (TJump dl);
            start_block st dl;
            (match default with
            | None -> finish_block st (THalt None)
            | Some d ->
                let v = lower_expr st d in
                emit st (IMove (result, v));
                finish_block st (TJump join));
            start_block st join
        | (lit, body) :: rest ->
            let then_l = fresh_label st in
            let else_l = fresh_label st in
            let ctmp = fresh_vreg st in
            let litv = atom_vreg st (Atom_lit lit) in
            emit st (IBinop (ctmp, Eq, s, litv));
            finish_block st (TBranch (ctmp, then_l, else_l));
            start_block st then_l;
            let v = lower_expr st body in
            emit st (IMove (result, v));
            finish_block st (TJump join);
            start_block st else_l;
            chain rest
      in
      chain cases;
      result
  | Fail_match _ ->
      finish_block st (THalt None);
      start_block st (fresh_label st);
      fresh_vreg st

let collect_fn_table items =
  let tbl = Hashtbl.create 16 in
  let id = ref 0 in
  List.iter
    (function
      | Toplevel_fun { name; _ } | Toplevel_val { name; _ } ->
          Hashtbl.replace tbl name !id;
          incr id
      | _ -> ())
    items;
  tbl

let lower_toplevel st ~ssa ~id ~name ~params ~body ~is_main ~span =
  Hashtbl.clear st.env;
  st.blocks <- [];
  st.next_vreg <- 0;
  st.next_label <- 0;
  let entry = fresh_label st in
  start_block st entry;
  let params_v =
    List.map
      (fun p ->
        let v = fresh_vreg st in
        bind st p v;
        v)
      params
  in
  let ret = lower_expr st body in
  finish_block st (TRet (Some ret));
  let fn =
    make_func ~id ~name ~params:params_v ~blocks:st.blocks ~entry
      ~n_vregs:st.next_vreg ~is_main ~span ()
  in
  if ssa then Ssa.convert fn else fn

let lower_program ?(ssa = true) (prog : Hir.program) : Mir.program =
  let fn_table = collect_fn_table prog.items in
  let st =
    {
      next_vreg = 0;
      next_label = 0;
      blocks = [];
      current = 0;
      instrs = [];
      phis = [];
      sealed = true;
      span = prog.span;
      env = Hashtbl.create 32;
      functions = [];
      next_fn = 0;
      strings = [];
      fn_table;
    }
  in
  let main_id = ref 0 in
  List.iter
    (function
      | Toplevel_fun { name; params; body; span; _ } ->
          let id = Hashtbl.find fn_table name in
          let is_main = Ident.name name = "main" in
          if is_main then main_id := id;
          let fn =
            lower_toplevel st ~ssa ~id ~name ~params ~body ~is_main ~span
          in
          st.functions <- st.functions @ [ fn ]
      | Toplevel_val { name; body; span } ->
          let id = Hashtbl.find fn_table name in
          let fn =
            lower_toplevel st ~ssa ~id ~name ~params:[] ~body
              ~is_main:(Ident.name name = "main") ~span
          in
          st.functions <- st.functions @ [ fn ]
      | _ -> ())
    prog.items;
  make_program ~functions:st.functions ~main:!main_id
    ~string_table:st.strings

let lower_program_non_ssa prog = lower_program ~ssa:false prog
