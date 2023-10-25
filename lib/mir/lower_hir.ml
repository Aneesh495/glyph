(** Lower [Hir] expressions into non-SSA [Mir] CFGs, then optionally construct
    SSA. Public entry: [lower]. *)

open Hir

type builder = {
  mutable blocks : Mir.block Mir.Label.Map.t;
  mutable current : Mir.label;
  mutable sealed : bool;
  entry : Mir.label;
  mutable temps : int;
  span : Span.t;
  globals : (Ident.t, unit) Hashtbl.t;
  mutable nested_funs : Mir.func list;
}

let fresh_label ?(prefix = "bb") () = Mir.Label.fresh prefix
let fresh_vreg ?(prefix = "t") () = Mir.Vreg.fresh prefix

let make_builder ?(span = Span.dummy) () =
  let entry = fresh_label ~prefix:"entry" () in
  let b =
    {
      blocks = Mir.Label.Map.empty;
      current = entry;
      sealed = false;
      entry;
      temps = 0;
      span;
      globals = Hashtbl.create 16;
      nested_funs = [];
    }
  in
  let blk =
    Mir.make_block entry (Mir.Unreachable Span.dummy)
  in
  b.blocks <- Mir.Label.Map.add entry blk b.blocks;
  b

let current_block (b : builder) =
  Mir.Label.Map.find b.current b.blocks

let set_terminator (b : builder) term =
  let blk = current_block b in
  blk.Mir.terminator <- term

let emit (b : builder) (instr : Mir.instr) =
  let blk = current_block b in
  blk.Mir.instrs <- blk.Mir.instrs @ [ instr ]

let start_block (b : builder) (label : Mir.label) =
  let blk = Mir.make_block label (Mir.Unreachable Span.dummy) in
  b.blocks <- Mir.Label.Map.add label blk b.blocks;
  b.current <- label;
  blk

let atom_value (a : atom) : Mir.value =
  match a with
  | Atom_var v -> Mir.VReg v
  | Atom_lit Lit_unit -> Mir.VConst Mir.CUnit
  | Atom_lit (Lit_bool x) -> Mir.VConst (Mir.CBool x)
  | Atom_lit (Lit_int n) -> Mir.VConst (Mir.CInt n)
  | Atom_lit (Lit_float f) -> Mir.VConst (Mir.CFloat f)
  | Atom_lit (Lit_string s) -> Mir.VConst (Mir.CString s)
  | Atom_lit (Lit_char c) -> Mir.VConst (Mir.CChar c)

let lit_const = function
  | Lit_unit -> Mir.CUnit
  | Lit_bool b -> Mir.CBool b
  | Lit_int n -> Mir.CInt n
  | Lit_float f -> Mir.CFloat f
  | Lit_string s -> Mir.CString s
  | Lit_char c -> Mir.CChar c

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
  | Prim_fadd -> Some Mir.FAdd
  | Prim_fsub -> Some Mir.FSub
  | Prim_fmul -> Some Mir.FMul
  | Prim_fdiv -> Some Mir.FDiv
  | _ -> None

let prim_unop = function
  | Prim_neg -> Some Mir.Neg
  | Prim_not -> Some Mir.Not
  | Prim_is_unit -> Some Mir.IsNull
  | Prim_tag_of -> Some Mir.TagOf
  | Prim_box -> Some Mir.Box
  | Prim_unbox -> Some Mir.Unbox
  | _ -> None

let result_ty_of_prim = function
  | Prim_eq | Prim_ne | Prim_lt | Prim_le | Prim_gt | Prim_ge
  | Prim_and | Prim_or | Prim_not | Prim_is_unit ->
      Mir.Ty_bool
  | Prim_fadd | Prim_fsub | Prim_fmul | Prim_fdiv -> Mir.Ty_float
  | Prim_string_concat -> Mir.Ty_string
  | Prim_tag_of -> Mir.Ty_int
  | _ -> Mir.Ty_int

(** Bind [value] to a fresh vreg if needed, returning the vreg name. *)
let materialize (b : builder) ?(ty = Mir.Ty_any) ?(span = Span.dummy) v =
  match v with
  | Mir.VReg r -> r
  | _ ->
      let dst = fresh_vreg () in
      emit b (Mir.Assign { dst; src = v; ty; span });
      dst

let rec lower_expr (b : builder) (e : expr) : Mir.value =
  let sp = expr_span e in
  match e with
  | Atom (a, _) -> atom_value a
  | Prim (p, args, sp) -> lower_prim b p args sp
  | App (f, args, sp) ->
      let dst = fresh_vreg ~prefix:"call" () in
      let callee = atom_value f in
      let argv = List.map atom_value args in
      emit b
        (Mir.Call
           { dst = Some dst; callee; args = argv; ty = Mir.Ty_any; span = sp });
      Mir.VReg dst
  | Let (x, rhs, body, _) ->
      let v = lower_expr b rhs in
      emit b
        (Mir.Assign
           { dst = x; src = v; ty = Mir.Ty_any; span = expr_span rhs });
      lower_expr b body
  | Let_rec (binds, body, sp) ->
      (* Allocate closures / placeholders then assign. *)
      List.iter
        (fun (name, rhs) ->
          match rhs with
          | Fun (params, fbody, fsp) ->
              let nf = lower_nested_fun b ~name ~params ~body:fbody ~span:fsp in
              b.nested_funs <- nf :: b.nested_funs;
              emit b
                (Mir.Assign
                   {
                     dst = name;
                     src = Mir.VGlobal nf.Mir.name;
                     ty = Mir.Ty_fn ([], Mir.Ty_any);
                     span = fsp;
                   })
          | _ ->
              let v = lower_expr b rhs in
              emit b
                (Mir.Assign
                   { dst = name; src = v; ty = Mir.Ty_any; span = sp }))
        binds;
      lower_expr b body
  | Fun (params, body, sp) ->
      let name = Ident.fresh "lambda" in
      let nf = lower_nested_fun b ~name ~params ~body ~span:sp in
      b.nested_funs <- nf :: b.nested_funs;
      let dst = fresh_vreg ~prefix:"clo" () in
      emit b
        (Mir.Alloc
           {
             dst;
             tag = -1;
             (* closure sentinel; emit treats via global *)
             fields = [ Mir.VGlobal nf.Mir.name ];
             ty = Mir.Ty_fn ([], Mir.Ty_any);
             span = sp;
           });
      Mir.VReg dst
  | If (cond, thn, els, sp) ->
      lower_if b (atom_value cond) thn els sp
  | Match _ ->
      (* Should have been compiled away; treat as fail. *)
      emit b
        (Mir.Call
           {
             dst = None;
             callee = Mir.VGlobal (Ident.of_string "abort");
             args = [];
             ty = Mir.Ty_unit;
             span = sp;
           });
      Mir.VConst Mir.CUnit
  | Ctor (c, args, sp) ->
      let dst = fresh_vreg ~prefix:"adt" () in
      emit b
        (Mir.Alloc
           {
             dst;
             tag = c.ctor_tag;
             fields = List.map atom_value args;
             ty = Mir.Ty_adt (Option.value c.ctor_type ~default:c.ctor_name);
             span = sp;
           });
      Mir.VReg dst
  | Tuple (xs, sp) ->
      let dst = fresh_vreg ~prefix:"tup" () in
      emit b
        (Mir.Alloc
           {
             dst;
             tag = -2;
             fields = List.map atom_value xs;
             ty = Mir.Ty_tuple [];
             span = sp;
           });
      Mir.VReg dst
  | Project (a, i, sp) ->
      let dst = fresh_vreg ~prefix:"fld" () in
      emit b
        (Mir.GetField
           {
             dst;
             obj = atom_value a;
             index = i;
             ty = Mir.Ty_any;
             span = sp;
           });
      Mir.VReg dst
  | Seq (a, bdy, _) ->
      ignore (lower_expr b a);
      lower_expr b bdy
  | Raise (a, sp) ->
      emit b
        (Mir.Call
           {
             dst = None;
             callee = Mir.VGlobal (Ident.of_string "raise");
             args = [ atom_value a ];
             ty = Mir.Ty_unit;
             span = sp;
           });
      set_terminator b (Mir.Unreachable sp);
      Mir.VUndef
  | Switch_ctor (scrut, cases, default, sp) ->
      lower_switch_ctor b scrut cases default sp
  | Switch_lit (scrut, cases, default, sp) ->
      lower_switch_lit b scrut cases default sp
  | Fail_match sp ->
      emit b
        (Mir.Call
           {
             dst = None;
             callee = Mir.VGlobal (Ident.of_string "match_fail");
             args = [];
             ty = Mir.Ty_unit;
             span = sp;
           });
      set_terminator b (Mir.Unreachable sp);
      Mir.VUndef

and lower_prim b p args sp =
  let ty = result_ty_of_prim p in
  match (prim_binop p, args) with
  | Some op, [ a; c ] ->
      let dst = fresh_vreg () in
      emit b
        (Mir.Binop
           {
             dst;
             op;
             lhs = atom_value a;
             rhs = atom_value c;
             ty;
             span = sp;
           });
      Mir.VReg dst
  | _ -> (
      match (prim_unop p, args) with
      | Some op, [ a ] ->
          let dst = fresh_vreg () in
          emit b
            (Mir.Unop
               { dst; op; arg = atom_value a; ty; span = sp });
          Mir.VReg dst
      | _ -> (
          match (p, args) with
          | Prim_string_concat, [ a; c ] ->
              let dst = fresh_vreg () in
              emit b
                (Mir.Call
                   {
                     dst = Some dst;
                     callee = Mir.VGlobal (Ident.of_string "string_concat");
                     args = [ atom_value a; atom_value c ];
                     ty = Mir.Ty_string;
                     span = sp;
                   });
              Mir.VReg dst
          | Prim_print, [ a ]
          | Prim_print_int, [ a ]
          | Prim_print_bool, [ a ] ->
              let name =
                match p with
                | Prim_print_int -> "print_int"
                | Prim_print_bool -> "print_bool"
                | _ -> "print"
              in
              emit b
                (Mir.Call
                   {
                     dst = None;
                     callee = Mir.VGlobal (Ident.of_string name);
                     args = [ atom_value a ];
                     ty = Mir.Ty_unit;
                     span = sp;
                   });
              Mir.VConst Mir.CUnit
          | Prim_abort, _ ->
              emit b
                (Mir.Call
                   {
                     dst = None;
                     callee = Mir.VGlobal (Ident.of_string "abort");
                     args = [];
                     ty = Mir.Ty_unit;
                     span = sp;
                   });
              set_terminator b (Mir.Unreachable sp);
              Mir.VUndef
          | _ -> Mir.VConst Mir.CUnit))

and lower_if b cond thn els sp =
  let then_l = fresh_label ~prefix:"then" () in
  let else_l = fresh_label ~prefix:"else" () in
  let join_l = fresh_label ~prefix:"join" () in
  set_terminator b
    (Mir.Branch
       { cond; then_ = then_l; else_ = else_l; span = sp });
  ignore (start_block b then_l);
  let tv = lower_expr b thn in
  let then_end = b.current in
  (match (current_block b).Mir.terminator with
  | Mir.Unreachable _ -> set_terminator b (Mir.Jump (join_l, sp))
  | _ -> ());
  ignore (start_block b else_l);
  let ev = lower_expr b els in
  let else_end = b.current in
  (match (current_block b).Mir.terminator with
  | Mir.Unreachable _ -> set_terminator b (Mir.Jump (join_l, sp))
  | _ -> ());
  ignore (start_block b join_l);
  let dst = fresh_vreg ~prefix:"phi" () in
  (* Non-SSA: assign into shared destination from each arm via moves emitted
     at end of arms — insert assigns before jumps. *)
  (match Mir.Label.Map.find_opt then_end b.blocks with
  | Some blk ->
      blk.Mir.instrs <-
        blk.Mir.instrs
        @ [ Mir.Assign { dst; src = tv; ty = Mir.Ty_any; span = sp } ]
  | None -> ());
  (match Mir.Label.Map.find_opt else_end b.blocks with
  | Some blk ->
      blk.Mir.instrs <-
        blk.Mir.instrs
        @ [ Mir.Assign { dst; src = ev; ty = Mir.Ty_any; span = sp } ]
  | None -> ());
  Mir.VReg dst

and lower_switch_ctor b scrut cases default sp =
  let tag_r = fresh_vreg ~prefix:"tag" () in
  emit b
    (Mir.Unop
       {
         dst = tag_r;
         op = Mir.TagOf;
         arg = atom_value scrut;
         ty = Mir.Ty_int;
         span = sp;
       });
  let join_l = fresh_label ~prefix:"sw_join" () in
  let default_l =
    match default with
    | None -> fresh_label ~prefix:"sw_fail" ()
    | Some _ -> fresh_label ~prefix:"sw_def" ()
  in
  let case_labels =
    List.map
      (fun (c, binds, body) ->
        let l = fresh_label ~prefix:("case_" ^ Ident.name c.ctor_name) () in
        (c, binds, body, l))
      cases
  in
  let switch_cases =
    List.map (fun (c, _, _, l) -> (c.ctor_tag, l)) case_labels
  in
  set_terminator b
    (Mir.Switch
       {
         scrut = Mir.VReg tag_r;
         cases = switch_cases;
         default = default_l;
         span = sp;
       });
  let dst = fresh_vreg ~prefix:"sw" () in
  let arm_ends = ref [] in
  List.iter
    (fun (_c, binds, body, l) ->
      ignore (start_block b l);
      List.iteri
        (fun i bind ->
          emit b
            (Mir.GetField
               {
                 dst = bind;
                 obj = atom_value scrut;
                 index = i;
                 ty = Mir.Ty_any;
                 span = sp;
               }))
        binds;
      let v = lower_expr b body in
      let end_l = b.current in
      arm_ends := (end_l, v) :: !arm_ends;
      (match (current_block b).Mir.terminator with
      | Mir.Unreachable _ -> set_terminator b (Mir.Jump (join_l, sp))
      | _ -> ()))
    case_labels;
  (* Default *)
  ignore (start_block b default_l);
  let def_v =
    match default with
    | None ->
        emit b
          (Mir.Call
             {
               dst = None;
               callee = Mir.VGlobal (Ident.of_string "match_fail");
               args = [];
               ty = Mir.Ty_unit;
               span = sp;
             });
        set_terminator b (Mir.Unreachable sp);
        Mir.VUndef
    | Some e ->
        let v = lower_expr b e in
        (match (current_block b).Mir.terminator with
        | Mir.Unreachable _ -> set_terminator b (Mir.Jump (join_l, sp))
        | _ -> ());
        v
  in
  let def_end = b.current in
  arm_ends := (def_end, def_v) :: !arm_ends;
  ignore (start_block b join_l);
  List.iter
    (fun (end_l, v) ->
      match Mir.Label.Map.find_opt end_l b.blocks with
      | Some blk -> (
          match blk.Mir.terminator with
          | Mir.Jump _ | Mir.Unreachable _ ->
              if not (Mir.value_equal v Mir.VUndef) then
                blk.Mir.instrs <-
                  blk.Mir.instrs
                  @ [
                      Mir.Assign
                        { dst; src = v; ty = Mir.Ty_any; span = sp };
                    ]
          | _ -> ())
      | None -> ())
    !arm_ends;
  Mir.VReg dst

and lower_switch_lit b scrut cases default sp =
  (* Cascade of comparisons for literals. *)
  let scrut_v = atom_value scrut in
  let join_l = fresh_label ~prefix:"lit_join" () in
  let dst = fresh_vreg ~prefix:"lit" () in
  let rec cascade remaining =
    match remaining with
    | [] ->
        let fail_l = fresh_label ~prefix:"lit_fail" () in
        set_terminator b (Mir.Jump (fail_l, sp));
        ignore (start_block b fail_l);
        (match default with
        | None ->
            emit b
              (Mir.Call
                 {
                   dst = None;
                   callee = Mir.VGlobal (Ident.of_string "match_fail");
                   args = [];
                   ty = Mir.Ty_unit;
                   span = sp;
                 });
            set_terminator b (Mir.Unreachable sp)
        | Some e ->
            let v = lower_expr b e in
            emit b
              (Mir.Assign
                 { dst; src = v; ty = Mir.Ty_any; span = sp });
            set_terminator b (Mir.Jump (join_l, sp)))
    | (lit, body) :: rest ->
        let cmp = fresh_vreg ~prefix:"cmp" () in
        emit b
          (Mir.Binop
             {
               dst = cmp;
               op = Mir.Eq;
               lhs = scrut_v;
               rhs = Mir.VConst (lit_const lit);
               ty = Mir.Ty_bool;
               span = sp;
             });
        let then_l = fresh_label ~prefix:"lit_then" () in
        let else_l = fresh_label ~prefix:"lit_else" () in
        set_terminator b
          (Mir.Branch
             {
               cond = Mir.VReg cmp;
               then_ = then_l;
               else_ = else_l;
               span = sp;
             });
        ignore (start_block b then_l);
        let v = lower_expr b body in
        emit b
          (Mir.Assign { dst; src = v; ty = Mir.Ty_any; span = sp });
        set_terminator b (Mir.Jump (join_l, sp));
        ignore (start_block b else_l);
        cascade rest
  in
  cascade cases;
  ignore (start_block b join_l);
  Mir.VReg dst

and lower_nested_fun b ~name ~params ~body ~span =
  let nb = make_builder ~span () in
  Hashtbl.iter (fun k () -> Hashtbl.replace nb.globals k ()) b.globals;
  let params =
    List.map (fun p -> (p, Mir.Ty_any)) params
  in
  let v = lower_expr nb body in
  (match (current_block nb).Mir.terminator with
  | Mir.Unreachable _ ->
      set_terminator nb (Mir.Return (Some v, span))
  | _ -> ());
  let func =
    Mir.make_func ~name ~params ~entry:nb.entry ~blocks:nb.blocks
      ~return_ty:Mir.Ty_any ~span ()
  in
  Cfg.recompute_edges func;
  func

let lower_toplevel_fun ~name ~params ~body ~span ~recursive =
  ignore recursive;
  let b = make_builder ~span () in
  let params' = List.map (fun p -> (p, Mir.Ty_any)) params in
  let v = lower_expr b body in
  (match (current_block b).Mir.terminator with
  | Mir.Unreachable _ -> set_terminator b (Mir.Return (Some v, span))
  | _ -> ());
  let func =
    Mir.make_func ~name ~params:params' ~entry:b.entry ~blocks:b.blocks
      ~return_ty:Mir.Ty_any ~span ()
  in
  Cfg.recompute_edges func;
  (func, List.rev b.nested_funs)

(** Lower a full HIR program into MIR (non-SSA), then construct SSA. *)
let lower ?(ssa = true) (prog : program) : Mir.program =
  let mprog = Mir.empty_program ~span:prog.span () in
  List.iter
    (function
      | Toplevel_extern { name; arity; _ } ->
          mprog.Mir.externs <- (name, arity) :: mprog.Mir.externs
      | Toplevel_type _ -> ()
      | Toplevel_val { name; body; span } ->
          let f, nested =
            lower_toplevel_fun ~name ~params:[] ~body ~span ~recursive:false
          in
          List.iter (Mir.add_func mprog) nested;
          Mir.add_func mprog f
      | Toplevel_fun { name; params; body; recursive; span } ->
          let f, nested =
            lower_toplevel_fun ~name ~params ~body ~span ~recursive
          in
          List.iter (Mir.add_func mprog) nested;
          Mir.add_func mprog f)
    prog.items;
  mprog.Mir.externs <- List.rev mprog.Mir.externs;
  if ssa then Ssa.construct_program mprog else mprog

let lower_expr_to_func ?(name = Ident.of_string "main") (e : expr) : Mir.func =
  let f, _ =
    lower_toplevel_fun ~name ~params:[] ~body:e ~span:(expr_span e)
      ~recursive:false
  in
  Ssa.construct f
