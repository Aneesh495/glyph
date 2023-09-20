(** Mid-level IR: CFG of basic blocks in SSA form.

    Values are virtual registers ([vreg]). Each assignment defines a unique
    vreg. Control flow uses explicit terminators; φ-nodes sit at block heads.
    Codegen lowers this to register-based bytecode via linear-scan allocation.
*)

type vreg = int
type label = int
type fn_id = int

type typ =
  | TInt
  | TFloat
  | TBool
  | TChar
  | TUnit
  | TString
  | TTuple of typ list
  | TAdt of Ident.t * typ list
  | TFun of typ list * typ
  | TPtr
  | TAny

type const =
  | CInt of int
  | CFloat of float
  | CBool of bool
  | CChar of char
  | CUnit
  | CString of string

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | AddF
  | SubF
  | MulF
  | DivF
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | EqF
  | NeF
  | LtF
  | LeF
  | GtF
  | GeF
  | And
  | Or

type unop =
  | Neg
  | NegF
  | Not

(** Pure / side-effecting SSA instructions (no control flow). *)
type instr =
  | IConst of vreg * const
  | IMove of vreg * vreg
  | IBinop of vreg * binop * vreg * vreg
  | IUnop of vreg * unop * vreg
  | ICall of vreg * fn_id * vreg list
  | ICallClosure of vreg * vreg * vreg list
  | IAlloc of vreg * int * vreg list
      (** [IAlloc (dst, tag, fields)] — heap ADT / tuple with tag. *)
  | IGetField of vreg * vreg * int
  | ISetField of vreg * int * vreg
  | IMakeClosure of vreg * fn_id * vreg list
  | ITupleGet of vreg * vreg * int
  | ICons of vreg * vreg * vreg
  | ICar of vreg * vreg
  | ICdr of vreg * vreg
  | IPrint of vreg
  | IPhi of vreg * (label * vreg) list
  | INop

type terminator =
  | TJump of label
  | TBranch of vreg * label * label
      (** [TBranch (cond, then_lbl, else_lbl)] *)
  | TSwitch of vreg * (int * label) list * label
      (** tag/value switch with default label *)
  | TRet of vreg option
  | TTailCall of fn_id * vreg list
  | TTailCallClosure of vreg * vreg list
  | THalt of vreg option

type block = {
  label : label;
  phis : instr list;
  instrs : instr list;
  term : terminator;
  span : Span.t;
}

type func = {
  id : fn_id;
  name : Ident.t;
  params : vreg list;
  param_tys : typ list;
  ret_ty : typ;
  blocks : block list;
  entry : label;
  n_vregs : int;
  is_main : bool;
  span : Span.t;
}

type program = {
  functions : func list;
  main : fn_id;
  string_table : string list;
}

(* -------------------------------------------------------------------------- *)
(* Constructors                                                               *)
(* -------------------------------------------------------------------------- *)

let dummy_span = Span.dummy

let make_block ?(phis = []) ?(span = dummy_span) label instrs term =
  { label; phis; instrs; term; span }

let make_func ~id ~name ~params ?(param_tys = []) ?(ret_ty = TAny)
    ~blocks ~entry ?(n_vregs = 0) ?(is_main = false) ?(span = dummy_span) () =
  let n_vregs =
    if n_vregs > 0 then n_vregs
    else
      let max_v = ref (-1) in
      let touch v = if v > !max_v then max_v := v in
      List.iter touch params;
      List.iter
        (fun (b : block) ->
          let scan_instr = function
            | IConst (d, _)
            | IMove (d, _)
            | IBinop (d, _, _, _)
            | IUnop (d, _, _)
            | ICall (d, _, _)
            | ICallClosure (d, _, _)
            | IAlloc (d, _, _)
            | IGetField (d, _, _)
            | IMakeClosure (d, _, _)
            | ITupleGet (d, _, _)
            | ICons (d, _, _)
            | ICar (d, _)
            | ICdr (d, _)
            | IPhi (d, _) ->
                touch d
            | ISetField (obj, _, _) -> touch obj
            | IPrint v -> touch v
            | INop -> ()
          in
          List.iter scan_instr b.phis;
          List.iter scan_instr b.instrs;
          match b.term with
          | TBranch (c, _, _) -> touch c
          | TSwitch (v, _, _) -> touch v
          | TRet (Some v) | THalt (Some v) -> touch v
          | TTailCall (_, args) | TTailCallClosure (_, args) ->
              List.iter touch args
          | _ -> ())
        blocks;
      !max_v + 1
  in
  {
    id;
    name;
    params;
    param_tys;
    ret_ty;
    blocks;
    entry;
    n_vregs;
    is_main;
    span;
  }

let make_program ?(string_table = []) ~functions ~main =
  { functions; main; string_table }

let find_func prog id =
  List.find (fun (f : func) -> f.id = id) prog.functions

let find_func_opt prog id =
  List.find_opt (fun (f : func) -> f.id = id) prog.functions

let find_block (fn : func) lbl =
  List.find (fun (b : block) -> b.label = lbl) fn.blocks

let find_block_opt (fn : func) lbl =
  List.find_opt (fun (b : block) -> b.label = lbl) fn.blocks

let block_labels (fn : func) = List.map (fun (b : block) -> b.label) fn.blocks

(* -------------------------------------------------------------------------- *)
(* Def / use                                                                  *)
(* -------------------------------------------------------------------------- *)

let instr_def = function
  | IConst (d, _)
  | IMove (d, _)
  | IBinop (d, _, _, _)
  | IUnop (d, _, _)
  | ICall (d, _, _)
  | ICallClosure (d, _, _)
  | IAlloc (d, _, _)
  | IGetField (d, _, _)
  | IMakeClosure (d, _, _)
  | ITupleGet (d, _, _)
  | ICons (d, _, _)
  | ICar (d, _)
  | ICdr (d, _)
  | IPhi (d, _) ->
      Some d
  | ISetField _ | IPrint _ | INop -> None

let instr_uses = function
  | IConst _ | INop -> []
  | IMove (_, s) -> [ s ]
  | IBinop (_, _, a, b) -> [ a; b ]
  | IUnop (_, _, a) -> [ a ]
  | ICall (_, _, args) -> args
  | ICallClosure (_, clo, args) -> clo :: args
  | IAlloc (_, _, fields) -> fields
  | IGetField (_, obj, _) -> [ obj ]
  | ISetField (obj, _, v) -> [ obj; v ]
  | IMakeClosure (_, _, env) -> env
  | ITupleGet (_, tup, _) -> [ tup ]
  | ICons (_, h, t) -> [ h; t ]
  | ICar (_, c) | ICdr (_, c) -> [ c ]
  | IPrint v -> [ v ]
  | IPhi (_, incoming) -> List.map snd incoming

let term_uses = function
  | TJump _ -> []
  | TBranch (c, _, _) -> [ c ]
  | TSwitch (v, _, _) -> [ v ]
  | TRet (Some v) | THalt (Some v) -> [ v ]
  | TRet None | THalt None -> []
  | TTailCall (_, args) -> args
  | TTailCallClosure (clo, args) -> clo :: args

let term_successors = function
  | TJump l -> [ l ]
  | TBranch (_, t, e) -> [ t; e ]
  | TSwitch (_, cases, d) -> d :: List.map snd cases
  | TRet _ | THalt _ | TTailCall _ | TTailCallClosure _ -> []

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                            *)
(* -------------------------------------------------------------------------- *)

let pp_const fmt = function
  | CInt i -> Format.fprintf fmt "%d" i
  | CFloat f -> Format.fprintf fmt "%g" f
  | CBool b -> Format.fprintf fmt "%b" b
  | CChar c -> Format.fprintf fmt "%C" c
  | CUnit -> Format.fprintf fmt "()"
  | CString s -> Format.fprintf fmt "%S" s

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | AddF -> "+."
  | SubF -> "-."
  | MulF -> "*."
  | DivF -> "/."
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | EqF -> "==."
  | NeF -> "!=."
  | LtF -> "<."
  | LeF -> "<=."
  | GtF -> ">."
  | GeF -> ">=."
  | And -> "&&"
  | Or -> "||"

let unop_to_string = function
  | Neg -> "-"
  | NegF -> "-."
  | Not -> "!"

let pp_vreg fmt v = Format.fprintf fmt "%%%d" v

let pp_instr fmt = function
  | IConst (d, c) -> Format.fprintf fmt "%a = const %a" pp_vreg d pp_const c
  | IMove (d, s) -> Format.fprintf fmt "%a = %a" pp_vreg d pp_vreg s
  | IBinop (d, op, a, b) ->
      Format.fprintf fmt "%a = %a %s %a" pp_vreg d pp_vreg a
        (binop_to_string op) pp_vreg b
  | IUnop (d, op, a) ->
      Format.fprintf fmt "%a = %s%a" pp_vreg d (unop_to_string op) pp_vreg a
  | ICall (d, fid, args) ->
      Format.fprintf fmt "%a = call fn%d (%a)" pp_vreg d fid
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        args
  | ICallClosure (d, clo, args) ->
      Format.fprintf fmt "%a = callclo %a (%a)" pp_vreg d pp_vreg clo
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        args
  | IAlloc (d, tag, fields) ->
      Format.fprintf fmt "%a = alloc tag=%d [%a]" pp_vreg d tag
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        fields
  | IGetField (d, obj, i) ->
      Format.fprintf fmt "%a = %a.field[%d]" pp_vreg d pp_vreg obj i
  | ISetField (obj, i, v) ->
      Format.fprintf fmt "%a.field[%d] := %a" pp_vreg obj i pp_vreg v
  | IMakeClosure (d, fid, env) ->
      Format.fprintf fmt "%a = closure fn%d env=[%a]" pp_vreg d fid
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        env
  | ITupleGet (d, t, i) ->
      Format.fprintf fmt "%a = %a.tuple[%d]" pp_vreg d pp_vreg t i
  | ICons (d, h, t) ->
      Format.fprintf fmt "%a = cons %a %a" pp_vreg d pp_vreg h pp_vreg t
  | ICar (d, c) -> Format.fprintf fmt "%a = car %a" pp_vreg d pp_vreg c
  | ICdr (d, c) -> Format.fprintf fmt "%a = cdr %a" pp_vreg d pp_vreg c
  | IPrint v -> Format.fprintf fmt "print %a" pp_vreg v
  | IPhi (d, incoming) ->
      Format.fprintf fmt "%a = φ(" pp_vreg d;
      List.iteri
        (fun i (lbl, v) ->
          if i > 0 then Format.fprintf fmt ", ";
          Format.fprintf fmt "L%d:%a" lbl pp_vreg v)
        incoming;
      Format.fprintf fmt ")"
  | INop -> Format.fprintf fmt "nop"

let pp_term fmt = function
  | TJump l -> Format.fprintf fmt "jump L%d" l
  | TBranch (c, t, e) ->
      Format.fprintf fmt "br %a, L%d, L%d" pp_vreg c t e
  | TSwitch (v, cases, d) ->
      Format.fprintf fmt "switch %a [" pp_vreg v;
      List.iter
        (fun (tag, l) -> Format.fprintf fmt " %d -> L%d;" tag l)
        cases;
      Format.fprintf fmt " default -> L%d ]" d
  | TRet None -> Format.fprintf fmt "ret"
  | TRet (Some v) -> Format.fprintf fmt "ret %a" pp_vreg v
  | TTailCall (fid, args) ->
      Format.fprintf fmt "tailcall fn%d (%a)" fid
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        args
  | TTailCallClosure (clo, args) ->
      Format.fprintf fmt "tailcallclo %a (%a)" pp_vreg clo
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.fprintf f ", ")
           pp_vreg)
        args
  | THalt None -> Format.fprintf fmt "halt"
  | THalt (Some v) -> Format.fprintf fmt "halt %a" pp_vreg v

let pp_block fmt (b : block) =
  Format.fprintf fmt "L%d:\n" b.label;
  List.iter (fun i -> Format.fprintf fmt "  %a\n" pp_instr i) b.phis;
  List.iter (fun i -> Format.fprintf fmt "  %a\n" pp_instr i) b.instrs;
  Format.fprintf fmt "  %a\n" pp_term b.term

let pp_func fmt (f : func) =
  Format.fprintf fmt "fn%d %s(%a) {\n" f.id (Ident.to_string f.name)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       pp_vreg)
    f.params;
  List.iter (pp_block fmt) f.blocks;
  Format.fprintf fmt "}\n"

let pp_program fmt (p : program) =
  Format.fprintf fmt "; Glyph MIR — main=fn%d\n" p.main;
  List.iter (pp_func fmt) p.functions

(* -------------------------------------------------------------------------- *)
(* CFG helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let predecessors (fn : func) : (label, label list) Hashtbl.t =
  let pred = Hashtbl.create (List.length fn.blocks) in
  List.iter
    (fun (b : block) -> Hashtbl.replace pred b.label [])
    fn.blocks;
  List.iter
    (fun (b : block) ->
      List.iter
        (fun succ ->
          let ps = Hashtbl.find pred succ in
          if not (List.mem b.label ps) then
            Hashtbl.replace pred succ (b.label :: ps))
        (term_successors b.term))
    fn.blocks;
  pred

let successors_of (fn : func) lbl =
  match find_block_opt fn lbl with
  | None -> []
  | Some b -> term_successors b.term

let iter_instrs (fn : func) f =
  List.iter
    (fun (b : block) ->
      List.iter (fun i -> f b.label i) b.phis;
      List.iter (fun i -> f b.label i) b.instrs)
    fn.blocks

let all_vregs (fn : func) : vreg list =
  let set = Hashtbl.create fn.n_vregs in
  List.iter (fun v -> Hashtbl.replace set v ()) fn.params;
  iter_instrs fn (fun _ i ->
      (match instr_def i with
      | Some d -> Hashtbl.replace set d ()
      | None -> ());
      List.iter (fun u -> Hashtbl.replace set u ()) (instr_uses i));
  List.iter
    (fun (b : block) ->
      List.iter (fun u -> Hashtbl.replace set u ()) (term_uses b.term))
    fn.blocks;
  Hashtbl.fold (fun v () acc -> v :: acc) set [] |> List.sort Int.compare

let validate_func (fn : func) : string list =
  let errs = ref [] in
  let err s = errs := s :: !errs in
  (match find_block_opt fn fn.entry with
  | None -> err (Printf.sprintf "entry L%d missing" fn.entry)
  | Some _ -> ());
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (b : block) ->
      if Hashtbl.mem seen b.label then
        err (Printf.sprintf "duplicate block L%d" b.label)
      else Hashtbl.add seen b.label ();
      List.iter
        (fun succ ->
          if find_block_opt fn succ = None then
            err
              (Printf.sprintf "L%d jumps to missing L%d" b.label succ))
        (term_successors b.term))
    fn.blocks;
  List.rev !errs

let validate_program (p : program) : string list =
  let errs = ref [] in
  (match find_func_opt p p.main with
  | None -> errs := Printf.sprintf "main fn%d not found" p.main :: !errs
  | Some _ -> ());
  List.iter
    (fun f ->
      List.iter
        (fun e -> errs := Printf.sprintf "fn%d: %s" f.id e :: !errs)
        (validate_func f))
    p.functions;
  List.rev !errs
