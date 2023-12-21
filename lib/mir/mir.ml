(** Mid-level IR: CFG of basic blocks, optionally in SSA form. *)

type vreg = int
type label = int
type fn_id = int

type typ =
  | TInt | TFloat | TBool | TChar | TUnit | TString
  | TTuple of typ list
  | TAdt of Ident.t * typ list
  | TFun of typ list * typ
  | TPtr | TAny

type const =
  | CInt of int | CFloat of float | CBool of bool
  | CChar of char | CUnit | CString of string

type binop =
  | Add | Sub | Mul | Div | Mod
  | AddF | SubF | MulF | DivF
  | Eq | Ne | Lt | Le | Gt | Ge
  | EqF | NeF | LtF | LeF | GtF | GeF
  | And | Or

type unop = Neg | NegF | Not

type instr =
  | IConst of vreg * const
  | IMove of vreg * vreg
  | IBinop of vreg * binop * vreg * vreg
  | IUnop of vreg * unop * vreg
  | ICall of vreg * fn_id * vreg list
  | ICallClosure of vreg * vreg * vreg list
  | IAlloc of vreg * int * vreg list
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
  | TSwitch of vreg * (int * label) list * label
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

let make_block ?(phis = []) ?(span = Span.dummy) label instrs term =
  { label; phis; instrs; term; span }

let instr_def = function
  | IConst (d, _) | IMove (d, _) | IBinop (d, _, _, _) | IUnop (d, _, _)
  | ICall (d, _, _) | ICallClosure (d, _, _) | IAlloc (d, _, _)
  | IGetField (d, _, _) | IMakeClosure (d, _, _) | ITupleGet (d, _, _)
  | ICons (d, _, _) | ICar (d, _) | ICdr (d, _) | IPhi (d, _) -> Some d
  | ISetField _ | IPrint _ | INop -> None

let instr_uses = function
  | IConst _ | INop -> []
  | IMove (_, s) | IUnop (_, _, s) | IGetField (_, s, _) | ITupleGet (_, s, _)
  | ICar (_, s) | ICdr (_, s) | IPrint s -> [ s ]
  | IBinop (_, _, a, b) | ICons (_, a, b) -> [ a; b ]
  | ISetField (obj, _, src) -> [ obj; src ]
  | ICall (_, _, args) | IAlloc (_, _, args) | IMakeClosure (_, _, args) -> args
  | ICallClosure (_, clo, args) -> clo :: args
  | IPhi (_, incoming) -> List.map snd incoming

let term_uses = function
  | TJump _ -> []
  | TBranch (c, _, _) -> [ c ]
  | TSwitch (s, _, _) -> [ s ]
  | TRet (Some v) | THalt (Some v) -> [ v ]
  | TRet None | THalt None -> []
  | TTailCall (_, args) -> args
  | TTailCallClosure (clo, args) -> clo :: args

let term_successors = function
  | TJump l -> [ l ]
  | TBranch (_, t, f) -> [ t; f ]
  | TSwitch (_, cases, dflt) -> dflt :: List.map snd cases
  | TRet _ | TTailCall _ | TTailCallClosure _ | THalt _ -> []

let make_func ~id ~name ~params ?(param_tys = []) ?(ret_ty = TAny) ~blocks
    ~entry ?(n_vregs = 0) ?(is_main = false) ?(span = Span.dummy) () =
  let n_vregs =
    if n_vregs > 0 then n_vregs
    else
      let m = ref (-1) in
      let c v = if v > !m then m := v in
      List.iter c params;
      List.iter
        (fun (b : block) ->
          List.iter
            (fun i ->
              Option.iter c (instr_def i);
              List.iter c (instr_uses i))
            (b.phis @ b.instrs);
          List.iter c (term_uses b.term))
        blocks;
      !m + 1
  in
  { id; name; params; param_tys; ret_ty; blocks; entry; n_vregs; is_main; span }

let make_program ?(string_table = []) ~functions ~main =
  { functions; main; string_table }

let find_func_opt prog id =
  List.find_opt (fun (f : func) -> f.id = id) prog.functions

let find_func prog id =
  match find_func_opt prog id with
  | Some f -> f
  | None -> invalid_arg (Printf.sprintf "Mir.find_func: %d" id)

let find_block_opt fn label =
  List.find_opt (fun (b : block) -> b.label = label) fn.blocks

let find_block fn label =
  match find_block_opt fn label with
  | Some b -> b
  | None -> invalid_arg (Printf.sprintf "Mir.find_block: %d" label)

let block_labels fn = List.map (fun (b : block) -> b.label) fn.blocks

let successors_of fn label =
  match find_block_opt fn label with None -> [] | Some b -> term_successors b.term

let predecessors fn =
  let tbl = Hashtbl.create (List.length fn.blocks) in
  List.iter (fun (b : block) -> Hashtbl.replace tbl b.label []) fn.blocks;
  List.iter
    (fun (b : block) ->
      List.iter
        (fun succ ->
          let preds = try Hashtbl.find tbl succ with Not_found -> [] in
          Hashtbl.replace tbl succ (b.label :: preds))
        (term_successors b.term))
    fn.blocks;
  tbl

let iter_instrs fn f =
  List.iter
    (fun (b : block) ->
      List.iter (fun i -> f b.label i) b.phis;
      List.iter (fun i -> f b.label i) b.instrs)
    fn.blocks

let all_vregs fn =
  let set = Hashtbl.create fn.n_vregs in
  let add v = Hashtbl.replace set v () in
  List.iter add fn.params;
  iter_instrs fn (fun _ i ->
      Option.iter add (instr_def i);
      List.iter add (instr_uses i));
  List.iter (fun (b : block) -> List.iter add (term_uses b.term)) fn.blocks;
  Hashtbl.fold (fun v _ acc -> v :: acc) set [] |> List.sort Int.compare

let validate_func fn =
  let errs = ref [] in
  if not (List.exists (fun (b : block) -> b.label = fn.entry) fn.blocks) then
    errs := "entry missing" :: !errs;
  List.rev !errs

let validate_program prog =
  let errs = ref [] in
  if find_func_opt prog prog.main = None then errs := "main missing" :: !errs;
  List.iter (fun f -> List.iter (fun e -> errs := e :: !errs) (validate_func f)) prog.functions;
  List.rev !errs

let binop_to_string = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | AddF -> "+." | SubF -> "-." | MulF -> "*." | DivF -> "/."
  | Eq -> "=" | Ne -> "<>" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | EqF -> "=." | NeF -> "<>." | LtF -> "<." | LeF -> "<=." | GtF -> ">." | GeF -> ">=."
  | And -> "&&" | Or -> "||"

let unop_to_string = function Neg -> "-" | NegF -> "-." | Not -> "not"

let pp_const fmt = function
  | CInt i -> Format.fprintf fmt "%d" i
  | CFloat f -> Format.fprintf fmt "%g" f
  | CBool b -> Format.pp_print_bool fmt b
  | CChar c -> Format.fprintf fmt "%C" c
  | CUnit -> Format.pp_print_string fmt "()"
  | CString s -> Format.fprintf fmt "%S" s

let pp_vreg fmt v = Format.fprintf fmt "%%%d" v

let pp_instr fmt i =
  match i with
  | IConst (d, c) -> Format.fprintf fmt "%a = const %a" pp_vreg d pp_const c
  | IMove (d, s) -> Format.fprintf fmt "%a = %a" pp_vreg d pp_vreg s
  | IBinop (d, op, a, b) ->
      Format.fprintf fmt "%a = %a %s %a" pp_vreg d pp_vreg a (binop_to_string op) pp_vreg b
  | IUnop (d, op, s) -> Format.fprintf fmt "%a = %s %a" pp_vreg d (unop_to_string op) pp_vreg s
  | ICall (d, fn, args) -> Format.fprintf fmt "%a = call @%d" pp_vreg d fn
  | ICallClosure (d, clo, _) -> Format.fprintf fmt "%a = callclo %a" pp_vreg d pp_vreg clo
  | IAlloc (d, tag, _) -> Format.fprintf fmt "%a = alloc %d" pp_vreg d tag
  | IGetField (d, o, i) -> Format.fprintf fmt "%a = %a[%d]" pp_vreg d pp_vreg o i
  | ISetField (o, i, s) -> Format.fprintf fmt "%a[%d] = %a" pp_vreg o i pp_vreg s
  | IMakeClosure (d, fn, _) -> Format.fprintf fmt "%a = clo @%d" pp_vreg d fn
  | ITupleGet (d, t, i) -> Format.fprintf fmt "%a = %a.%d" pp_vreg d pp_vreg t i
  | ICons (d, h, t) -> Format.fprintf fmt "%a = cons %a %a" pp_vreg d pp_vreg h pp_vreg t
  | ICar (d, c) -> Format.fprintf fmt "%a = car %a" pp_vreg d pp_vreg c
  | ICdr (d, c) -> Format.fprintf fmt "%a = cdr %a" pp_vreg d pp_vreg c
  | IPrint v -> Format.fprintf fmt "print %a" pp_vreg v
  | IPhi (d, _) -> Format.fprintf fmt "%a = phi" pp_vreg d
  | INop -> Format.pp_print_string fmt "nop"

let pp_term fmt = function
  | TJump l -> Format.fprintf fmt "jump L%d" l
  | TBranch (c, t, f) -> Format.fprintf fmt "br %a L%d L%d" pp_vreg c t f
  | TSwitch (s, _, d) -> Format.fprintf fmt "switch %a default L%d" pp_vreg s d
  | TRet None -> Format.pp_print_string fmt "ret"
  | TRet (Some v) -> Format.fprintf fmt "ret %a" pp_vreg v
  | TTailCall (fn, _) -> Format.fprintf fmt "tail @%d" fn
  | TTailCallClosure (c, _) -> Format.fprintf fmt "tailclo %a" pp_vreg c
  | THalt None -> Format.pp_print_string fmt "halt"
  | THalt (Some v) -> Format.fprintf fmt "halt %a" pp_vreg v

let pp_block fmt (b : block) =
  Format.fprintf fmt "L%d:" b.label;
  List.iter (fun i -> Format.fprintf fmt "@,%a" pp_instr i) (b.phis @ b.instrs);
  Format.fprintf fmt "@,%a" pp_term b.term

let pp_func fmt (f : func) =
  Format.fprintf fmt "fn %a@%d" Ident.pp f.name f.id;
  List.iter (fun b -> Format.fprintf fmt "@,%a" pp_block b) f.blocks

let pp_program fmt (p : program) =
  Format.fprintf fmt "program main=@%d" p.main;
  List.iter (fun f -> Format.fprintf fmt "@,@,%a" pp_func f) p.functions
