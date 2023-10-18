(** Mid-level IR: CFG of basic blocks, optionally in SSA form. *)

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

val make_block :
  ?phis:instr list ->
  ?span:Span.t ->
  label ->
  instr list ->
  terminator ->
  block

val make_func :
  id:fn_id ->
  name:Ident.t ->
  params:vreg list ->
  ?param_tys:typ list ->
  ?ret_ty:typ ->
  blocks:block list ->
  entry:label ->
  ?n_vregs:int ->
  ?is_main:bool ->
  ?span:Span.t ->
  unit ->
  func

val make_program :
  ?string_table:string list ->
  functions:func list ->
  main:fn_id ->
  program

val find_func : program -> fn_id -> func
val find_func_opt : program -> fn_id -> func option
val find_block : func -> label -> block
val find_block_opt : func -> label -> block option
val block_labels : func -> label list

val instr_def : instr -> vreg option
val instr_uses : instr -> vreg list
val term_uses : terminator -> vreg list
val term_successors : terminator -> label list

val predecessors : func -> (label, label list) Hashtbl.t
val successors_of : func -> label -> label list
val iter_instrs : func -> (label -> instr -> unit) -> unit
val all_vregs : func -> vreg list
val validate_func : func -> string list
val validate_program : program -> string list

val pp_const : Format.formatter -> const -> unit
val pp_vreg : Format.formatter -> vreg -> unit
val pp_instr : Format.formatter -> instr -> unit
val pp_term : Format.formatter -> terminator -> unit
val pp_block : Format.formatter -> block -> unit
val pp_func : Format.formatter -> func -> unit
val pp_program : Format.formatter -> program -> unit

val binop_to_string : binop -> string
val unop_to_string : unop -> string
