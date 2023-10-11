(** SSA mid-level intermediate representation.

    Blocks contain φ-nodes, a straight-line instruction list, and a single
    terminator. Functions own a map of blocks plus an entry label. Programs
    are collections of functions (plus optional string/float constant pools). *)

(* -------------------------------------------------------------------------- *)
(* Labels, virtual registers, types                                           *)
(* -------------------------------------------------------------------------- *)

type label = Ident.t

module Label = struct
  type t = label
  let equal = Ident.equal
  let compare = Ident.compare
  let hash = Ident.hash
  let fresh name = Ident.fresh name
  let of_string = Ident.of_string
  let to_string = Ident.to_string
  let pp = Ident.pp
  module Set = Ident.Set
  module Map = Ident.Map
  module Tbl = Ident.Tbl
end

type vreg = Ident.t

module Vreg = struct
  type t = vreg
  let equal = Ident.equal
  let compare = Ident.compare
  let hash = Ident.hash
  let fresh name = Ident.fresh name
  let of_string = Ident.of_string
  let to_string = Ident.to_string
  let pp = Ident.pp
  module Set = Ident.Set
  module Map = Ident.Map
  module Tbl = Ident.Tbl
end

type ty =
  | Ty_unit
  | Ty_bool
  | Ty_int
  | Ty_float
  | Ty_string
  | Ty_char
  | Ty_ptr
  | Ty_fn of ty list * ty
  | Ty_tuple of ty list
  | Ty_adt of Ident.t
  | Ty_any

let rec ty_to_string = function
  | Ty_unit -> "unit"
  | Ty_bool -> "bool"
  | Ty_int -> "int"
  | Ty_float -> "float"
  | Ty_string -> "string"
  | Ty_char -> "char"
  | Ty_ptr -> "ptr"
  | Ty_fn (args, ret) ->
      Printf.sprintf "(%s) -> %s"
        (String.concat ", " (List.map ty_to_string args))
        (ty_to_string ret)
  | Ty_tuple ts ->
      "(" ^ String.concat " * " (List.map ty_to_string ts) ^ ")"
  | Ty_adt n -> Ident.to_string n
  | Ty_any -> "?"

(* -------------------------------------------------------------------------- *)
(* Constants & values                                                         *)
(* -------------------------------------------------------------------------- *)

type const =
  | CUnit
  | CBool of bool
  | CInt of int
  | CFloat of float
  | CString of string
  | CChar of char
  | CNull

type value =
  | VConst of const
  | VReg of vreg
  | VGlobal of Ident.t
  | VUndef

let const_equal a b =
  match (a, b) with
  | CUnit, CUnit | CNull, CNull -> true
  | CBool x, CBool y -> x = y
  | CInt x, CInt y -> x = y
  | CFloat x, CFloat y -> Float.equal x y
  | CString x, CString y -> String.equal x y
  | CChar x, CChar y -> Char.equal x y
  | _ -> false

let value_equal a b =
  match (a, b) with
  | VConst c1, VConst c2 -> const_equal c1 c2
  | VReg r1, VReg r2 -> Vreg.equal r1 r2
  | VGlobal g1, VGlobal g2 -> Ident.equal g1 g2
  | VUndef, VUndef -> true
  | _ -> false

let const_to_string = function
  | CUnit -> "()"
  | CBool true -> "true"
  | CBool false -> "false"
  | CInt n -> string_of_int n
  | CFloat f -> string_of_float f
  | CString s -> Printf.sprintf "%S" s
  | CChar c -> Printf.sprintf "%C" c
  | CNull -> "null"

let value_to_string = function
  | VConst c -> const_to_string c
  | VReg r -> Vreg.to_string r
  | VGlobal g -> "@" ^ Ident.to_string g
  | VUndef -> "undef"

(* -------------------------------------------------------------------------- *)
(* Operators                                                                  *)
(* -------------------------------------------------------------------------- *)

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or | Xor | Shl | Shr
  | FAdd | FSub | FMul | FDiv

type unop =
  | Neg | Not | FNeg | BitNot | IsNull | TagOf | Box | Unbox

let binop_to_string = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Mod -> "mod"
  | Eq -> "eq" | Ne -> "ne" | Lt -> "lt" | Le -> "le" | Gt -> "gt" | Ge -> "ge"
  | And -> "and" | Or -> "or" | Xor -> "xor" | Shl -> "shl" | Shr -> "shr"
  | FAdd -> "fadd" | FSub -> "fsub" | FMul -> "fmul" | FDiv -> "fdiv"

let unop_to_string = function
  | Neg -> "neg" | Not -> "not" | FNeg -> "fneg" | BitNot -> "bitnot"
  | IsNull -> "isnull" | TagOf -> "tagof" | Box -> "box" | Unbox -> "unbox"

(* -------------------------------------------------------------------------- *)
(* Instructions & terminators                                                 *)
(* -------------------------------------------------------------------------- *)

type instr =
  | Assign of { dst : vreg; src : value; ty : ty; span : Span.t }
  | Binop of {
      dst : vreg;
      op : binop;
      lhs : value;
      rhs : value;
      ty : ty;
      span : Span.t;
    }
  | Unop of {
      dst : vreg;
      op : unop;
      arg : value;
      ty : ty;
      span : Span.t;
    }
  | Call of {
      dst : vreg option;
      callee : value;
      args : value list;
      ty : ty;
      span : Span.t;
    }
  | Alloc of {
      dst : vreg;
      tag : int;
      fields : value list;
      ty : ty;
      span : Span.t;
    }
  | Load of { dst : vreg; ptr : value; ty : ty; span : Span.t }
  | Store of { ptr : value; value : value; ty : ty; span : Span.t }
  | GetField of {
      dst : vreg;
      obj : value;
      index : int;
      ty : ty;
      span : Span.t;
    }
  | SetField of {
      obj : value;
      index : int;
      value : value;
      ty : ty;
      span : Span.t;
    }
  | Cast of { dst : vreg; src : value; ty : ty; span : Span.t }
  | Phi of {
      dst : vreg;
      ty : ty;
      incoming : (label * value) list;
      span : Span.t;
    }

type terminator =
  | Return of value option * Span.t
  | Jump of label * Span.t
  | Branch of {
      cond : value;
      then_ : label;
      else_ : label;
      span : Span.t;
    }
  | Switch of {
      scrut : value;
      cases : (int * label) list;
      default : label;
      span : Span.t;
    }
  | TailCall of {
      callee : value;
      args : value list;
      span : Span.t;
    }
  | Unreachable of Span.t

type block = {
  label : label;
  mutable phis : instr list;
  mutable instrs : instr list;
  mutable terminator : terminator;
  mutable preds : label list;
  mutable succs : label list;
}

type func = {
  name : Ident.t;
  params : (vreg * ty) list;
  mutable blocks : block Label.Map.t;
  entry : label;
  return_ty : ty;
  span : Span.t;
  mutable is_ssa : bool;
}

type program = {
  mutable funcs : func Ident.Map.t;
  mutable externs : (Ident.t * int) list;
  span : Span.t;
}

(* -------------------------------------------------------------------------- *)
(* Constructors                                                               *)
(* -------------------------------------------------------------------------- *)

let make_block ?(phis = []) ?(instrs = []) ?(preds = []) ?(succs = [])
    label terminator =
  { label; phis; instrs; terminator; preds; succs }

let make_func ?(is_ssa = false) ~name ~params ~entry ~blocks ~return_ty
    ?(span = Span.dummy) () =
  { name; params; blocks; entry; return_ty; span; is_ssa }

let make_program ?(externs = []) ?(span = Span.dummy) funcs =
  let fmap =
    List.fold_left (fun m f -> Ident.Map.add f.name f m) Ident.Map.empty funcs
  in
  { funcs = fmap; externs; span }

let empty_program ?(span = Span.dummy) () =
  { funcs = Ident.Map.empty; externs = []; span }

let add_func prog f =
  prog.funcs <- Ident.Map.add f.name f prog.funcs

let find_func prog name = Ident.Map.find_opt name prog.funcs

let find_block (f : func) (l : label) = Label.Map.find_opt l f.blocks

let set_block (f : func) (b : block) =
  f.blocks <- Label.Map.add b.label b f.blocks

(* -------------------------------------------------------------------------- *)
(* Instruction helpers                                                        *)
(* -------------------------------------------------------------------------- *)

let instr_span = function
  | Assign { span; _ } | Binop { span; _ } | Unop { span; _ }
  | Call { span; _ } | Alloc { span; _ } | Load { span; _ }
  | Store { span; _ } | GetField { span; _ } | SetField { span; _ }
  | Cast { span; _ } | Phi { span; _ } ->
      span

let terminator_span = function
  | Return (_, sp) | Jump (_, sp) | Branch { span = sp; _ }
  | Switch { span = sp; _ } | TailCall { span = sp; _ }
  | Unreachable sp ->
      sp

let instr_defs = function
  | Assign { dst; _ } | Binop { dst; _ } | Unop { dst; _ }
  | Alloc { dst; _ } | Load { dst; _ } | GetField { dst; _ }
  | Cast { dst; _ } | Phi { dst; _ } ->
      [ dst ]
  | Call { dst = Some d; _ } -> [ d ]
  | Call { dst = None; _ } | Store _ | SetField _ -> []

let value_uses = function
  | VReg r -> [ r ]
  | VConst _ | VGlobal _ | VUndef -> []

let instr_uses = function
  | Assign { src; _ } -> value_uses src
  | Binop { lhs; rhs; _ } -> value_uses lhs @ value_uses rhs
  | Unop { arg; _ } -> value_uses arg
  | Call { callee; args; _ } ->
      value_uses callee @ List.concat_map value_uses args
  | Alloc { fields; _ } -> List.concat_map value_uses fields
  | Load { ptr; _ } -> value_uses ptr
  | Store { ptr; value; _ } -> value_uses ptr @ value_uses value
  | GetField { obj; _ } -> value_uses obj
  | SetField { obj; value; _ } -> value_uses obj @ value_uses value
  | Cast { src; _ } -> value_uses src
  | Phi { incoming; _ } ->
      List.concat_map (fun (_, v) -> value_uses v) incoming

let terminator_uses = function
  | Return (Some v, _) -> value_uses v
  | Return (None, _) | Jump _ | Unreachable _ -> []
  | Branch { cond; _ } -> value_uses cond
  | Switch { scrut; _ } -> value_uses scrut
  | TailCall { callee; args; _ } ->
      value_uses callee @ List.concat_map value_uses args

let terminator_succs = function
  | Return _ | TailCall _ | Unreachable _ -> []
  | Jump (l, _) -> [ l ]
  | Branch { then_; else_; _ } -> [ then_; else_ ]
  | Switch { cases; default; _ } ->
      default :: List.map snd cases

let is_phi = function Phi _ -> true | _ -> false

let map_instr_values ~on_use ~on_def instr =
  let mv v = on_use v in
  match instr with
  | Assign ({ dst; src; _ } as i) ->
      Assign { i with dst = on_def dst; src = mv src }
  | Binop ({ dst; lhs; rhs; _ } as i) ->
      Binop { i with dst = on_def dst; lhs = mv lhs; rhs = mv rhs }
  | Unop ({ dst; arg; _ } as i) ->
      Unop { i with dst = on_def dst; arg = mv arg }
  | Call ({ dst; callee; args; _ } as i) ->
      Call
        {
          i with
          dst = Option.map on_def dst;
          callee = mv callee;
          args = List.map mv args;
        }
  | Alloc ({ dst; fields; _ } as i) ->
      Alloc { i with dst = on_def dst; fields = List.map mv fields }
  | Load ({ dst; ptr; _ } as i) ->
      Load { i with dst = on_def dst; ptr = mv ptr }
  | Store ({ ptr; value; _ } as i) ->
      Store { i with ptr = mv ptr; value = mv value }
  | GetField ({ dst; obj; _ } as i) ->
      GetField { i with dst = on_def dst; obj = mv obj }
  | SetField ({ obj; value; _ } as i) ->
      SetField { i with obj = mv obj; value = mv value }
  | Cast ({ dst; src; _ } as i) ->
      Cast { i with dst = on_def dst; src = mv src }
  | Phi ({ dst; incoming; _ } as i) ->
      Phi
        {
          i with
          dst = on_def dst;
          incoming = List.map (fun (l, v) -> (l, mv v)) incoming;
        }

let map_terminator_values f = function
  | Return (v, sp) -> Return (Option.map f v, sp)
  | Jump _ as t -> t
  | Branch ({ cond; _ } as b) -> Branch { b with cond = f cond }
  | Switch ({ scrut; _ } as s) -> Switch { s with scrut = f scrut }
  | TailCall ({ callee; args; _ } as t) ->
      TailCall { t with callee = f callee; args = List.map f args }
  | Unreachable _ as t -> t

(* -------------------------------------------------------------------------- *)
(* Block / function iteration                                                 *)
(* -------------------------------------------------------------------------- *)

let block_all_instrs b = b.phis @ b.instrs

let iter_blocks f func = Label.Map.iter (fun _ b -> f b) func.blocks

let fold_blocks f acc func =
  Label.Map.fold (fun _ b acc -> f acc b) func.blocks acc

let func_labels f = Label.Map.bindings f.blocks |> List.map fst

let replace_value_in_instr ~(from : vreg) ~(to_ : value) instr =
  let on_use = function
    | VReg r when Vreg.equal r from -> to_
    | v -> v
  in
  map_instr_values ~on_use ~on_def:Fun.id instr

let replace_value_in_terminator ~(from : vreg) ~(to_ : value) term =
  map_terminator_values
    (function VReg r when Vreg.equal r from -> to_ | v -> v)
    term

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                            *)
(* -------------------------------------------------------------------------- *)

let pp_ty fmt ty = Format.pp_print_string fmt (ty_to_string ty)
let pp_value fmt v = Format.pp_print_string fmt (value_to_string v)

let pp_instr fmt = function
  | Assign { dst; src; ty; _ } ->
      Format.fprintf fmt "  %a : %a = %a" Vreg.pp dst pp_ty ty pp_value src
  | Binop { dst; op; lhs; rhs; ty; _ } ->
      Format.fprintf fmt "  %a : %a = %s %a, %a" Vreg.pp dst pp_ty ty
        (binop_to_string op) pp_value lhs pp_value rhs
  | Unop { dst; op; arg; ty; _ } ->
      Format.fprintf fmt "  %a : %a = %s %a" Vreg.pp dst pp_ty ty
        (unop_to_string op) pp_value arg
  | Call { dst; callee; args; ty; _ } ->
      (match dst with
      | Some d -> Format.fprintf fmt "  %a : %a = call %a(" Vreg.pp d pp_ty ty pp_value callee
      | None -> Format.fprintf fmt "  call %a(" pp_value callee);
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_value fmt args;
      Format.pp_print_string fmt ")"
  | Alloc { dst; tag; fields; ty; _ } ->
      Format.fprintf fmt "  %a : %a = alloc tag=%d [" Vreg.pp dst pp_ty ty tag;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_value fmt fields;
      Format.pp_print_string fmt "]"
  | Load { dst; ptr; ty; _ } ->
      Format.fprintf fmt "  %a : %a = load %a" Vreg.pp dst pp_ty ty pp_value ptr
  | Store { ptr; value; _ } ->
      Format.fprintf fmt "  store %a, %a" pp_value ptr pp_value value
  | GetField { dst; obj; index; ty; _ } ->
      Format.fprintf fmt "  %a : %a = getfield %a, %d" Vreg.pp dst pp_ty ty
        pp_value obj index
  | SetField { obj; index; value; _ } ->
      Format.fprintf fmt "  setfield %a, %d, %a" pp_value obj index pp_value value
  | Cast { dst; src; ty; _ } ->
      Format.fprintf fmt "  %a : %a = cast %a" Vreg.pp dst pp_ty ty pp_value src
  | Phi { dst; ty; incoming; _ } ->
      Format.fprintf fmt "  %a : %a = phi " Vreg.pp dst pp_ty ty;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt (l, v) ->
          Format.fprintf fmt "[%a: %a]" Label.pp l pp_value v)
        fmt incoming

let pp_terminator fmt = function
  | Return (None, _) -> Format.pp_print_string fmt "  ret"
  | Return (Some v, _) -> Format.fprintf fmt "  ret %a" pp_value v
  | Jump (l, _) -> Format.fprintf fmt "  jump %a" Label.pp l
  | Branch { cond; then_; else_; _ } ->
      Format.fprintf fmt "  branch %a, %a, %a" pp_value cond Label.pp then_
        Label.pp else_
  | Switch { scrut; cases; default; _ } ->
      Format.fprintf fmt "  switch %a" pp_value scrut;
      List.iter
        (fun (tag, l) -> Format.fprintf fmt " [%d -> %a]" tag Label.pp l)
        cases;
      Format.fprintf fmt " default %a" Label.pp default
  | TailCall { callee; args; _ } ->
      Format.fprintf fmt "  tailcall %a(" pp_value callee;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        pp_value fmt args;
      Format.pp_print_string fmt ")"
  | Unreachable _ -> Format.pp_print_string fmt "  unreachable"

let pp_block fmt b =
  Format.fprintf fmt "%a:\n" Label.pp b.label;
  List.iter
    (fun i ->
      pp_instr fmt i;
      Format.pp_print_newline fmt ())
    b.phis;
  List.iter
    (fun i ->
      pp_instr fmt i;
      Format.pp_print_newline fmt ())
    b.instrs;
  pp_terminator fmt b.terminator;
  Format.pp_print_newline fmt ()

let pp_func fmt f =
  Format.fprintf fmt "fun %a(" Ident.pp f.name;
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    (fun fmt (r, ty) -> Format.fprintf fmt "%a: %a" Vreg.pp r pp_ty ty)
    fmt f.params;
  Format.fprintf fmt ") -> %a {%s\n" pp_ty f.return_ty
    (if f.is_ssa then " ; ssa" else "");
  (* Print entry first, then others in label order. *)
  (match find_block f f.entry with
  | Some b -> pp_block fmt b
  | None -> ());
  Label.Map.iter
    (fun l b ->
      if not (Label.equal l f.entry) then pp_block fmt b)
    f.blocks;
  Format.pp_print_string fmt "}\n"

let pp_program fmt prog =
  Ident.Map.iter (fun _ f -> pp_func fmt f) prog.funcs

(** Count instructions across a function (phis + instrs, not terminators). *)
let func_instr_count f =
  fold_blocks
    (fun n b -> n + List.length b.phis + List.length b.instrs + 1)
    0 f

let program_funcs prog =
  Ident.Map.bindings prog.funcs |> List.map snd
