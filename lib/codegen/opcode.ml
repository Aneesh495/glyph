(** Glyph bytecode ISA.

    Register-based instructions. Each opcode packs into one or more [int32]
    words via [encode] / [decode]. The interpreter usually keeps the structured
    [opcode] form; encoding is for serialization and the constant pool layout.
*)

type reg = int
type const_idx = int
type proto_id = int
type offset = int

type opcode =
  (* Moves / constants *)
  | LoadConst of reg * const_idx
  | Move of reg * reg
  (* Integer arithmetic *)
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Mod of reg * reg * reg
  | Neg of reg * reg
  (* Float arithmetic *)
  | AddF of reg * reg * reg
  | SubF of reg * reg * reg
  | MulF of reg * reg * reg
  | DivF of reg * reg * reg
  | NegF of reg * reg
  (* Comparisons → bool in dst *)
  | Eq of reg * reg * reg
  | Ne of reg * reg * reg
  | Lt of reg * reg * reg
  | Le of reg * reg * reg
  | Gt of reg * reg * reg
  | Ge of reg * reg * reg
  | EqF of reg * reg * reg
  | NeF of reg * reg * reg
  | LtF of reg * reg * reg
  | LeF of reg * reg * reg
  | GtF of reg * reg * reg
  | GeF of reg * reg * reg
  (* Boolean *)
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Not of reg * reg
  (* Control flow — offsets are absolute instruction indices *)
  | Jump of offset
  | JumpIf of reg * offset
  | JumpIfNot of reg * offset
  | Switch of reg * offset list * offset
      (** [Switch (scrutinee, case_targets, default)] — scrutinee is int tag. *)
  (* Calls *)
  | Call of reg * proto_id * reg list
      (** [Call (dst, proto, args)] *)
  | TailCall of proto_id * reg list
  | CallClosure of reg * reg * reg list
      (** [CallClosure (dst, clo_reg, args)] *)
  | TailCallClosure of reg * reg list
  | Ret of reg option
  (* Heap *)
  | Alloc of reg * int * int
      (** [Alloc (dst, tag, nfields)] — fields filled via subsequent SetField
          or loaded from consecutive arg regs by the emitter convention:
          fields are in registers [dst+1 ..] is NOT used; emitter emits
          Alloc then SetField. For efficiency we also support AllocArgs. *)
  | AllocArgs of reg * int * reg list
      (** Allocate ADT/tuple with tag and field registers. *)
  | GetField of reg * reg * int
  | SetField of reg * int * reg
  | MakeClosure of reg * proto_id * reg list
  | TupleGet of reg * reg * int
  (* List-style ops (cons cell = ADT tag 1 with 2 fields) *)
  | Cons of reg * reg * reg
  | Car of reg * reg
  | Cdr of reg * reg
  (* Misc *)
  | Print of reg
  | Halt of reg option
  | Nop

(* -------------------------------------------------------------------------- *)
(* Opcode tags for encoding                                                   *)
(* -------------------------------------------------------------------------- *)

let tag_of = function
  | LoadConst _ -> 1
  | Move _ -> 2
  | Add _ -> 3
  | Sub _ -> 4
  | Mul _ -> 5
  | Div _ -> 6
  | Mod _ -> 7
  | Neg _ -> 8
  | AddF _ -> 9
  | SubF _ -> 10
  | MulF _ -> 11
  | DivF _ -> 12
  | NegF _ -> 13
  | Eq _ -> 14
  | Ne _ -> 15
  | Lt _ -> 16
  | Le _ -> 17
  | Gt _ -> 18
  | Ge _ -> 19
  | EqF _ -> 20
  | NeF _ -> 21
  | LtF _ -> 22
  | LeF _ -> 23
  | GtF _ -> 24
  | GeF _ -> 25
  | And _ -> 26
  | Or _ -> 27
  | Not _ -> 28
  | Jump _ -> 29
  | JumpIf _ -> 30
  | JumpIfNot _ -> 31
  | Switch _ -> 32
  | Call _ -> 33
  | TailCall _ -> 34
  | CallClosure _ -> 35
  | TailCallClosure _ -> 36
  | Ret _ -> 37
  | Alloc _ -> 38
  | AllocArgs _ -> 39
  | GetField _ -> 40
  | SetField _ -> 41
  | MakeClosure _ -> 42
  | TupleGet _ -> 43
  | Cons _ -> 44
  | Car _ -> 45
  | Cdr _ -> 46
  | Print _ -> 47
  | Halt _ -> 48
  | Nop -> 49

let name_of_tag = function
  | 1 -> "LoadConst"
  | 2 -> "Move"
  | 3 -> "Add"
  | 4 -> "Sub"
  | 5 -> "Mul"
  | 6 -> "Div"
  | 7 -> "Mod"
  | 8 -> "Neg"
  | 9 -> "AddF"
  | 10 -> "SubF"
  | 11 -> "MulF"
  | 12 -> "DivF"
  | 13 -> "NegF"
  | 14 -> "Eq"
  | 15 -> "Ne"
  | 16 -> "Lt"
  | 17 -> "Le"
  | 18 -> "Gt"
  | 19 -> "Ge"
  | 20 -> "EqF"
  | 21 -> "NeF"
  | 22 -> "LtF"
  | 23 -> "LeF"
  | 24 -> "GtF"
  | 25 -> "GeF"
  | 26 -> "And"
  | 27 -> "Or"
  | 28 -> "Not"
  | 29 -> "Jump"
  | 30 -> "JumpIf"
  | 31 -> "JumpIfNot"
  | 32 -> "Switch"
  | 33 -> "Call"
  | 34 -> "TailCall"
  | 35 -> "CallClosure"
  | 36 -> "TailCallClosure"
  | 37 -> "Ret"
  | 38 -> "Alloc"
  | 39 -> "AllocArgs"
  | 40 -> "GetField"
  | 41 -> "SetField"
  | 42 -> "MakeClosure"
  | 43 -> "TupleGet"
  | 44 -> "Cons"
  | 45 -> "Car"
  | 46 -> "Cdr"
  | 47 -> "Print"
  | 48 -> "Halt"
  | 49 -> "Nop"
  | n -> Printf.sprintf "Unknown(%d)" n

let name_of op = name_of_tag (tag_of op)

(* Packing helpers: word0 = tag | a<<8 | b<<16 | c<<24 (each field 8 bits).
   Wider immediates and register lists follow as extra int32 words. *)

let pack3 tag a b c : int32 =
  Int32.(
    logor
      (of_int (tag land 0xff))
      (logor
         (shift_left (of_int (a land 0xff)) 8)
         (logor
            (shift_left (of_int (b land 0xff)) 16)
            (shift_left (of_int (c land 0xff)) 24))))

let pack2 tag a b = pack3 tag a b 0
let pack1 tag a = pack3 tag a 0 0
let pack0 tag = pack3 tag 0 0 0

let unpack_tag (w : int32) = Int32.to_int w land 0xff
let unpack_a w = Int32.(to_int (shift_right_logical w 8) land 0xff)
let unpack_b w = Int32.(to_int (shift_right_logical w 16) land 0xff)
let unpack_c w = Int32.(to_int (shift_right_logical w 24) land 0xff)

let i32 n = Int32.of_int n
let of_i32 w = Int32.to_int w

(** Encode one opcode to a list of int32 words (length ≥ 1). *)
let encode (op : opcode) : int32 list =
  let regs_words regs =
    let n = List.length regs in
    i32 n :: List.map i32 regs
  in
  match op with
  | LoadConst (dst, ci) -> [ pack2 1 dst (ci land 0xff); i32 ci ]
  | Move (dst, src) -> [ pack2 2 dst src ]
  | Add (d, a, b) -> [ pack3 3 d a b ]
  | Sub (d, a, b) -> [ pack3 4 d a b ]
  | Mul (d, a, b) -> [ pack3 5 d a b ]
  | Div (d, a, b) -> [ pack3 6 d a b ]
  | Mod (d, a, b) -> [ pack3 7 d a b ]
  | Neg (d, a) -> [ pack2 8 d a ]
  | AddF (d, a, b) -> [ pack3 9 d a b ]
  | SubF (d, a, b) -> [ pack3 10 d a b ]
  | MulF (d, a, b) -> [ pack3 11 d a b ]
  | DivF (d, a, b) -> [ pack3 12 d a b ]
  | NegF (d, a) -> [ pack2 13 d a ]
  | Eq (d, a, b) -> [ pack3 14 d a b ]
  | Ne (d, a, b) -> [ pack3 15 d a b ]
  | Lt (d, a, b) -> [ pack3 16 d a b ]
  | Le (d, a, b) -> [ pack3 17 d a b ]
  | Gt (d, a, b) -> [ pack3 18 d a b ]
  | Ge (d, a, b) -> [ pack3 19 d a b ]
  | EqF (d, a, b) -> [ pack3 20 d a b ]
  | NeF (d, a, b) -> [ pack3 21 d a b ]
  | LtF (d, a, b) -> [ pack3 22 d a b ]
  | LeF (d, a, b) -> [ pack3 23 d a b ]
  | GtF (d, a, b) -> [ pack3 24 d a b ]
  | GeF (d, a, b) -> [ pack3 25 d a b ]
  | And (d, a, b) -> [ pack3 26 d a b ]
  | Or (d, a, b) -> [ pack3 27 d a b ]
  | Not (d, a) -> [ pack2 28 d a ]
  | Jump off -> [ pack0 29; i32 off ]
  | JumpIf (r, off) -> [ pack1 30 r; i32 off ]
  | JumpIfNot (r, off) -> [ pack1 31 r; i32 off ]
  | Switch (r, cases, def) ->
      pack1 32 r :: i32 (List.length cases) :: i32 def
      :: List.map i32 cases
  | Call (dst, proto, args) ->
      pack2 33 dst (proto land 0xff) :: i32 proto :: regs_words args
  | TailCall (proto, args) ->
      pack1 34 (proto land 0xff) :: i32 proto :: regs_words args
  | CallClosure (dst, clo, args) ->
      pack2 35 dst clo :: regs_words args
  | TailCallClosure (clo, args) -> pack1 36 clo :: regs_words args
  | Ret None -> [ pack1 37 0 ]
  | Ret (Some r) -> [ pack2 37 1 r ]
  | Alloc (dst, tag, n) -> [ pack3 38 dst (tag land 0xff) (n land 0xff); i32 tag; i32 n ]
  | AllocArgs (dst, tag, fields) ->
      pack2 39 dst (tag land 0xff) :: i32 tag :: regs_words fields
  | GetField (dst, obj, i) -> [ pack3 40 dst obj i ]
  | SetField (obj, i, v) -> [ pack3 41 obj i v ]
  | MakeClosure (dst, proto, env) ->
      pack2 42 dst (proto land 0xff) :: i32 proto :: regs_words env
  | TupleGet (dst, tup, i) -> [ pack3 43 dst tup i ]
  | Cons (dst, h, t) -> [ pack3 44 dst h t ]
  | Car (dst, c) -> [ pack2 45 dst c ]
  | Cdr (dst, c) -> [ pack2 46 dst c ]
  | Print r -> [ pack1 47 r ]
  | Halt None -> [ pack1 48 0 ]
  | Halt (Some r) -> [ pack2 48 1 r ]
  | Nop -> [ pack0 49 ]

(** Decode one opcode starting at [words.(idx)]. Returns [(opcode, next_idx)]. *)
let decode_at (words : int32 array) (idx : int) : opcode * int =
  let w = words.(idx) in
  let tag = unpack_tag w in
  let a = unpack_a w in
  let b = unpack_b w in
  let c = unpack_c w in
  let get i = of_i32 words.(idx + i) in
  let read_regs start =
    let n = get start in
    let rec loop i acc =
      if i >= n then List.rev acc
      else loop (i + 1) (get (start + 1 + i) :: acc)
    in
    (loop 0 [], start + 1 + n)
  in
  match tag with
  | 1 -> (LoadConst (a, get 1), idx + 2)
  | 2 -> (Move (a, b), idx + 1)
  | 3 -> (Add (a, b, c), idx + 1)
  | 4 -> (Sub (a, b, c), idx + 1)
  | 5 -> (Mul (a, b, c), idx + 1)
  | 6 -> (Div (a, b, c), idx + 1)
  | 7 -> (Mod (a, b, c), idx + 1)
  | 8 -> (Neg (a, b), idx + 1)
  | 9 -> (AddF (a, b, c), idx + 1)
  | 10 -> (SubF (a, b, c), idx + 1)
  | 11 -> (MulF (a, b, c), idx + 1)
  | 12 -> (DivF (a, b, c), idx + 1)
  | 13 -> (NegF (a, b), idx + 1)
  | 14 -> (Eq (a, b, c), idx + 1)
  | 15 -> (Ne (a, b, c), idx + 1)
  | 16 -> (Lt (a, b, c), idx + 1)
  | 17 -> (Le (a, b, c), idx + 1)
  | 18 -> (Gt (a, b, c), idx + 1)
  | 19 -> (Ge (a, b, c), idx + 1)
  | 20 -> (EqF (a, b, c), idx + 1)
  | 21 -> (NeF (a, b, c), idx + 1)
  | 22 -> (LtF (a, b, c), idx + 1)
  | 23 -> (LeF (a, b, c), idx + 1)
  | 24 -> (GtF (a, b, c), idx + 1)
  | 25 -> (GeF (a, b, c), idx + 1)
  | 26 -> (And (a, b, c), idx + 1)
  | 27 -> (Or (a, b, c), idx + 1)
  | 28 -> (Not (a, b), idx + 1)
  | 29 -> (Jump (get 1), idx + 2)
  | 30 -> (JumpIf (a, get 1), idx + 2)
  | 31 -> (JumpIfNot (a, get 1), idx + 2)
  | 32 ->
      let ncases = get 1 in
      let def = get 2 in
      let rec cases i acc =
        if i >= ncases then List.rev acc
        else cases (i + 1) (get (3 + i) :: acc)
      in
      (Switch (a, cases 0 [], def), idx + 3 + ncases)
  | 33 ->
      let proto = get 1 in
      let args, next = read_regs 2 in
      (Call (a, proto, args), idx + next)
  | 34 ->
      let proto = get 1 in
      let args, next = read_regs 2 in
      (TailCall (proto, args), idx + next)
  | 35 ->
      let args, next = read_regs 1 in
      (CallClosure (a, b, args), idx + next)
  | 36 ->
      let args, next = read_regs 1 in
      (TailCallClosure (a, args), idx + next)
  | 37 ->
      if a = 0 then (Ret None, idx + 1) else (Ret (Some b), idx + 1)
  | 38 -> (Alloc (a, get 1, get 2), idx + 3)
  | 39 ->
      let tag = get 1 in
      let fields, next = read_regs 2 in
      (AllocArgs (a, tag, fields), idx + next)
  | 40 -> (GetField (a, b, c), idx + 1)
  | 41 -> (SetField (a, b, c), idx + 1)
  | 42 ->
      let proto = get 1 in
      let env, next = read_regs 2 in
      (MakeClosure (a, proto, env), idx + next)
  | 43 -> (TupleGet (a, b, c), idx + 1)
  | 44 -> (Cons (a, b, c), idx + 1)
  | 45 -> (Car (a, b), idx + 1)
  | 46 -> (Cdr (a, b), idx + 1)
  | 47 -> (Print a, idx + 1)
  | 48 ->
      if a = 0 then (Halt None, idx + 1) else (Halt (Some b), idx + 1)
  | 49 -> (Nop, idx + 1)
  | t -> failwith (Printf.sprintf "Opcode.decode: bad tag %d at %d" t idx)

let decode_all (words : int32 array) : opcode array =
  let out = ref [] in
  let i = ref 0 in
  let n = Array.length words in
  while !i < n do
    let op, next = decode_at words !i in
    out := op :: !out;
    i := next
  done;
  Array.of_list (List.rev !out)

let encode_all (ops : opcode array) : int32 array =
  Array.to_list ops |> List.concat_map encode |> Array.of_list

let encoded_size op = List.length (encode op)

(* -------------------------------------------------------------------------- *)
(* Pretty-printing                                                            *)
(* -------------------------------------------------------------------------- *)

let pp_reg fmt r = Format.fprintf fmt "r%d" r

let pp_regs fmt regs =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f ", ")
       pp_reg)
    regs

let pp_opcode fmt op =
  match op with
  | LoadConst (d, ci) ->
      Format.fprintf fmt "LoadConst %a, const[%d]" pp_reg d ci
  | Move (d, s) -> Format.fprintf fmt "Move %a, %a" pp_reg d pp_reg s
  | Add (d, a, b) ->
      Format.fprintf fmt "Add %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Sub (d, a, b) ->
      Format.fprintf fmt "Sub %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Mul (d, a, b) ->
      Format.fprintf fmt "Mul %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Div (d, a, b) ->
      Format.fprintf fmt "Div %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Mod (d, a, b) ->
      Format.fprintf fmt "Mod %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Neg (d, a) -> Format.fprintf fmt "Neg %a, %a" pp_reg d pp_reg a
  | AddF (d, a, b) ->
      Format.fprintf fmt "AddF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | SubF (d, a, b) ->
      Format.fprintf fmt "SubF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | MulF (d, a, b) ->
      Format.fprintf fmt "MulF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | DivF (d, a, b) ->
      Format.fprintf fmt "DivF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | NegF (d, a) -> Format.fprintf fmt "NegF %a, %a" pp_reg d pp_reg a
  | Eq (d, a, b) ->
      Format.fprintf fmt "Eq %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Ne (d, a, b) ->
      Format.fprintf fmt "Ne %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Lt (d, a, b) ->
      Format.fprintf fmt "Lt %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Le (d, a, b) ->
      Format.fprintf fmt "Le %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Gt (d, a, b) ->
      Format.fprintf fmt "Gt %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Ge (d, a, b) ->
      Format.fprintf fmt "Ge %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | EqF (d, a, b) ->
      Format.fprintf fmt "EqF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | NeF (d, a, b) ->
      Format.fprintf fmt "NeF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | LtF (d, a, b) ->
      Format.fprintf fmt "LtF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | LeF (d, a, b) ->
      Format.fprintf fmt "LeF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | GtF (d, a, b) ->
      Format.fprintf fmt "GtF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | GeF (d, a, b) ->
      Format.fprintf fmt "GeF %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | And (d, a, b) ->
      Format.fprintf fmt "And %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Or (d, a, b) ->
      Format.fprintf fmt "Or %a, %a, %a" pp_reg d pp_reg a pp_reg b
  | Not (d, a) -> Format.fprintf fmt "Not %a, %a" pp_reg d pp_reg a
  | Jump off -> Format.fprintf fmt "Jump %d" off
  | JumpIf (r, off) -> Format.fprintf fmt "JumpIf %a, %d" pp_reg r off
  | JumpIfNot (r, off) ->
      Format.fprintf fmt "JumpIfNot %a, %d" pp_reg r off
  | Switch (r, cases, def) ->
      Format.fprintf fmt "Switch %a, cases=[" pp_reg r;
      List.iteri
        (fun i t ->
          if i > 0 then Format.fprintf fmt ", ";
          Format.fprintf fmt "%d" t)
        cases;
      Format.fprintf fmt "], default=%d" def
  | Call (d, p, args) ->
      Format.fprintf fmt "Call %a, proto%d %a" pp_reg d p pp_regs args
  | TailCall (p, args) ->
      Format.fprintf fmt "TailCall proto%d %a" p pp_regs args
  | CallClosure (d, clo, args) ->
      Format.fprintf fmt "CallClosure %a, %a %a" pp_reg d pp_reg clo
        pp_regs args
  | TailCallClosure (clo, args) ->
      Format.fprintf fmt "TailCallClosure %a %a" pp_reg clo pp_regs args
  | Ret None -> Format.fprintf fmt "Ret"
  | Ret (Some r) -> Format.fprintf fmt "Ret %a" pp_reg r
  | Alloc (d, tag, n) ->
      Format.fprintf fmt "Alloc %a, tag=%d, n=%d" pp_reg d tag n
  | AllocArgs (d, tag, fields) ->
      Format.fprintf fmt "AllocArgs %a, tag=%d %a" pp_reg d tag pp_regs
        fields
  | GetField (d, obj, i) ->
      Format.fprintf fmt "GetField %a, %a, %d" pp_reg d pp_reg obj i
  | SetField (obj, i, v) ->
      Format.fprintf fmt "SetField %a, %d, %a" pp_reg obj i pp_reg v
  | MakeClosure (d, p, env) ->
      Format.fprintf fmt "MakeClosure %a, proto%d %a" pp_reg d p pp_regs
        env
  | TupleGet (d, t, i) ->
      Format.fprintf fmt "TupleGet %a, %a, %d" pp_reg d pp_reg t i
  | Cons (d, h, t) ->
      Format.fprintf fmt "Cons %a, %a, %a" pp_reg d pp_reg h pp_reg t
  | Car (d, c) -> Format.fprintf fmt "Car %a, %a" pp_reg d pp_reg c
  | Cdr (d, c) -> Format.fprintf fmt "Cdr %a, %a" pp_reg d pp_reg c
  | Print r -> Format.fprintf fmt "Print %a" pp_reg r
  | Halt None -> Format.fprintf fmt "Halt"
  | Halt (Some r) -> Format.fprintf fmt "Halt %a" pp_reg r
  | Nop -> Format.fprintf fmt "Nop"

let to_string op =
  Format.asprintf "%a" pp_opcode op

(** Whether this opcode transfers control and does not fall through. *)
let is_terminator = function
  | Jump _ | Switch _ | TailCall _ | TailCallClosure _ | Ret _ | Halt _
    ->
      true
  | JumpIf _ | JumpIfNot _ -> false
  | _ -> false

let max_reg_used op =
  let m = ref (-1) in
  let touch r = if r > !m then m := r in
  let touches = List.iter touch in
  (match op with
  | LoadConst (d, _) | Neg (d, _) | NegF (d, _) | Not (d, _) | Print d
  | JumpIf (d, _) | JumpIfNot (d, _) | Car (d, _) | Cdr (d, _)
  | Alloc (d, _, _) | Switch (d, _, _) ->
      touch d
  | Move (d, s) | GetField (d, s, _) | TupleGet (d, s, _) ->
      touch d;
      touch s
  | Add (d, a, b)
  | Sub (d, a, b)
  | Mul (d, a, b)
  | Div (d, a, b)
  | Mod (d, a, b)
  | AddF (d, a, b)
  | SubF (d, a, b)
  | MulF (d, a, b)
  | DivF (d, a, b)
  | Eq (d, a, b)
  | Ne (d, a, b)
  | Lt (d, a, b)
  | Le (d, a, b)
  | Gt (d, a, b)
  | Ge (d, a, b)
  | EqF (d, a, b)
  | NeF (d, a, b)
  | LtF (d, a, b)
  | LeF (d, a, b)
  | GtF (d, a, b)
  | GeF (d, a, b)
  | And (d, a, b)
  | Or (d, a, b)
  | Cons (d, a, b)
  | SetField (d, _, b) ->
      touch d;
      touch a;
      touch b
  | Call (d, _, args) | CallClosure (d, _, args) | AllocArgs (d, _, args)
  | MakeClosure (d, _, args) ->
      touch d;
      touches args
  | CallClosure (_, clo, _) as _ when false -> ()
  | CallClosure _ -> () (* handled above *)
  | TailCall (_, args) | TailCallClosure (_, args) -> touches args
  | TailCallClosure (clo, args) ->
      touch clo;
      touches args
  | Ret (Some r) | Halt (Some r) -> touch r
  | Ret None | Halt None | Jump _ | Nop -> ()
  | GetField _ | TupleGet _ | Move _ | LoadConst _ | Neg _ | NegF _
  | Not _ | Print _ | JumpIf _ | JumpIfNot _ | Car _ | Cdr _ | Alloc _
  | Switch _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | AddF _ | SubF _
  | MulF _ | DivF _ | Eq _ | Ne _ | Lt _ | Le _ | Gt _ | Ge _ | EqF _
  | NeF _ | LtF _ | LeF _ | GtF _ | GeF _ | And _ | Or _ | Cons _
  | SetField _ | Call _ | CallClosure _ | AllocArgs _ | MakeClosure _
    ->
      () (* exhaustiveness for already-handled *; keep compiler quiet *));
  (* Re-do cleanly without the broken match: *)
  m := -1;
  (match op with
  | LoadConst (d, _) -> touch d
  | Move (d, s) ->
      touch d;
      touch s
  | Add (d, a, b)
  | Sub (d, a, b)
  | Mul (d, a, b)
  | Div (d, a, b)
  | Mod (d, a, b)
  | AddF (d, a, b)
  | SubF (d, a, b)
  | MulF (d, a, b)
  | DivF (d, a, b)
  | Eq (d, a, b)
  | Ne (d, a, b)
  | Lt (d, a, b)
  | Le (d, a, b)
  | Gt (d, a, b)
  | Ge (d, a, b)
  | EqF (d, a, b)
  | NeF (d, a, b)
  | LtF (d, a, b)
  | LeF (d, a, b)
  | GtF (d, a, b)
  | GeF (d, a, b)
  | And (d, a, b)
  | Or (d, a, b)
  | Cons (d, a, b) ->
      touch d;
      touch a;
      touch b
  | Neg (d, a) | NegF (d, a) | Not (d, a) | Car (d, a) | Cdr (d, a) ->
      touch d;
      touch a
  | JumpIf (r, _) | JumpIfNot (r, _) | Print r | Switch (r, _, _) ->
      touch r
  | Jump _ | Nop -> ()
  | Call (d, _, args) ->
      touch d;
      touches args
  | TailCall (_, args) -> touches args
  | CallClosure (d, clo, args) ->
      touch d;
      touch clo;
      touches args
  | TailCallClosure (clo, args) ->
      touch clo;
      touches args
  | Ret (Some r) | Halt (Some r) -> touch r
  | Ret None | Halt None -> ()
  | Alloc (d, _, _) -> touch d
  | AllocArgs (d, _, fields) ->
      touch d;
      touches fields
  | GetField (d, obj, _) | TupleGet (d, obj, _) ->
      touch d;
      touch obj
  | SetField (obj, _, v) ->
      touch obj;
      touch v
  | MakeClosure (d, _, env) ->
      touch d;
      touches env);
  !m
