(** Glyph bytecode opcodes and instruction encoding. *)

type op =
  | Op_nop
  | Op_load_const
  | Op_move
  | Op_load_global
  | Op_store_global
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_mod
  | Op_add_f
  | Op_sub_f
  | Op_mul_f
  | Op_div_f
  | Op_neg
  | Op_neg_f
  | Op_not
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_eq_f
  | Op_ne_f
  | Op_lt_f
  | Op_le_f
  | Op_gt_f
  | Op_ge_f
  | Op_and
  | Op_or
  | Op_jump
  | Op_jump_if
  | Op_jump_if_not
  | Op_switch
  | Op_call
  | Op_call_closure
  | Op_tail_call
  | Op_tail_call_closure
  | Op_ret
  | Op_ret_void
  | Op_alloc_tuple
  | Op_alloc_adt
  | Op_alloc_closure
  | Op_get_field
  | Op_set_field
  | Op_get_tag
  | Op_tuple_get
  | Op_cons
  | Op_car
  | Op_cdr
  | Op_gc_safepoint
  | Op_print
  | Op_print_int
  | Op_print_string
  | Op_print_bool
  | Op_halt

type instr = {
  op : op;
  a : int;
  b : int;
  c : int;
  extra : int array;
}

let op_to_int = function
  | Op_nop -> 0
  | Op_load_const -> 1
  | Op_move -> 2
  | Op_load_global -> 3
  | Op_store_global -> 4
  | Op_add -> 5
  | Op_sub -> 6
  | Op_mul -> 7
  | Op_div -> 8
  | Op_mod -> 9
  | Op_add_f -> 10
  | Op_sub_f -> 11
  | Op_mul_f -> 12
  | Op_div_f -> 13
  | Op_neg -> 14
  | Op_neg_f -> 15
  | Op_not -> 16
  | Op_eq -> 17
  | Op_ne -> 18
  | Op_lt -> 19
  | Op_le -> 20
  | Op_gt -> 21
  | Op_ge -> 22
  | Op_eq_f -> 23
  | Op_ne_f -> 24
  | Op_lt_f -> 25
  | Op_le_f -> 26
  | Op_gt_f -> 27
  | Op_ge_f -> 28
  | Op_and -> 29
  | Op_or -> 30
  | Op_jump -> 31
  | Op_jump_if -> 32
  | Op_jump_if_not -> 33
  | Op_switch -> 34
  | Op_call -> 35
  | Op_call_closure -> 36
  | Op_tail_call -> 37
  | Op_tail_call_closure -> 38
  | Op_ret -> 39
  | Op_ret_void -> 40
  | Op_alloc_tuple -> 41
  | Op_alloc_adt -> 42
  | Op_alloc_closure -> 43
  | Op_get_field -> 44
  | Op_set_field -> 45
  | Op_get_tag -> 46
  | Op_tuple_get -> 47
  | Op_cons -> 48
  | Op_car -> 49
  | Op_cdr -> 50
  | Op_gc_safepoint -> 51
  | Op_print -> 52
  | Op_print_int -> 53
  | Op_print_string -> 54
  | Op_print_bool -> 55
  | Op_halt -> 56

let op_of_int = function
  | 0 -> Op_nop
  | 1 -> Op_load_const
  | 2 -> Op_move
  | 3 -> Op_load_global
  | 4 -> Op_store_global
  | 5 -> Op_add
  | 6 -> Op_sub
  | 7 -> Op_mul
  | 8 -> Op_div
  | 9 -> Op_mod
  | 10 -> Op_add_f
  | 11 -> Op_sub_f
  | 12 -> Op_mul_f
  | 13 -> Op_div_f
  | 14 -> Op_neg
  | 15 -> Op_neg_f
  | 16 -> Op_not
  | 17 -> Op_eq
  | 18 -> Op_ne
  | 19 -> Op_lt
  | 20 -> Op_le
  | 21 -> Op_gt
  | 22 -> Op_ge
  | 23 -> Op_eq_f
  | 24 -> Op_ne_f
  | 25 -> Op_lt_f
  | 26 -> Op_le_f
  | 27 -> Op_gt_f
  | 28 -> Op_ge_f
  | 29 -> Op_and
  | 30 -> Op_or
  | 31 -> Op_jump
  | 32 -> Op_jump_if
  | 33 -> Op_jump_if_not
  | 34 -> Op_switch
  | 35 -> Op_call
  | 36 -> Op_call_closure
  | 37 -> Op_tail_call
  | 38 -> Op_tail_call_closure
  | 39 -> Op_ret
  | 40 -> Op_ret_void
  | 41 -> Op_alloc_tuple
  | 42 -> Op_alloc_adt
  | 43 -> Op_alloc_closure
  | 44 -> Op_get_field
  | 45 -> Op_set_field
  | 46 -> Op_get_tag
  | 47 -> Op_tuple_get
  | 48 -> Op_cons
  | 49 -> Op_car
  | 50 -> Op_cdr
  | 51 -> Op_gc_safepoint
  | 52 -> Op_print
  | 53 -> Op_print_int
  | 54 -> Op_print_string
  | 55 -> Op_print_bool
  | 56 -> Op_halt
  | n -> invalid_arg (Printf.sprintf "Opcode.op_of_int: unknown %d" n)

let op_name = function
  | Op_nop -> "nop"
  | Op_load_const -> "load_const"
  | Op_move -> "move"
  | Op_load_global -> "load_global"
  | Op_store_global -> "store_global"
  | Op_add -> "add"
  | Op_sub -> "sub"
  | Op_mul -> "mul"
  | Op_div -> "div"
  | Op_mod -> "mod"
  | Op_add_f -> "add_f"
  | Op_sub_f -> "sub_f"
  | Op_mul_f -> "mul_f"
  | Op_div_f -> "div_f"
  | Op_neg -> "neg"
  | Op_neg_f -> "neg_f"
  | Op_not -> "not"
  | Op_eq -> "eq"
  | Op_ne -> "ne"
  | Op_lt -> "lt"
  | Op_le -> "le"
  | Op_gt -> "gt"
  | Op_ge -> "ge"
  | Op_eq_f -> "eq_f"
  | Op_ne_f -> "ne_f"
  | Op_lt_f -> "lt_f"
  | Op_le_f -> "le_f"
  | Op_gt_f -> "gt_f"
  | Op_ge_f -> "ge_f"
  | Op_and -> "and"
  | Op_or -> "or"
  | Op_jump -> "jump"
  | Op_jump_if -> "jump_if"
  | Op_jump_if_not -> "jump_if_not"
  | Op_switch -> "switch"
  | Op_call -> "call"
  | Op_call_closure -> "call_closure"
  | Op_tail_call -> "tail_call"
  | Op_tail_call_closure -> "tail_call_closure"
  | Op_ret -> "ret"
  | Op_ret_void -> "ret_void"
  | Op_alloc_tuple -> "alloc_tuple"
  | Op_alloc_adt -> "alloc_adt"
  | Op_alloc_closure -> "alloc_closure"
  | Op_get_field -> "get_field"
  | Op_set_field -> "set_field"
  | Op_get_tag -> "get_tag"
  | Op_tuple_get -> "tuple_get"
  | Op_cons -> "cons"
  | Op_car -> "car"
  | Op_cdr -> "cdr"
  | Op_gc_safepoint -> "gc_safepoint"
  | Op_print -> "print"
  | Op_print_int -> "print_int"
  | Op_print_string -> "print_string"
  | Op_print_bool -> "print_bool"
  | Op_halt -> "halt"

let op_arity_regs = function
  | Op_nop | Op_ret_void | Op_gc_safepoint -> 0
  | Op_jump | Op_ret | Op_print | Op_print_int | Op_print_string
  | Op_print_bool | Op_halt | Op_neg | Op_neg_f | Op_not | Op_car | Op_cdr
  | Op_get_tag ->
      1
  | Op_load_const | Op_move | Op_load_global | Op_store_global | Op_jump_if
  | Op_jump_if_not | Op_switch | Op_alloc_tuple | Op_tail_call
  | Op_tail_call_closure ->
      2
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod | Op_add_f | Op_sub_f
  | Op_mul_f | Op_div_f | Op_eq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
  | Op_eq_f | Op_ne_f | Op_lt_f | Op_le_f | Op_gt_f | Op_ge_f | Op_and
  | Op_or | Op_call | Op_call_closure | Op_alloc_adt | Op_alloc_closure
  | Op_get_field | Op_set_field | Op_tuple_get | Op_cons ->
      3

let make ?(a = 0) ?(b = 0) ?(c = 0) ?(extra = [||]) op = { op; a; b; c; extra }

let pp_op fmt op = Format.pp_print_string fmt (op_name op)

let pp_extra fmt arr =
  if Array.length arr = 0 then ()
  else (
    Format.fprintf fmt " [";
    Array.iteri
      (fun i x ->
        if i > 0 then Format.fprintf fmt ", ";
        Format.fprintf fmt "%d" x)
      arr;
    Format.fprintf fmt "]")

let pp_instr fmt (i : instr) =
  match i.op with
  | Op_nop | Op_ret_void | Op_gc_safepoint ->
      Format.fprintf fmt "%s" (op_name i.op)
  | Op_jump -> Format.fprintf fmt "%s %d" (op_name i.op) i.a
  | Op_ret | Op_print | Op_print_int | Op_print_string | Op_print_bool ->
      Format.fprintf fmt "%s r%d" (op_name i.op) i.a
  | Op_halt ->
      if i.b <> 0 then Format.fprintf fmt "halt r%d" i.a
      else Format.fprintf fmt "halt"
  | Op_load_const ->
      Format.fprintf fmt "r%d = const[%d]" i.a i.b
  | Op_move -> Format.fprintf fmt "r%d = r%d" i.a i.b
  | Op_load_global -> Format.fprintf fmt "r%d = global[%d]" i.a i.b
  | Op_store_global -> Format.fprintf fmt "global[%d] = r%d" i.a i.b
  | Op_jump_if -> Format.fprintf fmt "jump_if r%d, %d" i.a i.b
  | Op_jump_if_not -> Format.fprintf fmt "jump_if_not r%d, %d" i.a i.b
  | Op_neg | Op_neg_f | Op_not | Op_car | Op_cdr | Op_get_tag ->
      Format.fprintf fmt "r%d = %s r%d" i.a (op_name i.op) i.b
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod | Op_add_f | Op_sub_f
  | Op_mul_f | Op_div_f | Op_eq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
  | Op_eq_f | Op_ne_f | Op_lt_f | Op_le_f | Op_gt_f | Op_ge_f | Op_and
  | Op_or | Op_cons ->
      Format.fprintf fmt "r%d = r%d %s r%d" i.a i.b (op_name i.op) i.c
  | Op_get_field | Op_tuple_get ->
      Format.fprintf fmt "r%d = r%d[%d]" i.a i.b i.c
  | Op_set_field -> Format.fprintf fmt "r%d[%d] := r%d" i.a i.b i.c
  | Op_call ->
      Format.fprintf fmt "r%d = call fn%d/%d" i.a i.b i.c;
      pp_extra fmt i.extra
  | Op_call_closure ->
      Format.fprintf fmt "r%d = callclo r%d/%d" i.a i.b i.c;
      pp_extra fmt i.extra
  | Op_tail_call ->
      Format.fprintf fmt "tailcall fn%d/%d" i.a i.b;
      pp_extra fmt i.extra
  | Op_tail_call_closure ->
      Format.fprintf fmt "tailcallclo r%d/%d" i.a i.b;
      pp_extra fmt i.extra
  | Op_alloc_tuple ->
      Format.fprintf fmt "r%d = tuple/%d" i.a i.b;
      pp_extra fmt i.extra
  | Op_alloc_adt ->
      Format.fprintf fmt "r%d = adt tag=%d/%d" i.a i.b i.c;
      pp_extra fmt i.extra
  | Op_alloc_closure ->
      Format.fprintf fmt "r%d = closure fn%d/%d" i.a i.b i.c;
      pp_extra fmt i.extra
  | Op_switch ->
      Format.fprintf fmt "switch r%d, %d cases" i.a i.b;
      pp_extra fmt i.extra

(* Binary layout per instruction:
     u8  op
     u8  n_extra
     u16 a
     u16 b
     u16 c
     u16 extra[n_extra]
*)

let encode (i : instr) =
  let n = Array.length i.extra in
  let buf = Bytes.create (8 + (2 * n)) in
  Bytes.set_uint8 buf 0 (op_to_int i.op);
  Bytes.set_uint8 buf 1 n;
  Bytes.set_uint16_le buf 2 i.a;
  Bytes.set_uint16_le buf 4 i.b;
  Bytes.set_uint16_le buf 6 i.c;
  Array.iteri
    (fun idx v -> Bytes.set_uint16_le buf (8 + (2 * idx)) v)
    i.extra;
  buf

let decode buf off =
  let op = op_of_int (Bytes.get_uint8 buf off) in
  let n = Bytes.get_uint8 buf (off + 1) in
  let a = Bytes.get_uint16_le buf (off + 2) in
  let b = Bytes.get_uint16_le buf (off + 4) in
  let c = Bytes.get_uint16_le buf (off + 6) in
  let extra = Array.init n (fun i -> Bytes.get_uint16_le buf (off + 8 + (2 * i))) in
  ({ op; a; b; c; extra }, off + 8 + (2 * n))
