(** Runtime values for the Glyph VM.

    Values are an OCaml ADT for clarity (not bit-stealing tagged pointers),
    but the shape mirrors a tagged representation:

    - Immediate payload tags: int / float / bool / char / unit
    - [Ptr addr] — heap reference (address into the semi-space heap)

    GC never moves immediates; only [Ptr] roots are evacuated.
*)

type t =
  | ImmInt of int
  | ImmFloat of float
  | ImmBool of bool
  | ImmChar of char
  | ImmUnit
  | Ptr of int
      (** Heap address (index into the active space). *)

let unit = ImmUnit
let int i = ImmInt i
let float f = ImmFloat f
let bool b = ImmBool b
let char c = ImmChar c
let ptr addr = Ptr addr

let is_ptr = function Ptr _ -> true | _ -> false
let is_imm = function Ptr _ -> false | _ -> true

let as_ptr = function
  | Ptr a -> a
  | _ -> invalid_arg "Value.as_ptr: not a pointer"

let as_int = function
  | ImmInt i -> i
  | _ -> invalid_arg "Value.as_int"

let as_float = function
  | ImmFloat f -> f
  | _ -> invalid_arg "Value.as_float"

let as_bool = function
  | ImmBool b -> b
  | _ -> invalid_arg "Value.as_bool"

let as_char = function
  | ImmChar c -> c
  | _ -> invalid_arg "Value.as_char"

let truthy = function
  | ImmBool b -> b
  | ImmInt 0 -> false
  | ImmUnit -> false
  | ImmInt _ | ImmFloat _ | ImmChar _ | Ptr _ -> true

let equal a b =
  match (a, b) with
  | ImmInt x, ImmInt y -> x = y
  | ImmFloat x, ImmFloat y -> Float.equal x y
  | ImmBool x, ImmBool y -> x = y
  | ImmChar x, ImmChar y -> x = y
  | ImmUnit, ImmUnit -> true
  | Ptr x, Ptr y -> x = y
  | _ -> false

let compare a b =
  match (a, b) with
  | ImmInt x, ImmInt y -> Int.compare x y
  | ImmFloat x, ImmFloat y -> Float.compare x y
  | ImmBool x, ImmBool y -> Bool.compare x y
  | ImmChar x, ImmChar y -> Char.compare x y
  | ImmUnit, ImmUnit -> 0
  | Ptr x, Ptr y -> Int.compare x y
  | ImmInt _, _ -> -1
  | _, ImmInt _ -> 1
  | ImmFloat _, _ -> -1
  | _, ImmFloat _ -> 1
  | ImmBool _, _ -> -1
  | _, ImmBool _ -> 1
  | ImmChar _, _ -> -1
  | _, ImmChar _ -> 1
  | ImmUnit, _ -> -1
  | _, ImmUnit -> 1

let to_string = function
  | ImmInt i -> string_of_int i
  | ImmFloat f -> string_of_float f
  | ImmBool b -> string_of_bool b
  | ImmChar c -> String.make 1 c
  | ImmUnit -> "()"
  | Ptr a -> Printf.sprintf "<ptr:%d>" a

let pp fmt v = Format.pp_print_string fmt (to_string v)

(** Tag bits as if we used a low-bit tagged representation (documentation /
    debugging aid — not used for actual storage). *)
type tag_kind =
  | Tag_int
  | Tag_float
  | Tag_bool
  | Tag_char
  | Tag_unit
  | Tag_ptr

let tag_kind = function
  | ImmInt _ -> Tag_int
  | ImmFloat _ -> Tag_float
  | ImmBool _ -> Tag_bool
  | ImmChar _ -> Tag_char
  | ImmUnit -> Tag_unit
  | Ptr _ -> Tag_ptr

let tag_kind_name = function
  | Tag_int -> "int"
  | Tag_float -> "float"
  | Tag_bool -> "bool"
  | Tag_char -> "char"
  | Tag_unit -> "unit"
  | Tag_ptr -> "ptr"

(** Hypothetical machine word encoding (for tests / docs).
    Immediates: [value << 3 | tag]; pointers: [addr << 3 | 0b001]. *)
let fake_encode = function
  | ImmInt i -> Int64.(logor (shift_left (of_int i) 3) 0b000L)
  | ImmFloat f ->
      (* Store float bits with tag 0b010 — truncated illustration. *)
      Int64.(logor (logand (bits_of_float f) (shift_left minus_one 3)) 0b010L)
  | ImmBool false -> 0b011L
  | ImmBool true -> Int64.(logor (shift_left 1L 3) 0b011L)
  | ImmChar c ->
      Int64.(logor (shift_left (of_int (Char.code c)) 3) 0b100L)
  | ImmUnit -> 0b101L
  | Ptr a -> Int64.(logor (shift_left (of_int a) 3) 0b001L)

let of_chunk_const = function
  | Chunk.CInt i -> ImmInt i
  | Chunk.CFloat f -> ImmFloat f
  | Chunk.CBool b -> ImmBool b
  | Chunk.CChar c -> ImmChar c
  | Chunk.CUnit -> ImmUnit
  | Chunk.CString _ ->
      (* Strings must be heap-allocated by the loader/interpreter. *)
      invalid_arg "Value.of_chunk_const: string requires heap"
  | Chunk.CFn _ ->
      invalid_arg "Value.of_chunk_const: fn ref requires closure"

(** Hash for use in tables (structural for imms, address for ptrs). *)
let hash = function
  | ImmInt i -> Hashtbl.hash (0, i)
  | ImmFloat f -> Hashtbl.hash (1, f)
  | ImmBool b -> Hashtbl.hash (2, b)
  | ImmChar c -> Hashtbl.hash (3, c)
  | ImmUnit -> Hashtbl.hash 4
  | Ptr a -> Hashtbl.hash (5, a)

module Tbl = Hashtbl.Make (struct
  type nonrec t = t
  let equal = equal
  let hash = hash
end)

(** Map a pointer address through a relocation function (used by GC). *)
let relocate f = function
  | Ptr a -> Ptr (f a)
  | v -> v

let iter_ptr f = function Ptr a -> f a | _ -> ()
