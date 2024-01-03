(** Bytecode chunk serialization and constant pool. *)

type const =
  | CInt of int
  | CFloat of float
  | CBool of bool
  | CChar of char
  | CUnit
  | CString of string

type func = {
  name : string;
  fn_id : int;
  arity : int;
  nregs : int;
  entry : int;
  is_main : bool;
}

type t = {
  mutable code : Opcode.instr array;
  mutable consts : const array;
  mutable funcs : func array;
  mutable globals : string array;
  main : int;
}

let magic = "GBC1"

let empty () =
  { code = [||]; consts = [||]; funcs = [||]; globals = [||]; main = 0 }

let create ~code ~consts ~funcs ?(globals = [||]) ~main () =
  { code; consts; funcs; globals; main }

let const_equal a b =
  match (a, b) with
  | CInt x, CInt y -> x = y
  | CFloat x, CFloat y -> Float.equal x y
  | CBool x, CBool y -> x = y
  | CChar x, CChar y -> x = y
  | CUnit, CUnit -> true
  | CString x, CString y -> String.equal x y
  | _ -> false

let add_const chunk c =
  let n = Array.length chunk.consts in
  match
    Array.find_index (fun x -> const_equal x c) chunk.consts
  with
  | Some i -> i
  | None ->
      chunk.consts <- Array.append chunk.consts [| c |];
      n

let find_func chunk id =
  try Array.find_opt (fun (f : func) -> f.fn_id = id) chunk.funcs |> Option.get
  with _ ->
    invalid_arg (Printf.sprintf "Chunk.find_func: fn%d missing" id)

let find_func_by_name chunk name =
  Array.find_opt (fun (f : func) -> String.equal f.name name) chunk.funcs

let func_count chunk = Array.length chunk.funcs
let code_length chunk = Array.length chunk.code

let pp_const fmt = function
  | CInt i -> Format.fprintf fmt "%d" i
  | CFloat f -> Format.fprintf fmt "%g" f
  | CBool b -> Format.fprintf fmt "%b" b
  | CChar c -> Format.fprintf fmt "%C" c
  | CUnit -> Format.fprintf fmt "()"
  | CString s -> Format.fprintf fmt "%S" s

let pp_func fmt (f : func) =
  Format.fprintf fmt "fn%d %s arity=%d nregs=%d entry=%d%s" f.fn_id f.name
    f.arity f.nregs f.entry
    (if f.is_main then " [main]" else "")

let pp fmt chunk =
  Format.fprintf fmt "; Glyph bytecode chunk — main=fn%d\n" chunk.main;
  Format.fprintf fmt "; %d constants, %d functions, %d instructions\n"
    (Array.length chunk.consts) (Array.length chunk.funcs)
    (Array.length chunk.code);
  Array.iteri
    (fun i c -> Format.fprintf fmt "  const[%d] = %a\n" i pp_const c)
    chunk.consts;
  Array.iter (fun f -> Format.fprintf fmt "  %a\n" pp_func f) chunk.funcs;
  Array.iteri
    (fun i instr -> Format.fprintf fmt "%4d: %a\n" i Opcode.pp_instr instr)
    chunk.code

(* ---- Binary I/O ------------------------------------------------------------ *)

let write_string_buf buf s =
  Buffer.add_int32_le buf (Int32.of_int (String.length s));
  Buffer.add_string buf s

let read_string bytes off =
  let len = Int32.to_int (Bytes.get_int32_le bytes off) in
  let s = Bytes.sub_string bytes (off + 4) len in
  (s, off + 4 + len)

let const_tag = function
  | CInt _ -> 1
  | CFloat _ -> 2
  | CBool _ -> 3
  | CChar _ -> 4
  | CUnit -> 5
  | CString _ -> 6

let write_const buf = function
  | CInt i ->
      Buffer.add_uint8 buf 1;
      Buffer.add_int64_le buf (Int64.of_int i)
  | CFloat f ->
      Buffer.add_uint8 buf 2;
      Buffer.add_int64_le buf (Int64.bits_of_float f)
  | CBool b ->
      Buffer.add_uint8 buf 3;
      Buffer.add_uint8 buf (if b then 1 else 0)
  | CChar c ->
      Buffer.add_uint8 buf 4;
      Buffer.add_uint8 buf (Char.code c)
  | CUnit -> Buffer.add_uint8 buf 5
  | CString s ->
      Buffer.add_uint8 buf 6;
      write_string_buf buf s

let read_const bytes off =
  let tag = Bytes.get_uint8 bytes off in
  match tag with
  | 1 ->
      let i = Int64.to_int (Bytes.get_int64_le bytes (off + 1)) in
      (CInt i, off + 9)
  | 2 ->
      let f = Int64.float_of_bits (Bytes.get_int64_le bytes (off + 1)) in
      (CFloat f, off + 9)
  | 3 -> (CBool (Bytes.get_uint8 bytes (off + 1) <> 0), off + 2)
  | 4 -> (CChar (Char.chr (Bytes.get_uint8 bytes (off + 1))), off + 2)
  | 5 -> (CUnit, off + 1)
  | 6 ->
      let s, off' = read_string bytes (off + 1) in
      (CString s, off')
  | n -> failwith (Printf.sprintf "Chunk.read_const: bad tag %d" n)

let to_bytes chunk =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf magic;
  Buffer.add_int32_le buf (Int32.of_int chunk.main);
  Buffer.add_int32_le buf (Int32.of_int (Array.length chunk.consts));
  Array.iter (write_const buf) chunk.consts;
  Buffer.add_int32_le buf (Int32.of_int (Array.length chunk.globals));
  Array.iter (write_string_buf buf) chunk.globals;
  Buffer.add_int32_le buf (Int32.of_int (Array.length chunk.funcs));
  Array.iter
    (fun (f : func) ->
      write_string_buf buf f.name;
      Buffer.add_int32_le buf (Int32.of_int f.fn_id);
      Buffer.add_int32_le buf (Int32.of_int f.arity);
      Buffer.add_int32_le buf (Int32.of_int f.nregs);
      Buffer.add_int32_le buf (Int32.of_int f.entry);
      Buffer.add_uint8 buf (if f.is_main then 1 else 0))
    chunk.funcs;
  Buffer.add_int32_le buf (Int32.of_int (Array.length chunk.code));
  Array.iter
    (fun instr -> Buffer.add_bytes buf (Opcode.encode instr))
    chunk.code;
  Buffer.to_bytes buf

let of_bytes bytes =
  if Bytes.length bytes < 4 || Bytes.sub_string bytes 0 4 <> magic then
    failwith "Chunk.of_bytes: bad magic";
  let off = ref 4 in
  let read_i32 () =
    let v = Int32.to_int (Bytes.get_int32_le bytes !off) in
    off := !off + 4;
    v
  in
  let main = read_i32 () in
  let nconsts = read_i32 () in
  let consts =
    Array.init nconsts (fun _ ->
        let c, off' = read_const bytes !off in
        off := off';
        c)
  in
  let nglobals = read_i32 () in
  let globals =
    Array.init nglobals (fun _ ->
        let s, off' = read_string bytes !off in
        off := off';
        s)
  in
  let nfuncs = read_i32 () in
  let funcs =
    Array.init nfuncs (fun _ ->
        let name, off' = read_string bytes !off in
        off := off';
        let fn_id = read_i32 () in
        let arity = read_i32 () in
        let nregs = read_i32 () in
        let entry = read_i32 () in
        let is_main = Bytes.get_uint8 bytes !off <> 0 in
        off := !off + 1;
        { name; fn_id; arity; nregs; entry; is_main })
  in
  let ncode = read_i32 () in
  let code =
    Array.init ncode (fun _ ->
        let instr, off' = Opcode.decode bytes !off in
        off := off';
        instr)
  in
  { code; consts; funcs; globals; main }

let write_file chunk path =
  let oc = open_out_bin path in
  output_bytes oc (to_bytes chunk);
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let bytes = Bytes.create len in
  really_input ic bytes 0 len;
  close_in ic;
  of_bytes bytes
