(** Disassembler for Glyph bytecode chunks. *)

let const_summary chunk idx =
  if idx < 0 || idx >= Array.length chunk.Chunk.consts then "?"
  else Format.asprintf "%a" Chunk.pp_const chunk.consts.(idx)

let annotate chunk (i : Opcode.instr) =
  match i.op with
  | Opcode.Op_load_const ->
      Printf.sprintf "  ; %s" (const_summary chunk i.b)
  | Opcode.Op_call -> (
      match
        Array.find_opt (fun (f : Chunk.func) -> f.fn_id = i.b) chunk.funcs
      with
      | Some f -> Printf.sprintf "  ; %s" f.name
      | None -> "")
  | _ -> ""

let disasm_instr chunk ip (i : Opcode.instr) =
  let base = Format.asprintf "%a" Opcode.pp_instr i in
  Printf.sprintf "%s%s" base (annotate chunk i)

let disasm_func chunk (f : Chunk.func) =
  let buf = Buffer.create 256 in
  let pf fmt = Printf.bprintf buf fmt in
  pf "function %s (fn%d) arity=%d nregs=%d entry=%d%s\n" f.name f.fn_id f.arity
    f.nregs f.entry
    (if f.is_main then "  [main]" else "");
  let ends =
    Array.fold_left
      (fun acc (g : Chunk.func) ->
        if g.entry > f.entry then min acc g.entry else acc)
      (Array.length chunk.code) chunk.funcs
  in
  for ip = f.entry to ends - 1 do
    pf "  %4d: %s\n" ip (disasm_instr chunk ip chunk.code.(ip))
  done;
  Buffer.contents buf

let pp fmt chunk =
  Format.fprintf fmt "; Glyph bytecode disassembly — main=fn%d\n" chunk.Chunk.main;
  Array.iteri
    (fun i c -> Format.fprintf fmt "const[%d] = %a\n" i Chunk.pp_const c)
    chunk.consts;
  Format.fprintf fmt "\n";
  Array.iter
    (fun f -> Format.pp_print_string fmt (disasm_func chunk f))
    chunk.funcs;
  if Array.length chunk.funcs = 0 then
    Array.iteri
      (fun ip instr ->
        Format.fprintf fmt "%4d: %s\n" ip (disasm_instr chunk ip instr))
      chunk.code

let to_string chunk = Format.asprintf "%a" pp chunk

let print ?(oc = stdout) chunk =
  let fmt = Format.formatter_of_out_channel oc in
  pp fmt chunk;
  Format.pp_print_flush fmt ()

let disassemble_file path = to_string (Chunk.read_file path)

type stats = {
  n_funcs : int;
  n_constants : int;
  n_instructions : int;
  max_regs : int;
}

let stats (ch : Chunk.t) =
  {
    n_funcs = Array.length ch.funcs;
    n_constants = Array.length ch.consts;
    n_instructions = Array.length ch.code;
    max_regs =
      Array.fold_left
        (fun acc (f : Chunk.func) -> max acc f.nregs)
        0 ch.funcs;
  }

let pp_stats fmt s =
  Format.fprintf fmt "funcs=%d consts=%d instrs=%d max_regs=%d" s.n_funcs
    s.n_constants s.n_instructions s.max_regs

let cfg_edges (ch : Chunk.t) (f : Chunk.func) =
  let ends =
    Array.fold_left
      (fun acc (g : Chunk.func) ->
        if g.entry > f.entry then min acc g.entry else acc)
      (Array.length ch.code) ch.funcs
  in
  let edges = ref [] in
  for ip = f.entry to ends - 1 do
    let i = ch.code.(ip) in
    match i.op with
    | Opcode.Op_jump -> edges := (ip, i.a) :: !edges
    | Opcode.Op_jump_if | Opcode.Op_jump_if_not ->
        edges := (ip, i.b) :: !edges;
        if ip + 1 < ends then edges := (ip, ip + 1) :: !edges
    | Opcode.Op_switch ->
        for k = 0 to i.b - 1 do
          edges := (ip, i.extra.((2 * k) + 1)) :: !edges
        done;
        let def_slot = i.b * 2 in
        if def_slot < Array.length i.extra then
          edges := (ip, i.extra.(def_slot)) :: !edges
        else edges := (ip, i.c) :: !edges
    | Opcode.Op_ret | Opcode.Op_ret_void | Opcode.Op_halt
    | Opcode.Op_tail_call | Opcode.Op_tail_call_closure ->
        ()
    | _ -> if ip + 1 < ends then edges := (ip, ip + 1) :: !edges
  done;
  List.rev !edges

let verify_encoding (ch : Chunk.t) =
  try
    Array.iteri
      (fun ip instr ->
        let enc = Opcode.encode instr in
        let instr2, _ = Opcode.decode enc 0 in
        let s1 = Format.asprintf "%a" Opcode.pp_instr instr in
        let s2 = Format.asprintf "%a" Opcode.pp_instr instr2 in
        if s1 <> s2 then
          failwith (Printf.sprintf "pc %d roundtrip %s vs %s" ip s1 s2))
      ch.code;
    Ok ()
  with Failure msg -> Error msg
