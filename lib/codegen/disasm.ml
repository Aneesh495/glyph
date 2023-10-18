(** Pretty-print bytecode chunks for [glyph disasm]. *)

let const_summary chunk idx =
  if idx < 0 || idx >= Array.length chunk.Chunk.consts then "?"
  else Format.asprintf "%a" Chunk.pp_const chunk.consts.(idx)

let disasm_instr chunk ip (i : Opcode.instr) =
  let base = Format.asprintf "%a" Opcode.pp_instr i in
  match i.op with
  | Opcode.Op_load_const ->
      Printf.sprintf "r%d = const[%d] ; %s" i.a i.b (const_summary chunk i.b)
  | Opcode.Op_call ->
      let name =
        match
          Array.find_opt (fun (f : Chunk.func) -> f.fn_id = i.b) chunk.funcs
        with
        | Some f -> f.name
        | None -> Printf.sprintf "fn%d" i.b
      in
      Printf.sprintf "r%d = call %s/%d%s" i.a name i.c
        (if Array.length i.extra = 0 then ""
         else
           " args=["
           ^ String.concat "," (Array.to_list (Array.map string_of_int i.extra))
           ^ "]")
  | Opcode.Op_jump | Opcode.Op_jump_if | Opcode.Op_jump_if_not ->
      Printf.sprintf "%s  ; -> %d" base i.a
  | _ ->
      let _ = ip in
      base

let disasm_func chunk (f : Chunk.func) =
  let buf = Buffer.create 256 in
  let pf fmt = Printf.bprintf buf fmt in
  pf "function %s (fn%d) arity=%d nregs=%d entry=%d%s\n" f.name f.fn_id f.arity
    f.nregs f.entry
    (if f.is_main then "  [main]" else "");
  (* Heuristic: print until next function entry or end *)
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
  Format.fprintf fmt "; disassembly — main=fn%d\n" chunk.Chunk.main;
  Array.iteri
    (fun i c ->
      Format.fprintf fmt "const[%d] = %a\n" i Chunk.pp_const c)
    chunk.consts;
  Format.fprintf fmt "\n";
  Array.iter
    (fun f -> Format.pp_print_string fmt (disasm_func chunk f))
    chunk.funcs;
  (* Also dump raw if no funcs *)
  if Array.length chunk.funcs = 0 then
    Array.iteri
      (fun ip instr ->
        Format.fprintf fmt "%4d: %s\n" ip (disasm_instr chunk ip instr))
      chunk.code

let to_string chunk = Format.asprintf "%a" pp chunk
