(** Source locations and spans. *)

type pos = {
  line : int;
  col : int;
  offset : int;
}

type t = {
  file : string;
  start : pos;
  end_ : pos;
}

let dummy_pos = { line = 1; col = 1; offset = 0 }

let dummy = { file = "<unknown>"; start = dummy_pos; end_ = dummy_pos }

let make ~file ~start ~end_ = { file; start; end_ }

let of_positions ~file start end_ = { file; start; end_ }

let merge a b =
  if a.file <> b.file then a
  else
    let start =
      if a.start.offset <= b.start.offset then a.start else b.start
    in
    let end_ =
      if a.end_.offset >= b.end_.offset then a.end_ else b.end_
    in
    { file = a.file; start; end_ }

let merge_list = function
  | [] -> dummy
  | x :: xs -> List.fold_left merge x xs

let start_line t = t.start.line
let start_col t = t.start.col
let end_line t = t.end_.line
let end_col t = t.end_.col
let start_offset t = t.start.offset
let end_offset t = t.end_.offset
let file t = t.file
let length t = t.end_.offset - t.start.offset

let is_dummy t = t.file = "<unknown>" && t.start.offset = 0 && t.end_.offset = 0
let is_empty t = t.start.offset = t.end_.offset

let contains_offset t off = t.start.offset <= off && off < t.end_.offset

let contains_pos t p =
  t.start.offset <= p.offset && p.offset < t.end_.offset

let equal a b =
  String.equal a.file b.file
  && a.start.offset = b.start.offset
  && a.end_.offset = b.end_.offset

let compare a b =
  match String.compare a.file b.file with
  | 0 -> (
      match Int.compare a.start.offset b.start.offset with
      | 0 -> Int.compare a.end_.offset b.end_.offset
      | c -> c)
  | c -> c

let to_string t =
  if t.start.line = t.end_.line then
    Printf.sprintf "%s:%d:%d-%d" t.file t.start.line t.start.col t.end_.col
  else
    Printf.sprintf "%s:%d:%d-%d:%d" t.file t.start.line t.start.col
      t.end_.line t.end_.col

let pp fmt t = Format.pp_print_string fmt (to_string t)

let pp_pos fmt p =
  Format.fprintf fmt "%d:%d" p.line p.col

let advance_pos pos ~ch =
  if ch = '\n' then
    { line = pos.line + 1; col = 1; offset = pos.offset + 1 }
  else
    { line = pos.line; col = pos.col + 1; offset = pos.offset + 1 }

let advance_string pos s =
  let rec loop i pos =
    if i >= String.length s then pos
    else loop (i + 1) (advance_pos pos ~ch:s.[i])
  in
  loop 0 pos

let spanning ~file ~start ~text =
  { file; start; end_ = advance_string start text }

let slice ~source t =
  let len = String.length source in
  let s = max 0 (min t.start.offset len) in
  let e = max s (min t.end_.offset len) in
  String.sub source s (e - s)

let line_range t = (t.start.line, t.end_.line)
