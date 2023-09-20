(** Structured compiler diagnostics with source snippets. *)

type severity =
  | Error
  | Warning
  | Note
  | Hint

type label = {
  span : Span.t;
  message : string;
  primary : bool;
}

type t = {
  severity : severity;
  code : string option;
  message : string;
  span : Span.t;
  labels : label list;
  notes : string list;
  help : string option;
}

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"
  | Hint -> "hint"

let severity_rank = function
  | Error -> 3
  | Warning -> 2
  | Note -> 1
  | Hint -> 0

let label ?(primary = false) span message = { span; message; primary }

let make ?(code = None) ?(labels = []) ?(notes = []) ?(help = None) severity
    span message =
  let code = match code with Some "" -> None | other -> other in
  { severity; code; message; span; labels; notes; help }

let error ?code ?(labels = []) ?(notes = []) ?help span message =
  make ?code ~labels ~notes ?help Error span message

let warning ?code ?(labels = []) ?(notes = []) ?help span message =
  make ?code ~labels ~notes ?help Warning span message

let note span message = make Note span message
let hint span message = make Hint span message

let with_label t lbl = { t with labels = t.labels @ [ lbl ] }
let with_note t msg = { t with notes = t.notes @ [ msg ] }
let with_help t msg = { t with help = Some msg }
let with_code t code = { t with code = Some code }

let equal a b =
  a.severity = b.severity
  && Option.equal String.equal a.code b.code
  && String.equal a.message b.message
  && Span.equal a.span b.span

module Source = struct
  type t = {
    text : string;
    lines : string array;
  }

  let of_string text =
    let lines =
      match String.split_on_char '\n' text with
      | [] -> [| "" |]
      | xs -> Array.of_list xs
    in
    { text; lines }

  let line_count t = Array.length t.lines

  let line t n =
    let idx = n - 1 in
    if idx < 0 || idx >= Array.length t.lines then None
    else Some t.lines.(idx)

  let gutter_width ~lo ~hi =
    let digits n =
      if n <= 0 then 1 else int_of_float (log10 (float_of_int n)) + 1
    in
    max 2 (digits hi)

  let pad_left width n =
    let s = string_of_int n in
    let pad = max 0 (width - String.length s) in
    String.make pad ' ' ^ s

  let underline ~start_col ~end_col ~same_line =
    let start_col = max 1 start_col in
    let end_col = max start_col end_col in
    let spaces = String.make (start_col - 1) ' ' in
    let len =
      if same_line then max 1 (end_col - start_col) else 1
    in
    spaces ^ String.make len '^'

  let snippet t ~span ?(context = 1) ?(primary_message = "") () =
    let lo = max 1 (span.start.line - context) in
    let hi = min (line_count t) (span.end_.line + context) in
    if lo > hi then ""
    else
      let width = gutter_width ~lo ~hi in
      let buf = Buffer.create 256 in
      let pf fmt = Printf.bprintf buf fmt in
      for n = lo to hi do
        match line t n with
        | None -> ()
        | Some content ->
            pf "  %s | %s\n" (pad_left width n) content;
            if n >= span.start.line && n <= span.end_.line then (
              let start_col =
                if n = span.start.line then span.start.col else 1
              in
              let end_col =
                if n = span.end_.line then span.end_.col
                else String.length content + 1
              in
              let caret =
                underline ~start_col ~end_col
                  ~same_line:(span.start.line = span.end_.line)
              in
              pf "  %s | %s" (String.make width ' ') caret;
              if n = span.end_.line && primary_message <> "" then
                pf " %s" primary_message;
              pf "\n")
      done;
      Buffer.contents buf
end

let render_header d =
  let code =
    match d.code with
    | None -> ""
    | Some c -> "[" ^ c ^ "] "
  in
  Printf.sprintf "%s: %s%s: %s" (Span.to_string d.span)
    code
    (severity_to_string d.severity) d.message

let render ?(source = None) ?(context_lines = 1) d =
  let buf = Buffer.create 512 in
  let pf fmt = Printf.bprintf buf fmt in
  pf "%s\n" (render_header d);
  (match source with
  | None -> ()
  | Some src ->
      let src = Source.of_string src in
      let primary =
        match
          List.find_opt (fun l -> l.primary) d.labels
        with
        | Some l -> l.message
        | None -> ""
      in
      let body =
        Source.snippet src ~span:d.span ~context:context_lines
          ~primary_message:primary ()
      in
      if body <> "" then Buffer.add_string buf body;
      List.iter
        (fun lbl ->
          if not (Span.equal lbl.span d.span) then (
            pf "  %s: %s\n" (Span.to_string lbl.span) lbl.message;
            let extra =
              Source.snippet src ~span:lbl.span ~context:0
                ~primary_message:lbl.message ()
            in
            if extra <> "" then Buffer.add_string buf extra))
        d.labels);
  List.iter (fun note -> pf "  = note: %s\n" note) d.notes;
  (match d.help with
  | None -> ()
  | Some h -> pf "  = help: %s\n" h);
  Buffer.contents buf

let pp fmt d = Format.pp_print_string fmt (render d)

module Bag = struct
  type diag = t
  type t = diag list ref

  let create () = ref []
  let add bag d = bag := d :: !bag

  let error bag ?code ?(labels = []) ?(notes = []) ?help span message =
    add bag (error ?code ~labels ~notes ?help span message)

  let warning bag ?code ?(labels = []) ?(notes = []) ?help span message =
    add bag (warning ?code ~labels ~notes ?help span message)

  let to_list bag = List.rev !bag

  let has_errors bag =
    List.exists (fun d -> d.severity = Error) !bag

  let error_count bag =
    List.fold_left
      (fun n d -> if d.severity = Error then n + 1 else n)
      0 !bag

  let warning_count bag =
    List.fold_left
      (fun n d -> if d.severity = Warning then n + 1 else n)
      0 !bag

  let clear bag = bag := []
  let length bag = List.length !bag

  let emit ?(source = None) bag =
    List.iter
      (fun d ->
        output_string stderr (render ~source d);
        flush stderr)
      (to_list bag)

  let render_all ?(source = None) bag =
    let buf = Buffer.create 1024 in
    List.iter
      (fun d -> Buffer.add_string buf (render ~source d))
      (to_list bag);
    Buffer.contents buf

  let merge dst src = dst := List.rev_append !src !dst
end
