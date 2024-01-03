(** Structured compiler diagnostics. *)

type severity = Error | Warning | Note | Hint

type label = {
  span : Span.t;
  message : string;
  primary : bool;
}

type t = {
  severity : severity;
  message : string;
  span : Span.t;
  notes : string list;
  labels : label list;
  code : string option;
  help : string option;
}

let label ?(primary = false) span message = { span; message; primary }

let error ?(notes = []) ?(labels = []) ?code ?help span message =
  { severity = Error; message; span; notes; labels; code; help }

let warning ?(notes = []) ?(labels = []) ?code ?help span message =
  { severity = Warning; message; span; notes; labels; code; help }

let note span message = error ~notes:[] span message |> fun d -> { d with severity = Note; message }
let hint span message = { (error span message) with severity = Hint }

let with_help d msg = { d with help = Some msg; notes = d.notes @ [ msg ] }
let with_note d msg = { d with notes = d.notes @ [ msg ] }
let with_label d lbl = { d with labels = d.labels @ [ lbl ] }
let with_code d code = { d with code = Some code }

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"
  | Hint -> "hint"

let severity_rank = function Error -> 3 | Warning -> 2 | Note -> 1 | Hint -> 0

let equal a b =
  a.severity = b.severity && a.message = b.message && Span.equal a.span b.span

let render ?source d =
  ignore source;
  let code =
    match d.code with None -> "" | Some c -> "[" ^ c ^ "] "
  in
  Printf.sprintf "%s: %s%s: %s\n" (Span.to_string d.span) code
    (severity_to_string d.severity) d.message
  ^ String.concat "" (List.map (fun n -> Printf.sprintf "  = note: %s\n" n) d.notes)

let pp fmt d = Format.pp_print_string fmt (render d)

module Bag = struct
  type diag = t
  type t = diag list ref
  let create () = ref []
  let add bag d = bag := d :: !bag
  let error bag span message = add bag (error span message)
  let warning bag span message = add bag (warning span message)
  let to_list bag = List.rev !bag
  let has_errors bag = List.exists (fun d -> d.severity = Error) !bag
end

module Source = struct
  type t = string
  let of_string s = s
  let line _ _ = None
  let line_count _ = 0
  let snippet _ ~span:_ ?context:_ ?primary_message:_ () = ""
end
