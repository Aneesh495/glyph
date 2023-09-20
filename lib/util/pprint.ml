(** Pretty-printing helpers built on Format. *)

open Format

let comma_sep pp_item fmt xs =
  let pp_sep fmt () = fprintf fmt ",@ " in
  pp_print_list ~pp_sep pp_item fmt xs

let semi_sep pp_item fmt xs =
  let pp_sep fmt () = fprintf fmt ";@ " in
  pp_print_list ~pp_sep pp_item fmt xs

let vbox pp fmt x =
  fprintf fmt "@[<v 0>";
  pp fmt x;
  fprintf fmt "@]"

let hvbox pp fmt x =
  fprintf fmt "@[<hv 0>";
  pp fmt x;
  fprintf fmt "@]"

let parens pp fmt x = fprintf fmt "@[(%a)@]" pp x
let brackets pp fmt x = fprintf fmt "@[[%a]@]" pp x
let braces pp fmt x = fprintf fmt "@[{%a}@]" pp x

let option pp fmt = function
  | None -> pp_print_string fmt "<none>"
  | Some x -> pp fmt x

let to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = formatter_of_buffer buf in
  pp fmt x;
  pp_print_flush fmt ();
  Buffer.contents buf

let indented ~spaces s =
  let pad = String.make spaces ' ' in
  String.split_on_char '\n' s
  |> List.map (fun line -> if line = "" then "" else pad ^ line)
  |> String.concat "\n"
