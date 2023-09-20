(** String and buffer helpers. *)

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let ends_with ~suffix s =
  let n = String.length suffix in
  let m = String.length s in
  m >= n && String.sub s (m - n) n = suffix

let strip s =
  let n = String.length s in
  let rec left i =
    if i >= n then n
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> left (i + 1)
      | _ -> i
  in
  let rec right i =
    if i < 0 then -1
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> right (i - 1)
      | _ -> i
  in
  let l = left 0 in
  let r = right (n - 1) in
  if l > r then "" else String.sub s l (r - l + 1)

let split_lines s = String.split_on_char '\n' s

let indent ?(n = 2) s =
  let pad = String.make n ' ' in
  split_lines s
  |> List.map (fun line -> if line = "" then "" else pad ^ line)
  |> String.concat "\n"

let escape_string s =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c ->
          let code = Char.code c in
          if code < 32 || code > 126 then
            Buffer.add_string buf (Printf.sprintf "\\x%02x" code)
          else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

module List_ext = struct
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: xs -> x :: take (n - 1) xs

  let rec drop n = function
    | xs when n <= 0 -> xs
    | [] -> []
    | _ :: xs -> drop (n - 1) xs

  let init n f =
    let rec loop i acc =
      if i >= n then List.rev acc else loop (i + 1) (f i :: acc)
    in
    loop 0 []

  let find_map f xs =
    let rec go = function
      | [] -> None
      | x :: xs -> ( match f x with Some _ as y -> y | None -> go xs)
    in
    go xs

  let filter_map f xs =
    List.rev
      (List.fold_left
         (fun acc x -> match f x with None -> acc | Some y -> y :: acc)
         [] xs)

  let flat_map f xs = List.concat (List.map f xs)

  let rec last = function
    | [] -> invalid_arg "List_ext.last"
    | [ x ] -> x
    | _ :: xs -> last xs

  let index_of ~eq x xs =
    let rec go i = function
      | [] -> None
      | y :: ys -> if eq x y then Some i else go (i + 1) ys
    in
    go 0 xs
end

module Option_ext = struct
  let map f = function None -> None | Some x -> Some (f x)
  let bind x f = match x with None -> None | Some v -> f v
  let value ~default = function None -> default | Some x -> x
  let some x = Some x
  let iter f = function None -> () | Some x -> f x
  let to_list = function None -> [] | Some x -> [ x ]
end

module Result_ext = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
  let bind x f = match x with Ok v -> f v | Error e -> Error e
  let map_error f = function Ok x -> Ok x | Error e -> Error (f e)
  let value ~default = function Ok x -> x | Error _ -> default

  let rec map_list f = function
    | [] -> Ok []
    | x :: xs -> (
        match f x with
        | Error e -> Error e
        | Ok y -> (
            match map_list f xs with
            | Error e -> Error e
            | Ok ys -> Ok (y :: ys)))

  let fold_list ~f ~init xs =
    let rec go acc = function
      | [] -> Ok acc
      | x :: xs -> ( match f acc x with Error e -> Error e | Ok acc -> go acc xs)
    in
    go init xs
end
