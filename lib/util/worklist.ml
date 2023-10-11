(** Worklist algorithm utilities for iterative dataflow. *)

type 'a t = {
  mutable front : 'a list;
  mutable back : 'a list;
  mutable len : int;
}

let create () = { front = []; back = []; len = 0 }

let of_list xs = { front = xs; back = []; len = List.length xs }

let clear t =
  t.front <- [];
  t.back <- [];
  t.len <- 0

let is_empty t = t.len = 0
let length t = t.len

let push t x =
  t.back <- x :: t.back;
  t.len <- t.len + 1

let push_front t x =
  t.front <- x :: t.front;
  t.len <- t.len + 1

let normalize t =
  if t.front = [] then (
    t.front <- List.rev t.back;
    t.back <- [])

let pop t =
  normalize t;
  match t.front with
  | [] -> None
  | x :: xs ->
      t.front <- xs;
      t.len <- t.len - 1;
      Some x

let peek t =
  normalize t;
  match t.front with
  | [] -> None
  | x :: _ -> Some x

let mem t ~equal x =
  List.exists (equal x) t.front || List.exists (equal x) t.back

module Unique = struct
  type ('k, 'a) t = {
    mutable front : 'a list;
    mutable back : 'a list;
    mutable len : int;
    pending : ('k, unit) Hashtbl.t;
    key : 'a -> 'k;
  }

  let create ~hash ~equal ~key =
    ignore hash;
    ignore equal;
    {
      front = [];
      back = [];
      len = 0;
      pending = Hashtbl.create 128;
      key;
    }

  let clear t =
    t.front <- [];
    t.back <- [];
    t.len <- 0;
    Hashtbl.clear t.pending

  let is_empty t = t.len = 0
  let length t = t.len

  let push t x =
    let k = t.key x in
    if Hashtbl.mem t.pending k then false
    else (
      Hashtbl.add t.pending k ();
      t.back <- x :: t.back;
      t.len <- t.len + 1;
      true)

  let normalize t =
    if t.front = [] then (
      t.front <- List.rev t.back;
      t.back <- [])

  let pop t =
    normalize t;
    match t.front with
    | [] -> None
    | x :: xs ->
        t.front <- xs;
        t.len <- t.len - 1;
        Hashtbl.remove t.pending (t.key x);
        Some x
end

let run ~initial ~transfer ?(max_iters = max_int) () =
  let wl = of_list initial in
  let iters = ref 0 in
  while (not (is_empty wl)) && !iters < max_iters do
    incr iters;
    match pop wl with
    | None -> ()
    | Some node -> List.iter (push wl) (transfer node)
  done;
  !iters

module Int = struct
  type t = {
    mutable q : int list;
    mutable len : int;
    in_q : bool array;
  }

  let create ~n = { q = []; len = 0; in_q = Array.make n false }

  let clear t =
    List.iter (fun i -> t.in_q.(i) <- false) t.q;
    t.q <- [];
    t.len <- 0

  let is_empty t = t.len = 0
  let length t = t.len

  let push t i =
    if i < 0 || i >= Array.length t.in_q then
      invalid_arg "Worklist.Int.push";
    if t.in_q.(i) then false
    else (
      t.in_q.(i) <- true;
      t.q <- i :: t.q;
      t.len <- t.len + 1;
      true)

  let pop t =
    match t.q with
    | [] -> None
    | i :: rest ->
        t.q <- rest;
        t.len <- t.len - 1;
        t.in_q.(i) <- false;
        Some i
end

module Bitset = struct
  type t = {
    bits : Bitvec.t;
    mutable q : int list;
    mutable len : int;
  }

  let create ~n = { bits = Bitvec.create ~size:n (); q = []; len = 0 }

  let clear t =
    Bitvec.clear t.bits;
    t.q <- [];
    t.len <- 0

  let is_empty t = t.len = 0
  let length t = t.len
  let mem t i = Bitvec.get t.bits i

  let push t i =
    if Bitvec.get t.bits i then false
    else (
      Bitvec.set_bit t.bits i;
      t.q <- i :: t.q;
      t.len <- t.len + 1;
      true)

  let pop t =
    match t.q with
    | [] -> None
    | i :: rest ->
        t.q <- rest;
        t.len <- t.len - 1;
        Bitvec.clear_bit t.bits i;
        Some i
end
