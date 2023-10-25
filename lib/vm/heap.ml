(** Bump-allocated semi-space heap. *)

type loc = int

type obj_kind =
  | String of string
  | Tuple of Value.t array
  | Adt of int * Value.t array
  | Closure of int * Value.t array

type cell = {
  mutable forward : loc option;
  mutable kind : obj_kind;
}

type t = {
  mutable fromspace : cell option array;
  mutable tospace : cell option array;
  mutable top : int;
  mutable capacity : int;
  mutable allocs_since_gc : int;
  mutable collect : (t -> unit) option;
}

let create ?(capacity = 1024) () =
  {
    fromspace = Array.make capacity None;
    tospace = Array.make capacity None;
    top = 0;
    capacity;
    allocs_since_gc = 0;
    collect = None;
  }

let set_collect_fn heap f = heap.collect <- Some f

let size heap = heap.top
let capacity heap = heap.capacity

let grow heap =
  let new_cap = heap.capacity * 2 in
  let from' = Array.make new_cap None in
  Array.blit heap.fromspace 0 from' 0 heap.top;
  heap.fromspace <- from';
  heap.tospace <- Array.make new_cap None;
  heap.capacity <- new_cap

let flip_spaces heap =
  let tmp = heap.fromspace in
  heap.fromspace <- heap.tospace;
  heap.tospace <- tmp;
  (* Clear tospace (old fromspace) slots for the next collection. *)
  Array.fill heap.tospace 0 heap.capacity None;
  heap.top <- 0

let tospace_alloc heap cell =
  if heap.top >= heap.capacity then grow heap;
  (* During GC we allocate into what will become the new fromspace.
     Cheney uses tospace as the destination; we keep [top] as the tospace
     bump during collection by writing into [tospace] directly here. *)
  let loc = heap.top in
  if loc >= Array.length heap.tospace then (
    let new_cap = Array.length heap.tospace * 2 in
    let to' = Array.make new_cap None in
    Array.blit heap.tospace 0 to' 0 loc;
    heap.tospace <- to';
    if Array.length heap.fromspace < new_cap then
      heap.fromspace <- Array.make new_cap None;
    heap.capacity <- new_cap);
  heap.tospace.(loc) <- Some cell;
  heap.top <- loc + 1;
  loc

let get_cell heap loc =
  match heap.fromspace.(loc) with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Heap.get_cell: empty slot %d" loc)

let get heap loc = (get_cell heap loc).kind

let resolve heap = function
  | Value.Ptr loc -> (
      match get heap loc with
      | String s -> Value.String s
      | Tuple xs -> Value.Tuple xs
      | Adt (tag, xs) -> Value.Adt (tag, xs)
      | Closure (fid, env) -> Value.Closure (fid, env))
  | v -> v

let maybe_collect heap =
  heap.allocs_since_gc <- heap.allocs_since_gc + 1;
  if heap.top >= heap.capacity - 1 then (
    match heap.collect with
    | Some f -> f heap
    | None -> grow heap)

let raw_alloc heap kind =
  maybe_collect heap;
  if heap.top >= heap.capacity then grow heap;
  let loc = heap.top in
  let cell = { forward = None; kind } in
  heap.fromspace.(loc) <- Some cell;
  heap.top <- loc + 1;
  Value.Ptr loc

let alloc heap kind = raw_alloc heap kind
let alloc_string heap s = raw_alloc heap (String s)
let alloc_tuple heap xs = raw_alloc heap (Tuple xs)
let alloc_adt heap tag xs = raw_alloc heap (Adt (tag, xs))
let alloc_closure heap fid env = raw_alloc heap (Closure (fid, env))
