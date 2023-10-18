(** Semi-space bump allocator.

    Two equal-sized spaces ([fromspace] / [tospace]). Allocation bumps
    [next] in fromspace. When allocation fails, the caller triggers GC
    ([Gc.collect]), which flips spaces and evacuates live objects into the
    former tospace (now fromspace).
*)

type space = {
  mutable slots : Object_.t option array;
  mutable next : int;
  size : int;
}

type t = {
  mutable fromspace : space;
  mutable tospace : space;
  mutable gc_count : int;
  mutable bytes_allocated : int;
  mutable last_live : int;
}

let make_space size =
  { slots = Array.make size None; next = 0; size }

let create ?(initial_size = 256) () =
  if initial_size < 8 then invalid_arg "Heap.create: size too small";
  {
    fromspace = make_space initial_size;
    tospace = make_space initial_size;
    gc_count = 0;
    bytes_allocated = 0;
    last_live = 0;
  }

let space_used (s : space) = s.next
let space_size (s : space) = s.size
let space_remaining (s : space) = s.size - s.next

let used h = space_used h.fromspace
let capacity h = space_size h.fromspace
let remaining h = space_remaining h.fromspace

let get h addr =
  if addr < 0 || addr >= h.fromspace.size then
    invalid_arg (Printf.sprintf "Heap.get: bad addr %d" addr);
  match h.fromspace.slots.(addr) with
  | Some obj -> obj
  | None ->
      invalid_arg (Printf.sprintf "Heap.get: empty slot %d" addr)

let get_opt h addr =
  if addr < 0 || addr >= h.fromspace.size then None
  else h.fromspace.slots.(addr)

let set h addr obj =
  if addr < 0 || addr >= h.fromspace.size then
    invalid_arg (Printf.sprintf "Heap.set: bad addr %d" addr);
  h.fromspace.slots.(addr) <- Some obj

let try_alloc h (obj : Object_.t) : int option =
  let s = h.fromspace in
  if s.next >= s.size then None
  else (
    let addr = s.next in
    s.slots.(addr) <- Some obj;
    s.next <- s.next + 1;
    h.bytes_allocated <- h.bytes_allocated + Object_.size_slots obj;
    Some addr)

let alloc h obj =
  match try_alloc h obj with
  | Some addr -> addr
  | None -> raise Out_of_memory

(** Grow both spaces to at least [new_size], copying fromspace contents
    into a larger fromspace (used when GC cannot free enough). *)
let grow h new_size =
  let new_size = max new_size (h.fromspace.size * 2) in
  let new_from = make_space new_size in
  for i = 0 to h.fromspace.next - 1 do
    new_from.slots.(i) <- h.fromspace.slots.(i)
  done;
  new_from.next <- h.fromspace.next;
  h.fromspace <- new_from;
  h.tospace <- make_space new_size

(** Flip spaces after a successful collection: tospace becomes fromspace. *)
let flip h =
  let old_from = h.fromspace in
  h.fromspace <- h.tospace;
  h.tospace <- old_from;
  (* Clear the new tospace (old fromspace) for the next cycle. *)
  Array.fill h.tospace.slots 0 h.tospace.size None;
  h.tospace.next <- 0;
  h.gc_count <- h.gc_count + 1

(** Allocate into tospace during GC evacuation. *)
let alloc_tospace h (obj : Object_.t) : int =
  let s = h.tospace in
  if s.next >= s.size then (
    (* Emergency grow of tospace (and matching fromspace later). *)
    let new_size = s.size * 2 in
    let bigger = make_space new_size in
    for i = 0 to s.next - 1 do
      bigger.slots.(i) <- s.slots.(i)
    done;
    bigger.next <- s.next;
    h.tospace <- bigger;
    (* Also enlarge fromspace array so addresses remain valid for scanning. *)
    if h.fromspace.size < new_size then (
      let nf = make_space new_size in
      for i = 0 to h.fromspace.size - 1 do
        nf.slots.(i) <- h.fromspace.slots.(i)
      done;
      nf.next <- h.fromspace.next;
      h.fromspace <- nf));
  let s = h.tospace in
  let addr = s.next in
  s.slots.(addr) <- Some obj;
  s.next <- s.next + 1;
  addr

let get_tospace h addr =
  match h.tospace.slots.(addr) with
  | Some o -> o
  | None -> invalid_arg "Heap.get_tospace: empty"

let set_tospace h addr obj = h.tospace.slots.(addr) <- Some obj

let tospace_next h = h.tospace.next

let record_live h n = h.last_live <- n

let stats h =
  Printf.sprintf
    "heap: used=%d/%d gc_count=%d allocated_slots=%d last_live=%d"
    h.fromspace.next h.fromspace.size h.gc_count h.bytes_allocated
    h.last_live

let pp fmt h =
  Format.fprintf fmt "%s" (stats h)

(** Iterate all live objects currently in fromspace. *)
let iter h f =
  for i = 0 to h.fromspace.next - 1 do
    match h.fromspace.slots.(i) with
    | Some obj -> f i obj
    | None -> ()
  done

(** Debug dump. *)
let dump fmt h =
  Format.fprintf fmt "=== heap dump (fromspace next=%d size=%d) ===\n"
    h.fromspace.next h.fromspace.size;
  iter h (fun addr obj ->
      Format.fprintf fmt "  [%d] %a\n" addr Object_.pp obj)

(** Allocate helpers for common objects. *)
let alloc_string h s = try_alloc h (Object_.string s)
let alloc_tuple h fields = try_alloc h (Object_.tuple fields)
let alloc_adt h tag fields = try_alloc h (Object_.adt tag fields)
let alloc_closure h proto env = try_alloc h (Object_.closure proto env)
let alloc_array h xs = try_alloc h (Object_.array xs)

let alloc_string_exn h s = alloc h (Object_.string s)
let alloc_tuple_exn h fields = alloc h (Object_.tuple fields)
let alloc_adt_exn h tag fields = alloc h (Object_.adt tag fields)
let alloc_closure_exn h proto env = alloc h (Object_.closure proto env)

(** Reset heap to empty (keeps capacity). *)
let reset h =
  Array.fill h.fromspace.slots 0 h.fromspace.size None;
  h.fromspace.next <- 0;
  Array.fill h.tospace.slots 0 h.tospace.size None;
  h.tospace.next <- 0;
  h.bytes_allocated <- 0;
  h.last_live <- 0
