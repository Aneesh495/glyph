(** Cheney semi-space copying collector. *)

type roots = {
  get_stack_roots : unit -> Value.t array array;
  get_globals : unit -> Value.t array;
  set_stack_roots : Value.t array array -> unit;
  set_globals : Value.t array -> unit;
}

let collections = ref 0
let objects_copied = ref 0

let stats () = (!collections, !objects_copied)

(** Evacuate one object from fromspace into tospace.
    Returns the new location. *)
let rec copy_object heap (loc : Heap.loc) : Heap.loc =
  let cell = Heap.get_cell heap loc in
  match cell.forward with
  | Some new_loc ->
      (* Already evacuated — return forwarding address. *)
      new_loc
  | None ->
      (* Deep-copy the payload, then allocate in tospace. *)
      let kind' =
        match cell.kind with
        | Heap.String s -> Heap.String s
        | Heap.Tuple xs -> Heap.Tuple (Array.copy xs)
        | Heap.Adt (tag, xs) -> Heap.Adt (tag, Array.copy xs)
        | Heap.Closure (fid, env) -> Heap.Closure (fid, Array.copy env)
      in
      let new_cell = { Heap.forward = None; kind = kind' } in
      let new_loc = Heap.tospace_alloc heap new_cell in
      cell.forward <- Some new_loc;
      incr objects_copied;
      new_loc

(** Rewrite a single value: if it is a pointer, evacuate and retarget. *)
and forward_value heap (v : Value.t) : Value.t =
  match v with
  | Value.Ptr loc -> Value.Ptr (copy_object heap loc)
  | other -> other

(** After an object has been copied into tospace, its fields may still
    point at fromspace. Chase and rewrite them. *)
let scan_cell heap (cell : Heap.cell) =
  match cell.kind with
  | Heap.String _ -> ()
  | Heap.Tuple xs | Heap.Adt (_, xs) | Heap.Closure (_, xs) ->
      for i = 0 to Array.length xs - 1 do
        xs.(i) <- forward_value heap xs.(i)
      done

let collect heap roots =
  incr collections;
  let old_top = heap.Heap.top in
  (* 1. Prepare tospace as empty destination. flip_spaces swaps the arrays
     and zeroes the bump pointer. After flip, the OLD fromspace is in
     [tospace] slot name-wise — we need care.

     Our Heap representation:
       - Allocation always bumps [fromspace] via [top].
       - [tospace_alloc] writes into [tospace] and bumps [top].

     Algorithm:
       a. Remember old fromspace array.
       b. Reset top=0; evacuate into tospace via tospace_alloc.
       c. Scan tospace[0 .. top).
       d. Swap: fromspace <- tospace contents; clear old. *)
  let old_from = heap.fromspace in
  let old_cap = heap.capacity in
  (* Ensure tospace is fresh and large enough. *)
  heap.tospace <- Array.make old_cap None;
  heap.top <- 0;

  let rewrite_array arr =
    Array.map (fun v -> forward_value heap v) arr
  in

  (* 2. Evacuate roots. copy_object reads cells from [fromspace], which
     still holds the old objects. *)
  heap.fromspace <- old_from;
  let stack = roots.get_stack_roots () in
  let stack' = Array.map rewrite_array stack in
  let globals = roots.get_globals () in
  let globals' = rewrite_array globals in

  (* 3. Cheney scan: walk every evacuated object in tospace order. *)
  let scan = ref 0 in
  while !scan < heap.top do
    (match heap.tospace.(!scan) with
    | Some cell -> scan_cell heap cell
    | None -> ());
    incr scan
  done;

  (* 4. Install tospace as the new fromspace. *)
  heap.fromspace <- heap.tospace;
  heap.tospace <- Array.make heap.capacity None;
  heap.allocs_since_gc <- 0;

  roots.set_stack_roots stack';
  roots.set_globals globals';
  ignore old_top

let install heap roots =
  Heap.set_collect_fn heap (fun h -> collect h roots)


let check_heap (heap : Heap.t) : string list =
  let errs = ref [] in
  for i = 0 to heap.top - 1 do
    match heap.fromspace.(i) with
    | None -> errs := Printf.sprintf "empty live slot %d" i :: !errs
    | Some cell -> (
        match cell.forward with
        | Some _ ->
            errs := Printf.sprintf "forward left at %d" i :: !errs
        | None -> (
            match cell.kind with
            | Heap.String _ -> ()
            | Heap.Tuple xs | Heap.Adt (_, xs) | Heap.Closure (_, xs) ->
                Array.iter
                  (function
                    | Value.Ptr p when p < 0 || p >= heap.top ->
                        errs :=
                          Printf.sprintf "dangling ptr %d in %d" p i :: !errs
                    | _ -> ())
                  xs))
  done;
  List.rev !errs

let force_collect = collect
