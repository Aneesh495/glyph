(** Union-find with path compression and union by rank. *)

type 'a cell = {
  mutable parent : int;
  mutable rank : int;
  mutable value : 'a;
}

module Arr = struct
  type 'a t = {
    mutable data : 'a option array;
    mutable len : int;
  }

  let create () = { data = Array.make 8 None; len = 0 }
  let length a = a.len

  let get a i =
    match a.data.(i) with
    | Some v -> v
    | None -> invalid_arg "Union_find.Arr.get"

  let set a i v = a.data.(i) <- Some v

  let push a v =
    if a.len >= Array.length a.data then (
      let nd = Array.make (Array.length a.data * 2) None in
      Array.blit a.data 0 nd 0 a.len;
      a.data <- nd);
    let i = a.len in
    a.data.(i) <- Some v;
    a.len <- i + 1;
    i

  let iteri f a =
    for i = 0 to a.len - 1 do
      f i (get a i)
    done
end

type 'a node = int

type 'a t = { cells : 'a cell Arr.t }

let create () = { cells = Arr.create () }

let make uf value =
  let idx = Arr.push uf.cells { parent = -1; rank = 0; value } in
  (Arr.get uf.cells idx).parent <- idx;
  idx

let rec find_root uf i =
  let cell = Arr.get uf.cells i in
  if cell.parent = i then i
  else (
    let root = find_root uf cell.parent in
    cell.parent <- root;
    root)

let find uf n = find_root uf n

let get uf n =
  let r = find uf n in
  (Arr.get uf.cells r).value

let set uf n v =
  let r = find uf n in
  (Arr.get uf.cells r).value <- v

let rank uf n =
  let r = find uf n in
  (Arr.get uf.cells r).rank

let length uf = Arr.length uf.cells

let union uf a b =
  let ra = find uf a in
  let rb = find uf b in
  if ra = rb then ra
  else
    let ca = Arr.get uf.cells ra in
    let cb = Arr.get uf.cells rb in
    if ca.rank < cb.rank then (
      ca.parent <- rb;
      rb)
    else if ca.rank > cb.rank then (
      cb.parent <- ra;
      ra)
    else (
      cb.parent <- ra;
      ca.rank <- ca.rank + 1;
      ra)

let union_with uf ~merge a b =
  let ra = find uf a in
  let rb = find uf b in
  if ra = rb then ra
  else
    let ca = Arr.get uf.cells ra in
    let cb = Arr.get uf.cells rb in
    let merged = merge ca.value cb.value in
    if ca.rank < cb.rank then (
      ca.parent <- rb;
      cb.value <- merged;
      rb)
    else if ca.rank > cb.rank then (
      cb.parent <- ra;
      ca.value <- merged;
      ra)
    else (
      cb.parent <- ra;
      ca.value <- merged;
      ca.rank <- ca.rank + 1;
      ra)

let same uf a b = find uf a = find uf b

type 'a snapshot_entry = {
  parent : int;
  rank : int;
  value : 'a;
}

type 'a snapshot = 'a snapshot_entry array

let snapshot uf =
  Array.init (Arr.length uf.cells) (fun i ->
      let c = Arr.get uf.cells i in
      { parent = c.parent; rank = c.rank; value = c.value })

let restore uf snap =
  let n = min (Array.length snap) (Arr.length uf.cells) in
  for i = 0 to n - 1 do
    let c = Arr.get uf.cells i in
    let s = snap.(i) in
    c.parent <- s.parent;
    c.rank <- s.rank;
    c.value <- s.value
  done

let iter_roots uf f =
  Arr.iteri (fun i c -> if c.parent = i then f i c.value) uf.cells

let fold_roots uf f acc =
  let acc = ref acc in
  Arr.iteri
    (fun i c -> if c.parent = i then acc := f i c.value !acc)
    uf.cells;
  !acc

module Persistent = struct
  module IM = Map.Make (Int)

  type 'a node = int

  type 'a entry = {
    parent : int;
    rank : int;
    value : 'a;
  }

  type 'a state = {
    next : int;
    nodes : 'a entry IM.t;
  }

  let empty = { next = 0; nodes = IM.empty }

  let fresh st value =
    let id = st.next in
    let entry = { parent = id; rank = 0; value } in
    ({ next = id + 1; nodes = IM.add id entry st.nodes }, id)

  let rec find st n =
    let e = IM.find n st.nodes in
    if e.parent = n then (st, n)
    else
      let st, root = find st e.parent in
      let e' = { e with parent = root } in
      ({ st with nodes = IM.add n e' st.nodes }, root)

  let get st n =
    let st, r = find st n in
    let e = IM.find r st.nodes in
    (st, e.value)

  let set st n v =
    let st, r = find st n in
    let e = IM.find r st.nodes in
    { st with nodes = IM.add r { e with value = v } st.nodes }

  let union st ~merge a b =
    let st, ra = find st a in
    let st, rb = find st b in
    if ra = rb then (st, ra)
    else
      let ea = IM.find ra st.nodes in
      let eb = IM.find rb st.nodes in
      let merged = merge ea.value eb.value in
      if ea.rank < eb.rank then
        let nodes =
          st.nodes
          |> IM.add ra { ea with parent = rb }
          |> IM.add rb { eb with value = merged }
        in
        ({ st with nodes }, rb)
      else if ea.rank > eb.rank then
        let nodes =
          st.nodes
          |> IM.add rb { eb with parent = ra }
          |> IM.add ra { ea with value = merged }
        in
        ({ st with nodes }, ra)
      else
        let nodes =
          st.nodes
          |> IM.add rb { eb with parent = ra }
          |> IM.add ra
               { ea with value = merged; rank = ea.rank + 1 }
        in
        ({ st with nodes }, ra)

  let same st a b =
    let st, ra = find st a in
    let st, rb = find st b in
    (st, ra = rb)
end
