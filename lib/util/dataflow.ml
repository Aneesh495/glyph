(** Worklist-based dataflow framework. *)

module type LATTICE = sig
  type t
  val bottom : t
  val equal : t -> t -> bool
  val join : t -> t -> t
  val to_string : t -> string
end

module Make (L : LATTICE) = struct
  type direction = Forward | Backward

  type transfer = block:int -> L.t -> L.t

  type result = {
    in_ : L.t array;
    out : L.t array;
  }

  let analyze ~(direction : direction) ~(graph : Digraph.t) ~entry ~transfer
      ~init =
    let n = Digraph.node_count graph in
    let in_ = Array.make n L.bottom in
    let out = Array.make n L.bottom in
    for i = 0 to n - 1 do
      in_.(i) <- init i;
      out.(i) <- init i
    done;
    let work = Queue.create () in
    let pending = Array.make n false in
    let enqueue v =
      if not pending.(v) then (
        pending.(v) <- true;
        Queue.push v work)
    in
    (match direction with
    | Forward ->
        let order = Digraph.reverse_postorder graph ~entry in
        List.iter enqueue order
    | Backward ->
        Digraph.iter_nodes graph enqueue);
    while not (Queue.is_empty work) do
      let b = Queue.pop work in
      pending.(b) <- false;
      match direction with
      | Forward ->
          let preds = Digraph.predecessors graph b in
          let in_b =
            Digraph.IntSet.fold
              (fun p acc -> L.join acc out.(p))
              preds L.bottom
          in
          in_.(b) <- in_b;
          let out_b = transfer ~block:b in_b in
          if not (L.equal out.(b) out_b) then (
            out.(b) <- out_b;
            Digraph.IntSet.iter enqueue (Digraph.successors graph b))
      | Backward ->
          let succs = Digraph.successors graph b in
          let out_b =
            Digraph.IntSet.fold
              (fun s acc -> L.join acc in_.(s))
              succs L.bottom
          in
          out.(b) <- out_b;
          let in_b = transfer ~block:b out_b in
          if not (L.equal in_.(b) in_b) then (
            in_.(b) <- in_b;
            Digraph.IntSet.iter enqueue (Digraph.predecessors graph b))
    done;
    { in_; out }
end

(** Bitset lattice for classic bit-vector dataflow. *)
module Bitset = struct
  type t = bool array

  let make n = Array.make n false
  let bottom_size n = Array.make n false
  let copy a = Array.copy a
  let equal a b =
    let n = Array.length a in
    let rec loop i =
      if i >= n then true else a.(i) = b.(i) && loop (i + 1)
    in
    loop 0

  let join a b =
    let n = Array.length a in
    Array.init n (fun i -> a.(i) || b.(i))

  let meet a b =
    let n = Array.length a in
    Array.init n (fun i -> a.(i) && b.(i))

  let diff a b =
    let n = Array.length a in
    Array.init n (fun i -> a.(i) && not b.(i))

  let set a i = a.(i) <- true
  let clear a i = a.(i) <- false
  let mem a i = a.(i)

  let to_list a =
    let acc = ref [] in
    Array.iteri (fun i v -> if v then acc := i :: !acc) a;
    List.rev !acc

  let to_string a =
    to_list a |> List.map string_of_int |> String.concat ","
end
