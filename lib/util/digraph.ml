(** Directed graph utilities used by CFG / SSA construction. *)

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

type t = {
  n : int;
  succ : IntSet.t array;
  pred : IntSet.t array;
}

let create n =
  {
    n;
    succ = Array.make n IntSet.empty;
    pred = Array.make n IntSet.empty;
  }

let add_edge g ~src ~dst =
  if src < 0 || dst < 0 || src >= g.n || dst >= g.n then
    invalid_arg "Digraph.add_edge";
  g.succ.(src) <- IntSet.add dst g.succ.(src);
  g.pred.(dst) <- IntSet.add src g.pred.(dst)

let successors g v = g.succ.(v)
let predecessors g v = g.pred.(v)
let node_count g = g.n

let iter_nodes g f =
  for i = 0 to g.n - 1 do
    f i
  done

let iter_edges g f =
  for src = 0 to g.n - 1 do
    IntSet.iter (fun dst -> f src dst) g.succ.(src)
  done

let edges g =
  let acc = ref [] in
  iter_edges g (fun s d -> acc := (s, d) :: !acc);
  List.rev !acc

(** Depth-first search yielding preorder and postorder. *)
let dfs g ~entry =
  let visited = Array.make g.n false in
  let preorder = ref [] in
  let postorder = ref [] in
  let rec visit v =
    if not visited.(v) then (
      visited.(v) <- true;
      preorder := v :: !preorder;
      IntSet.iter visit g.succ.(v);
      postorder := v :: !postorder)
  in
  visit entry;
  (List.rev !preorder, List.rev !postorder)

let reverse_postorder g ~entry =
  let _, post = dfs g ~entry in
  List.rev post

(** Topological sort; None if a cycle exists among reachable nodes. *)
let topo_sort g ~entry =
  let visited = Array.make g.n false in
  let on_stack = Array.make g.n false in
  let order = ref [] in
  let cycle = ref false in
  let rec visit v =
    if on_stack.(v) then cycle := true
    else if not visited.(v) then (
      visited.(v) <- true;
      on_stack.(v) <- true;
      IntSet.iter visit g.succ.(v);
      on_stack.(v) <- false;
      order := v :: !order)
  in
  visit entry;
  if !cycle then None else Some (List.rev !order)

(** Transpose of the graph. *)
let transpose g =
  let g' = create g.n in
  iter_edges g (fun s d -> add_edge g' ~src:d ~dst:s);
  g'

(** Strongly connected components (Tarjan). *)
let strongly_connected_components g =
  let index = ref 0 in
  let stack = ref [] in
  let on_stack = Array.make g.n false in
  let indices = Array.make g.n (-1) in
  let lowlink = Array.make g.n (-1) in
  let sccs = ref [] in
  let rec strongconnect v =
    indices.(v) <- !index;
    lowlink.(v) <- !index;
    incr index;
    stack := v :: !stack;
    on_stack.(v) <- true;
    IntSet.iter
      (fun w ->
        if indices.(w) < 0 then (
          strongconnect w;
          lowlink.(v) <- min lowlink.(v) lowlink.(w))
        else if on_stack.(w) then
          lowlink.(v) <- min lowlink.(v) indices.(w))
      g.succ.(v);
    if lowlink.(v) = indices.(v) then (
      let rec pop acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
            stack := rest;
            on_stack.(w) <- false;
            let acc = w :: acc in
            if w = v then acc else pop acc
      in
      sccs := pop [] :: !sccs)
  in
  for v = 0 to g.n - 1 do
    if indices.(v) < 0 then strongconnect v
  done;
  List.rev !sccs

(** Breadth-first distances from entry; ~-1 if unreachable. *)
let bfs_dist g ~entry =
  let dist = Array.make g.n (-1) in
  let q = Queue.create () in
  dist.(entry) <- 0;
  Queue.push entry q;
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    IntSet.iter
      (fun w ->
        if dist.(w) < 0 then (
          dist.(w) <- dist.(v) + 1;
          Queue.push w q))
      g.succ.(v)
  done;
  dist

let reachable g ~entry =
  let vis = Array.make g.n false in
  let rec go v =
    if not vis.(v) then (
      vis.(v) <- true;
      IntSet.iter go g.succ.(v))
  in
  go entry;
  vis

let pp fmt g =
  Format.fprintf fmt "digraph {@,";
  iter_edges g (fun s d -> Format.fprintf fmt "  n%d -> n%d;@," s d);
  Format.fprintf fmt "}"
