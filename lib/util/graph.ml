(** Directed graphs for CFG and dependency analysis. *)

type node = int

type 'a t = {
  mutable payloads : 'a option array;
  mutable ncount : int;
  succ : (node, node list) Hashtbl.t;
  pred : (node, node list) Hashtbl.t;
  mutable ecount : int;
}

let create ?(size = 32) () =
  {
    payloads = Array.make size None;
    ncount = 0;
    succ = Hashtbl.create size;
    pred = Hashtbl.create size;
    ecount = 0;
  }

let ensure g n =
  if n >= Array.length g.payloads then (
    let size = max (n + 1) (Array.length g.payloads * 2) in
    let arr = Array.make size None in
    Array.blit g.payloads 0 arr 0 (Array.length g.payloads);
    g.payloads <- arr)

let copy g =
  let g' = create ~size:(max 8 g.ncount) () in
  g'.payloads <- Array.copy g.payloads;
  g'.ncount <- g.ncount;
  g'.ecount <- g.ecount;
  Hashtbl.iter (fun k v -> Hashtbl.add g'.succ k (List.map Fun.id v)) g.succ;
  Hashtbl.iter (fun k v -> Hashtbl.add g'.pred k (List.map Fun.id v)) g.pred;
  g'

let clear g =
  g.payloads <- Array.make 32 None;
  g.ncount <- 0;
  Hashtbl.clear g.succ;
  Hashtbl.clear g.pred;
  g.ecount <- 0

let add_node g payload =
  let n = g.ncount in
  ensure g n;
  g.payloads.(n) <- Some payload;
  g.ncount <- n + 1;
  Hashtbl.replace g.succ n [];
  Hashtbl.replace g.pred n [];
  n

let set_payload g n payload =
  if n < 0 || n >= g.ncount then invalid_arg "Graph.set_payload";
  g.payloads.(n) <- Some payload

let payload g n =
  if n < 0 || n >= g.ncount then invalid_arg "Graph.payload";
  match g.payloads.(n) with
  | Some p -> p
  | None -> invalid_arg "Graph.payload: missing"

let node_count g = g.ncount
let mem_node g n = n >= 0 && n < g.ncount

let add_edge g ~src ~dst =
  if not (mem_node g src && mem_node g dst) then
    invalid_arg "Graph.add_edge";
  let succs = Hashtbl.find g.succ src in
  if not (List.mem dst succs) then (
    Hashtbl.replace g.succ src (dst :: succs);
    let preds = Hashtbl.find g.pred dst in
    Hashtbl.replace g.pred dst (src :: preds);
    g.ecount <- g.ecount + 1)

let remove_edge g ~src ~dst =
  if not (mem_node g src && mem_node g dst) then
    invalid_arg "Graph.remove_edge";
  let succs = Hashtbl.find g.succ src in
  if List.mem dst succs then (
    Hashtbl.replace g.succ src (List.filter (( <> ) dst) succs);
    let preds = Hashtbl.find g.pred dst in
    Hashtbl.replace g.pred dst (List.filter (( <> ) src) preds);
    g.ecount <- g.ecount - 1)

let has_edge g ~src ~dst =
  mem_node g src && List.mem dst (Hashtbl.find g.succ src)

let successors g n =
  if not (mem_node g n) then invalid_arg "Graph.successors";
  List.rev (Hashtbl.find g.succ n)

let predecessors g n =
  if not (mem_node g n) then invalid_arg "Graph.predecessors";
  List.rev (Hashtbl.find g.pred n)

let out_degree g n = List.length (Hashtbl.find g.succ n)
let in_degree g n = List.length (Hashtbl.find g.pred n)
let edge_count g = g.ecount

let iter_nodes g f =
  for n = 0 to g.ncount - 1 do
    match g.payloads.(n) with
    | Some p -> f n p
    | None -> ()
  done

let fold_nodes g f acc =
  let acc = ref acc in
  iter_nodes g (fun n p -> acc := f n p !acc);
  !acc

let iter_edges g f =
  Hashtbl.iter
    (fun src succs -> List.iter (fun dst -> f ~src ~dst) succs)
    g.succ

let dfs g ~start f =
  if not (mem_node g start) then invalid_arg "Graph.dfs";
  let visited = Array.make g.ncount false in
  let rec go n =
    if not visited.(n) then (
      visited.(n) <- true;
      f n;
      List.iter go (successors g n))
  in
  go start

let bfs g ~start f =
  if not (mem_node g start) then invalid_arg "Graph.bfs";
  let visited = Array.make g.ncount false in
  let q = Queue.create () in
  Queue.push start q;
  visited.(start) <- true;
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    f n;
    List.iter
      (fun s ->
        if not visited.(s) then (
          visited.(s) <- true;
          Queue.push s q))
      (successors g n)
  done

let reachable g ~start =
  let acc = ref [] in
  dfs g ~start (fun n -> acc := n :: !acc);
  List.rev !acc

let reverse_postorder g ~entry =
  if not (mem_node g entry) then invalid_arg "Graph.reverse_postorder";
  let visited = Array.make g.ncount false in
  let order = ref [] in
  let rec visit n =
    if not visited.(n) then (
      visited.(n) <- true;
      List.iter visit (successors g n);
      order := n :: !order)
  in
  visit entry;
  !order

let topo_sort g =
  let indeg = Array.init g.ncount (fun n -> in_degree g n) in
  let q = Queue.create () in
  for n = 0 to g.ncount - 1 do
    if indeg.(n) = 0 then Queue.push n q
  done;
  let order = ref [] in
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    order := n :: !order;
    List.iter
      (fun s ->
        indeg.(s) <- indeg.(s) - 1;
        if indeg.(s) = 0 then Queue.push s q)
      (successors g n)
  done;
  let order = List.rev !order in
  if List.length order = g.ncount then Ok order
  else
    let cycle =
      List.init g.ncount Fun.id
      |> List.filter (fun n -> indeg.(n) > 0)
    in
    Error cycle

let sccs g =
  let index = ref 0 in
  let stack = ref [] in
  let on_stack = Array.make g.ncount false in
  let indices = Array.make g.ncount (-1) in
  let lowlink = Array.make g.ncount (-1) in
  let comps = ref [] in
  let rec strongconnect v =
    indices.(v) <- !index;
    lowlink.(v) <- !index;
    incr index;
    stack := v :: !stack;
    on_stack.(v) <- true;
    List.iter
      (fun w ->
        if indices.(w) < 0 then (
          strongconnect w;
          lowlink.(v) <- min lowlink.(v) lowlink.(w))
        else if on_stack.(w) then
          lowlink.(v) <- min lowlink.(v) indices.(w))
      (successors g v);
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
      comps := pop [] :: !comps)
  in
  for v = 0 to g.ncount - 1 do
    if indices.(v) < 0 then strongconnect v
  done;
  List.rev !comps

let condensation g =
  let comps = sccs g in
  let comp_of = Array.make g.ncount (-1) in
  List.iteri
    (fun ci nodes -> List.iter (fun n -> comp_of.(n) <- ci) nodes)
    comps;
  let cg = create ~size:(List.length comps) () in
  List.iteri
    (fun i nodes -> ignore (add_node cg nodes))
    comps;
  let seen = Hashtbl.create 64 in
  iter_edges g (fun ~src ~dst ->
      let a = comp_of.(src) in
      let b = comp_of.(dst) in
      if a <> b then
        let key = (a, b) in
        if not (Hashtbl.mem seen key) then (
          Hashtbl.add seen key ();
          add_edge cg ~src:a ~dst:b));
  (cg, fun n -> comp_of.(n))

module Dom = struct
  let immediate_dominators g ~entry =
    if not (mem_node g entry) then
      invalid_arg "Graph.Dom.immediate_dominators";
    let n = g.ncount in
    let idom = Array.make n (-1) in
    let order = reverse_postorder g ~entry in
    let rpo_index = Array.make n (-1) in
    List.iteri (fun i b -> rpo_index.(b) <- i) order;
    idom.(entry) <- entry;
    let intersect b1 b2 =
      let finger1 = ref b1 in
      let finger2 = ref b2 in
      while !finger1 <> !finger2 do
        while rpo_index.(!finger1) > rpo_index.(!finger2) do
          finger1 := idom.(!finger1)
        done;
        while rpo_index.(!finger2) > rpo_index.(!finger1) do
          finger2 := idom.(!finger2)
        done
      done;
      !finger1
    in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun b ->
          if b <> entry then
            let preds =
              List.filter (fun p -> idom.(p) >= 0) (predecessors g b)
            in
            match preds with
            | [] -> ()
            | p0 :: rest ->
                let new_idom =
                  List.fold_left intersect p0 rest
                in
                if idom.(b) <> new_idom then (
                  idom.(b) <- new_idom;
                  changed := true))
        order
    done;
    idom

  let children ~idom =
    let n = Array.length idom in
    let ch = Array.make n [] in
    for i = 0 to n - 1 do
      if idom.(i) >= 0 && idom.(i) <> i then
        ch.(idom.(i)) <- i :: ch.(idom.(i))
    done;
    ch

  let dominates ~idom a b =
    if a = b then true
    else
      let rec walk x =
        if x = a then true
        else if idom.(x) = x || idom.(x) < 0 then false
        else walk idom.(x)
      in
      walk b

  let dominance_frontiers g ~entry ~idom =
    ignore entry;
    let n = g.ncount in
    let df = Array.make n [] in
    for b = 0 to n - 1 do
      let preds = predecessors g b in
      if List.length preds >= 2 then
        List.iter
          (fun p ->
            let runner = ref p in
            while !runner <> idom.(b) && !runner >= 0 do
              if not (List.mem b df.(!runner)) then
                df.(!runner) <- b :: df.(!runner);
              runner := idom.(!runner)
            done)
          preds
    done;
    df
end
