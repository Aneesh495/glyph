open Mir
type t = {
  idom : (label, label) Hashtbl.t;
  children : (label, label list) Hashtbl.t;
  df : (label, label list) Hashtbl.t;
  rpo : label list;
}
let compute f =
  let rpo = Cfg.reverse_postorder f in
  let idom = Hashtbl.create 16 in
  (match rpo with e :: _ -> Hashtbl.replace idom e e | [] -> ());
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun b ->
      if b <> f.entry then
        let preds = Cfg.predecessors f b |> List.filter (Hashtbl.mem idom) in
        match preds with
        | [] -> ()
        | p0 :: rest ->
            let rec depth x =
              if x = f.entry then 0 else
              match Hashtbl.find_opt idom x with Some p when p <> x -> 1+depth p | _ -> 0
            in
            let rec intersect a b =
              if a = b then a
              else if depth a > depth b then intersect (Hashtbl.find idom a) b
              else if depth b > depth a then intersect a (Hashtbl.find idom b)
              else intersect (Hashtbl.find idom a) (Hashtbl.find idom b)
            in
            let ni = List.fold_left intersect p0 rest in
            match Hashtbl.find_opt idom b with Some o when o = ni -> () | _ ->
              Hashtbl.replace idom b ni; changed := true
    ) rpo
  done;
  let children = Hashtbl.create 16 in
  Hashtbl.iter (fun n p -> if n <> p then
    Hashtbl.replace children p (n :: (try Hashtbl.find children p with Not_found -> []))) idom;
  let df = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace df l []) rpo;
  List.iter (fun b ->
    let preds = Cfg.predecessors f b in
    if List.length preds >= 2 then
      List.iter (fun p ->
        let rec runner r =
          if r <> Hashtbl.find idom b then (
            let set = try Hashtbl.find df r with Not_found -> [] in
            if not (List.mem b set) then Hashtbl.replace df r (b :: set);
            match Hashtbl.find_opt idom r with Some r' when r' <> r -> runner r' | _ -> ())
        in runner p) preds
  ) rpo;
  { idom; children; df; rpo }
let idom_of t l = match Hashtbl.find_opt t.idom l with Some p when p = l -> None | x -> x
let dominates t d n =
  let rec go x = if x = d then true else match Hashtbl.find_opt t.idom x with
    | None -> false | Some p when p = x -> x = d | Some p -> go p
  in go n
let dominance_frontier t l = try Hashtbl.find t.df l with Not_found -> []
let children_of t l = try Hashtbl.find t.children l with Not_found -> []
let dominator_tree_preorder t = t.rpo
let iterated_dominance_frontier t nodes =
  let q = Queue.create () and r = Hashtbl.create 8 in
  List.iter (fun n -> Queue.add n q) nodes;
  while not (Queue.is_empty q) do
    let n = Queue.take q in
    List.iter (fun d -> if not (Hashtbl.mem r d) then (Hashtbl.replace r d (); Queue.add d q))
      (dominance_frontier t n)
  done;
  Hashtbl.fold (fun d _ a -> d::a) r []
let pp fmt t = Format.fprintf fmt "doms(%d)" (List.length t.rpo)
