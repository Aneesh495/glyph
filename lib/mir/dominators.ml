(** Dominator tree and dominance frontiers (Cytron / Cooper–Harvey–Kennedy).

    Uses the iterative dataflow algorithm of Cooper, Harvey, Kennedy
    ("A Simple, Fast Dominance Algorithm") over the [Cfg] digraph view. *)

open Mir

type tree = {
  idom : (label, label) Hashtbl.t;
  (** Immediate dominator; entry maps to itself. *)
  children : (label, label list) Hashtbl.t;
  (** Dominator-tree children. *)
  df : (label, Label.Set.t) Hashtbl.t;
  (** Dominance frontier. *)
  dom_depth : (label, int) Hashtbl.t;
  rpo : label list;
}

let dominates (t : tree) ~dominator ~node =
  let rec go n =
    if Label.equal n dominator then true
    else if Label.equal n (List.hd t.rpo) (* won't hit *) then false
    else
      match Hashtbl.find_opt t.idom n with
      | None -> false
      | Some p when Label.equal p n -> Label.equal dominator n
      | Some p -> go p
  in
  go node

let strictly_dominates t ~dominator ~node =
  (not (Label.equal dominator node)) && dominates t ~dominator ~node

let idom_of t l = Hashtbl.find_opt t.idom l

let children_of t l =
  try Hashtbl.find t.children l with Not_found -> []

let dominance_frontier t l =
  try Hashtbl.find t.df l with Not_found -> Label.Set.empty

(* -------------------------------------------------------------------------- *)
(* Cooper–Harvey–Kennedy                                                      *)
(* -------------------------------------------------------------------------- *)

let compute (f : func) : tree =
  let f = Cfg.ensure_cfg f in
  let rpo = Cfg.reachable_rpo f in
  let n = List.length rpo in
  if n = 0 then
    {
      idom = Hashtbl.create 1;
      children = Hashtbl.create 1;
      df = Hashtbl.create 1;
      dom_depth = Hashtbl.create 1;
      rpo = [];
    }
  else
    let idx = Cfg.to_digraph f in
    (* Restrict to reachable RPO labels. *)
    let labels = Array.of_list rpo in
    let index_of = Hashtbl.create n in
    Array.iteri (fun i l -> Hashtbl.replace index_of l i) labels;
    let entry = f.entry in
    let entry_i = Hashtbl.find index_of entry in

    (* idom as indices; -1 = undefined *)
    let idom = Array.make n (-1) in
    idom.(entry_i) <- entry_i;

    let intersect i1 i2 =
      let finger1 = ref i1 in
      let finger2 = ref i2 in
      while !finger1 <> !finger2 do
        while !finger1 > !finger2 do
          finger1 := idom.(!finger1)
        done;
        while !finger2 > !finger1 do
          finger2 := idom.(!finger2)
        done
      done;
      !finger1
    in

    let rpo_order = Array.init n (fun i -> i) in
    (* Process in RPO excluding entry. *)
    let changed = ref true in
    while !changed do
      changed := false;
      for k = 0 to n - 1 do
        let i = rpo_order.(k) in
        if i = entry_i then ()
        else
          let l = labels.(i) in
          let preds = Cfg.predecessors f l in
          let new_idom = ref (-1) in
          List.iter
            (fun p ->
              match Hashtbl.find_opt index_of p with
              | None -> ()
              | Some pi ->
                  if idom.(pi) >= 0 then
                    if !new_idom < 0 then new_idom := pi
                    else new_idom := intersect !new_idom pi)
            preds;
          if !new_idom >= 0 && idom.(i) <> !new_idom then (
            idom.(i) <- !new_idom;
            changed := true)
      done
    done;

    let idom_tbl = Hashtbl.create n in
    let children = Hashtbl.create n in
    Array.iteri
      (fun i l ->
        let d = labels.(idom.(i)) in
        Hashtbl.replace idom_tbl l d;
        if not (Label.equal l d) then
          let kids = try Hashtbl.find children d with Not_found -> [] in
          Hashtbl.replace children d (l :: kids))
      labels;
    Hashtbl.iter
      (fun k vs -> Hashtbl.replace children k (List.sort_uniq Label.compare vs))
      children;

    (* Dominance frontiers. *)
    let df = Hashtbl.create n in
    Array.iter (fun l -> Hashtbl.replace df l Label.Set.empty) labels;
    Array.iter
      (fun l ->
        let preds = Cfg.predecessors f l in
        if List.length preds >= 2 then
          List.iter
            (fun p ->
              let runner = ref p in
              let idom_l = Hashtbl.find idom_tbl l in
              while not (Label.equal !runner idom_l) do
                let set =
                  try Hashtbl.find df !runner with Not_found -> Label.Set.empty
                in
                Hashtbl.replace df !runner (Label.Set.add l set);
                match Hashtbl.find_opt idom_tbl !runner with
                | None -> runner := idom_l (* break *)
                | Some d ->
                    if Label.equal d !runner then runner := idom_l
                    else runner := d
              done)
            preds)
      labels;

    let dom_depth = Hashtbl.create n in
    let rec depth l =
      match Hashtbl.find_opt dom_depth l with
      | Some d -> d
      | None ->
          let d =
            if Label.equal l entry then 0
            else
              match Hashtbl.find_opt idom_tbl l with
              | None -> 0
              | Some p when Label.equal p l -> 0
              | Some p -> 1 + depth p
          in
          Hashtbl.replace dom_depth l d;
          d
    in
    Array.iter (fun l -> ignore (depth l)) labels;

    { idom = idom_tbl; children; df; dom_depth; rpo }

(** Iterate dominator-tree children in DFS preorder starting at entry. *)
let iter_dom_tree (t : tree) ~(entry : label) f =
  let rec go l =
    f l;
    List.iter go (children_of t l)
  in
  go entry

let pp fmt (t : tree) =
  Format.fprintf fmt "Dominators:@,";
  List.iter
    (fun l ->
      let id =
        match Hashtbl.find_opt t.idom l with
        | None -> "?"
        | Some i -> Label.to_string i
      in
      let frontier =
        dominance_frontier t l |> Label.Set.elements
        |> List.map Label.to_string |> String.concat ","
      in
      Format.fprintf fmt "  %a idom=%s df={%s}@," Label.pp l id frontier)
    t.rpo
