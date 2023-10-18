(** Dominator tree and dominance frontiers for SSA construction.

    Implements the iterative dataflow algorithm of Cooper, Harvey & Kennedy
    (a simplified Lengauer–Tarjan alternative that is fast on typical CFGs),
    plus Cytron-style dominance frontiers. *)

open Mir

type t = {
  idom : (label, label) Hashtbl.t;
  (** Immediate dominator of each label (entry maps to itself). *)
  df : (label, label list) Hashtbl.t;
  (** Dominance frontier. *)
  children : (label, label list) Hashtbl.t;
  (** Dominator-tree children. *)
  rpo : label list;
  entry : label;
}

let compute (fn : func) : t =
  let cfg = Cfg.build fn in
  let entry = fn.entry in
  let rpo = Cfg.reverse_postorder cfg in
  let rpo_index = Hashtbl.create 32 in
  List.iteri (fun i lbl -> Hashtbl.replace rpo_index lbl i) rpo;
  let idom = Hashtbl.create 32 in
  Hashtbl.replace idom entry entry;
  let intersect b1 b2 =
    let finger1 = ref b1 in
    let finger2 = ref b2 in
    while !finger1 <> !finger2 do
      while Hashtbl.find rpo_index !finger1 > Hashtbl.find rpo_index !finger2 do
        finger1 := Hashtbl.find idom !finger1
      done;
      while Hashtbl.find rpo_index !finger2 > Hashtbl.find rpo_index !finger1 do
        finger2 := Hashtbl.find idom !finger2
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
            List.filter (fun p -> Hashtbl.mem idom p) (Cfg.predecessors cfg b)
          in
          match preds with
          | [] -> ()
          | p0 :: rest ->
              let new_idom = List.fold_left intersect p0 rest in
              (match Hashtbl.find_opt idom b with
              | Some old when old = new_idom -> ()
              | _ ->
                  Hashtbl.replace idom b new_idom;
                  changed := true))
      rpo
  done;
  (* Dominator tree children *)
  let children = Hashtbl.create 32 in
  List.iter (fun lbl -> Hashtbl.replace children lbl []) rpo;
  Hashtbl.iter
    (fun b d ->
      if b <> d then
        let ch = Hashtbl.find children d in
        Hashtbl.replace children d (b :: ch))
    idom;
  (* Dominance frontiers (Cytron) *)
  let df = Hashtbl.create 32 in
  List.iter (fun lbl -> Hashtbl.replace df lbl []) rpo;
  List.iter
    (fun b ->
      let preds = Cfg.predecessors cfg b in
      if List.length preds >= 2 then
        List.iter
          (fun p ->
            let runner = ref p in
            while
              (match Hashtbl.find_opt idom b with
              | Some d -> !runner <> d
              | None -> false)
            do
              let cur = Hashtbl.find df !runner in
              if not (List.mem b cur) then
                Hashtbl.replace df !runner (b :: cur);
              match Hashtbl.find_opt idom !runner with
              | Some d when d <> !runner -> runner := d
              | _ -> runner := entry (* break *)
            done)
          preds)
    rpo;
  { idom; df; children; rpo; entry }

let idom_of dom lbl = Hashtbl.find_opt dom.idom lbl

let dominates dom a b =
  if a = b then true
  else
    let rec walk x =
      if x = a then true
      else
        match Hashtbl.find_opt dom.idom x with
        | Some d when d <> x -> walk d
        | _ -> false
    in
    walk b

let dominance_frontier dom lbl =
  match Hashtbl.find_opt dom.df lbl with
  | Some xs -> xs
  | None -> []

let children_of dom lbl =
  match Hashtbl.find_opt dom.children lbl with
  | Some xs -> xs
  | None -> []

let dominator_tree_preorder dom =
  let acc = ref [] in
  let rec walk n =
    acc := n :: !acc;
    List.iter walk (children_of dom n)
  in
  walk dom.entry;
  List.rev !acc

(** Iterated dominance frontier of a set of labels (used for φ placement). *)
let iterated_dominance_frontier dom (defs : label list) : label list =
  let work = Queue.create () in
  let in_work = Hashtbl.create 16 in
  let result = Hashtbl.create 16 in
  List.iter
    (fun d ->
      Queue.push d work;
      Hashtbl.replace in_work d ())
    defs;
  while not (Queue.is_empty work) do
    let b = Queue.pop work in
    Hashtbl.remove in_work b;
    List.iter
      (fun y ->
        if not (Hashtbl.mem result y) then (
          Hashtbl.replace result y ();
          if not (Hashtbl.mem in_work y) then (
            Hashtbl.replace in_work y ();
            Queue.push y work)))
      (dominance_frontier dom b)
  done;
  Hashtbl.fold (fun y () acc -> y :: acc) result []

let pp fmt dom =
  Format.fprintf fmt "Dominators (entry L%d):@." dom.entry;
  List.iter
    (fun lbl ->
      let id =
        match idom_of dom lbl with
        | Some d -> Printf.sprintf "L%d" d
        | None -> "?"
      in
      let df =
        dominance_frontier dom lbl
        |> List.map (fun l -> Printf.sprintf "L%d" l)
        |> String.concat ", "
      in
      Format.fprintf fmt "  L%d idom=%s DF={%s}@." lbl id df)
    dom.rpo
