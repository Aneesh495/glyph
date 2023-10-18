(** Control-flow graph utilities over [Mir.func].

    Maintains predecessor / successor edges on blocks, reverse-postorder
    traversal, reachability, and a [Digraph] view used by dominators and
    dataflow analyses. *)

open Mir

(* -------------------------------------------------------------------------- *)
(* Edge maintenance                                                           *)
(* -------------------------------------------------------------------------- *)

let recompute_edges (f : func) : unit =
  Label.Map.iter
    (fun _ (b : block) ->
      b.preds <- [];
      b.succs <- terminator_succs b.terminator)
    f.blocks;
  Label.Map.iter
    (fun _ (b : block) ->
      List.iter
        (fun succ ->
          match find_block f succ with
          | None -> ()
          | Some sb -> sb.preds <- b.label :: sb.preds)
        b.succs)
    f.blocks;
  (* Deterministic order. *)
  Label.Map.iter
    (fun _ (b : block) ->
      b.preds <- List.sort_uniq Label.compare b.preds;
      b.succs <- List.sort_uniq Label.compare b.succs)
    f.blocks

let successors (f : func) (l : label) : label list =
  match find_block f l with
  | Some b -> b.succs
  | None -> []

let predecessors (f : func) (l : label) : label list =
  match find_block f l with
  | Some b -> b.preds
  | None -> []

let successors_of = successors

(* -------------------------------------------------------------------------- *)
(* Traversal                                                                  *)
(* -------------------------------------------------------------------------- *)

let reachable_labels (f : func) : Label.Set.t =
  let vis = ref Label.Set.empty in
  let rec dfs l =
    if Label.Set.mem l !vis then ()
    else (
      vis := Label.Set.add l !vis;
      List.iter dfs (successors f l))
  in
  dfs f.entry;
  !vis

let reverse_postorder (f : func) : label list =
  let visited = Hashtbl.create 32 in
  let post = ref [] in
  let rec dfs l =
    if Hashtbl.mem visited l then ()
    else (
      Hashtbl.add visited l ();
      List.iter dfs (successors f l);
      post := l :: !post)
  in
  dfs f.entry;
  (* Unreachable blocks, stable by label name. *)
  let unreachable =
    Label.Map.bindings f.blocks
    |> List.map fst
    |> List.filter (fun l -> not (Hashtbl.mem visited l))
    |> List.sort Label.compare
  in
  List.iter
    (fun l ->
      Hashtbl.add visited l ();
      post := l :: !post)
    (List.rev unreachable);
  List.rev !post

let postorder (f : func) : label list = List.rev (reverse_postorder f)

let preorder (f : func) : label list =
  let visited = Hashtbl.create 32 in
  let acc = ref [] in
  let rec dfs l =
    if Hashtbl.mem visited l then ()
    else (
      Hashtbl.add visited l ();
      acc := l :: !acc;
      List.iter dfs (successors f l))
  in
  dfs f.entry;
  List.rev !acc

let rpo_blocks (f : func) : block list =
  List.filter_map (find_block f) (reverse_postorder f)

(** Blocks in RPO that are reachable from entry. *)
let reachable_rpo (f : func) : label list =
  let reach = reachable_labels f in
  List.filter (fun l -> Label.Set.mem l reach) (reverse_postorder f)

(* -------------------------------------------------------------------------- *)
(* Digraph view                                                               *)
(* -------------------------------------------------------------------------- *)

type indexed = {
  labels : label array;
  index_of : (label, int) Hashtbl.t;
  graph : Digraph.t;
  entry_idx : int;
}

let to_digraph (f : func) : indexed =
  recompute_edges f;
  let labels =
    reverse_postorder f |> Array.of_list
  in
  let n = Array.length labels in
  let index_of = Hashtbl.create n in
  Array.iteri (fun i l -> Hashtbl.replace index_of l i) labels;
  let g = Digraph.create n in
  Array.iteri
    (fun i l ->
      List.iter
        (fun s ->
          match Hashtbl.find_opt index_of s with
          | None -> ()
          | Some j -> Digraph.add_edge g ~src:i ~dst:j)
        (successors f l))
    labels;
  let entry_idx =
    match Hashtbl.find_opt index_of f.entry with
    | Some i -> i
    | None -> 0
  in
  { labels; index_of; graph = g; entry_idx }

let label_of_index (idx : indexed) i = idx.labels.(i)

let index_of_label (idx : indexed) l = Hashtbl.find idx.index_of l

(* -------------------------------------------------------------------------- *)
(* Critical edges / splits                                                    *)
(* -------------------------------------------------------------------------- *)

let is_critical_edge (f : func) ~src ~dst =
  let src_succs = successors f src in
  let dst_preds = predecessors f dst in
  List.length src_succs > 1 && List.length dst_preds > 1

(** Insert an empty block on every critical edge; returns the updated function
    and the list of newly created labels. *)
let split_critical_edges (f : func) : func * label list =
  recompute_edges f;
  let created = ref [] in
  let edges = ref [] in
  iter_blocks
    (fun (b : block) ->
      List.iter
        (fun succ ->
          if is_critical_edge f ~src:b.label ~dst:succ then
            edges := (b.label, succ) :: !edges)
        b.succs)
    f;
  List.iter
    (fun (src, dst) ->
      let mid = Label.fresh "crit" in
      created := mid :: !created;
      let mid_block =
        make_block mid (Jump (dst, Span.dummy)) ~preds:[ src ] ~succs:[ dst ]
      in
      set_block f mid_block;
      (match find_block f src with
      | None -> ()
      | Some sb ->
          let rewrite_term = function
            | Jump (l, sp) when Label.equal l dst -> Jump (mid, sp)
            | Branch ({ then_; else_; _ } as br) ->
                Branch
                  {
                    br with
                    then_ = (if Label.equal then_ dst then mid else then_);
                    else_ = (if Label.equal else_ dst then mid else else_);
                  }
            | Switch ({ cases; default; _ } as sw) ->
                Switch
                  {
                    sw with
                    default =
                      (if Label.equal default dst then mid else default);
                    cases =
                      List.map
                        (fun (tag, l) ->
                          (tag, if Label.equal l dst then mid else l))
                        cases;
                  }
            | t -> t
          in
          sb.terminator <- rewrite_term sb.terminator);
      (match find_block f dst with
      | None -> ()
      | Some db ->
          db.phis <-
            List.map
              (function
                | Phi ({ incoming; _ } as p) ->
                    Phi
                      {
                        p with
                        incoming =
                          List.map
                            (fun (l, v) ->
                              if Label.equal l src then (mid, v) else (l, v))
                            incoming;
                      }
                | i -> i)
              db.phis))
    !edges;
  recompute_edges f;
  (f, List.rev !created)

(* -------------------------------------------------------------------------- *)
(* CFG validation                                                             *)
(* -------------------------------------------------------------------------- *)

let validate (f : func) : string list =
  recompute_edges f;
  let errs = ref [] in
  let push msg = errs := msg :: !errs in
  (match find_block f f.entry with
  | None -> push "missing entry block"
  | Some _ -> ());
  iter_blocks
    (fun (b : block) ->
      List.iter
        (fun s ->
          if find_block f s = None then
            push
              (Printf.sprintf "%s: succ %s missing"
                 (Label.to_string b.label) (Label.to_string s)))
        (terminator_succs b.terminator);
      List.iter
        (fun p ->
          match find_block f p with
          | None ->
              push
                (Printf.sprintf "%s: pred %s missing"
                   (Label.to_string b.label) (Label.to_string p))
          | Some pb ->
              if not (List.exists (Label.equal b.label) pb.succs) then
                push
                  (Printf.sprintf "%s: pred %s does not list this as succ"
                     (Label.to_string b.label) (Label.to_string p)))
        b.preds)
    f;
  List.rev !errs

(* -------------------------------------------------------------------------- *)
(* Pretty                                                                     *)
(* -------------------------------------------------------------------------- *)

let pp_cfg fmt (f : func) =
  recompute_edges f;
  Format.fprintf fmt "CFG %a entry=%a@," Ident.pp f.name Label.pp f.entry;
  iter_blocks
    (fun (b : block) ->
      Format.fprintf fmt "  %a preds=[" Label.pp b.label;
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
        Label.pp fmt b.preds;
      Format.fprintf fmt "] succs=[";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
        Label.pp fmt b.succs;
      Format.fprintf fmt "]@,")
    f

(** Remove blocks unreachable from entry. *)
let prune_unreachable (f : func) : func =
  let reach = reachable_labels f in
  f.blocks <-
    Label.Map.filter (fun l _ -> Label.Set.mem l reach) f.blocks;
  recompute_edges f;
  f

(** Ensure every block has up-to-date pred/succ lists. *)
let ensure_cfg (f : func) : func =
  recompute_edges f;
  f
