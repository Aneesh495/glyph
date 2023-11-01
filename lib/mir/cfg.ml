open Mir
let successors f l = successors_of f l
let predecessors f l =
  let tbl = Mir.predecessors f in
  try Hashtbl.find tbl l with Not_found -> []
let reverse_postorder f =
  let seen = Hashtbl.create 16 in
  let order = ref [] in
  let rec dfs l =
    if not (Hashtbl.mem seen l) then (
      Hashtbl.add seen l ();
      List.iter dfs (successors_of f l);
      order := l :: !order)
  in
  dfs f.entry; !order
let reachable = reverse_postorder
let remove_unreachable f =
  let live = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace live l ()) (reachable f);
  { f with blocks = List.filter (fun (b : block) -> Hashtbl.mem live b.label) f.blocks }
