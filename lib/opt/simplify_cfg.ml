(** CFG simplification: unreachable block elimination and branch simplification. *)

open Mir

let prune_unreachable (f : func) : func =
  let reachable = Hashtbl.create 16 in
  let rec dfs l =
    if not (Hashtbl.mem reachable l) then (
      Hashtbl.add reachable l ();
      List.iter dfs (Mir.successors_of f l))
  in
  dfs f.entry;
  let blocks =
    List.filter (fun (b : block) -> Hashtbl.mem reachable b.label) f.blocks
  in
  let clean_phis (b : block) =
    let phis =
      List.map
        (function
          | IPhi (d, incoming) ->
              let incoming' =
                List.filter
                  (fun (pred, _) -> Hashtbl.mem reachable pred)
                  incoming
              in
              IPhi (d, incoming')
          | other -> other)
        b.phis
    in
    { b with phis }
  in
  let blocks = List.map clean_phis blocks in
  { f with blocks }

let collapse_same_target_branches (f : func) : func =
  let clean_term = function
    | TBranch (_, t, e) when t = e -> TJump t
    | other -> other
  in
  let blocks =
    List.map (fun (b : block) -> { b with term = clean_term b.term }) f.blocks
  in
  { f with blocks }

let run_func (_ctx : Pass.context) (fn : func) : func =
  fn |> collapse_same_target_branches |> prune_unreachable

let pass = Pass.make_func_pass ~name:"simplify-cfg" run_func
