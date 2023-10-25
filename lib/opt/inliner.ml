(** Function inlining with size heuristics.

    Candidates: callees marked small enough by instruction count, not
    recursive, single-call-site bonus, and not address-taken (all Mir
    functions are addressable via [VGlobal], so we treat direct calls only).

    Inlining clones the callee's blocks into the caller, renames labels and
    vregs, and splices returns into a join block that receives the result. *)

open Mir

type heuristic = {
  max_callee_size : int;
  max_growth : int;
  always_inline_size : int;
}

let default_heuristic =
  { max_callee_size = 64; max_growth = 256; always_inline_size = 12 }

let func_size = func_instr_count

let is_recursive (f : func) : bool =
  let rec seen = Label.Tbl.create 8 in
  ignore seen;
  let found = ref false in
  Label.Map.iter
    (fun _ b ->
      List.iter
        (function
          | Call { callee = VGlobal g; _ }
          | Call { callee = VReg _; _ } ->
              () (* vreg calls ignored *)
          | _ -> ())
        b.instrs;
      match b.terminator with
      | TailCall { callee = VGlobal g; _ } when Ident.equal g f.name ->
          found := true
      | _ -> ();
      List.iter
        (function
          | Call { callee = VGlobal g; _ } when Ident.equal g f.name ->
              found := true
          | _ -> ())
        b.instrs)
    f.blocks;
  !found

let direct_callee = function
  | Call { callee = VGlobal g; _ } -> Some g
  | _ -> None

let count_call_sites (prog : program) (name : Ident.t) : int =
  let n = ref 0 in
  Ident.Map.iter
    (fun _ f ->
      Label.Map.iter
        (fun _ b ->
          List.iter
            (fun instr ->
              match direct_callee instr with
              | Some g when Ident.equal g name -> incr n
              | _ -> ())
            b.instrs;
          match b.terminator with
          | TailCall { callee = VGlobal g; _ } when Ident.equal g name ->
              incr n
          | _ -> ())
        f.blocks)
    prog.funcs;
  !n

let should_inline ~(heur : heuristic) ~(caller : func) ~(callee : func)
    ~(call_sites : int) : bool =
  if Ident.equal caller.name callee.name then false
  else if is_recursive callee then false
  else
    let sz = func_size callee in
    if sz <= heur.always_inline_size then true
    else if sz > heur.max_callee_size then false
    else if call_sites = 1 then sz <= heur.max_callee_size * 2
    else
      let growth = sz * call_sites in
      growth <= heur.max_growth && sz <= heur.max_callee_size

(* -------------------------------------------------------------------------- *)
(* Cloning                                                                    *)
(* -------------------------------------------------------------------------- *)

type rename = {
  labels : label Label.Map.t;
  vregs : vreg Vreg.Map.t;
}

let empty_rename = { labels = Label.Map.empty; vregs = Vreg.Map.empty }

let map_label rn l =
  match Label.Map.find_opt l rn.labels with Some l' -> l' | None -> l

let map_vreg rn r =
  match Vreg.Map.find_opt r rn.vregs with Some r' -> r' | None -> r

let map_value rn = function
  | VReg r -> VReg (map_vreg rn r)
  | v -> v

let clone_instr rn = function
  | instr ->
      map_instr_values
        ~on_use:(map_value rn)
        ~on_def:(map_vreg rn) instr

let clone_term rn term =
  let term = map_terminator_values (map_value rn) term in
  match term with
  | Jump (l, sp) -> Jump (map_label rn l, sp)
  | Branch ({ then_; else_; _ } as b) ->
      Branch
        { b with then_ = map_label rn then_; else_ = map_label rn else_ }
  | Switch ({ cases; default; _ } as s) ->
      Switch
        {
          s with
          cases = List.map (fun (t, l) -> (t, map_label rn l)) cases;
          default = map_label rn default;
        }
  | t -> t

(** Build rename maps for all labels and vregs defined in [callee]. *)
let build_rename (callee : func) : rename =
  let labels =
    Label.Map.fold
      (fun l _ m -> Label.Map.add l (Label.fresh (Ident.name l)) m)
      callee.blocks Label.Map.empty
  in
  let vregs = ref Vreg.Map.empty in
  let add r =
    if not (Vreg.Map.mem r !vregs) then
      vregs := Vreg.Map.add r (Vreg.fresh (Ident.name r)) !vregs
  in
  List.iter (fun (p, _) -> add p) callee.params;
  Label.Map.iter
    (fun _ b ->
      List.iter
        (fun instr -> List.iter add (instr_defs instr))
        (block_all_instrs b))
    callee.blocks;
  { labels; vregs = !vregs }

(** Inline [callee] at a [Call] in [caller]'s block [host], instruction index
    [idx]. Returns [true] on success. *)
let inline_at ~(caller : func) ~(callee : func) ~(host : label) ~(idx : int)
    : bool =
  match find_block caller host with
  | None -> false
  | Some host_b ->
      let instrs = Array.of_list host_b.instrs in
      if idx < 0 || idx >= Array.length instrs then false
      else
        match instrs.(idx) with
        | Call { dst; callee = VGlobal g; args; span; _ }
          when Ident.equal g callee.name ->
            let rn = build_rename callee in
            let entry' = map_label rn callee.entry in
            let join = Label.fresh "inline_join" in
            let result =
              match dst with
              | Some d -> d
              | None -> Vreg.fresh "inline_unit"
            in
            (* Split host block: before / after the call. *)
            let before =
              Array.to_list (Array.sub instrs 0 idx)
            in
            let after =
              Array.to_list
                (Array.sub instrs (idx + 1) (Array.length instrs - idx - 1))
            in
            let old_term = host_b.terminator in
            host_b.instrs <- before;
            (* Bind parameters. *)
            List.iter2
              (fun (p, ty) arg ->
                let p' = map_vreg rn p in
                host_b.instrs <-
                  host_b.instrs
                  @ [ Assign { dst = p'; src = arg; ty; span } ])
              callee.params args;
            host_b.terminator <- Jump (entry', span);
            (* Clone callee blocks; rewrite returns to store + jump join. *)
            Label.Map.iter
              (fun _ cb ->
                let label' = map_label rn cb.label in
                let phis = List.map (clone_instr rn) cb.phis in
                let body = List.map (clone_instr rn) cb.instrs in
                let term =
                  match cb.terminator with
                  | Return (Some v, sp) ->
                      let v = map_value rn v in
                      let assign =
                        Assign
                          {
                            dst = result;
                            src = v;
                            ty = Ty_any;
                            span = sp;
                          }
                      in
                      (body @ [ assign ], Jump (join, sp))
                  | Return (None, sp) ->
                      ( body
                        @ [
                            Assign
                              {
                                dst = result;
                                src = VConst CUnit;
                                ty = Ty_unit;
                                span = sp;
                              };
                          ],
                        Jump (join, sp) )
                  | TailCall ({ callee = c; args; span } as tc) ->
                      (* Demote tail call to normal call into result. *)
                      let c = map_value rn c in
                      let args = List.map (map_value rn) args in
                      ( body
                        @ [
                            Call
                              {
                                dst = Some result;
                                callee = c;
                                args;
                                ty = Ty_any;
                                span;
                              };
                          ],
                        Jump (join, span) )
                  | t -> (body, clone_term rn t)
                in
                let body', term' = term in
                let nb =
                  make_block label' term' ~phis ~instrs:body'
                in
                set_block caller nb)
              callee.blocks;
            (* Join block continues with leftover instrs + old terminator. *)
            let join_b =
              make_block join old_term ~instrs:after
            in
            set_block caller join_b;
            Cfg.prepare caller;
            (* Cloned code is already SSA-renamed uniquely; mark still ssa. *)
            true
        | _ -> false

let find_inline_sites (prog : program) (heur : heuristic) :
    (Ident.t * Ident.t * label * int) list =
  let sites = ref [] in
  Ident.Map.iter
    (fun _ caller ->
      Label.Map.iter
        (fun _ b ->
          List.iteri
            (fun idx instr ->
              match direct_callee instr with
              | None -> ()
              | Some g -> (
                  match find_func prog g with
                  | None -> ()
                  | Some callee ->
                      let sites_n = count_call_sites prog g in
                      if
                        should_inline ~heur ~caller ~callee
                          ~call_sites:sites_n
                      then
                        sites :=
                          (caller.name, g, b.label, idx) :: !sites))
            b.instrs)
        caller.blocks)
    prog.funcs;
  List.rev !sites

let run_program ?(heur = default_heuristic) (prog : program) : bool =
  let sites = find_inline_sites prog heur in
  let changed = ref false in
  (* Inline from the end of each block's instr list backwards so indices stay
     valid within a block; across functions just apply sequentially carefully. *)
  let by_caller =
    List.fold_left
      (fun m (caller, callee, host, idx) ->
        let xs =
          match Ident.Map.find_opt caller m with Some xs -> xs | None -> []
        in
        Ident.Map.add caller ((callee, host, idx) :: xs) m)
      Ident.Map.empty sites
  in
  Ident.Map.iter
    (fun caller_name sites ->
      match find_func prog caller_name with
      | None -> ()
      | Some caller ->
          (* Sort indices descending per host block. *)
          let by_host =
            List.fold_left
              (fun m (callee, host, idx) ->
                let xs =
                  match Label.Map.find_opt host m with
                  | Some xs -> xs
                  | None -> []
                in
                Label.Map.add host ((callee, idx) :: xs) m)
              Label.Map.empty sites
          in
          Label.Map.iter
            (fun host entries ->
              let entries =
                List.sort (fun (_, i1) (_, i2) -> Int.compare i2 i1) entries
              in
              List.iter
                (fun (callee_name, idx) ->
                  match find_func prog callee_name with
                  | None -> ()
                  | Some callee ->
                      if inline_at ~caller ~callee ~host ~idx then
                        changed := true)
                entries)
            by_host)
    by_caller;
  !changed

let pass =
  Pass_manager.make_program_pass "inline" (run_program ?heur:None)

let run = run_program
