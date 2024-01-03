(** Copy propagation: replace uses of [x] by [y] after [x = y]. *)

open Mir

let run_func (ctx : Pass.context) (fn : func) : func =
  (* In SSA, a copy [d = s] means all uses of [d] can become [s] if [d] is
     only defined once (always true in SSA) and we update φ operands too. *)
  let alias : (vreg, vreg) Hashtbl.t = Hashtbl.create fn.n_vregs in
  let rec resolve v =
    match Hashtbl.find_opt alias v with
    | Some v' when v' <> v -> resolve v'
    | Some v' -> v'
    | None -> v
  in
  List.iter
    (fun (b : block) ->
      List.iter
        (fun i ->
          match i with
          | IMove (d, s) -> Hashtbl.replace alias d (resolve s)
          | IPhi (d, incoming) ->
              let ops = List.map (fun (_, v) -> resolve v) incoming in
              (match ops with
              | v :: rest when List.for_all (( = ) v) rest ->
                  Hashtbl.replace alias d v
              | _ -> ())
          | _ -> ())
        (b.phis @ b.instrs))
    fn.blocks;
  let subst = resolve in
  let rw_i i =
    let i' =
      match i with
      | IConst _ | INop -> i
      | IMove (d, s) -> IMove (d, subst s)
      | IBinop (d, op, a, b) -> IBinop (d, op, subst a, subst b)
      | IUnop (d, op, a) -> IUnop (d, op, subst a)
      | ICall (d, fid, args) -> ICall (d, fid, List.map subst args)
      | ICallClosure (d, clo, args) ->
          ICallClosure (d, subst clo, List.map subst args)
      | IAlloc (d, tag, fields) -> IAlloc (d, tag, List.map subst fields)
      | IGetField (d, obj, idx) -> IGetField (d, subst obj, idx)
      | ISetField (obj, idx, v) -> ISetField (subst obj, idx, subst v)
      | IMakeClosure (d, fid, env) ->
          IMakeClosure (d, fid, List.map subst env)
      | ITupleGet (d, t, idx) -> ITupleGet (d, subst t, idx)
      | ICons (d, h, t) -> ICons (d, subst h, subst t)
      | ICar (d, c) -> ICar (d, subst c)
      | ICdr (d, c) -> ICdr (d, subst c)
      | IPrint v -> IPrint (subst v)
      | IPhi (d, incoming) ->
          IPhi (d, List.map (fun (l, v) -> (l, subst v)) incoming)
    in
    if i' <> i then ctx.stats.rewritten <- ctx.stats.rewritten + 1;
    i'
  in
  let rw_t = function
    | TJump _ as t -> t
    | TBranch (c, a, b) -> TBranch (subst c, a, b)
    | TSwitch (v, cases, d) -> TSwitch (subst v, cases, d)
    | TRet (Some v) -> TRet (Some (subst v))
    | TRet None as t -> t
    | THalt (Some v) -> THalt (Some (subst v))
    | THalt None as t -> t
    | TTailCall (fid, args) -> TTailCall (fid, List.map subst args)
    | TTailCallClosure (clo, args) ->
        TTailCallClosure (subst clo, List.map subst args)
  in
  let blocks =
    List.map
      (fun (b : block) ->
        {
          b with
          phis = List.map rw_i b.phis;
          instrs = List.map rw_i b.instrs;
          term = rw_t b.term;
        })
      fn.blocks
  in
  { fn with blocks }

let pass = Pass.make_func_pass ~name:"copy-prop" run_func
