(** Sparse / local constant propagation on SSA MIR. *)

open Mir

type lattice =
  | Top
  (** Not a constant / unknown. *)
  | Const of const
  | Bottom
  (** Undefined / unreachable. *)

let lattice_equal a b =
  match (a, b) with
  | Top, Top | Bottom, Bottom -> true
  | Const x, Const y -> x = y
  | _ -> false

let meet a b =
  match (a, b) with
  | Bottom, x | x, Bottom -> x
  | Const x, Const y when x = y -> Const x
  | Const _, Const _ -> Top
  | Top, _ | _, Top -> Top

let eval_binop op a b =
  match (op, a, b) with
  | Add, CInt x, CInt y -> Some (CInt (x + y))
  | Sub, CInt x, CInt y -> Some (CInt (x - y))
  | Mul, CInt x, CInt y -> Some (CInt (x * y))
  | Div, CInt x, CInt y when y <> 0 -> Some (CInt (x / y))
  | Mod, CInt x, CInt y when y <> 0 -> Some (CInt (x mod y))
  | AddF, CFloat x, CFloat y -> Some (CFloat (x +. y))
  | SubF, CFloat x, CFloat y -> Some (CFloat (x -. y))
  | MulF, CFloat x, CFloat y -> Some (CFloat (x *. y))
  | DivF, CFloat x, CFloat y when y <> 0. -> Some (CFloat (x /. y))
  | Eq, CInt x, CInt y -> Some (CBool (x = y))
  | Ne, CInt x, CInt y -> Some (CBool (x <> y))
  | Lt, CInt x, CInt y -> Some (CBool (x < y))
  | Le, CInt x, CInt y -> Some (CBool (x <= y))
  | Gt, CInt x, CInt y -> Some (CBool (x > y))
  | Ge, CInt x, CInt y -> Some (CBool (x >= y))
  | Eq, CBool x, CBool y -> Some (CBool (x = y))
  | And, CBool x, CBool y -> Some (CBool (x && y))
  | Or, CBool x, CBool y -> Some (CBool (x || y))
  | Eq, CFloat x, CFloat y -> Some (CBool (Float.equal x y))
  | _ -> None

let eval_unop op a =
  match (op, a) with
  | Neg, CInt x -> Some (CInt (-x))
  | NegF, CFloat x -> Some (CFloat (-.x))
  | Not, CBool x -> Some (CBool (not x))
  | _ -> None

let run_func (ctx : Pass.context) (fn : func) : func =
  let values : (vreg, lattice) Hashtbl.t = Hashtbl.create fn.n_vregs in
  let get v = Option.value ~default:Top (Hashtbl.find_opt values v) in
  let set v lat =
    match Hashtbl.find_opt values v with
    | Some old when lattice_equal old lat -> false
    | _ ->
        Hashtbl.replace values v lat;
        true
  in
  (* Seed: walk instructions in RPO until fixpoint (SSA so one pass often
     suffices, but loops via φ need iteration). *)
  let cfg = Cfg.build fn in
  let order = Cfg.reverse_postorder cfg in
  let changed = ref true in
  let iterations = ref 0 in
  while !changed && !iterations < 64 do
    incr iterations;
    changed := false;
    List.iter
      (fun lbl ->
        match find_block_opt fn lbl with
        | None -> ()
        | Some b ->
            let eval_instr i =
              match i with
              | IConst (d, c) -> if set d (Const c) then changed := true
              | IMove (d, s) -> if set d (get s) then changed := true
              | IBinop (d, op, a, b) ->
                  let lat =
                    match (get a, get b) with
                    | Const ca, Const cb -> (
                        match eval_binop op ca cb with
                        | Some c -> Const c
                        | None -> Top)
                    | Bottom, _ | _, Bottom -> Bottom
                    | _ -> Top
                  in
                  if set d lat then changed := true
              | IUnop (d, op, a) ->
                  let lat =
                    match get a with
                    | Const ca -> (
                        match eval_unop op ca with
                        | Some c -> Const c
                        | None -> Top)
                    | Bottom -> Bottom
                    | _ -> Top
                  in
                  if set d lat then changed := true
              | IPhi (d, incoming) ->
                  let lat =
                    List.fold_left
                      (fun acc (_, v) -> meet acc (get v))
                      Bottom incoming
                  in
                  if set d lat then changed := true
              | ICall (d, _, _)
              | ICallClosure (d, _, _)
              | IAlloc (d, _, _)
              | IGetField (d, _, _)
              | IMakeClosure (d, _, _)
              | ITupleGet (d, _, _)
              | ICons (d, _, _)
              | ICar (d, _)
              | ICdr (d, _) ->
                  if set d Top then changed := true
              | ISetField _ | IPrint _ | INop -> ()
            in
            List.iter eval_instr b.phis;
            List.iter eval_instr b.instrs)
      order
  done;
  (* Rewrite constant vregs to IConst where profitable. *)
  let rewrite_instr i =
    match instr_def i with
    | Some d -> (
        match get d with
        | Const c ->
            (match i with
            | IConst _ -> i
            | _ ->
                ctx.stats.rewritten <- ctx.stats.rewritten + 1;
                IConst (d, c))
        | _ -> i)
    | None -> i
  in
  let blocks =
    List.map
      (fun (b : block) ->
        {
          b with
          phis = List.map rewrite_instr b.phis;
          instrs = List.map rewrite_instr b.instrs;
        })
      fn.blocks
  in
  { fn with blocks }

let pass =
  Pass.make_func_pass ~name:"const-prop" run_func
