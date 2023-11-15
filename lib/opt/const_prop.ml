(** Constant propagation on SSA MIR. *)

open Mir

type lattice =
  | Top
  | Const of const
  | Bottom

let lattice_equal a b =
  match (a, b) with
  | Top, Top | Bottom, Bottom -> true
  | Const x, Const y -> const_equal x y
  | _ -> false

let meet a b =
  match (a, b) with
  | Bottom, x | x, Bottom -> x
  | Const x, Const y when const_equal x y -> Const x
  | Const _, Const _ -> Top
  | Top, _ | _, Top -> Top

let eval_binop op a b =
  match (op, a, b) with
  | Add, CInt x, CInt y -> Some (CInt (x + y))
  | Sub, CInt x, CInt y -> Some (CInt (x - y))
  | Mul, CInt x, CInt y -> Some (CInt (x * y))
  | Div, CInt x, CInt y when y <> 0 -> Some (CInt (x / y))
  | Mod, CInt x, CInt y when y <> 0 -> Some (CInt (x mod y))
  | FAdd, CFloat x, CFloat y -> Some (CFloat (x +. y))
  | FSub, CFloat x, CFloat y -> Some (CFloat (x -. y))
  | FMul, CFloat x, CFloat y -> Some (CFloat (x *. y))
  | FDiv, CFloat x, CFloat y when y <> 0. -> Some (CFloat (x /. y))
  | Eq, CInt x, CInt y -> Some (CBool (x = y))
  | Ne, CInt x, CInt y -> Some (CBool (x <> y))
  | Lt, CInt x, CInt y -> Some (CBool (x < y))
  | Le, CInt x, CInt y -> Some (CBool (x <= y))
  | Gt, CInt x, CInt y -> Some (CBool (x > y))
  | Ge, CInt x, CInt y -> Some (CBool (x >= y))
  | And, CBool x, CBool y -> Some (CBool (x && y))
  | Or, CBool x, CBool y -> Some (CBool (x || y))
  | _ -> None

let eval_unop op a =
  match (op, a) with
  | Neg, CInt x -> Some (CInt (-x))
  | FNeg, CFloat x -> Some (CFloat (-.x))
  | Not, CBool x -> Some (CBool (not x))
  | _ -> None

let value_lat vals = function
  | VConst c -> Const c
  | VReg r -> (
      match Hashtbl.find_opt vals r with Some l -> l | None -> Top)
  | _ -> Top

let run_func (f : func) : bool =
  let vals = Hashtbl.create 64 in
  let set v lat =
    match Hashtbl.find_opt vals v with
    | Some old when lattice_equal old lat -> false
    | _ ->
        Hashtbl.replace vals v lat;
        true
  in
  let changed = ref false in
  for _ = 1 to 32 do
    let progressing = ref false in
    List.iter
      (fun lbl ->
        match find_block f lbl with
        | None -> ()
        | Some b ->
            let eval = function
              | Assign { dst; src; _ } ->
                  if set dst (value_lat vals src) then progressing := true
              | Binop { dst; op; lhs; rhs; _ } ->
                  let lat =
                    match (value_lat vals lhs, value_lat vals rhs) with
                    | Const ca, Const cb -> (
                        match eval_binop op ca cb with
                        | Some c -> Const c
                        | None -> Top)
                    | Bottom, _ | _, Bottom -> Bottom
                    | _ -> Top
                  in
                  if set dst lat then progressing := true
              | Unop { dst; op; arg; _ } ->
                  let lat =
                    match value_lat vals arg with
                    | Const ca -> (
                        match eval_unop op ca with
                        | Some c -> Const c
                        | None -> Top)
                    | Bottom -> Bottom
                    | _ -> Top
                  in
                  if set dst lat then progressing := true
              | Phi { dst; incoming; _ } ->
                  let lat =
                    List.fold_left
                      (fun acc (_, v) -> meet acc (value_lat vals v))
                      Bottom incoming
                  in
                  if set dst lat then progressing := true
              | Call { dst = Some d; _ }
              | Alloc { dst = d; _ }
              | Load { dst = d; _ }
              | GetField { dst = d; _ }
              | Cast { dst = d; _ } ->
                  if set d Top then progressing := true
              | _ -> ()
            in
            List.iter eval (b.phis @ b.instrs))
      (Cfg.reverse_postorder f);
    ignore progressing
  done;
  Label.Map.iter
    (fun _ (b : block) ->
      let rw = function
        | ( Assign { dst; _ }
          | Binop { dst; _ }
          | Unop { dst; _ }
          | Phi { dst; _ } ) as i -> (
            match Hashtbl.find_opt vals dst with
            | Some (Const c) ->
                changed := true;
                Assign
                  {
                    dst;
                    src = VConst c;
                    ty = Ty_any;
                    span = instr_span i;
                  }
            | _ -> i)
        | i -> i
      in
      b.phis <- List.map rw b.phis;
      b.instrs <- List.map rw b.instrs)
    f.blocks;
  !changed

let pass = Pass_manager.make_func_pass "const_prop" run_func
let run = run_func
