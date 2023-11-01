(** Constant folding and propagation on SSA [Mir]. *)

open Mir

let as_int = function VConst (CInt n) -> Some n | _ -> None
let as_bool = function VConst (CBool b) -> Some b | _ -> None
let as_float = function VConst (CFloat f) -> Some f | _ -> None

let eval_binop op lhs rhs =
  match (op, as_int lhs, as_int rhs) with
  | Add, Some a, Some b -> Some (VConst (CInt (a + b)))
  | Sub, Some a, Some b -> Some (VConst (CInt (a - b)))
  | Mul, Some a, Some b -> Some (VConst (CInt (a * b)))
  | Div, Some a, Some b when b <> 0 -> Some (VConst (CInt (a / b)))
  | Mod, Some a, Some b when b <> 0 -> Some (VConst (CInt (a mod b)))
  | Eq, Some a, Some b -> Some (VConst (CBool (a = b)))
  | Ne, Some a, Some b -> Some (VConst (CBool (a <> b)))
  | Lt, Some a, Some b -> Some (VConst (CBool (a < b)))
  | Le, Some a, Some b -> Some (VConst (CBool (a <= b)))
  | Gt, Some a, Some b -> Some (VConst (CBool (a > b)))
  | Ge, Some a, Some b -> Some (VConst (CBool (a >= b)))
  | And, Some a, Some b -> Some (VConst (CInt (a land b)))
  | Or, Some a, Some b -> Some (VConst (CInt (a lor b)))
  | Xor, Some a, Some b -> Some (VConst (CInt (a lxor b)))
  | Shl, Some a, Some b -> Some (VConst (CInt (a lsl b)))
  | Shr, Some a, Some b -> Some (VConst (CInt (a asr b)))
  | _ -> (
      match (op, as_bool lhs, as_bool rhs) with
      | Eq, Some a, Some b -> Some (VConst (CBool (a = b)))
      | Ne, Some a, Some b -> Some (VConst (CBool (a <> b)))
      | And, Some a, Some b -> Some (VConst (CBool (a && b)))
      | Or, Some a, Some b -> Some (VConst (CBool (a || b)))
      | _ -> (
          match (op, as_float lhs, as_float rhs) with
          | FAdd, Some a, Some b -> Some (VConst (CFloat (a +. b)))
          | FSub, Some a, Some b -> Some (VConst (CFloat (a -. b)))
          | FMul, Some a, Some b -> Some (VConst (CFloat (a *. b)))
          | FDiv, Some a, Some b when b <> 0. ->
              Some (VConst (CFloat (a /. b)))
          | Eq, Some a, Some b -> Some (VConst (CBool (Float.equal a b)))
          | _ -> None))

let eval_unop op arg =
  match (op, as_int arg) with
  | Neg, Some a -> Some (VConst (CInt (~-a)))
  | BitNot, Some a -> Some (VConst (CInt (lnot a)))
  | _ -> (
      match (op, as_bool arg) with
      | Not, Some a -> Some (VConst (CBool (not a)))
      | _ -> (
          match (op, as_float arg) with
          | FNeg, Some a -> Some (VConst (CFloat (~-.a)))
          | _ -> None))

(** Also accept bare consts (for SCCP). *)
let fold_binop op a b =
  match (a, b) with
  | VConst ca, VConst cb -> eval_binop op (VConst ca) (VConst cb)
  | _ -> eval_binop op a b

let fold_unop op a =
  match a with VConst _ -> eval_unop op a | _ -> eval_unop op a

let simplify_binop op lhs rhs =
  match eval_binop op lhs rhs with
  | Some v -> Some v
  | None -> (
      match (op, lhs, rhs) with
      | Add, VConst (CInt 0), v | Add, v, VConst (CInt 0) -> Some v
      | Sub, v, VConst (CInt 0) -> Some v
      | Mul, VConst (CInt 1), v | Mul, v, VConst (CInt 1) -> Some v
      | Mul, VConst (CInt 0), _ | Mul, _, VConst (CInt 0) ->
          Some (VConst (CInt 0))
      | Or, VConst (CBool true), _ | Or, _, VConst (CBool true) ->
          Some (VConst (CBool true))
      | Or, VConst (CBool false), v | Or, v, VConst (CBool false) -> Some v
      | And, VConst (CBool false), _ | And, _, VConst (CBool false) ->
          Some (VConst (CBool false))
      | And, VConst (CBool true), v | And, v, VConst (CBool true) -> Some v
      | _ -> None)

type env = value Vreg.Map.t

let lookup env = function
  | VReg r as v -> (
      match Vreg.Map.find_opt r env with Some c -> c | None -> v)
  | v -> v

let fold_instr env instr =
  let lu v = lookup env v in
  match instr with
  | Binop ({ dst; op; lhs; rhs; ty; span } as b) -> (
      let lhs, rhs = (lu lhs, lu rhs) in
      match simplify_binop op lhs rhs with
      | Some v ->
          (Assign { dst; src = v; ty; span }, Vreg.Map.add dst v env, true)
      | None ->
          ( Binop { b with lhs; rhs },
            env,
            not (value_equal lhs b.lhs && value_equal rhs b.rhs) ))
  | Unop ({ dst; op; arg; ty; span } as u) -> (
      let arg = lu arg in
      match eval_unop op arg with
      | Some v ->
          (Assign { dst; src = v; ty; span }, Vreg.Map.add dst v env, true)
      | None -> (Unop { u with arg }, env, not (value_equal arg u.arg)))
  | Assign ({ dst; src; _ } as a) ->
      let src = lu src in
      let env =
        match src with
        | VConst _ -> Vreg.Map.add dst src env
        | _ -> Vreg.Map.remove dst env
      in
      (Assign { a with src }, env, not (value_equal src a.src))
  | Phi ({ dst; incoming; ty; span } as p) ->
      let incoming = List.map (fun (l, v) -> (l, lu v)) incoming in
      (match List.map snd incoming with
      | v :: rest
        when (match v with VConst _ -> true | _ -> false)
             && List.for_all (value_equal v) rest ->
          (Assign { dst; src = v; ty; span }, Vreg.Map.add dst v env, true)
      | _ -> (Phi { p with incoming }, env, false))
  | Call ({ callee; args; _ } as c) ->
      (Call { c with callee = lu callee; args = List.map lu args }, env, false)
  | Alloc ({ fields; _ } as a) ->
      (Alloc { a with fields = List.map lu fields }, env, false)
  | Load ({ ptr; _ } as l) -> (Load { l with ptr = lu ptr }, env, false)
  | Store ({ ptr; value; _ } as s) ->
      (Store { s with ptr = lu ptr; value = lu value }, env, false)
  | GetField ({ obj; _ } as g) ->
      (GetField { g with obj = lu obj }, env, false)
  | SetField ({ obj; value; _ } as s) ->
      (SetField { s with obj = lu obj; value = lu value }, env, false)
  | Cast ({ src; _ } as c) -> (Cast { c with src = lu src }, env, false)

let run_block (b : block) : bool =
  let env = ref Vreg.Map.empty in
  let changed = ref false in
  let rewrite instr =
    let instr', env', ch = fold_instr !env instr in
    env := env';
    if ch then changed := true;
    instr'
  in
  b.phis <- List.map rewrite b.phis;
  b.instrs <- List.map rewrite b.instrs;
  let term = map_terminator_values (lookup !env) b.terminator in
  let term =
    match term with
    | Branch { cond = VConst (CBool true); then_; span; _ } ->
        changed := true;
        Jump (then_, span)
    | Branch { cond = VConst (CBool false); else_; span; _ } ->
        changed := true;
        Jump (else_, span)
    | t -> t
  in
  if not (term == b.terminator) then changed := true;
  b.terminator <- term;
  !changed

let run_func (f : func) : bool =
  ignore (Cfg.ensure_cfg f);
  let changed = ref false in
  for _ = 1 to 3 do
    List.iter
      (fun l ->
        match find_block f l with
        | Some b -> if run_block b then changed := true
        | None -> ())
      (Cfg.reverse_postorder f)
  done;
  ignore (Ssa.cleanup f);
  !changed

let pass = Pass_manager.make_func_pass "const_prop" run_func
let run = run_func
