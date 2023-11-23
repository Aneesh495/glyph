(** CSE / simple GVN. *)

open Mir

type exp =
  | EBin of binop * value * value
  | EUn of unop * value
  | EField of value * int

let exp_of = function
  | Binop { op; lhs; rhs; _ } -> Some (EBin (op, lhs, rhs))
  | Unop { op; arg; _ } -> Some (EUn (op, arg))
  | GetField { obj; index; _ } -> Some (EField (obj, index))
  | _ -> None

let exp_equal a b =
  match (a, b) with
  | EBin (o1, a1, b1), EBin (o2, a2, b2) ->
      o1 = o2 && value_equal a1 a2 && value_equal b1 b2
  | EUn (o1, a1), EUn (o2, a2) -> o1 = o2 && value_equal a1 a2
  | EField (v1, i1), EField (v2, i2) -> value_equal v1 v2 && i1 = i2
  | _ -> false

let exp_hash = function
  | EBin (op, a, b) ->
      Hashtbl.hash (0, binop_to_string op, value_to_string a, value_to_string b)
  | EUn (op, a) -> Hashtbl.hash (1, unop_to_string op, value_to_string a)
  | EField (v, i) -> Hashtbl.hash (2, value_to_string v, i)

module E = Hashtbl.Make (struct
  type t = exp
  let equal = exp_equal
  let hash = exp_hash
end)

let run_func (f : func) : bool =
  let dom = Dominators.compute f in
  let avail = E.create 64 in
  let changed = ref false in
  List.iter
    (fun lbl ->
      match find_block f lbl with
      | None -> ()
      | Some b ->
          b.instrs <-
            List.map
              (fun i ->
                match (exp_of i, instr_defs i) with
                | Some e, [ d ] -> (
                    match E.find_opt avail e with
                    | Some (def_blk, prev)
                      when Dominators.dominates dom ~dominator:def_blk
                             ~node:lbl ->
                        changed := true;
                        Assign
                          {
                            dst = d;
                            src = VReg prev;
                            ty = Ty_any;
                            span = instr_span i;
                          }
                    | _ ->
                        E.replace avail e (lbl, d);
                        i)
                | _ -> i)
              b.instrs)
    dom.Dominators.rpo;
  !changed

let pass = Pass_manager.make_func_pass "cse" run_func
let run = run_func
