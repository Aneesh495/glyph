(** Local value numbering / common subexpression elimination (CSE).

    Within each basic block, hash congruent pure expressions and reuse
    prior destinations. *)

open Mir

type exp =
  | EBin of binop * vreg * vreg
  | EUn of unop * vreg
  | EField of vreg * int
  | ETuple of vreg * int
  | EConst of const
  | EAlloc of int * vreg list

let exp_equal a b =
  match (a, b) with
  | EBin (o1, a1, b1), EBin (o2, a2, b2) ->
      o1 = o2 && a1 = a2 && b1 = b2
  | EUn (o1, a1), EUn (o2, a2) -> o1 = o2 && a1 = a2
  | EField (v1, i1), EField (v2, i2) -> v1 = v2 && i1 = i2
  | ETuple (v1, i1), ETuple (v2, i2) -> v1 = v2 && i1 = i2
  | EConst c1, EConst c2 -> c1 = c2
  | EAlloc (t1, fs1), EAlloc (t2, fs2) -> t1 = t2 && fs1 = fs2
  | _ -> false

let exp_hash = function
  | EBin (op, a, b) -> Hashtbl.hash (0, Mir.binop_to_string op, a, b)
  | EUn (op, a) -> Hashtbl.hash (1, Mir.unop_to_string op, a)
  | EField (v, i) -> Hashtbl.hash (2, v, i)
  | ETuple (v, i) -> Hashtbl.hash (3, v, i)
  | EConst c -> Hashtbl.hash (4, c)
  | EAlloc (t, fs) -> Hashtbl.hash (5, t, fs)

module ExpTbl = Hashtbl.Make (struct
  type t = exp
  let equal = exp_equal
  let hash = exp_hash
end)

let instr_exp = function
  | IBinop (_, op, a, b) -> Some (EBin (op, a, b))
  | IUnop (_, op, a) -> Some (EUn (op, a))
  | IGetField (_, obj, i) -> Some (EField (obj, i))
  | ITupleGet (_, t, i) -> Some (ETuple (t, i))
  | IConst (_, c) -> Some (EConst c)
  | IAlloc (_, tag, fs) -> Some (EAlloc (tag, fs))
  | _ -> None

(** Purely local CSE (single-block hash consing). *)
let run_local (ctx : Pass.context) (fn : func) : func =
  let blocks =
    List.map
      (fun (b : block) ->
        let table = ExpTbl.create 32 in
        let instrs =
          List.map
            (fun i ->
              match (instr_exp i, instr_def i) with
              | Some e, Some d -> (
                  match ExpTbl.find_opt table e with
                  | Some prev ->
                      ctx.stats.rewritten <- ctx.stats.rewritten + 1;
                      IMove (d, prev)
                  | None ->
                      ExpTbl.replace table e d;
                      i)
              | _ -> i)
            b.instrs
        in
        { b with instrs })
      fn.blocks
  in
  { fn with blocks }

let run_func = run_local

let pass = Pass.make_func_pass ~name:"cse" run_local
let pass_local = Pass.make_func_pass ~name:"cse-local" run_local
