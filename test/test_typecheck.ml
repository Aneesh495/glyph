open Alcotest

let typecheck_src src =
  match Parser.parse_program src with
  | Error e -> Error [ Parser.error_to_diagnostic e ]
  | Ok prog -> Infer.infer_program prog

let test_hm_infer () =
  let src = "let id x = x\nlet a = id 42\nlet b = id true" in
  match typecheck_src src with
  | Error _ -> fail "expected HM typecheck success"
  | Ok env ->
      check bool "id bound" true (Env.find_value env (Ident.Intern.intern "id") <> None);
      check bool "a bound" true (Env.find_value env (Ident.Intern.intern "a") <> None);
      check bool "b bound" true (Env.find_value env (Ident.Intern.intern "b") <> None)

let test_rec_fun () =
  let src = "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)" in
  match typecheck_src src with
  | Error _ -> fail "expected recursive function typecheck success"
  | Ok _ -> ()

let test_adt () =
  let src = "type Tree = Leaf Int | Node Tree Tree\nlet rec depth t = match t with | Leaf _ -> 1 | Node l r -> depth l + depth r" in
  match typecheck_src src with
  | Error _ -> fail "expected ADT typecheck success"
  | Ok _ -> ()

let test_mismatch_error () =
  let src = "let x = 1 + true" in
  match typecheck_src src with
  | Error diags -> check bool "has errors" true (List.length diags > 0)
  | Ok _ -> fail "expected type mismatch error"

let test_unbound_error () =
  let src = "let x = undefined_var + 1" in
  match typecheck_src src with
  | Error diags -> check bool "has errors" true (List.length diags > 0)
  | Ok _ -> fail "expected unbound var error"

let test_occurs_check () =
  let src = "let f x = x x" in
  match typecheck_src src with
  | Error diags -> check bool "has errors" true (List.length diags > 0)
  | Ok _ -> fail "expected occurs check error"

let tests =
  [
    ("hm_infer", `Quick, test_hm_infer);
    ("rec_fun", `Quick, test_rec_fun);
    ("adt", `Quick, test_adt);
    ("mismatch_error", `Quick, test_mismatch_error);
    ("unbound_error", `Quick, test_unbound_error);
    ("occurs_check", `Quick, test_occurs_check);
  ]
