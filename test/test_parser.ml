open Alcotest

let test_parse_let () =
  let src = "let x = 42" in
  match Parser.parse_program src with
  | Error _ -> fail "failed to parse let"
  | Ok prog ->
      check int "1 item" 1 (List.length prog.Ast.items);
      (match List.hd prog.Ast.items with
      | Ast.Item_let lb ->
          check string "name is x" "x" (Ident.name lb.Ast.lb_name);
          check bool "not rec" false lb.Ast.lb_rec
      | _ -> fail "expected Item_let")

let test_parse_precedence () =
  let src = "let x = 1 + 2 * 3" in
  match Parser.parse_program src with
  | Error _ -> fail "failed to parse precedence"
  | Ok prog ->
      (match List.hd prog.Ast.items with
      | Ast.Item_let lb ->
          (match lb.Ast.lb_body.Ast.expr_desc with
          | Ast.Expr_bin (Token.Op_add, _lhs, rhs) ->
              (match rhs.Ast.expr_desc with
              | Ast.Expr_bin (Token.Op_mul, _, _) -> ()
              | _ -> fail "expected multiplication to bind tighter than addition")
          | _ -> fail "expected outer addition")
      | _ -> fail "expected Item_let")

let test_parse_match () =
  let src = "let f x = match x with | 0 -> 1 | _ -> 2" in
  match Parser.parse_program src with
  | Error _ -> fail "failed to parse match"
  | Ok prog ->
      (match List.hd prog.Ast.items with
      | Ast.Item_fn lb | Ast.Item_let lb ->
          (match lb.Ast.lb_body.Ast.expr_desc with
          | Ast.Expr_match (_, cases) ->
              check int "2 cases" 2 (List.length cases)
          | _ -> fail "expected Expr_match")
      | _ -> fail "expected let binding")

let test_parse_adt () =
  let src = "type List a = Nil | Cons a (List a)" in
  match Parser.parse_program src with
  | Error _ -> fail "failed to parse adt"
  | Ok prog ->
      (match List.hd prog.Ast.items with
      | Ast.Item_type td ->
          check string "name is List" "List" (Ident.name td.Ast.td_name);
          check int "2 constructors" 2 (List.length td.Ast.td_ctors)
      | _ -> fail "expected Item_type")

let test_parse_syntax_error () =
  let src = "let x = " in
  match Parser.parse_program src with
  | Error _ -> ()
  | Ok _ -> fail "expected syntax error for incomplete let"

let tests =
  [
    ("parse_let", `Quick, test_parse_let);
    ("parse_precedence", `Quick, test_parse_precedence);
    ("parse_match", `Quick, test_parse_match);
    ("parse_adt", `Quick, test_parse_adt);
    ("parse_syntax_error", `Quick, test_parse_syntax_error);
  ]
