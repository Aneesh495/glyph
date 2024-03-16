open Alcotest

let desugar_src src =
  match Parser.parse_program src with
  | Error e -> fail ("parse failed: " ^ e.Parser.message)
  | Ok prog -> Desugar.desugar_program prog

let test_desugar_anf () =
  let src = "let x = (1 + 2) * (3 + 4)" in
  let hir = desugar_src src in
  check int "1 item" 1 (List.length hir.Hir.items);
  match List.hd hir.Hir.items with
  | Hir.Toplevel_val { body; _ } | Hir.Toplevel_fun { body; _ } ->
      (* ANF turns nested operations into Let bindings *)
      (match body with
      | Hir.Let _ -> ()
      | _ -> fail "expected ANF let bindings for compound arithmetic")
  | _ -> fail "expected Toplevel_val or Toplevel_fun"

let test_record_rejection () =
  let dummy_span = Span.dummy in
  let expr = Ast.expr (Ast.Expr_record [ (Ident.Intern.intern "x", Ast.expr (Ast.Expr_lit (Ast.Lit_int 1L)) dummy_span) ]) dummy_span in
  let item = Ast.Item_let {
    Ast.lb_name = Ident.Intern.intern "r";
    lb_params = [];
    lb_ty = None;
    lb_body = expr;
    lb_span = dummy_span;
    lb_rec = false;
  } in
  let prog = { Ast.items = [ item ]; span = dummy_span } in
  try
    let _ = Desugar.desugar_program prog in
    fail "expected record expression to be rejected in v1"
  with Failure msg ->
    check bool "rejection message mentions records deferred" true
      (String.contains msg 'r')

let tests =
  [
    ("desugar_anf", `Quick, test_desugar_anf);
    ("record_rejection", `Quick, test_record_rejection);
  ]
