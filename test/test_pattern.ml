open Alcotest

let compile_src src =
  match Parser.parse_program src with
  | Error e -> fail ("parse failed: " ^ e.Parser.message)
  | Ok prog ->
      let hir = Desugar.desugar_program prog in
      Pattern.compile_program hir

let rec has_switch_lit (e : Hir.expr) =
  match e with
  | Hir.Switch_lit _ -> true
  | Hir.Let (_, _, body, _) -> has_switch_lit body
  | Hir.Fun (_, body, _) -> has_switch_lit body
  | _ -> false

let rec has_switch_ctor (e : Hir.expr) =
  match e with
  | Hir.Switch_ctor _ -> true
  | Hir.Let (_, _, body, _) -> has_switch_ctor body
  | Hir.Fun (_, body, _) -> has_switch_ctor body
  | _ -> false

let test_switch_lit () =
  let src = "let f x = match x with | 0 -> 10 | 1 -> 20 | _ -> 30" in
  let hir = compile_src src in
  match List.hd hir.Hir.items with
  | Hir.Toplevel_fun { body; _ } ->
      check bool "has switch_lit" true (has_switch_lit body)
  | _ -> fail "expected toplevel fun"

let test_switch_ctor () =
  let src = "type Option a = None | Some a\nlet f x = match x with | None -> 0 | Some y -> y" in
  let hir = compile_src src in
  match List.find_map (function Hir.Toplevel_fun { body; _ } -> Some body | _ -> None) hir.Hir.items with
  | Some body ->
      check bool "has switch_ctor" true (has_switch_ctor body)
  | None -> fail "expected toplevel fun"

let test_exhaustiveness () =
  let u = [ { Hir.ctor_name = Ident.Intern.intern "A"; ctor_tag = 0; ctor_arity = 0; ctor_type = None };
            { Hir.ctor_name = Ident.Intern.intern "B"; ctor_tag = 1; ctor_arity = 0; ctor_type = None } ] in
  let arm_a = { Hir.arm_pat = Hir.Pat_ctor (List.hd u, [], Span.dummy); arm_guard = None; arm_body = Hir.Atom (Hir.Atom_lit (Hir.Lit_int 1), Span.dummy); arm_span = Span.dummy } in
  let arm_b = { Hir.arm_pat = Hir.Pat_ctor (List.nth u 1, [], Span.dummy); arm_guard = None; arm_body = Hir.Atom (Hir.Atom_lit (Hir.Lit_int 2), Span.dummy); arm_span = Span.dummy } in
  check bool "exhaustive with A and B" true (Pattern.is_exhaustive ~universe:u [ arm_a; arm_b ]);
  check bool "not exhaustive with only A" false (Pattern.is_exhaustive ~universe:u [ arm_a ])

let tests =
  [
    ("switch_lit", `Quick, test_switch_lit);
    ("switch_ctor", `Quick, test_switch_ctor);
    ("exhaustiveness", `Quick, test_exhaustiveness);
  ]
