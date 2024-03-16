open Alcotest

let lower_src src =
  match Parser.parse_program src with
  | Error e -> fail ("parse failed: " ^ e.Parser.message)
  | Ok prog ->
      let hir = Desugar.desugar_program prog in
      let hir_compiled = Pattern.compile_program hir in
      Mir_lower.lower_program hir_compiled

let test_mir_lower_valid () =
  let src = "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nlet main = fact 5" in
  let mir = lower_src src in
  match Mir_verify.verify_program mir with
  | Ok () -> check bool "mir verification passed" true true
  | Error errs -> fail ("mir verification failed:\n" ^ String.concat "\n" errs)

let test_mir_reject_bad_target () =
  let mir = lower_src "let x = 42" in
  let fn = List.hd mir.Mir.functions in
  (* Corrupt entry block terminator to jump to nonexistent block 9999 *)
  let bad_block = {
    Mir.label = fn.Mir.entry;
    phis = [];
    instrs = [];
    term = Mir.TJump 9999;
    span = Span.dummy;
  } in
  let bad_fn = { fn with Mir.blocks = [ bad_block ] } in
  let bad_prog = { mir with Mir.functions = [ bad_fn ] } in
  match Mir_verify.verify_program bad_prog with
  | Ok () -> fail "expected verification failure for nonexistent successor"
  | Error errs ->
      check bool "catches bad target" true
        (List.exists (fun msg -> String.contains msg '9') errs)

let test_mir_reject_bad_vreg () =
  let mir = lower_src "let x = 42" in
  let fn = List.hd mir.Mir.functions in
  (* Corrupt an instruction to write to out-of-range vreg *)
  let bad_block = {
    Mir.label = fn.Mir.entry;
    phis = [];
    instrs = [ Mir.IConst (fn.Mir.n_vregs + 100, Mir.CInt 42) ];
    term = Mir.TRet (Some 0);
    span = Span.dummy;
  } in
  let bad_fn = { fn with Mir.blocks = [ bad_block ] } in
  let bad_prog = { mir with Mir.functions = [ bad_fn ] } in
  match Mir_verify.verify_program bad_prog with
  | Ok () -> fail "expected verification failure for out-of-range vreg"
  | Error errs ->
      check bool "catches bad vreg" true
        (List.exists (fun msg -> String.contains msg 'v' || String.contains msg 'r') errs)

let tests =
  [
    ("mir_lower_valid", `Quick, test_mir_lower_valid);
    ("mir_reject_bad_target", `Quick, test_mir_reject_bad_target);
    ("mir_reject_bad_vreg", `Quick, test_mir_reject_bad_vreg);
  ]
