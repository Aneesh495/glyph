open Alcotest

let compile_and_run ~opt_level src =
  let options = {
    Compile.optimize = (opt_level > 0);
    opt_level;
    dump_ast = false;
    dump_types = false;
  } in
  match Compile.compile_string ~options src with
  | Error diags ->
      let msg = String.concat "\n" (List.map (fun d -> d.Diagnostic.message) diags) in
      fail ("compilation failed: " ^ msg)
  | Ok res -> (
      match res.chunk with
      | None -> fail "no chunk"
      | Some chunk -> (
          let vm = Interp.create chunk in
          match Interp.run vm with
          | Interp.Runtime_error err -> fail ("VM runtime error: " ^ err)
          | Interp.Ok v -> Value.to_string v))

let test_o0_o1_o2_equivalence () =
  let src = "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)\nlet main = fib 10" in
  let r0 = compile_and_run ~opt_level:0 src in
  let r1 = compile_and_run ~opt_level:1 src in
  let r2 = compile_and_run ~opt_level:2 src in
  check string "O0 = O1" r0 r1;
  check string "O1 = O2" r1 r2;
  check string "result is 55" "55" r0

let test_const_folding () =
  let src = "let x = 10 + 20 * 3" in
  let prog =
    match Parser.parse_program src with
    | Error e -> fail ("parse: " ^ e.Parser.message)
    | Ok p ->
        let hir = Desugar.desugar_program p in
        let hir_c = Pattern.compile_program hir in
        Mir_lower.lower_program hir_c
  in
  let opt = Pass_manager.run_pipeline (Pass_manager.o1_pipeline ()) prog in
  match Mir_verify.verify_program opt with
  | Ok () -> check bool "opt verified" true true
  | Error errs -> fail ("opt verification failed: " ^ String.concat "\n" errs)

let tests =
  [
    ("o0_o1_o2_equivalence", `Quick, test_o0_o1_o2_equivalence);
    ("const_folding", `Quick, test_const_folding);
  ]
