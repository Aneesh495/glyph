open Alcotest

let compile_src src =
  match Compile.compile_string src with
  | Error diags ->
      let msg = String.concat "\n" (List.map (fun d -> d.Diagnostic.message) diags) in
      fail ("compile failed: " ^ msg)
  | Ok res -> (
      match res.chunk with
      | None -> fail "no chunk generated"
      | Some chunk -> chunk)

let test_encoding_roundtrip () =
  let chunk = compile_src "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)\nlet main = fib 5" in
  match Disasm.verify_encoding chunk with
  | Ok () -> check bool "encoding roundtrip" true true
  | Error msg -> fail ("roundtrip mismatch: " ^ msg)

let test_chunk_serialization_roundtrip () =
  let chunk = compile_src "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nlet main = fact 5" in
  let bytes = Chunk.to_bytes chunk in
  let chunk2 = Chunk.of_bytes bytes in
  check int "same number of funcs" (Array.length chunk.funcs) (Array.length chunk2.funcs);
  check int "same number of consts" (Array.length chunk.consts) (Array.length chunk2.consts);
  check int "same number of instrs" (Array.length chunk.code) (Array.length chunk2.code);
  check int "same main id" chunk.main chunk2.main;
  match Bytecode_verify.verify_chunk chunk2 with
  | Ok () -> check bool "deserialized chunk verifies" true true
  | Error errs -> fail ("verification failed: " ^ String.concat "\n" errs)

let test_verifier_reject_bad_reg () =
  let chunk = compile_src "let x = 42" in
  (* Corrupt instruction 0 to use register 999 where nregs is smaller *)
  let bad_instr = { (chunk.code.(0)) with a = 999 } in
  let bad_code = Array.copy chunk.code in
  bad_code.(0) <- bad_instr;
  let bad_chunk = { chunk with code = bad_code } in
  match Bytecode_verify.verify_chunk bad_chunk with
  | Ok () -> fail "expected bytecode verification to reject out of bounds register"
  | Error errs ->
      check bool "catches out of bounds register" true
        (List.exists (fun msg -> String.contains msg 'r' || String.contains msg 'R' || String.contains msg '9') errs)

let test_verifier_reject_bad_jump () =
  let chunk = compile_src "let x = 42" in
  (* Corrupt instruction 0 to jump out of bounds *)
  let bad_instr = { Opcode.op = Opcode.Op_jump; a = 99999; b = 0; c = 0; extra = [||] } in
  let bad_code = Array.copy chunk.code in
  bad_code.(0) <- bad_instr;
  let bad_chunk = { chunk with code = bad_code } in
  match Bytecode_verify.verify_chunk bad_chunk with
  | Ok () -> fail "expected bytecode verification to reject out of bounds jump"
  | Error errs ->
      check bool "catches out of bounds jump" true
        (List.exists (fun msg -> String.contains msg '9') errs)

let test_disasm_output () =
  let chunk = compile_src "let x = 42" in
  let s = Disasm.to_string chunk in
  check bool "non empty disassembly" true (String.length s > 0);
  check bool "contains main" true (String.contains s 'm')

let tests =
  [
    ("encoding_roundtrip", `Quick, test_encoding_roundtrip);
    ("chunk_serialization_roundtrip", `Quick, test_chunk_serialization_roundtrip);
    ("verifier_reject_bad_reg", `Quick, test_verifier_reject_bad_reg);
    ("verifier_reject_bad_jump", `Quick, test_verifier_reject_bad_jump);
    ("disasm_output", `Quick, test_disasm_output);
  ]
