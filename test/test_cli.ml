open Alcotest

let project_root =
  let rec find dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then "."
      else find parent
  in
  find (Sys.getcwd ())

let resolve_path p =
  if Sys.file_exists p then p
  else
    let p' = Filename.concat project_root p in
    if Sys.file_exists p' then p' else p

let cli =
  let candidates =
    [
      resolve_path "_build/default/bin/glyph_cli.exe";
      resolve_path "bin/glyph_cli.exe";
      "_build/default/bin/glyph_cli.exe";
      "bin/glyph_cli.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> "_build/default/bin/glyph_cli.exe"

let run_cmd cmd =
  let code = Sys.command cmd in
  code

let test_cli_parse () =
  let fib = resolve_path "examples/fib.gl" in
  let err_fixture = resolve_path "test/fixtures/type_error.gl" in
  let code = run_cmd (Printf.sprintf "%s parse %s > /dev/null 2>&1" cli fib) in
  check int "parse fib succeeds" 0 code;
  let err_code = run_cmd (Printf.sprintf "%s parse %s > /dev/null 2>&1" cli err_fixture) in
  check int "parse type_error succeeds (syntactically valid)" 0 err_code

let test_cli_typecheck () =
  let fib = resolve_path "examples/fib.gl" in
  let err_fixture = resolve_path "test/fixtures/type_error.gl" in
  let code = run_cmd (Printf.sprintf "%s typecheck %s > /dev/null 2>&1" cli fib) in
  check int "typecheck fib succeeds" 0 code;
  let err_code = run_cmd (Printf.sprintf "%s typecheck %s > /dev/null 2>&1" cli err_fixture) in
  check bool "typecheck error exits non-zero" true (err_code <> 0)

let test_cli_compile_and_disasm () =
  let fib = resolve_path "examples/fib.gl" in
  let tmp = Filename.temp_file "glyph_test" ".gbc" in
  let code = run_cmd (Printf.sprintf "%s compile %s -o %s > /dev/null 2>&1" cli fib tmp) in
  check int "compile fib succeeds" 0 code;
  check bool "gbc exists" true (Sys.file_exists tmp);
  let dis_code = run_cmd (Printf.sprintf "%s disasm %s > /dev/null 2>&1" cli tmp) in
  check int "disasm succeeds" 0 dis_code;
  let run_gbc_code = run_cmd (Printf.sprintf "%s run %s > /dev/null 2>&1" cli tmp) in
  check int "run gbc succeeds" 0 run_gbc_code;
  if Sys.file_exists tmp then Sys.remove tmp

let test_cli_run () =
  let fib = resolve_path "examples/fib.gl" in
  let err_fixture = resolve_path "test/fixtures/type_error.gl" in
  let code = run_cmd (Printf.sprintf "%s run %s > /dev/null 2>&1" cli fib) in
  check int "run fib succeeds" 0 code;
  let err_code = run_cmd (Printf.sprintf "%s run %s > /dev/null 2>&1" cli err_fixture) in
  check bool "run type_error exits non-zero" true (err_code <> 0)

let tests =
  [
    ("cli_parse", `Quick, test_cli_parse);
    ("cli_typecheck", `Quick, test_cli_typecheck);
    ("cli_compile_and_disasm", `Quick, test_cli_compile_and_disasm);
    ("cli_run", `Quick, test_cli_run);
  ]
