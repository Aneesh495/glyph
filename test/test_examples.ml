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

let run_file_expect path expected =
  let resolved = resolve_path path in
  match Compile.compile_file resolved with
  | Error diags ->
      let msg = String.concat "\n" (List.map (fun d -> d.Diagnostic.message) diags) in
      fail ("failed to compile " ^ path ^ ": " ^ msg)
  | Ok res -> (
      match res.chunk with
      | None -> fail ("no chunk for " ^ path)
      | Some chunk -> (
          let output_buf = Buffer.create 64 in
          Builtin.set_output_sink (Some (fun s -> Buffer.add_string output_buf s));
          Fun.protect
            ~finally:(fun () -> Builtin.set_output_sink None)
            (fun () ->
              let vm = Interp.create chunk in
              match Interp.run vm with
              | Interp.Runtime_error err -> fail ("VM error in " ^ path ^ ": " ^ err)
              | Interp.Ok v ->
                  let printed = String.trim (Buffer.contents output_buf) in
                  let result =
                    if printed <> "" then printed
                    else Value.to_string v
                  in
                  check string (path ^ " result") expected result)))

let test_fib () = run_file_expect "examples/fib.gl" "55"
let test_ackermann () = run_file_expect "examples/ackermann.gl" "61"
let test_list_map () = run_file_expect "examples/list_map.gl" "30"
let test_tree_depth () = run_file_expect "test/fixtures/tree_depth.gl" "4"
let test_closure_capture () = run_file_expect "test/fixtures/closure_capture.gl" "65"
let test_tail_sum () = run_file_expect "test/fixtures/tail_sum.gl" "50005000"

let tests =
  [
    ("fib", `Quick, test_fib);
    ("ackermann", `Quick, test_ackermann);
    ("list_map", `Quick, test_list_map);
    ("tree_depth", `Quick, test_tree_depth);
    ("closure_capture", `Quick, test_closure_capture);
    ("tail_sum", `Quick, test_tail_sum);
  ]
