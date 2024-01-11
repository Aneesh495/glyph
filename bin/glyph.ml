open Cmdliner
let file_arg = Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE")
let check_only = Arg.(value & flag & info ["c";"check"])
let run file check_only =
  if check_only then
    match Compile.compile_file file with
    | Ok _ -> print_endline "OK"; `Ok ()
    | Error ds -> List.iter (fun d -> output_string stderr (Diagnostic.render d)) ds; `Error (false, "type error")
  else
    match Compile.run_file file with
    | Ok v -> Format.printf "%a@." Value.pp v; `Ok ()
    | Error m -> `Error (false, m)
let () = exit (Cmd.eval (Cmd.v (Cmd.info "glyph" ~version:"0.1.0") Term.(ret (const run $ file_arg $ check_only))))
