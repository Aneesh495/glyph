(** Glyph command-line interface. *)

open Cmdliner

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let emit_diag d source =
  output_string stderr (Diagnostic.render ~source d);
  flush stderr

let cmd_parse file =
  let source = read_file file in
  match Parser.parse_program ~file source with
  | Ok prog ->
      print_string (Pretty.program_to_string prog);
      Ok ()
  | Error e ->
      emit_diag (Parser.error_to_diagnostic e) source;
      Error "parse failed"

let cmd_typecheck file =
  let source = read_file file in
  match Parser.parse_program ~file source with
  | Error e ->
      emit_diag (Parser.error_to_diagnostic e) source;
      Error "parse failed"
  | Ok prog -> (
      match Infer.infer_program prog with
      | Ok _env ->
          Printf.printf "typecheck ok\n";
          Ok ()
      | Error diags ->
          List.iter (fun d -> emit_diag d source) diags;
          Error "typecheck failed")

let cmd_run opt opt_level file =
  if Filename.extension file = ".gbc" then (
    try
      let chunk = Chunk.read_file file in
      match Bytecode_verify.verify_chunk chunk with
      | Error errs ->
          List.iter (fun e -> Printf.eprintf "Verification error: %s\n" e) errs;
          flush stderr;
          Error "bytecode verification failed"
      | Ok () -> (
          match Interp.run_chunk chunk with
          | Interp.Ok _v -> Ok ()
          | Interp.Runtime_error msg ->
              Printf.eprintf "Runtime error: %s\n" msg;
              flush stderr;
              Error "runtime error")
    with exn ->
      Printf.eprintf "Error reading/running bytecode: %s\n" (Printexc.to_string exn);
      flush stderr;
      Error "runtime error")
  else
    let source = read_file file in
    let options =
      {
        Compile.default_options with
        optimize = opt;
        opt_level;
      }
    in
    match Compile.compile_string ~file ~options source with
    | Error diags ->
        List.iter (fun d -> emit_diag d source) diags;
        Error "compile failed"
    | Ok res -> (
        match res.chunk with
        | None ->
            Printf.eprintf "Internal error: no bytecode produced\n";
            Error "no bytecode"
        | Some chunk -> (
            match Interp.run_chunk chunk with
            | Interp.Ok _v -> Ok ()
            | Interp.Runtime_error msg ->
                Printf.eprintf "Runtime error: %s\n" msg;
                flush stderr;
                Error "runtime error"))

let cmd_compile opt opt_level output file =
  let source = read_file file in
  let options =
    {
      Compile.default_options with
      optimize = opt;
      opt_level;
    }
  in
  match Compile.compile_string ~file ~options source with
  | Error diags ->
      List.iter (fun d -> emit_diag d source) diags;
      Error "compile failed"
  | Ok res -> (
      match res.chunk with
      | None ->
          Printf.eprintf "Internal error: no bytecode produced\n";
          Error "no bytecode"
      | Some chunk ->
          let out_path =
            match output with
            | Some p -> p
            | None ->
                if Filename.check_suffix file ".gl" then
                  Filename.chop_suffix file ".gl" ^ ".gbc"
                else file ^ ".gbc"
          in
          Chunk.write_file chunk out_path;
          Ok ())

let cmd_disasm file =
  if Filename.extension file = ".gbc" then
    try
      print_string (Disasm.disassemble_file file);
      Ok ()
    with exn ->
      Printf.eprintf "Error disassembling bytecode: %s\n" (Printexc.to_string exn);
      flush stderr;
      Error "disasm failed"
  else
    let source = read_file file in
    let options = Compile.default_options in
    match Compile.compile_string ~file ~options source with
    | Error diags ->
        List.iter (fun d -> emit_diag d source) diags;
        Error "compile failed"
    | Ok res -> (
        match res.chunk with
        | None ->
            Printf.eprintf "Internal error: no bytecode produced\n";
            Error "no bytecode"
        | Some chunk ->
            print_string (Disasm.to_string chunk);
            Ok ())

(* CLI Options *)
let file_arg = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let opt_arg =
  let doc = "Enable optimizations." in
  Arg.(value & flag & info [ "opt" ] ~doc)

let opt_level_arg =
  let doc = "Optimization level (0, 1, or 2)." in
  Arg.(value & opt int 1 & info [ "O"; "opt-level" ] ~doc ~docv:"LEVEL")

let output_arg =
  let doc = "Output file path." in
  Arg.(value & opt (some string) None & info [ "o"; "output" ] ~doc ~docv:"FILE")

(* Commands *)
let parse_cmd =
  Cmd.v
    (Cmd.info "parse" ~doc:"Parse a Glyph source file and print surface AST")
    Term.(const cmd_parse $ file_arg)

let typecheck_cmd =
  Cmd.v
    (Cmd.info "typecheck" ~doc:"Type-check a Glyph source file")
    Term.(const cmd_typecheck $ file_arg)

let run_cmd =
  Cmd.v
    (Cmd.info "run" ~doc:"Execute a Glyph source or bytecode file")
    Term.(const cmd_run $ opt_arg $ opt_level_arg $ file_arg)

let compile_cmd =
  Cmd.v
    (Cmd.info "compile" ~doc:"Compile a Glyph source file to a bytecode (.gbc) file")
    Term.(const cmd_compile $ opt_arg $ opt_level_arg $ output_arg $ file_arg)

let disasm_cmd =
  Cmd.v
    (Cmd.info "disasm" ~doc:"Disassemble a Glyph bytecode or source file")
    Term.(const cmd_disasm $ file_arg)

let () =
  let info =
    Cmd.info "glyph" ~version:"0.1.0" ~doc:"Glyph language toolchain"
  in
  exit
    (Cmd.eval_result
       (Cmd.group info [ parse_cmd; typecheck_cmd; run_cmd; compile_cmd; disasm_cmd ]))
