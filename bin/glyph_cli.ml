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
      Error "failed"

let cmd_typecheck file =
  let source = read_file file in
  match Parser.parse_program ~file source with
  | Error e ->
      emit_diag (Parser.error_to_diagnostic e) source;
      Error "failed"
  | Ok prog -> (
      match Infer.infer_program prog with
      | Ok _env ->
          Printf.printf "typecheck ok\n";
          Ok ()
      | Error diags ->
          List.iter (fun d -> emit_diag d source) diags;
          Error "failed")

let cmd_run file =
  let source = read_file file in
  match Parser.parse_program ~file source with
  | Error e ->
      emit_diag (Parser.error_to_diagnostic e) source;
      Error "failed"
  | Ok prog -> (
      match Infer.infer_program prog with
      | Error diags ->
          List.iter (fun d -> emit_diag d source) diags;
          Error "failed"
      | Ok _env ->
          (* Full bytecode VM lives under research/; typecheck is the gate for now. *)
          Printf.printf "typecheck ok — runtime pipeline in research/\n";
          Ok ())

let file_arg = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let parse_cmd =
  Cmd.v
    (Cmd.info "parse" ~doc:"Parse a Glyph source file")
    Term.(const cmd_parse $ file_arg)

let typecheck_cmd =
  Cmd.v
    (Cmd.info "typecheck" ~doc:"Type-check a Glyph source file")
    Term.(const cmd_typecheck $ file_arg)

let run_cmd =
  Cmd.v
    (Cmd.info "run" ~doc:"Type-check (and eventually execute) a Glyph program")
    Term.(const cmd_run $ file_arg)

let () =
  let info =
    Cmd.info "glyph" ~version:"0.1.0" ~doc:"Glyph language toolchain"
  in
  exit (Cmd.eval_result (Cmd.group info [ parse_cmd; typecheck_cmd; run_cmd ]))
