(** End-to-end Glyph compilation pipeline. *)

type options = {
  optimize : bool;
  opt_level : int;
  dump_ast : bool;
  dump_types : bool;
}

let default_options = {
  optimize = true;
  opt_level = 1;
  dump_ast = false;
  dump_types = false;
}

type result = {
  program : Ast.program;
  env : Env.t;
  hir : Hir.program option;
  mir : Mir.program option;
  chunk : Chunk.t option;
}

let parse ?(file = "<input>") source =
  match Parser.parse_program ~file source with
  | Ok prog -> Ok prog
  | Error e -> Error [ Parser.error_to_diagnostic e ]

let typecheck prog = Infer.infer_program prog

let compile_string ?(file = "<input>") ?(options = default_options) source =
  match parse ~file source with
  | Error diags -> Error diags
  | Ok prog -> (
      match typecheck prog with
      | Error diags -> Error diags
      | Ok env -> (
          try
            let hir = Desugar.desugar_program prog in
            let hir_compiled = Pattern.compile_program hir in
            let mir = Mir_lower.lower_program hir_compiled in
            match Mir_verify.verify_program mir with
            | Error errs ->
                let msg = "MIR verification failed before opt:\n" ^ String.concat "\n" errs in
                Error [ Diagnostic.error prog.span msg ]
            | Ok () ->
                let mir_opt =
                  if options.optimize then
                    let pipe =
                      match options.opt_level with
                      | 0 -> Pass_manager.o0_pipeline
                      | 1 -> Pass_manager.o1_pipeline ()
                      | _ -> Pass_manager.o2_pipeline ()
                    in
                    let opt = Pass_manager.run_pipeline pipe mir in
                    match Mir_verify.verify_program opt with
                    | Error errs ->
                        let msg = "MIR verification failed after opt:\n" ^ String.concat "\n" errs in
                        failwith msg
                    | Ok () -> opt
                  else mir
                in
                let chunk = Emit.emit_program mir_opt in
                match Bytecode_verify.verify_chunk chunk with
                | Error errs ->
                    let msg = "Bytecode verification failed:\n" ^ String.concat "\n" errs in
                    Error [ Diagnostic.error prog.span msg ]
                | Ok () ->
                    Ok
                      {
                        program = prog;
                        env;
                        hir = Some hir_compiled;
                        mir = Some mir_opt;
                        chunk = Some chunk;
                      }
          with
          | Failure msg -> Error [ Diagnostic.error prog.span msg ]
          | exn -> Error [ Diagnostic.error prog.span (Printexc.to_string exn) ]))

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let compile_file ?(options = default_options) file =
  let source = read_file file in
  compile_string ~file ~options source

let run_file ?(options = default_options) file =
  match compile_file ~options file with
  | Error diags ->
      let msg = String.concat "\n" (List.map (fun d -> d.Diagnostic.message) diags) in
      Error msg
  | Ok res -> (
      match res.chunk with
      | None -> Error "no bytecode chunk generated"
      | Some chunk -> (
          match Interp.run_chunk chunk with
          | Interp.Ok v -> Ok v
          | Interp.Runtime_error msg -> Error msg))
