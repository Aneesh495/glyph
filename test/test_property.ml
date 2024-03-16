let prop_int_lex =
  QCheck.Test.make ~count:100 ~name:"lex_int_roundtrip"
    QCheck.(map (fun n -> Int64.of_int (abs n)) (int_range 0 1_000_000))
    (fun n ->
      let s = Int64.to_string n in
      match Lexer.tokenize ~filename:"<prop>" ~source:s () with
      | Ok [ { Token.kind = Token.Int n'; _ }; _ ] -> n = n'
      | _ -> false)

let prop_opcode_encode =
  QCheck.Test.make ~count:100 ~name:"opcode_encode_roundtrip"
    QCheck.(triple (int_range 0 255) (int_range 0 255) (int_range 0 255))
    (fun (a, b, c) ->
      let instr = { Opcode.op = Opcode.Op_add; a; b; c; extra = [||] } in
      let enc = Opcode.encode instr in
      let instr', _ = Opcode.decode enc 0 in
      instr.op = instr'.op && instr.a = instr'.a && instr.b = instr'.b && instr.c = instr'.c)

let prop_eval_add =
  QCheck.Test.make ~count:50 ~name:"eval_add_identity"
    QCheck.(pair (int_range 0 1000) (int_range 0 1000))
    (fun (a, b) ->
      let src = Printf.sprintf "let main = %d + %d" a b in
      match Compile.compile_string src with
      | Ok res -> (
          match res.chunk with
          | Some chunk -> (
              let vm = Interp.create chunk in
              match Interp.run vm with
              | Ok (Value.Int res_n) -> res_n = a + b
              | _ -> false)
          | None -> false)
      | Error _ -> false)

let tests =
  List.map QCheck_alcotest.to_alcotest [ prop_int_lex; prop_opcode_encode; prop_eval_add ]
