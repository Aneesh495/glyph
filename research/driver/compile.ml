(** Compile pipeline. *)

let parse ~file source = Parser.parse_program ~file source

let typecheck prog = Infer.infer_program prog
