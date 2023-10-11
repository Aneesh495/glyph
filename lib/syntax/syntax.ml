(** Syntax layer: tokens, lexing, parsing, AST, and pretty-printing.

    Downstream compiler passes should primarily depend on:

    - {!Ast.expr}, {!Ast.pattern}, {!Ast.type_expr}, {!Ast.program}
    - {!Parser.parse_program} : [string -> string -> (Ast.program, Diagnostic.t list) result]
      (filename, source)
    - {!Pretty.program_to_string} / {!Pretty.pp_program} for debugging dumps
*)

module Token = Token
module Lexer = Lexer
module Ast = Ast
module Parser = Parser
module Pretty = Pretty

let parse = Parser.parse_program
let lex = Lexer.tokenize

let parse_with_diagnostics filename source =
  match Parser.parse_program filename source with
  | Ok prog -> (Some prog, [])
  | Error diags -> (None, diags)
