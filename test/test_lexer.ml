open Alcotest

let test_literals () =
  let src = "42 0x2A 3.14 \"hello\\nworld\" 'z' true false" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error _ -> fail "expected lexer success"
  | Ok tokens ->
      let kinds = List.map Token.kind tokens in
      (match kinds with
      | [ Token.Int 42L;
          Token.Int 42L;
          Token.Float f;
          Token.String "hello\nworld";
          Token.Char 'z';
          Token.Keyword Token.Kw_true;
          Token.Keyword Token.Kw_false;
          Token.Eof ] ->
          check (float 0.001) "float literal" 3.14 f
      | _ -> fail "unexpected token kinds for literals")

let test_identifiers_and_keywords () =
  let src = "let rec match with type of fun fn extern" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error _ -> fail "expected lexer success"
  | Ok tokens ->
      let kinds = List.map Token.kind tokens in
      check bool "keywords recognized" true
        (List.mem (Token.Keyword Token.Kw_let) kinds
         && List.mem (Token.Keyword Token.Kw_rec) kinds
         && List.mem (Token.Keyword Token.Kw_match) kinds
         && List.mem (Token.Keyword Token.Kw_with) kinds
         && List.mem (Token.Keyword Token.Kw_type) kinds)

let test_operators () =
  let src = "+ - * / % = <> < <= > >= && || :: |>" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error _ -> fail "expected lexer success"
  | Ok tokens ->
      let kinds = List.map Token.kind tokens in
      check int "16 tokens including EOF" 16 (List.length kinds)

let test_comments () =
  let src = "// this is a comment\n42 /* block comment /* nested */ */ 99" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error _ -> fail "expected lexer success"
  | Ok tokens ->
      let kinds = List.map Token.kind tokens in
      check bool "comments ignored" true
        (kinds = [ Token.Int 42L; Token.Int 99L; Token.Eof ])

let test_spans () =
  let src = "let x = 10" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error _ -> fail "expected lexer success"
  | Ok tokens ->
      let tok = List.hd tokens in
      check int "start line" 1 tok.Token.span.Span.start.Span.line;
      check int "start col" 1 tok.Token.span.Span.start.Span.col

let test_invalid_tokens () =
  let src = "\"unterminated string" in
  match Lexer.tokenize ~filename:"<test>" ~source:src () with
  | Error diags -> check bool "has errors" true (List.length diags > 0)
  | Ok _ -> fail "expected error for unterminated string"

let tests =
  [
    ("literals", `Quick, test_literals);
    ("identifiers_and_keywords", `Quick, test_identifiers_and_keywords);
    ("operators", `Quick, test_operators);
    ("comments", `Quick, test_comments);
    ("spans", `Quick, test_spans);
    ("invalid_tokens", `Quick, test_invalid_tokens);
  ]
