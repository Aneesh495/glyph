(** Hand-written lexer for Glyph source text. *)

type t = {
  filename : string;
  source : string;
  length : int;
  mutable pos : Span.pos;
  mutable diagnostics : Diagnostic.t list;
  skip_comments : bool;
}

let create ?(skip_comments = true) ~filename ~source () =
  {
    filename;
    source;
    length = String.length source;
    pos = { Span.line = 1; col = 1; offset = 0 };
    diagnostics = [];
    skip_comments;
  }

let diagnostics lex = List.rev lex.diagnostics

let add_error lex span message =
  lex.diagnostics <- Diagnostic.error span message :: lex.diagnostics

let peek lex =
  if lex.pos.offset >= lex.length then None else Some lex.source.[lex.pos.offset]

let peek_at lex n =
  let i = lex.pos.offset + n in
  if i >= lex.length then None else Some lex.source.[i]

let advance lex =
  match peek lex with
  | None -> ()
  | Some ch -> lex.pos <- Span.advance_pos lex.pos ~ch

let advance_n lex n =
  for _ = 1 to n do
    advance lex
  done

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let skip_whitespace lex =
  let rec loop () =
    match peek lex with
    | Some ch when is_whitespace ch ->
        advance lex;
        loop ()
    | _ -> ()
  in
  loop ()

let make_token lex kind start raw =
  let span = Span.make ~file:lex.filename ~start ~end_:lex.pos in
  Token.make kind span raw

let slice_raw lex start =
  let len = lex.pos.offset - start.Span.offset in
  if len <= 0 then "" else String.sub lex.source start.offset len

let lex_line_comment lex start =
  while
    match peek lex with
    | Some '\n' | None -> false
    | Some _ -> true
  do
    advance lex
  done;
  let raw = slice_raw lex start in
  let body =
    if String.length raw >= 2 then String.sub raw 2 (String.length raw - 2)
    else ""
  in
  make_token lex (Token.Comment body) start raw

let lex_block_comment lex start =
  advance_n lex 2;
  let depth = ref 1 in
  let unterminated = ref false in
  while !depth > 0 do
    match peek lex with
    | None ->
        unterminated := true;
        depth := 0
    | Some '/' when peek_at lex 1 = Some '*' ->
        advance_n lex 2;
        incr depth
    | Some '*' when peek_at lex 1 = Some '/' ->
        advance_n lex 2;
        decr depth
    | Some _ -> advance lex
  done;
  let raw = slice_raw lex start in
  if !unterminated then (
    let span = Span.make ~file:lex.filename ~start ~end_:lex.pos in
    add_error lex span "unterminated block comment";
    make_token lex (Token.Error "unterminated block comment") start raw)
  else
    let body =
      let n = String.length raw in
      if n >= 4 then String.sub raw 2 (n - 4) else ""
    in
    make_token lex (Token.Comment body) start raw

let decode_hex_escape lex ~digits =
  let buf = Buffer.create digits in
  let rec loop i =
    if i >= digits then Ok (Buffer.contents buf)
    else
      match peek lex with
      | Some ch when is_hex_digit ch ->
          Buffer.add_char buf ch;
          advance lex;
          loop (i + 1)
      | _ -> Error "invalid hex escape"
  in
  match loop 0 with
  | Error e -> Error e
  | Ok hex -> (
      try Ok (Char.chr (int_of_string ("0x" ^ hex)))
      with Failure _ | Invalid_argument _ -> Error "hex escape out of range")

let lex_string_escapes lex start quote =
  advance lex;
  let buf = Buffer.create 32 in
  let error_msg = ref None in
  let finished = ref false in
  while not !finished do
    match peek lex with
    | None ->
        error_msg := Some "unterminated string literal";
        finished := true
    | Some ch when ch = quote ->
        advance lex;
        finished := true
    | Some '\\' -> (
        advance lex;
        match peek lex with
        | None ->
            error_msg := Some "unterminated string escape";
            finished := true
        | Some 'n' ->
            Buffer.add_char buf '\n';
            advance lex
        | Some 't' ->
            Buffer.add_char buf '\t';
            advance lex
        | Some 'r' ->
            Buffer.add_char buf '\r';
            advance lex
        | Some '\\' ->
            Buffer.add_char buf '\\';
            advance lex
        | Some '\'' ->
            Buffer.add_char buf '\'';
            advance lex
        | Some '"' ->
            Buffer.add_char buf '"';
            advance lex
        | Some '0' ->
            Buffer.add_char buf '\000';
            advance lex
        | Some 'x' -> (
            advance lex;
            match decode_hex_escape lex ~digits:2 with
            | Ok c -> Buffer.add_char buf c
            | Error msg ->
                error_msg := Some msg;
                finished := true)
        | Some 'u' when peek_at lex 1 = Some '{' -> (
            advance_n lex 2;
            let hex = Buffer.create 6 in
            let ok = ref true in
            while !ok do
              match peek lex with
              | Some '}' ->
                  advance lex;
                  ok := false
              | Some ch when is_hex_digit ch ->
                  Buffer.add_char hex ch;
                  advance lex
              | _ ->
                  error_msg := Some "invalid unicode escape";
                  ok := false;
                  finished := true
            done;
            if !error_msg = None then
              try
                let code = int_of_string ("0x" ^ Buffer.contents hex) in
                if code < 0 || code > 0x10FFFF then
                  error_msg := Some "unicode escape out of range"
                else if code <= 0xFF then Buffer.add_char buf (Char.chr code)
                else
                  let encode cp =
                    if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
                    else if cp <= 0x7FF then (
                      Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor (cp land 0x3F))))
                    else if cp <= 0xFFFF then (
                      Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor (cp land 0x3F))))
                    else (
                      Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
                      Buffer.add_char buf
                        (Char.chr (0x80 lor (cp land 0x3F))))
                  in
                  encode code
              with Failure _ -> error_msg := Some "invalid unicode escape")
        | Some c ->
            Buffer.add_char buf c;
            advance lex)
    | Some '\n' when quote = '"' ->
        error_msg := Some "newline in string literal";
        finished := true
    | Some c ->
        Buffer.add_char buf c;
        advance lex
  done;
  let raw = slice_raw lex start in
  match !error_msg with
  | Some msg ->
      let span = Span.make ~file:lex.filename ~start ~end_:lex.pos in
      add_error lex span msg;
      make_token lex (Token.Error msg) start raw
  | None ->
      let value = Buffer.contents buf in
      if quote = '\'' then
        if String.length value = 1 then
          make_token lex (Token.Lit_char value.[0]) start raw
        else if String.length value = 0 then (
          let span = Span.make ~file:lex.filename ~start ~end_:lex.pos in
          add_error lex span "empty character literal";
          make_token lex (Token.Error "empty character literal") start raw)
        else (
          let span = Span.make ~file:lex.filename ~start ~end_:lex.pos in
          add_error lex span
            "character literal must contain exactly one character";
          make_token lex
            (Token.Error "character literal must contain exactly one character")
            start raw)
      else make_token lex (Token.Lit_string value) start raw

let lex_number lex start =
  let is_float = ref false in
  (* hex or binary prefix *)
  (match (peek lex, peek_at lex 1) with
  | Some '0', Some ('x' | 'X' as x) ->
      advance_n lex 2;
      let had = ref false in
      while
        match peek lex with Some ch when is_hex_digit ch -> true | _ -> false
      do
        had := true;
        advance lex
      done;
      if not !had then
        add_error lex
          (Span.make ~file:lex.filename ~start ~end_:lex.pos)
          "hex literal has no digits";
      (* rewrite start raw later; stay in int mode *)
      ()
  | Some '0', Some ('b' | 'B') ->
      advance_n lex 2;
      let had = ref false in
      while
        match peek lex with
        | Some ('0' | '1') -> true
        | _ -> false
      do
        had := true;
        advance lex
      done;
      if not !had then
        add_error lex
          (Span.make ~file:lex.filename ~start ~end_:lex.pos)
          "binary literal has no digits"
  | _ ->
      while match peek lex with Some ch when is_digit ch -> true | _ -> false do
        advance lex
      done;
      (match (peek lex, peek_at lex 1) with
      | Some '.', Some ch when is_digit ch ->
          is_float := true;
          advance lex;
          while
            match peek lex with Some d when is_digit d -> true | _ -> false
          do
            advance lex
          done
      | _ -> ());
      (match peek lex with
      | Some ('e' | 'E') ->
          is_float := true;
          advance lex;
          (match peek lex with
          | Some ('+' | '-') -> advance lex
          | _ -> ());
          let had_digit = ref false in
          while
            match peek lex with Some d when is_digit d -> true | _ -> false
          do
            had_digit := true;
            advance lex
          done;
          if not !had_digit then
            add_error lex
              (Span.make ~file:lex.filename ~start ~end_:lex.pos)
              "exponent has no digits"
      | _ -> ()));
  (match peek lex with
  | Some ch when is_ident_start ch ->
      let span_start = lex.pos in
      while
        match peek lex with Some c when is_ident_continue c -> true | _ -> false
      do
        advance lex
      done;
      add_error lex
        (Span.make ~file:lex.filename ~start:span_start ~end_:lex.pos)
        "invalid numeric literal suffix"
  | _ -> ());
  let raw = slice_raw lex start in
  if !is_float then make_token lex (Token.Lit_float raw) start raw
  else make_token lex (Token.Lit_int raw) start raw

let lex_ident lex start =
  while
    match peek lex with Some ch when is_ident_continue ch -> true | _ -> false
  do
    advance lex
  done;
  let raw = slice_raw lex start in
  if raw = "_" then make_token lex (Token.Punct Token.Underscore) start raw
  else
    let kind = Token.keyword_of_ident raw in
    make_token lex kind start raw

let lex_operator lex start =
  let buf = Buffer.create 4 in
  while
    match peek lex with Some ch when Token.is_op_char ch -> true | _ -> false
  do
    Buffer.add_char buf (Option.get (peek lex));
    advance lex
  done;
  let raw = Buffer.contents buf in
  let rec try_split s =
    if s = "" then None
    else
      match Token.classify_operator s with
      | Token.Punct _ as kind -> Some (kind, s)
      | Token.Operator _ when String.length s > 1 ->
          try_split (String.sub s 0 (String.length s - 1))
      | Token.Operator _ as kind -> Some (kind, s)
      | _ -> None
  in
  match try_split raw with
  | None ->
      add_error lex
        (Span.make ~file:lex.filename ~start ~end_:lex.pos)
        ("unexpected operator " ^ raw);
      make_token lex (Token.Error raw) start raw
  | Some (kind, matched) ->
      if String.length matched < String.length raw then (
        let rewind = String.length raw - String.length matched in
        lex.pos <-
          {
            lex.pos with
            offset = lex.pos.offset - rewind;
            col = lex.pos.col - rewind;
          };
        make_token lex kind start matched)
      else make_token lex kind start raw

let lex_single lex start ch =
  advance lex;
  let kind =
    match ch with
    | '(' -> Token.Punct Token.LParen
    | ')' -> Token.Punct Token.RParen
    | '[' -> Token.Punct Token.LBracket
    | ']' -> Token.Punct Token.RBracket
    | '{' -> Token.Punct Token.LBrace
    | '}' -> Token.Punct Token.RBrace
    | ',' -> Token.Punct Token.Comma
    | ';' -> Token.Punct Token.Semicolon
    | '\\' -> Token.Punct Token.Backslash
    | _ -> Token.Error (String.make 1 ch)
  in
  let raw = String.make 1 ch in
  (match kind with
  | Token.Error msg ->
      add_error lex
        (Span.make ~file:lex.filename ~start ~end_:lex.pos)
        ("unexpected character " ^ msg)
  | _ -> ());
  make_token lex kind start raw

let looks_like_char_literal lex =
  match (peek_at lex 1, peek_at lex 2) with
  | Some '\\', _ -> true
  | Some ch, Some '\'' when ch <> '\'' -> true
  | _ -> false

let rec next_token lex =
  skip_whitespace lex;
  let start = lex.pos in
  match peek lex with
  | None -> make_token lex Token.Eof start ""
  | Some '/' when peek_at lex 1 = Some '/' ->
      let tok = lex_line_comment lex start in
      if lex.skip_comments then next_token lex else tok
  | Some '/' when peek_at lex 1 = Some '*' ->
      let tok = lex_block_comment lex start in
      if lex.skip_comments then next_token lex else tok
  | Some '"' -> lex_string_escapes lex start '"'
  | Some '\'' when looks_like_char_literal lex ->
      lex_string_escapes lex start '\''
  | Some '\'' ->
      advance lex;
      (match peek lex with
      | Some ch when is_ident_start ch ->
          let id_start = lex.pos in
          while
            match peek lex with
            | Some c when is_ident_continue c -> true
            | _ -> false
          do
            advance lex
          done;
          let name = "'" ^ slice_raw lex id_start in
          make_token lex (Token.Ident name) start name
      | _ -> make_token lex (Token.Punct Token.Apostrophe) start "'")
  | Some ch when is_digit ch -> lex_number lex start
  | Some ch when is_ident_start ch -> lex_ident lex start
  | Some ch when Token.is_op_char ch -> lex_operator lex start
  | Some ch -> lex_single lex start ch

let tokenize ?(skip_comments = true) ~filename ~source () =
  let lex = create ~skip_comments ~filename ~source () in
  let rec loop acc =
    let tok = next_token lex in
    match tok.Token.kind with
    | Token.Eof -> List.rev (tok :: acc)
    | _ -> loop (tok :: acc)
  in
  let tokens = loop [] in
  let diags = diagnostics lex in
  if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
    Error diags
  else Ok tokens

let tokenize_allowing_errors ?(skip_comments = true) ~filename ~source () =
  let lex = create ~skip_comments ~filename ~source () in
  let rec loop acc =
    let tok = next_token lex in
    match tok.Token.kind with
    | Token.Eof -> List.rev (tok :: acc)
    | Token.Error _ -> loop (tok :: acc)
    | _ -> loop (tok :: acc)
  in
  let tokens = loop [] in
  (tokens, diagnostics lex)

let lex_all ~filename ~source = tokenize_allowing_errors ~filename ~source ()
