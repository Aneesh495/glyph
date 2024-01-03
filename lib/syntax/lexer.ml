(** Hand-written lexer producing spanned [Token.t] values.

    Supports integers (decimal / hex), floats, strings with escapes, character
    literals, identifiers / constructors, operators, punctuation, [//] line
    comments, and nested [/* */] block comments. *)

type t = {
  filename : string;
  source : string;
  mutable pos : Span.pos;
  mutable diagnostics : Diagnostic.t list;
  mutable peeked : Token.t option;
}

let create ~filename ~source =
  {
    filename;
    source;
    pos = { Span.line = 1; col = 1; offset = 0 };
    diagnostics = [];
    peeked = None;
  }

let filename t = t.filename
let source t = t.source
let position t = t.pos
let diagnostics t = List.rev t.diagnostics

let length t = String.length t.source
let at_end t = t.pos.offset >= length t

let peek_char t =
  if at_end t then None else Some t.source.[t.pos.offset]

let peek_char_n t n =
  let i = t.pos.offset + n in
  if i >= length t then None else Some t.source.[i]

let advance t =
  match peek_char t with
  | None -> ()
  | Some ch -> t.pos <- Span.advance_pos t.pos ~ch

let advance_n t n =
  for _ = 1 to n do
    advance t
  done

let report t ?help span message =
  let d = Diagnostic.error span message in
  let d = match help with None -> d | Some h -> Diagnostic.with_help d h in
  t.diagnostics <- d :: t.diagnostics

let span_from t start =
  Span.make ~file:t.filename ~start ~end_:t.pos

let emit t kind ~start ~lexeme =
  Token.make kind ~span:(span_from t start) ~lexeme

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_op_char = function
  | '+' | '-' | '*' | '/' | '%' | '=' | '!' | '<' | '>' | '&' | '|' | ':'
  | '@' | '^' | '~' | '?' ->
      true
  | _ -> false

let read_while t pred =
  let start_off = t.pos.offset in
  while
    match peek_char t with
    | Some ch when pred ch ->
        advance t;
        true
    | _ -> false
  do
    ()
  done;
  String.sub t.source start_off (t.pos.offset - start_off)

let skip_line_comment t =
  while
    match peek_char t with
    | Some '\n' | None -> false
    | Some _ -> true
  do
    advance t
  done

let skip_block_comment t =
  let start = t.pos in
  let depth = ref 1 in
  while !depth > 0 do
    match peek_char t with
    | None ->
        report t (span_from t start) "unterminated block comment"
          ~help:"add a closing */ before the end of the file";
        depth := 0
    | Some '/' when peek_char_n t 1 = Some '*' ->
        advance_n t 2;
        incr depth
    | Some '*' when peek_char_n t 1 = Some '/' ->
        advance_n t 2;
        decr depth
    | Some _ -> advance t
  done

(** Also accept traditional OCaml-style [(* *)] comments for docs/examples. *)
let skip_ocaml_block_comment t =
  let start = t.pos in
  let depth = ref 1 in
  while !depth > 0 do
    match peek_char t with
    | None ->
        report t (span_from t start) "unterminated block comment"
          ~help:"add a closing *) before the end of the file";
        depth := 0
    | Some '(' when peek_char_n t 1 = Some '*' ->
        advance_n t 2;
        incr depth
    | Some '*' when peek_char_n t 1 = Some ')' ->
        advance_n t 2;
        decr depth
    | Some _ -> advance t
  done

let skip_trivia t =
  let rec loop () =
    match peek_char t with
    | Some ch when is_whitespace ch ->
        advance t;
        loop ()
    | Some '/' when peek_char_n t 1 = Some '/' ->
        advance_n t 2;
        skip_line_comment t;
        loop ()
    | Some '/' when peek_char_n t 1 = Some '*' ->
        advance_n t 2;
        skip_block_comment t;
        loop ()
    | Some '(' when peek_char_n t 1 = Some '*' ->
        advance_n t 2;
        skip_ocaml_block_comment t;
        loop ()
    | _ -> ()
  in
  loop ()

let decode_escape t ~start =
  match peek_char t with
  | None ->
      report t (span_from t start) "unterminated escape sequence";
      '\000'
  | Some 'n' ->
      advance t;
      '\n'
  | Some 't' ->
      advance t;
      '\t'
  | Some 'r' ->
      advance t;
      '\r'
  | Some '\\' ->
      advance t;
      '\\'
  | Some '"' ->
      advance t;
      '"'
  | Some '\'' ->
      advance t;
      '\''
  | Some '0' ->
      advance t;
      '\000'
  | Some 'x' ->
      advance t;
      let hi = peek_char t in
      let lo = peek_char_n t 1 in
      (match (hi, lo) with
      | Some h, Some l when is_hex_digit h && is_hex_digit l ->
          advance_n t 2;
          Char.chr (int_of_string ("0x" ^ String.make 1 h ^ String.make 1 l))
      | _ ->
          report t (span_from t start) "invalid hex escape \\xNN";
          '\000')
  | Some ch ->
      advance t;
      report t (span_from t start)
        (Printf.sprintf "unknown escape sequence \\%c" ch);
      ch

let read_string t =
  let start = t.pos in
  advance t;
  (* opening double-quote *)
  let buf = Buffer.create 32 in
  let rec loop () =
    match peek_char t with
    | None ->
        report t (span_from t start) "unterminated string literal"
          ~help:"add a closing \" before the end of the file";
        Buffer.contents buf
    | Some '"' ->
        advance t;
        Buffer.contents buf
    | Some '\\' ->
        advance t;
        Buffer.add_char buf (decode_escape t ~start);
        loop ()
    | Some '\n' ->
        report t (span_from t start) "newline in string literal"
          ~help:"use \\n or close the string before the newline";
        advance t;
        Buffer.add_char buf '\n';
        loop ()
    | Some ch ->
        advance t;
        Buffer.add_char buf ch;
        loop ()
  in
  let value = loop () in
  let lexeme = String.sub t.source start.offset (t.pos.offset - start.offset) in
  emit t (Token.String value) ~start ~lexeme

let read_char t =
  let start = t.pos in
  advance t;
  (* opening ' *)
  let ch =
    match peek_char t with
    | None ->
        report t (span_from t start) "unterminated character literal";
        '\000'
    | Some '\\' ->
        advance t;
        decode_escape t ~start
    | Some '\'' ->
        report t (span_from t start) "empty character literal";
        advance t;
        '\000'
    | Some c ->
        advance t;
        c
  in
  (match peek_char t with
  | Some '\'' -> advance t
  | _ ->
      report t (span_from t start) "expected closing ' for character literal");
  let lexeme = String.sub t.source start.offset (t.pos.offset - start.offset) in
  emit t (Token.Char ch) ~start ~lexeme

let parse_int64 t ~start lexeme =
  try Int64.of_string lexeme
  with Failure _ ->
    report t (span_from t start)
      (Printf.sprintf "integer literal out of range: %s" lexeme);
    0L

let read_hex_number t ~start =
  (* caller already consumed leading 0 and saw x/X *)
  advance t;
  (* x/X *)
  let digits = read_while t is_hex_digit in
  if digits = "" then
    report t (span_from t start) "expected hex digits after 0x";
  let lexeme = String.sub t.source start.offset (t.pos.offset - start.offset) in
  let value = parse_int64 t ~start lexeme in
  emit t (Token.Int value) ~start ~lexeme

let read_number t =
  let start = t.pos in
  match (peek_char t, peek_char_n t 1) with
  | Some '0', Some ('x' | 'X') -> read_hex_number t ~start
  | _ ->
      let int_part = read_while t is_digit in
      let is_float = ref false in
      (match peek_char t with
      | Some '.' when match peek_char_n t 1 with Some c -> is_digit c | None -> false
        ->
          is_float := true;
          advance t;
          ignore (read_while t is_digit)
      | _ -> ());
      (match peek_char t with
      | Some ('e' | 'E') ->
          is_float := true;
          advance t;
          (match peek_char t with
          | Some ('+' | '-') -> advance t
          | _ -> ());
          let exp = read_while t is_digit in
          if exp = "" then
            report t (span_from t start) "expected digits in float exponent"
      | _ -> ());
      let lexeme =
        String.sub t.source start.offset (t.pos.offset - start.offset)
      in
      if !is_float then
        let value =
          try float_of_string lexeme
          with Failure _ ->
            report t (span_from t start)
              (Printf.sprintf "invalid float literal: %s" lexeme);
            0.0
        in
        emit t (Token.Float value) ~start ~lexeme
      else
        let value = parse_int64 t ~start (if int_part = "" then "0" else int_part) in
        emit t (Token.Int value) ~start ~lexeme

let read_ident t =
  let start = t.pos in
  let name = read_while t is_ident_continue in
  let lexeme = name in
  if name = "_" then emit t Token.Underscore ~start ~lexeme
  else
    match Token.keyword_of_string name with
    | Some kw -> emit t (Token.Keyword kw) ~start ~lexeme
    | None ->
        let kind =
          if name <> "" && name.[0] >= 'A' && name.[0] <= 'Z' then
            Token.Ctor name
          else Token.Ident name
        in
        emit t kind ~start ~lexeme

(** Longest-match operators and punctuation that share characters. *)
let read_operator t =
  let start = t.pos in
  let two =
    match (peek_char t, peek_char_n t 1) with
    | Some '=', Some '=' -> Some (Token.Binop Token.Op_eq, 2)
    | Some '!', Some '=' -> Some (Token.Binop Token.Op_neq, 2)
    | Some '<', Some '=' -> Some (Token.Binop Token.Op_le, 2)
    | Some '>', Some '=' -> Some (Token.Binop Token.Op_ge, 2)
    | Some '&', Some '&' -> Some (Token.Binop Token.Op_and, 2)
    | Some '|', Some '|' -> Some (Token.Binop Token.Op_or, 2)
    | Some ':', Some ':' -> Some (Token.Binop Token.Op_cons, 2)
    | Some '|', Some '>' -> Some (Token.Binop Token.Op_pipe, 2)
    | Some '-', Some '>' -> Some (Token.Arrow, 2)
    | Some '=', Some '>' -> Some (Token.FatArrow, 2)
    | _ -> None
  in
  match two with
  | Some (kind, n) ->
      advance_n t n;
      let lexeme =
        String.sub t.source start.offset (t.pos.offset - start.offset)
      in
      emit t kind ~start ~lexeme
  | None -> (
      match peek_char t with
      | Some '+' ->
          advance t;
          emit t (Token.Binop Token.Op_add) ~start ~lexeme:"+"
      | Some '-' ->
          advance t;
          emit t (Token.Binop Token.Op_sub) ~start ~lexeme:"-"
      | Some '*' ->
          advance t;
          emit t (Token.Binop Token.Op_mul) ~start ~lexeme:"*"
      | Some '/' ->
          advance t;
          emit t (Token.Binop Token.Op_div) ~start ~lexeme:"/"
      | Some '%' ->
          advance t;
          emit t (Token.Binop Token.Op_mod) ~start ~lexeme:"%"
      | Some '<' ->
          advance t;
          emit t (Token.Binop Token.Op_lt) ~start ~lexeme:"<"
      | Some '>' ->
          advance t;
          emit t (Token.Binop Token.Op_gt) ~start ~lexeme:">"
      | Some '=' ->
          advance t;
          emit t Token.Equal ~start ~lexeme:"="
      | Some '|' ->
          advance t;
          emit t Token.Pipe ~start ~lexeme:"|"
      | Some ':' ->
          advance t;
          emit t Token.Colon ~start ~lexeme:":"
      | Some ch ->
          advance t;
          report t (span_from t start)
            (Printf.sprintf "unexpected character %C" ch);
          (* skip and emit a benign token so recovery can continue *)
          emit t Token.Underscore ~start ~lexeme:(String.make 1 ch)
      | None -> emit t Token.Eof ~start ~lexeme:"")

let next_token t =
  skip_trivia t;
  let start = t.pos in
  match peek_char t with
  | None -> emit t Token.Eof ~start ~lexeme:""
  | Some '(' ->
      advance t;
      emit t Token.LParen ~start ~lexeme:"("
  | Some ')' ->
      advance t;
      emit t Token.RParen ~start ~lexeme:")"
  | Some '[' ->
      advance t;
      emit t Token.LBracket ~start ~lexeme:"["
  | Some ']' ->
      advance t;
      emit t Token.RBracket ~start ~lexeme:"]"
  | Some '{' ->
      advance t;
      emit t Token.LBrace ~start ~lexeme:"{"
  | Some '}' ->
      advance t;
      emit t Token.RBrace ~start ~lexeme:"}"
  | Some ',' ->
      advance t;
      emit t Token.Comma ~start ~lexeme:","
  | Some '.' ->
      advance t;
      emit t Token.Dot ~start ~lexeme:"."
  | Some ';' ->
      advance t;
      emit t Token.Semicolon ~start ~lexeme:";"
  | Some '"' -> read_string t
  | Some '\'' ->
      (* Distinguish char literals from type variables like 'a.
         Char: 'x' or '\n'. Type-var style starts with ' then ident chars
         without a closing quote as second character — treat as Ident "'a". *)
      (match (peek_char_n t 1, peek_char_n t 2) with
      | Some '\\', _ -> read_char t
      | Some ch, Some '\'' when ch <> '\'' -> read_char t
      | Some ch, _ when is_ident_start ch || is_digit ch ->
          (* type variable: 'a, 'foo *)
          let start = t.pos in
          advance t;
          (* ' *)
          let name = "'" ^ read_while t is_ident_continue in
          emit t (Token.Ident name) ~start ~lexeme:name
      | Some '\'', _ ->
          report t (span_from t start) "empty character literal";
          advance_n t 2;
          emit t (Token.Char '\000') ~start ~lexeme:"''"
      | _ -> read_char t)
  | Some ch when is_digit ch -> read_number t
  | Some ch when is_ident_start ch -> read_ident t
  | Some ch when is_op_char ch -> read_operator t
  | Some ch ->
      advance t;
      report t (span_from t start)
        (Printf.sprintf "unexpected character %C" ch);
      emit t Token.Underscore ~start ~lexeme:(String.make 1 ch)

let next t =
  match t.peeked with
  | Some tok ->
      t.peeked <- None;
      tok
  | None -> next_token t

let peek t =
  match t.peeked with
  | Some tok -> tok
  | None ->
      let tok = next_token t in
      t.peeked <- Some tok;
      tok

let tokenize_allowing_errors ~filename ~source () =
  let lex = create ~filename ~source in
  let acc = ref [] in
  let rec loop () =
    let tok = next lex in
    acc := tok :: !acc;
    if not (Token.is_eof tok) then loop ()
  in
  loop ();
  (List.rev !acc, diagnostics lex)

let tokenize ~filename ~source () =
  let tokens, diags = tokenize_allowing_errors ~filename ~source () in
  if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
    Error diags
  else Ok tokens

(** Convenience for callers that prefer labeled [?file]. *)
let tokenize_list ?(file = "<input>") source =
  tokenize ~filename:file ~source ()

let tokenize_exn ?(file = "<input>") source =
  match tokenize ~filename:file ~source () with
  | Ok toks -> toks
  | Error diags ->
      (match diags with
      | d :: _ ->
          failwith
            (Printf.sprintf "%s: %s" (Span.to_string d.Diagnostic.span)
               d.Diagnostic.message)
      | [] -> failwith "lex error")
