(** Hand-written lexer producing spanned tokens. *)

type error = {
  message : string;
  span : Span.t;
}

exception Error of error

type t = {
  file : string;
  source : string;
  mutable pos : Span.pos;
  mutable peeked : Token.t option;
}

let create ?(file = "<input>") source =
  { file; source; pos = { line = 1; col = 1; offset = 0 }; peeked = None }

let file t = t.file
let source t = t.source
let position t = t.pos

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

let match_char t ch =
  match peek_char t with
  | Some c when c = ch ->
      advance t;
      true
  | _ -> false

let fail t msg =
  let span = Span.make ~file:t.file ~start:t.pos ~end_:t.pos in
  raise (Error { message = msg; span })

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

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
  (* consume opening '*' already seen after '/' *)
  let depth = ref 1 in
  while !depth > 0 do
    match peek_char t with
    | None ->
        let span = Span.make ~file:t.file ~start ~end_:t.pos in
        raise (Error { message = "unterminated block comment"; span })
    | Some '/' when peek_char_n t 1 = Some '*' ->
        advance t;
        advance t;
        incr depth
    | Some '*' when peek_char_n t 1 = Some '/' ->
        advance t;
        advance t;
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
        advance t;
        advance t;
        skip_line_comment t;
        loop ()
    | Some '/' when peek_char_n t 1 = Some '*' ->
        advance t;
        advance t;
        skip_block_comment t;
        loop ()
    | _ -> ()
  in
  loop ()

let emit t kind ~start ~lexeme =
  let span = Span.make ~file:t.file ~start ~end_:t.pos in
  Token.make kind ~span ~lexeme

let read_while t pred =
  let start = t.pos.offset in
  while
    match peek_char t with
    | Some ch when pred ch ->
        advance t;
        true
    | _ -> false
  do
    ()
  done;
  String.sub t.source start (t.pos.offset - start)

let decode_escape t =
  match peek_char t with
  | None -> fail t "unterminated escape sequence"
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
  | Some ch ->
      advance t;
      ch

let read_string t =
  let start = t.pos in
  advance t;
  (* opening quote *)
  let buf = Buffer.create 32 in
  let rec loop () =
    match peek_char t with
    | None ->
        let span = Span.make ~file:t.file ~start ~end_:t.pos in
        raise (Error { message = "unterminated string literal"; span })
    | Some '"' ->
        advance t;
        Buffer.contents buf
    | Some '\\' ->
        advance t;
        Buffer.add_char buf (decode_escape t);
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
  let ch =
    match peek_char t with
    | None -> fail t "unterminated character literal"
    | Some '\\' ->
        advance t;
        decode_escape t
    | Some c ->
        advance t;
        c
  in
  (match peek_char t with
  | Some '\'' -> advance t
  | _ -> fail t "expected closing quote for character literal");
  let lexeme = String.sub t.source start.offset (t.pos.offset - start.offset) in
  emit t (Token.Char ch) ~start ~lexeme

let read_number t =
  let start = t.pos in
  let int_part = read_while t is_digit in
  match peek_char t with
  | Some '.' when match peek_char_n t 1 with Some c -> is_digit c | None -> false
    ->
      advance t;
      let frac = read_while t is_digit in
      let lexeme =
        String.sub t.source start.offset (t.pos.offset - start.offset)
      in
      let value = float_of_string (int_part ^ "." ^ frac) in
      emit t (Token.Float value) ~start ~lexeme
  | Some ('e' | 'E') ->
      advance t;
      (match peek_char t with
      | Some ('+' | '-') -> advance t
      | _ -> ());
      ignore (read_while t is_digit);
      let lexeme =
        String.sub t.source start.offset (t.pos.offset - start.offset)
      in
      let value = float_of_string lexeme in
      emit t (Token.Float value) ~start ~lexeme
  | _ ->
      let lexeme = int_part in
      let value =
        try Int64.of_string lexeme
        with Failure _ -> fail t ("integer literal out of range: " ^ lexeme)
      in
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
          if name <> "" && (name.[0] >= 'A' && name.[0] <= 'Z') then
            Token.Ctor name
          else Token.Ident name
        in
        emit t kind ~start ~lexeme

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
      for _ = 1 to n do
        advance t
      done;
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
      | Some ch -> fail t (Printf.sprintf "unexpected character %C" ch)
      | None -> fail t "unexpected end of input")

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
  | Some '\'' -> read_char t
  | Some ch when is_digit ch -> read_number t
  | Some ch when is_ident_start ch -> read_ident t
  | Some _ -> read_operator t

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

let tokenize ?(file = "<input>") source =
  try
    let lex = create ~file source in
    let acc = ref [] in
    let rec loop () =
      let tok = next lex in
      acc := tok :: !acc;
      if not (Token.is_eof tok) then loop ()
    in
    loop ();
    Ok (Array.of_list (List.rev !acc))
  with Error e -> Error e

let tokenize_exn ?(file = "<input>") source =
  match tokenize ~file source with
  | Ok toks -> toks
  | Error e -> raise (Error e)
