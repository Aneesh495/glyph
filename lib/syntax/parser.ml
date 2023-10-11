(** Recursive-descent + Pratt parser for Glyph. *)

type t = {
  tokens : Token.t array;
  mutable index : int;
  diagnostics : Diagnostic.t list ref;
  filename : string;
}

let create ~filename tokens =
  let tokens =
    match tokens with
    | [] -> [| Token.make Token.Eof Span.dummy "" |]
    | xs -> Array.of_list xs
  in
  { tokens; index = 0; diagnostics = ref []; filename }

let add_error p span msg =
  p.diagnostics := Diagnostic.error span msg :: !(p.diagnostics)

let at_end p = p.index >= Array.length p.tokens

let peek p =
  if at_end p then
    let last = p.tokens.(Array.length p.tokens - 1) in
    Token.make Token.Eof last.span ""
  else p.tokens.(p.index)

let peek_kind p = (peek p).kind

let previous p =
  if p.index <= 0 then peek p else p.tokens.(p.index - 1)

let advance p =
  let tok = peek p in
  if not (at_end p) && tok.kind <> Token.Eof then p.index <- p.index + 1;
  tok

let check p kind =
  match peek_kind p, kind with
  | Token.Keyword k1, Token.Keyword k2 -> k1 = k2
  | Token.Punct p1, Token.Punct p2 -> p1 = p2
  | Token.Eof, Token.Eof -> true
  | a, b -> a = b

let check_punct p punct = check p (Token.Punct punct)
let check_kw p kw = check p (Token.Keyword kw)

let match_kind p kind =
  if check p kind then (
    ignore (advance p);
    true)
  else false

let match_punct p punct = match_kind p (Token.Punct punct)
let match_kw p kw = match_kind p (Token.Keyword kw)

let expect_punct p punct msg =
  if match_punct p punct then previous p
  else
    let tok = peek p in
    add_error p tok.span msg;
    tok

let expect_kw p kw msg =
  if match_kw p kw then previous p
  else
    let tok = peek p in
    add_error p tok.span msg;
    tok

let span_merge a b = Span.merge a b

(* ---- operator precedence (Pratt) ---- *)

type assoc = Left | Right

let infix_info = function
  | Token.Punct Token.PipePipe -> Some (10, Left, Ast.Or)
  | Token.Punct Token.AmpAmp -> Some (20, Left, Ast.And)
  | Token.Punct Token.Eq -> Some (30, Left, Ast.Eq)
  | Token.Punct Token.Neq -> Some (30, Left, Ast.Neq)
  | Token.Punct Token.Lt -> Some (30, Left, Ast.Lt)
  | Token.Punct Token.Le -> Some (30, Left, Ast.Le)
  | Token.Punct Token.Gt -> Some (30, Left, Ast.Gt)
  | Token.Punct Token.Ge -> Some (30, Left, Ast.Ge)
  | Token.Punct Token.ColonColon -> Some (40, Right, Ast.Cons)
  | Token.Punct Token.AtAt -> Some (40, Right, Ast.Append)
  | Token.Punct Token.Plus -> Some (50, Left, Ast.Add)
  | Token.Punct Token.Minus -> Some (50, Left, Ast.Sub)
  | Token.Punct Token.Star -> Some (60, Left, Ast.Mul)
  | Token.Punct Token.Slash -> Some (60, Left, Ast.Div)
  | Token.Punct Token.Percent -> Some (60, Left, Ast.Mod)
  | Token.Operator "|>" -> Some (5, Left, Ast.Pipe)
  | Token.Operator ">>" -> Some (5, Left, Ast.Compose)
  | Token.Operator "@@" -> Some (5, Right, Ast.Apply)
  | Token.Operator "@" -> Some (40, Right, Ast.Append)
  | Token.Operator "::" -> Some (40, Right, Ast.Cons)
  | Token.Operator "+" -> Some (50, Left, Ast.Add)
  | Token.Operator "-" -> Some (50, Left, Ast.Sub)
  | Token.Operator "*" -> Some (60, Left, Ast.Mul)
  | Token.Operator "/" -> Some (60, Left, Ast.Div)
  | Token.Operator "%" -> Some (60, Left, Ast.Mod)
  | Token.Operator "=" -> Some (30, Left, Ast.Eq)
  | Token.Operator "<>" -> Some (30, Left, Ast.Neq)
  | Token.Operator "<" -> Some (30, Left, Ast.Lt)
  | Token.Operator "<=" -> Some (30, Left, Ast.Le)
  | Token.Operator ">" -> Some (30, Left, Ast.Gt)
  | Token.Operator ">=" -> Some (30, Left, Ast.Ge)
  | Token.Operator "&&" -> Some (20, Left, Ast.And)
  | Token.Operator "||" -> Some (10, Left, Ast.Or)
  | _ -> None

let is_expr_start = function
  | Token.Keyword (Kw_let | Kw_if | Kw_match | Kw_fun | Kw_fn | Kw_true | Kw_false)
  | Token.Lit_int _ | Token.Lit_float _ | Token.Lit_string _ | Token.Lit_char _
  | Token.Ident _ | Token.UpperIdent _
  | Token.Punct (LParen | LBracket | LBrace | Bang | Minus | Tilde) ->
      true
  | _ -> false

let parse_int_lit raw =
  try
    if String.length raw > 2 && (raw.[1] = 'x' || raw.[1] = 'X') then
      Int64.of_string raw
    else Int64.of_string raw
  with _ -> 0L

let parse_float_lit raw = try float_of_string raw with _ -> 0.0

(* Forward decls via rec *)
let rec parse_type_expr p = parse_type_arrow p

and parse_type_arrow p =
  let left = parse_type_app p in
  if match_punct p Token.Arrow then
    let right = parse_type_arrow p in
    Ast.tarrow left right (span_merge left.typ_span right.typ_span)
  else left

and parse_type_app p =
  let head = parse_type_atom p in
  let rec loop acc =
    match peek_kind p with
    | Token.Ident _ | Token.UpperIdent _
    | Token.Punct Token.LParen | Token.Punct Token.LBracket ->
        let arg = parse_type_atom p in
        loop (arg :: acc)
    | _ -> List.rev acc
  in
  match loop [] with
  | [] -> head
  | args ->
      let span =
        span_merge head.typ_span (List.hd (List.rev args)).typ_span
      in
      Ast.tapp head args span

and parse_type_atom p =
  let tok = peek p in
  match tok.kind with
  | Token.Ident name ->
      ignore (advance p);
      Ast.tvar (Ident.Intern.intern name) tok.span
  | Token.UpperIdent name ->
      ignore (advance p);
      Ast.tcon (Ident.Intern.intern name) tok.span
  | Token.Punct Token.LParen ->
      ignore (advance p);
      if match_punct p Token.RParen then
        Ast.tcon (Ident.Intern.intern "Unit") tok.span
      else
        let first = parse_type_expr p in
        if match_punct p Token.Comma then (
          let rest = parse_type_list_until_rparen p in
          let span = span_merge tok.span (previous p).span in
          Ast.ttuple (first :: rest) span)
        else (
          ignore (expect_punct p Token.RParen "expected ')' after type");
          let span = span_merge tok.span (previous p).span in
          Ast.typ (Ast.Typ_paren first) span)
  | Token.Punct Token.LBracket ->
      ignore (advance p);
      let inner = parse_type_expr p in
      ignore (expect_punct p Token.RBracket "expected ']' after array type");
      Ast.tarray inner (span_merge tok.span (previous p).span)
  | Token.Punct Token.LBrace ->
      ignore (advance p);
      let fields = parse_record_type_fields p in
      ignore (expect_punct p Token.RBrace "expected '}' after record type");
      Ast.typ
        (Ast.Typ_record fields)
        (span_merge tok.span (previous p).span)
  | _ ->
      add_error p tok.span "expected type";
      ignore (advance p);
      Ast.tvar (Ident.fresh "?") tok.span

and parse_type_list_until_rparen p =
  let rec loop acc =
    let t = parse_type_expr p in
    let acc = t :: acc in
    if match_punct p Token.Comma then loop acc
    else (
      ignore (expect_punct p Token.RParen "expected ')' in tuple type");
      List.rev acc)
  in
  loop []

and parse_record_type_fields p =
  if check_punct p Token.RBrace then []
  else
    let rec loop acc =
      let mut = match_kw p Token.Kw_mutable in
      let name_tok = peek p in
      let name =
        match name_tok.kind with
        | Token.Ident n ->
            ignore (advance p);
            Ident.Intern.intern n
        | _ ->
            add_error p name_tok.span "expected field name";
            Ident.fresh "_"
      in
      ignore (expect_punct p Token.Colon "expected ':' in record field");
      let ty = parse_type_expr p in
      let acc = (name, ty, mut) :: acc in
      if match_punct p Token.Comma || match_punct p Token.Semicolon then
        if check_punct p Token.RBrace then List.rev acc else loop acc
      else List.rev acc
    in
    loop []

(* ---- patterns ---- *)

and parse_pattern p = parse_pattern_or p

and parse_pattern_or p =
  let left = parse_pattern_as p in
  if match_punct p Token.Pipe then
    let right = parse_pattern_or p in
    Ast.por left right (span_merge left.pat_span right.pat_span)
  else left

and parse_pattern_as p =
  let left = parse_pattern_cons p in
  if match_kw p Token.Kw_as then
    let name_tok = peek p in
    let name =
      match name_tok.kind with
      | Token.Ident n ->
          ignore (advance p);
          Ident.Intern.intern n
      | _ ->
          add_error p name_tok.span "expected name after 'as'";
          Ident.fresh "_"
    in
    Ast.pas left name (span_merge left.pat_span name_tok.span)
  else left

and parse_pattern_cons p =
  let left = parse_pattern_annot p in
  if match_punct p Token.ColonColon || match_kind p (Token.Operator "::") then
    let right = parse_pattern_cons p in
    Ast.pcons left right (span_merge left.pat_span right.pat_span)
  else left

and parse_pattern_annot p =
  let left = parse_pattern_atom p in
  if match_punct p Token.Colon then
    let ty = parse_type_expr p in
    Ast.pann left ty (span_merge left.pat_span ty.typ_span)
  else left

and parse_pattern_atom p =
  let tok = peek p in
  match tok.kind with
  | Token.Punct Token.Underscore ->
      ignore (advance p);
      Ast.pwildcard tok.span
  | Token.Ident name ->
      ignore (advance p);
      Ast.pvar (Ident.Intern.intern name) tok.span
  | Token.UpperIdent name ->
      ignore (advance p);
      let ctor = Ident.Intern.intern name in
      let args = parse_pattern_ctor_args p in
      let span =
        match args with
        | [] -> tok.span
        | xs -> span_merge tok.span (List.hd (List.rev xs)).pat_span
      in
      Ast.pconstruct ctor args span
  | Token.Lit_int raw ->
      ignore (advance p);
      Ast.plit (Ast.Lit_int (parse_int_lit raw)) tok.span
  | Token.Lit_float raw ->
      ignore (advance p);
      Ast.plit (Ast.Lit_float (parse_float_lit raw)) tok.span
  | Token.Lit_string s ->
      ignore (advance p);
      Ast.plit (Ast.Lit_string s) tok.span
  | Token.Lit_char c ->
      ignore (advance p);
      Ast.plit (Ast.Lit_char c) tok.span
  | Token.Keyword Kw_true ->
      ignore (advance p);
      Ast.plit (Ast.Lit_bool true) tok.span
  | Token.Keyword Kw_false ->
      ignore (advance p);
      Ast.plit (Ast.Lit_bool false) tok.span
  | Token.Punct Token.LParen ->
      ignore (advance p);
      if match_punct p Token.RParen then Ast.plit Ast.Lit_unit tok.span
      else
        let first = parse_pattern p in
        if match_punct p Token.Comma then (
          let rest = parse_pattern_tuple_rest p in
          Ast.ptuple (first :: rest) (span_merge tok.span (previous p).span))
        else (
          ignore (expect_punct p Token.RParen "expected ')' after pattern");
          first)
  | Token.Punct Token.LBracket ->
      ignore (advance p);
      if match_punct p Token.RBracket then Ast.plist [] tok.span
      else
        let items = parse_pattern_list_items p in
        ignore (expect_punct p Token.RBracket "expected ']' after list pattern");
        Ast.plist items (span_merge tok.span (previous p).span)
  | Token.Punct Token.LBrace ->
      ignore (advance p);
      let fields, open_ = parse_record_pattern_fields p in
      ignore (expect_punct p Token.RBrace "expected '}' after record pattern");
      Ast.pat
        (Ast.Pat_record (fields, open_))
        (span_merge tok.span (previous p).span)
  | _ ->
      add_error p tok.span "expected pattern";
      ignore (advance p);
      Ast.pwildcard tok.span

and parse_pattern_ctor_args p =
  match peek_kind p with
  | Token.Punct Token.LParen ->
      ignore (advance p);
      if match_punct p Token.RParen then []
      else
        let first = parse_pattern p in
        let rec loop acc =
          if match_punct p Token.Comma then
            loop (parse_pattern p :: acc)
          else (
            ignore (expect_punct p Token.RParen "expected ')' after constructor args");
            List.rev acc)
        in
        loop [ first ]
  | k when is_pattern_atom_start k -> [ parse_pattern_atom p ]
  | _ -> []

and is_pattern_atom_start = function
  | Token.Ident _ | Token.UpperIdent _ | Token.Lit_int _ | Token.Lit_float _
  | Token.Lit_string _ | Token.Lit_char _
  | Token.Keyword (Kw_true | Kw_false)
  | Token.Punct (LParen | LBracket | LBrace | Underscore) ->
      true
  | _ -> false

and parse_pattern_tuple_rest p =
  let rec loop acc =
    let pat = parse_pattern p in
    let acc = pat :: acc in
    if match_punct p Token.Comma then loop acc
    else (
      ignore (expect_punct p Token.RParen "expected ')' in tuple pattern");
      List.rev acc)
  in
  loop []

and parse_pattern_list_items p =
  let rec loop acc =
    let pat = parse_pattern p in
    let acc = pat :: acc in
    if match_punct p Token.Semicolon || match_punct p Token.Comma then
      if check_punct p Token.RBracket then List.rev acc else loop acc
    else List.rev acc
  in
  loop []

and parse_record_pattern_fields p =
  if check_punct p Token.RBrace then ([], false)
  else
    let rec loop acc =
      if match_punct p Token.DotDot then (List.rev acc, true)
      else
        let name_tok = peek p in
        let name =
          match name_tok.kind with
          | Token.Ident n ->
              ignore (advance p);
              Ident.Intern.intern n
          | _ ->
              add_error p name_tok.span "expected field name";
              Ident.fresh "_"
        in
        let pat_opt =
          if match_punct p Token.Eq then Some (parse_pattern p) else None
        in
        let acc = (name, pat_opt) :: acc in
        if match_punct p Token.Comma || match_punct p Token.Semicolon then
          if check_punct p Token.RBrace then (List.rev acc, false)
          else loop acc
        else (List.rev acc, false)
    in
    loop []

(* ---- expressions ---- *)

and parse_expr p = parse_pratt p 0

and parse_pratt p min_prec =
  let left = parse_unary p in
  let left = parse_application p left in
  parse_infix_loop p left min_prec

and parse_infix_loop p left min_prec =
  match infix_info (peek_kind p) with
  | Some (prec, assoc, op) when prec >= min_prec ->
      ignore (advance p);
      let next_min =
        match assoc with Left -> prec + 1 | Right -> prec
      in
      let right = parse_pratt p next_min in
      let node =
        match op with
        | Ast.Cons ->
            Ast.cons left right (span_merge left.exp_span right.exp_span)
        | _ ->
            Ast.binop op left right (span_merge left.exp_span right.exp_span)
      in
      parse_infix_loop p node min_prec
  | _ ->
      if match_punct p Token.Semicolon then
        let right = parse_pratt p 0 in
        let node = Ast.seq left right (span_merge left.exp_span right.exp_span) in
        parse_infix_loop p node min_prec
      else left

and parse_application p left =
  let rec loop acc =
    if is_arg_start (peek_kind p) then
      let arg = parse_unary p in
      loop (arg :: acc)
    else List.rev acc
  in
  match loop [] with
  | [] -> left
  | args ->
      let span = span_merge left.exp_span (List.hd (List.rev args)).exp_span in
      Ast.app left args span

and is_arg_start = function
  | Token.Keyword (Kw_true | Kw_false)
  | Token.Lit_int _ | Token.Lit_float _ | Token.Lit_string _ | Token.Lit_char _
  | Token.Ident _ | Token.UpperIdent _
  | Token.Punct (LParen | LBracket | LBrace) ->
      true
  | _ -> false

and parse_unary p =
  let tok = peek p in
  match tok.kind with
  | Token.Punct Token.Minus | Token.Operator "-" ->
      ignore (advance p);
      let e = parse_unary p in
      Ast.unop Ast.Neg e (span_merge tok.span e.exp_span)
  | Token.Punct Token.Bang | Token.Operator "!" ->
      ignore (advance p);
      let e = parse_unary p in
      Ast.unop Ast.Deref e (span_merge tok.span e.exp_span)
  | Token.Ident "not" ->
      ignore (advance p);
      let e = parse_unary p in
      Ast.unop Ast.Not e (span_merge tok.span e.exp_span)
  | Token.Keyword Kw_let -> parse_let_expr p
  | Token.Keyword Kw_if -> parse_if_expr p
  | Token.Keyword Kw_match -> parse_match_expr p
  | Token.Keyword (Kw_fun | Kw_fn) -> parse_fun_expr p
  | _ -> parse_postfix (parse_primary p)

and parse_postfix e =
  (* field / index handled in primary for simplicity *)
  e

and parse_primary p =
  let tok = peek p in
  match tok.kind with
  | Token.Lit_int raw ->
      ignore (advance p);
      Ast.lit (Ast.Lit_int (parse_int_lit raw)) tok.span
  | Token.Lit_float raw ->
      ignore (advance p);
      Ast.lit (Ast.Lit_float (parse_float_lit raw)) tok.span
  | Token.Lit_string s ->
      ignore (advance p);
      Ast.lit (Ast.Lit_string s) tok.span
  | Token.Lit_char c ->
      ignore (advance p);
      Ast.lit (Ast.Lit_char c) tok.span
  | Token.Keyword Kw_true ->
      ignore (advance p);
      Ast.lit (Ast.Lit_bool true) tok.span
  | Token.Keyword Kw_false ->
      ignore (advance p);
      Ast.lit (Ast.Lit_bool false) tok.span
  | Token.Ident name ->
      ignore (advance p);
      let e = Ast.var (Ident.Intern.intern name) tok.span in
      parse_field_index p e
  | Token.UpperIdent name ->
      ignore (advance p);
      let ctor = Ident.Intern.intern name in
      let args =
        if is_arg_start (peek_kind p) || check_punct p Token.LParen then
          match peek_kind p with
          | Token.Punct Token.LParen ->
              ignore (advance p);
              if match_punct p Token.RParen then []
              else
                let first = parse_expr p in
                let rec loop acc =
                  if match_punct p Token.Comma then
                    loop (parse_expr p :: acc)
                  else (
                    ignore
                      (expect_punct p Token.RParen
                         "expected ')' after constructor args");
                    List.rev acc)
                in
                loop [ first ]
          | _ -> [ parse_unary p ]
        else []
      in
      let span =
        match args with
        | [] -> tok.span
        | xs -> span_merge tok.span (List.hd (List.rev xs)).exp_span
      in
      Ast.construct ctor args span
  | Token.Punct Token.LParen ->
      ignore (advance p);
      if match_punct p Token.RParen then Ast.unit tok.span
      else
        let first = parse_expr p in
        if match_punct p Token.Comma then (
          let rest =
            let rec loop acc =
              let e = parse_expr p in
              let acc = e :: acc in
              if match_punct p Token.Comma then loop acc
              else (
                ignore (expect_punct p Token.RParen "expected ')' in tuple");
                List.rev acc)
            in
            loop []
          in
          Ast.tuple (first :: rest) (span_merge tok.span (previous p).span))
        else (
          ignore (expect_punct p Token.RParen "expected ')'");
          parse_field_index p first)
  | Token.Punct Token.LBracket ->
      ignore (advance p);
      if match_punct p Token.RBracket then Ast.list [] tok.span
      else
        let items =
          let rec loop acc =
            let e = parse_expr p in
            let acc = e :: acc in
            if match_punct p Token.Semicolon || match_punct p Token.Comma then
              if check_punct p Token.RBracket then List.rev acc else loop acc
            else List.rev acc
          in
          loop []
        in
        ignore (expect_punct p Token.RBracket "expected ']'");
        Ast.list items (span_merge tok.span (previous p).span)
  | Token.Punct Token.LBrace ->
      ignore (advance p);
      let fields = parse_record_expr_fields p in
      ignore (expect_punct p Token.RBrace "expected '}'");
      Ast.record fields (span_merge tok.span (previous p).span)
  | _ ->
      add_error p tok.span "expected expression";
      ignore (advance p);
      Ast.unit tok.span

and parse_field_index p e =
  let rec loop e =
    if match_punct p Token.Dot then
      let tok = peek p in
      match tok.kind with
      | Token.Ident name ->
          ignore (advance p);
          loop
            (Ast.field e (Ident.Intern.intern name)
               (span_merge e.exp_span tok.span))
      | Token.Lit_int raw ->
          ignore (advance p);
          let idx =
            Ast.lit (Ast.Lit_int (parse_int_lit raw)) tok.span
          in
          loop (Ast.index e idx (span_merge e.exp_span tok.span))
      | _ ->
          add_error p tok.span "expected field name after '.'";
          e
    else if match_punct p Token.LBracket then
      let idx = parse_expr p in
      ignore (expect_punct p Token.RBracket "expected ']' after index");
      loop (Ast.index e idx (span_merge e.exp_span (previous p).span))
    else if match_punct p Token.Colon then
      let ty = parse_type_expr p in
      Ast.annotated e ty (span_merge e.exp_span ty.typ_span)
    else e
  in
  loop e

and parse_record_expr_fields p =
  if check_punct p Token.RBrace then []
  else
    let rec loop acc =
      let name_tok = peek p in
      let name =
        match name_tok.kind with
        | Token.Ident n ->
            ignore (advance p);
            Ident.Intern.intern n
        | _ ->
            add_error p name_tok.span "expected field name";
            Ident.fresh "_"
      in
      ignore (expect_punct p Token.Eq "expected '=' in record field");
      let e = parse_expr p in
      let acc = (name, e) :: acc in
      if match_punct p Token.Comma || match_punct p Token.Semicolon then
        if check_punct p Token.RBrace then List.rev acc else loop acc
      else List.rev acc
    in
    loop []

and parse_let_expr p =
  let start = peek p in
  ignore (expect_kw p Token.Kw_let "expected let");
  let is_rec = match_kw p Token.Kw_rec in
  let vbs = parse_value_bindings p ~is_rec in
  ignore (expect_kw p Token.Kw_in "expected 'in' after let binding");
  let body = parse_expr p in
  let span = span_merge start.span body.exp_span in
  if is_rec then Ast.letrec vbs body span else Ast.let_ vbs body span

and parse_value_bindings p ~is_rec =
  let rec loop acc =
    let vb = parse_value_binding p ~is_rec in
    let acc = vb :: acc in
    if match_kw p Token.Kw_and then loop acc else List.rev acc
  in
  loop []

and parse_value_binding p ~is_rec =
  let start = peek p in
  let pat = parse_pattern_atom p in
  let params =
    let rec loop acc =
      if is_pattern_atom_start (peek_kind p) then
        loop (parse_pattern_atom p :: acc)
      else List.rev acc
    in
    loop []
  in
  ignore (expect_punct p Token.Eq "expected '=' in let binding");
  let expr = parse_expr p in
  Ast.value_binding ~is_rec ~params pat expr
    (span_merge start.span expr.exp_span)

and parse_if_expr p =
  let start = peek p in
  ignore (expect_kw p Token.Kw_if "expected if");
  let cond = parse_expr p in
  ignore (expect_kw p Token.Kw_then "expected 'then'");
  let then_ = parse_expr p in
  let else_ =
    if match_kw p Token.Kw_else then Some (parse_expr p) else None
  in
  let span =
    match else_ with
    | Some e -> span_merge start.span e.exp_span
    | None -> span_merge start.span then_.exp_span
  in
  Ast.if_ cond then_ else_ span

and parse_match_expr p =
  let start = peek p in
  ignore (expect_kw p Token.Kw_match "expected match");
  let scrut = parse_expr p in
  ignore (expect_kw p Token.Kw_with "expected 'with'");
  ignore (match_punct p Token.Pipe);
  let cases =
    let rec loop acc =
      let c = parse_case p in
      let acc = c :: acc in
      if match_punct p Token.Pipe then loop acc else List.rev acc
    in
    loop []
  in
  let span =
    match cases with
    | [] -> start.span
    | xs -> span_merge start.span (List.hd (List.rev xs)).case_span
  in
  Ast.match_ scrut cases span

and parse_case p =
  let start = peek p in
  let pat = parse_pattern p in
  let guard =
    if match_kw p Token.Kw_when then Some (parse_expr p) else None
  in
  if not (match_punct p Token.Arrow || match_kind p (Token.Operator "->"))
  then add_error p (peek p).span "expected '->' in match case";
  let body = parse_expr p in
  Ast.case ~guard pat body (span_merge start.span body.exp_span)

and parse_fun_expr p =
  let start = peek p in
  ignore (advance p);
  (* fun | fn *)
  let params =
    let rec loop acc =
      if is_pattern_atom_start (peek_kind p) then
        loop (parse_pattern_atom p :: acc)
      else List.rev acc
    in
    loop []
  in
  if not (match_punct p Token.Arrow || match_kind p (Token.Operator "->"))
  then add_error p (peek p).span "expected '->' after fun parameters";
  let body = parse_expr p in
  Ast.abs params body (span_merge start.span body.exp_span)

(* ---- toplevel ---- *)

and parse_type_decl p =
  let start = peek p in
  ignore (expect_kw p Token.Kw_type "expected type");
  let params =
    let rec loop acc =
      match peek_kind p with
      | Token.Ident n ->
          ignore (advance p);
          loop (Ident.Intern.intern n :: acc)
      | _ -> List.rev acc
    in
    loop []
  in
  let name_tok = peek p in
  let name =
    match name_tok.kind with
    | Token.UpperIdent n | Token.Ident n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        add_error p name_tok.span "expected type name";
        Ident.fresh "T"
  in
  (* allow `type List a` style where params came after — also `type a List` *)
  let params, name =
    match params with
    | [] ->
        let more =
          let rec loop acc =
            match peek_kind p with
            | Token.Ident n ->
                ignore (advance p);
                loop (Ident.Intern.intern n :: acc)
            | _ -> List.rev acc
          in
          loop []
        in
        (more, name)
    | ps -> (ps, name)
  in
  ignore (expect_punct p Token.Eq "expected '=' in type declaration");
  let kind =
    if check_punct p Token.LBrace then (
      ignore (advance p);
      let fields = parse_record_type_fields p in
      ignore (expect_punct p Token.RBrace "expected '}'");
      Ast.Type_record fields)
    else if check_punct p Token.Pipe || match peek_kind p with Token.UpperIdent _ -> true | _ -> false
    then (
      ignore (match_punct p Token.Pipe);
      let ctors =
        let rec loop acc =
          let c = parse_constructor_decl p in
          let acc = c :: acc in
          if match_punct p Token.Pipe then loop acc else List.rev acc
        in
        loop []
      in
      Ast.Type_variant ctors)
    else Ast.Type_abbrev (parse_type_expr p)
  in
  let decl =
    Ast.type_decl name params kind (span_merge start.span (previous p).span)
  in
  let rest =
    if match_kw p Token.Kw_and then
      (* mutual: `and Name = ...` simplified — parse another type_decl without type kw *)
      []
    else []
  in
  decl :: rest

and parse_constructor_decl p =
  let tok = peek p in
  let name =
    match tok.kind with
    | Token.UpperIdent n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        add_error p tok.span "expected constructor name";
        Ident.fresh "C"
  in
  let args =
    if match_kw p Token.Kw_of then
      let first = parse_type_expr p in
      let rec loop acc =
        if match_punct p Token.Star then loop (parse_type_expr p :: acc)
        else List.rev acc
      in
      loop [ first ]
    else if is_type_atom_start (peek_kind p) then [ parse_type_atom p ]
    else []
  in
  Ast.constructor_decl name args (span_merge tok.span (previous p).span)

and is_type_atom_start = function
  | Token.Ident _ | Token.UpperIdent _
  | Token.Punct (LParen | LBracket | LBrace) ->
      true
  | _ -> false

let parse_toplevel p =
  let tok = peek p in
  match tok.kind with
  | Token.Keyword Kw_type -> Ast.Top_type (parse_type_decl p)
  | Token.Keyword Kw_let ->
      ignore (advance p);
      let is_rec = match_kw p Token.Kw_rec in
      let vbs = parse_value_bindings p ~is_rec in
      if is_rec then Ast.Top_letrec vbs else Ast.Top_let vbs
  | Token.Keyword Kw_open ->
      ignore (advance p);
      let rec path acc =
        match peek_kind p with
        | Token.UpperIdent n | Token.Ident n ->
            ignore (advance p);
            let acc = Ident.Intern.intern n :: acc in
            if match_punct p Token.Dot then path acc else List.rev acc
        | _ -> List.rev acc
      in
      let ids = path [] in
      Ast.Top_open (ids, span_merge tok.span (previous p).span)
  | Token.Keyword Kw_external ->
      ignore (advance p);
      let name_tok = peek p in
      let name =
        match name_tok.kind with
        | Token.Ident n ->
            ignore (advance p);
            Ident.Intern.intern n
        | _ ->
            add_error p name_tok.span "expected external name";
            Ident.fresh "ext"
      in
      ignore (expect_punct p Token.Colon "expected ':' after external name");
      let ty = parse_type_expr p in
      ignore (expect_punct p Token.Eq "expected '=' after external type");
      let lit =
        match peek_kind p with
        | Token.Lit_string s ->
            ignore (advance p);
            s
        | _ ->
            add_error p (peek p).span "expected string literal for external";
            ""
      in
      Ast.Top_external (name, ty, lit, span_merge tok.span (previous p).span)
  | Token.Eof ->
      add_error p tok.span "unexpected end of file";
      Ast.Top_expr (Ast.unit tok.span)
  | _ ->
      let e = parse_expr p in
      Ast.Top_expr e

let parse_program_tokens ~filename tokens =
  let p = create ~filename tokens in
  let items = ref [] in
  while not (check p Token.Eof) do
    items := parse_toplevel p :: !items
  done;
  let prog = Ast.program (List.rev !items) in
  let diags = List.rev !(p.diagnostics) in
  if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
    Error diags
  else Ok (prog, diags)

let parse_program ~filename ~source =
  let tokens, lex_diags = Lexer.tokenize_allowing_errors ~filename ~source () in
  match parse_program_tokens ~filename tokens with
  | Ok (prog, diags) ->
      let diags = lex_diags @ diags in
      if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
        Error diags
      else Ok (prog, diags)
  | Error diags -> Error (lex_diags @ diags)

let parse_expr_string ~filename ~source =
  let tokens, lex_diags = Lexer.tokenize_allowing_errors ~filename ~source () in
  let p = create ~filename tokens in
  let e = parse_expr p in
  let diags = lex_diags @ List.rev !(p.diagnostics) in
  if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
    Error diags
  else Ok (e, diags)
