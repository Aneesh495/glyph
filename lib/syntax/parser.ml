(** Hand-written recursive-descent + Pratt parser for Glyph. *)

type parser = {
  tokens : Token.t array;
  mutable index : int;
  mutable diagnostics : Diagnostic.t list;
  filename : string;
}

let of_tokens ~filename tokens =
  let tokens =
    match tokens with
    | [] -> [| Token.make Token.Eof Span.dummy "" |]
    | xs -> Array.of_list xs
  in
  { tokens; index = 0; diagnostics = []; filename }

let diagnostics p = List.rev p.diagnostics

let error p span message =
  p.diagnostics <- Diagnostic.error span message :: p.diagnostics

let current p =
  if p.index >= Array.length p.tokens then
    let last = p.tokens.(Array.length p.tokens - 1) in
    Token.make Token.Eof last.Token.span ""
  else p.tokens.(p.index)

let peek_n p n =
  let i = p.index + n in
  if i >= Array.length p.tokens then
    Token.make Token.Eof Span.dummy ""
  else p.tokens.(i)

let advance p =
  let tok = current p in
  if not (Token.is_eof tok) then p.index <- p.index + 1;
  tok

let at_eof p = Token.is_eof (current p)

let check_punct p punct = Token.is_punct (current p) punct
let check_kw p kw = Token.is_keyword (current p) kw

let consume_punct p punct =
  if check_punct p punct then (
    ignore (advance p);
    true)
  else false

let consume_kw p kw =
  if check_kw p kw then (
    ignore (advance p);
    true)
  else false

let expect_punct p punct =
  let tok = current p in
  if Token.is_punct tok punct then Ok (advance p)
  else (
    error p tok.Token.span
      (Printf.sprintf "expected '%s', found '%s'"
         (Token.punct_to_string punct)
         (Token.kind_to_string tok.Token.kind));
    Error tok)

let expect_kw p kw =
  let tok = current p in
  if Token.is_keyword tok kw then Ok (advance p)
  else (
    error p tok.Token.span
      (Printf.sprintf "expected '%s', found '%s'"
         (Token.keyword_to_string kw)
         (Token.kind_to_string tok.Token.kind));
    Error tok)

let synchronize p =
  ignore (advance p);
  let stop = ref false in
  while (not !stop) && not (at_eof p) do
    match (current p).Token.kind with
    | Token.Punct Token.Semicolon ->
        ignore (advance p);
        stop := true
    | Token.Keyword
        ( Token.Kw_let | Token.Kw_type | Token.Kw_module | Token.Kw_open
        | Token.Kw_external ) ->
        stop := true
    | Token.Punct (Token.RBrace | Token.RParen | Token.RBracket) -> stop := true
    | _ -> ignore (advance p)
  done

type assoc = Left | Right | NonAssoc

type op_info = {
  precedence : int;
  assoc : assoc;
  binop : Ast.binop option;
}

let infix_info tok : op_info option =
  match tok.Token.kind with
  | Token.Punct Token.PipePipe | Token.Operator "||" ->
      Some { precedence = 10; assoc = Right; binop = Some Ast.Or }
  | Token.Punct Token.AmpAmp | Token.Operator "&&" ->
      Some { precedence = 20; assoc = Right; binop = Some Ast.And }
  | Token.Punct Token.Eq | Token.Operator "=" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Eq }
  | Token.Punct Token.Neq | Token.Operator "<>" | Token.Operator "!=" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Neq }
  | Token.Punct Token.Lt | Token.Operator "<" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Lt }
  | Token.Punct Token.Le | Token.Operator "<=" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Le }
  | Token.Punct Token.Gt | Token.Operator ">" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Gt }
  | Token.Punct Token.Ge | Token.Operator ">=" ->
      Some { precedence = 30; assoc = Left; binop = Some Ast.Ge }
  | Token.Punct Token.ColonColon | Token.Operator "::" ->
      Some { precedence = 40; assoc = Right; binop = Some Ast.Cons }
  | Token.Operator "@" ->
      Some { precedence = 40; assoc = Right; binop = Some Ast.Append }
  | Token.Punct Token.Plus | Token.Operator "+" ->
      Some { precedence = 50; assoc = Left; binop = Some Ast.Add }
  | Token.Punct Token.Minus | Token.Operator "-" ->
      Some { precedence = 50; assoc = Left; binop = Some Ast.Sub }
  | Token.Punct Token.Star | Token.Operator "*" ->
      Some { precedence = 60; assoc = Left; binop = Some Ast.Mul }
  | Token.Punct Token.Slash | Token.Operator "/" ->
      Some { precedence = 60; assoc = Left; binop = Some Ast.Div }
  | Token.Punct Token.Percent | Token.Operator "%" ->
      Some { precedence = 60; assoc = Left; binop = Some Ast.Mod }
  | Token.Operator "|>" ->
      Some { precedence = 5; assoc = Left; binop = Some Ast.Pipe }
  | Token.Punct Token.AtAt | Token.Operator "@@" ->
      Some { precedence = 5; assoc = Right; binop = Some Ast.Apply }
  | Token.Operator ">>" ->
      Some { precedence = 15; assoc = Left; binop = Some Ast.Compose }
  | Token.Punct Token.Semicolon ->
      Some { precedence = 1; assoc = Right; binop = None }
  | _ -> None

let parse_int_lit raw =
  try Int64.of_string raw with Failure _ -> 0L

let parse_float_lit raw =
  try float_of_string raw with Failure _ -> 0.0

let is_pattern_atom_start = function
  | Token.Ident _ | Token.UpperIdent _ | Token.Punct Token.Underscore
  | Token.Lit_int _ | Token.Lit_float _ | Token.Lit_string _ | Token.Lit_char _
  | Token.Keyword (Token.Kw_true | Token.Kw_false)
  | Token.Punct Token.LParen | Token.Punct Token.LBracket
  | Token.Punct Token.LBrace ->
      true
  | _ -> false

let is_atom_start tok =
  match tok.Token.kind with
  | Token.Ident _ | Token.UpperIdent _
  | Token.Lit_int _ | Token.Lit_float _ | Token.Lit_string _ | Token.Lit_char _
  | Token.Keyword (Token.Kw_true | Token.Kw_false)
  | Token.Punct Token.LParen | Token.Punct Token.LBracket
  | Token.Punct Token.LBrace ->
      true
  | _ -> false

let rec parse_type_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Ident name when String.length name > 0 && name.[0] = '\'' ->
      ignore (advance p);
      Ast.tvar (Ident.Intern.intern name) tok.Token.span
  | Token.Ident name | Token.UpperIdent name ->
      ignore (advance p);
      let base = Ast.tcon (Ident.Intern.intern name) tok.Token.span in
      parse_type_app_tail p base
  | Token.Punct Token.LParen ->
      ignore (advance p);
      if check_punct p Token.RParen then (
        ignore (advance p);
        Ast.tcon (Ident.Intern.intern "unit") tok.Token.span)
      else
        let first = parse_type p in
        if check_punct p Token.Comma then (
          let rest = ref [] in
          while consume_punct p Token.Comma do
            rest := parse_type p :: !rest
          done;
          ignore (expect_punct p Token.RParen);
          let ts = first :: List.rev !rest in
          Ast.ttuple ts (Span.merge tok.Token.span (current p).Token.span))
        else (
          ignore (expect_punct p Token.RParen);
          first)
  | Token.Punct Token.LBrace -> parse_type_record p
  | Token.Punct Token.LBracket ->
      ignore (advance p);
      let inner = parse_type p in
      ignore (expect_punct p Token.RBracket);
      Ast.tarray inner (Span.merge tok.Token.span (current p).Token.span)
  | _ ->
      error p tok.Token.span "expected type";
      ignore (advance p);
      Ast.tvar (Ident.Intern.intern "_") tok.Token.span

and parse_type_app_tail p base =
  let rec loop acc =
    match (current p).Token.kind with
    | Token.Ident _ | Token.UpperIdent _
    | Token.Punct Token.LParen | Token.Punct Token.LBrace
    | Token.Punct Token.LBracket ->
        loop (parse_type_atom p :: acc)
    | _ -> List.rev acc
  in
  match loop [] with
  | [] -> base
  | args ->
      let span =
        Span.merge base.Ast.typ_span (List.hd (List.rev args)).Ast.typ_span
      in
      Ast.tapp base args span

and parse_type_record p =
  let start = current p in
  ignore (expect_punct p Token.LBrace);
  let fields = ref [] in
  if not (check_punct p Token.RBrace) then (
    let rec loop () =
      let mutable_ = consume_kw p Token.Kw_mutable in
      let name_tok = current p in
      let name =
        match name_tok.Token.kind with
        | Token.Ident n ->
            ignore (advance p);
            Ident.Intern.intern n
        | _ ->
            error p name_tok.Token.span "expected field name";
            Ident.Intern.intern "_"
      in
      ignore (expect_punct p Token.Colon);
      let ty = parse_type p in
      fields := (name, ty, mutable_) :: !fields;
      if consume_punct p Token.Semicolon && not (check_punct p Token.RBrace)
      then loop ()
    in
    loop ());
  let end_tok =
    match expect_punct p Token.RBrace with Ok t -> t | Error t -> t
  in
  Ast.typ (Ast.Typ_record (List.rev !fields))
    (Span.merge start.Token.span end_tok.Token.span)

and parse_type p =
  let left = parse_type_atom p in
  if consume_punct p Token.Arrow then
    let right = parse_type p in
    Ast.tarrow left right (Span.merge left.Ast.typ_span right.Ast.typ_span)
  else left

and parse_pattern p = parse_pattern_or p

and parse_pattern_or p =
  let left = parse_pattern_as p in
  if consume_punct p Token.Pipe then
    let right = parse_pattern_or p in
    Ast.por left right (Span.merge left.Ast.pat_span right.Ast.pat_span)
  else left

and parse_pattern_as p =
  let left = parse_pattern_cons p in
  if consume_kw p Token.Kw_as then (
    let name_tok = current p in
    match name_tok.Token.kind with
    | Token.Ident n ->
        ignore (advance p);
        Ast.pas left (Ident.Intern.intern n)
          (Span.merge left.Ast.pat_span name_tok.Token.span)
    | _ ->
        error p name_tok.Token.span "expected identifier after 'as'";
        left)
  else left

and parse_pattern_cons p =
  let left = parse_pattern_annot p in
  if check_punct p Token.ColonColon then (
    ignore (advance p);
    let right = parse_pattern_cons p in
    Ast.pcons left right (Span.merge left.Ast.pat_span right.Ast.pat_span))
  else left

and parse_pattern_annot p =
  let left = parse_pattern_atom p in
  if check_punct p Token.Colon then (
    ignore (advance p);
    let ty = parse_type p in
    Ast.pann left ty (Span.merge left.Ast.pat_span ty.Ast.typ_span))
  else left

and parse_pattern_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Punct Token.Underscore ->
      ignore (advance p);
      Ast.pwildcard tok.Token.span
  | Token.Ident name ->
      ignore (advance p);
      Ast.pvar (Ident.Intern.intern name) tok.Token.span
  | Token.UpperIdent name ->
      ignore (advance p);
      let id = Ident.Intern.intern name in
      let args = parse_pattern_constructor_args p in
      let span =
        match args with
        | [] -> tok.Token.span
        | xs -> Span.merge tok.Token.span (List.hd (List.rev xs)).Ast.pat_span
      in
      Ast.pconstruct id args span
  | Token.Lit_int s ->
      ignore (advance p);
      Ast.plit (Ast.Lit_int (parse_int_lit s)) tok.Token.span
  | Token.Lit_float s ->
      ignore (advance p);
      Ast.plit (Ast.Lit_float (parse_float_lit s)) tok.Token.span
  | Token.Lit_string s ->
      ignore (advance p);
      Ast.plit (Ast.Lit_string s) tok.Token.span
  | Token.Lit_char c ->
      ignore (advance p);
      Ast.plit (Ast.Lit_char c) tok.Token.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.plit (Ast.Lit_bool true) tok.Token.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.plit (Ast.Lit_bool false) tok.Token.span
  | Token.Punct Token.LParen -> parse_pattern_paren p
  | Token.Punct Token.LBracket -> parse_pattern_list p
  | Token.Punct Token.LBrace -> parse_pattern_record p
  | _ ->
      error p tok.Token.span "expected pattern";
      ignore (advance p);
      Ast.pwildcard tok.Token.span

and parse_pattern_constructor_args p =
  let rec loop acc =
    if is_pattern_atom_start (current p).Token.kind then
      loop (parse_pattern_atom p :: acc)
    else List.rev acc
  in
  loop []

and parse_pattern_paren p =
  let start = current p in
  ignore (advance p);
  if check_punct p Token.RParen then (
    ignore (advance p);
    Ast.plit Ast.Lit_unit start.Token.span)
  else
    let first = parse_pattern p in
    if check_punct p Token.Comma then (
      let rest = ref [] in
      while consume_punct p Token.Comma do
        rest := parse_pattern p :: !rest
      done;
      ignore (expect_punct p Token.RParen);
      Ast.ptuple (first :: List.rev !rest)
        (Span.merge start.Token.span (current p).Token.span))
    else (
      ignore (expect_punct p Token.RParen);
      first)

and parse_pattern_list p =
  let start = current p in
  ignore (advance p);
  if check_punct p Token.RBracket then (
    let end_tok = advance p in
    Ast.plist [] (Span.merge start.Token.span end_tok.Token.span))
  else
    let items = ref [ parse_pattern p ] in
    while consume_punct p Token.Semicolon do
      if not (check_punct p Token.RBracket) then
        items := parse_pattern p :: !items
    done;
    let end_tok =
      match expect_punct p Token.RBracket with Ok t -> t | Error t -> t
    in
    Ast.plist (List.rev !items) (Span.merge start.Token.span end_tok.Token.span)

and parse_pattern_record p =
  let start = current p in
  ignore (advance p);
  let fields = ref [] in
  let open_ = ref false in
  if not (check_punct p Token.RBrace) then (
    let rec loop () =
      if check_punct p Token.DotDot then (
        ignore (advance p);
        open_ := true)
      else
        match (current p).Token.kind with
        | Token.Ident n ->
            ignore (advance p);
            let id = Ident.Intern.intern n in
            let pat_opt =
              if consume_punct p Token.Eq then Some (parse_pattern p) else None
            in
            fields := (id, pat_opt) :: !fields;
            if consume_punct p Token.Semicolon && not (check_punct p Token.RBrace)
            then loop ()
        | _ -> error p (current p).Token.span "expected field name in pattern"
    in
    loop ());
  let end_tok =
    match expect_punct p Token.RBrace with Ok t -> t | Error t -> t
  in
  Ast.pat
    (Ast.Pat_record (List.rev !fields, !open_))
    (Span.merge start.Token.span end_tok.Token.span)

and parse_expr p = parse_expr_bp p 0

and parse_expr_bp p min_bp =
  let left = ref (parse_prefix p) in
  let continue = ref true in
  while !continue do
    let tok = current p in
    if is_atom_start tok && min_bp <= 80 then (
      let arg = parse_prefix p in
      let span = Span.merge !left.Ast.exp_span arg.Ast.exp_span in
      match !left.Ast.exp_desc with
      | Ast.Exp_app (f, args) -> left := Ast.app f (args @ [ arg ]) span
      | Ast.Exp_constructor (name, args) ->
          left := Ast.exp (Ast.Exp_constructor (name, args @ [ arg ])) span
      | _ -> left := Ast.app !left [ arg ] span)
    else
      match infix_info tok with
      | Some info when info.precedence >= min_bp -> (
          ignore (advance p);
          match info.binop with
          | None ->
              let right = parse_expr_bp p info.precedence in
              left :=
                Ast.seq !left right
                  (Span.merge !left.Ast.exp_span right.Ast.exp_span)
          | Some Ast.Cons ->
              let right = parse_expr_bp p info.precedence in
              left :=
                Ast.cons !left right
                  (Span.merge !left.Ast.exp_span right.Ast.exp_span)
          | Some op ->
              let next_bp =
                match info.assoc with
                | Left | NonAssoc -> info.precedence + 1
                | Right -> info.precedence
              in
              let right = parse_expr_bp p next_bp in
              left :=
                Ast.binop op !left right
                  (Span.merge !left.Ast.exp_span right.Ast.exp_span))
      | Some _ -> continue := false
      | None ->
          if check_punct p Token.Dot then (
            ignore (advance p);
            let field_tok = current p in
            match field_tok.Token.kind with
            | Token.Ident n | Token.UpperIdent n ->
                ignore (advance p);
                left :=
                  Ast.field !left (Ident.Intern.intern n)
                    (Span.merge !left.Ast.exp_span field_tok.Token.span)
            | _ ->
                error p field_tok.Token.span "expected field name";
                continue := false)
          else if check_punct p Token.LBracket then (
            ignore (advance p);
            let idx = parse_expr p in
            ignore (expect_punct p Token.RBracket);
            left :=
              Ast.index !left idx
                (Span.merge !left.Ast.exp_span idx.Ast.exp_span))
          else if check_punct p Token.Colon && min_bp <= 2 then (
            ignore (advance p);
            let ty = parse_type p in
            left :=
              Ast.annotated !left ty
                (Span.merge !left.Ast.exp_span ty.Ast.typ_span))
          else continue := false
  done;
  !left

and parse_prefix p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Punct Token.Minus ->
      ignore (advance p);
      let e = parse_expr_bp p 70 in
      Ast.unop Ast.Neg e (Span.merge tok.Token.span e.Ast.exp_span)
  | Token.Punct Token.Bang ->
      ignore (advance p);
      let e = parse_expr_bp p 70 in
      Ast.unop Ast.Deref e (Span.merge tok.Token.span e.Ast.exp_span)
  | Token.Ident "not" ->
      ignore (advance p);
      let e = parse_expr_bp p 70 in
      Ast.unop Ast.Not e (Span.merge tok.Token.span e.Ast.exp_span)
  | Token.Keyword Token.Kw_if -> parse_if p
  | Token.Keyword Token.Kw_match -> parse_match p
  | Token.Keyword (Token.Kw_fun | Token.Kw_fn) -> parse_fun p
  | Token.Keyword Token.Kw_let -> parse_let_expr p
  | _ -> parse_atom p

and parse_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Lit_int s ->
      ignore (advance p);
      Ast.lit (Ast.Lit_int (parse_int_lit s)) tok.Token.span
  | Token.Lit_float s ->
      ignore (advance p);
      Ast.lit (Ast.Lit_float (parse_float_lit s)) tok.Token.span
  | Token.Lit_string s ->
      ignore (advance p);
      Ast.lit (Ast.Lit_string s) tok.Token.span
  | Token.Lit_char c ->
      ignore (advance p);
      Ast.lit (Ast.Lit_char c) tok.Token.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.lit (Ast.Lit_bool true) tok.Token.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.lit (Ast.Lit_bool false) tok.Token.span
  | Token.Ident name ->
      ignore (advance p);
      Ast.var (Ident.Intern.intern name) tok.Token.span
  | Token.UpperIdent name ->
      ignore (advance p);
      Ast.construct (Ident.Intern.intern name) [] tok.Token.span
  | Token.Punct Token.LParen -> parse_paren_expr p
  | Token.Punct Token.LBracket -> parse_list_expr p
  | Token.Punct Token.LBrace -> parse_record_expr p
  | _ ->
      error p tok.Token.span
        (Printf.sprintf "unexpected token '%s' in expression"
           (Token.kind_to_string tok.Token.kind));
      ignore (advance p);
      Ast.unit tok.Token.span

and parse_paren_expr p =
  let start = current p in
  ignore (advance p);
  if check_punct p Token.RParen then (
    let end_tok = advance p in
    Ast.unit (Span.merge start.Token.span end_tok.Token.span))
  else
    let first = parse_expr p in
    if check_punct p Token.Comma then (
      let items = ref [ first ] in
      while consume_punct p Token.Comma do
        items := parse_expr p :: !items
      done;
      ignore (expect_punct p Token.RParen);
      let es = List.rev !items in
      Ast.tuple es
        (Span.merge start.Token.span (List.hd (List.rev es)).Ast.exp_span))
    else (
      ignore (expect_punct p Token.RParen);
      first)

and parse_list_expr p =
  let start = current p in
  ignore (advance p);
  if check_punct p Token.RBracket then (
    let end_tok = advance p in
    Ast.list [] (Span.merge start.Token.span end_tok.Token.span))
  else
    let items = ref [ parse_expr p ] in
    while consume_punct p Token.Semicolon do
      if not (check_punct p Token.RBracket) then
        items := parse_expr p :: !items
    done;
    let end_tok =
      match expect_punct p Token.RBracket with Ok t -> t | Error t -> t
    in
    Ast.list (List.rev !items) (Span.merge start.Token.span end_tok.Token.span)

and parse_record_expr p =
  let start = current p in
  ignore (advance p);
  if check_punct p Token.RBrace then (
    let end_tok = advance p in
    Ast.record [] (Span.merge start.Token.span end_tok.Token.span))
  else
    let tok = current p in
    let is_field_start =
      match tok.Token.kind with
      | Token.Ident _ -> (
          match (peek_n p 1).Token.kind with
          | Token.Punct Token.Eq | Token.Punct Token.Semicolon
          | Token.Punct Token.RBrace ->
              true
          | _ -> false)
      | _ -> false
    in
    if is_field_start then parse_record_fields p start
    else
      let base = parse_expr p in
      if consume_kw p Token.Kw_with then parse_record_update p start base
      else (
        error p start.Token.span "expected record fields";
        ignore (expect_punct p Token.RBrace);
        Ast.record [] start.Token.span)

and parse_record_fields p start =
  let fields = ref [] in
  let rec loop () =
    match (current p).Token.kind with
    | Token.Ident n ->
        ignore (advance p);
        ignore (expect_punct p Token.Eq);
        let e = parse_expr p in
        fields := (Ident.Intern.intern n, e) :: !fields;
        if consume_punct p Token.Semicolon && not (check_punct p Token.RBrace)
        then loop ()
    | _ -> error p (current p).Token.span "expected field name"
  in
  loop ();
  let end_tok =
    match expect_punct p Token.RBrace with Ok t -> t | Error t -> t
  in
  Ast.record (List.rev !fields)
    (Span.merge start.Token.span end_tok.Token.span)

and parse_record_update p start base =
  let fields = ref [] in
  let rec loop () =
    match (current p).Token.kind with
    | Token.Ident n ->
        ignore (advance p);
        ignore (expect_punct p Token.Eq);
        let e = parse_expr p in
        fields := (Ident.Intern.intern n, e) :: !fields;
        if consume_punct p Token.Semicolon && not (check_punct p Token.RBrace)
        then loop ()
    | _ -> error p (current p).Token.span "expected field name"
  in
  if not (check_punct p Token.RBrace) then loop ();
  let end_tok =
    match expect_punct p Token.RBrace with Ok t -> t | Error t -> t
  in
  Ast.exp
    (Ast.Exp_record_update (base, List.rev !fields))
    (Span.merge start.Token.span end_tok.Token.span)

and parse_if p =
  let start = current p in
  ignore (expect_kw p Token.Kw_if);
  let cond = parse_expr p in
  ignore (expect_kw p Token.Kw_then);
  let then_ = parse_expr p in
  let else_ =
    if consume_kw p Token.Kw_else then Some (parse_expr p) else None
  in
  let end_span =
    match else_ with Some e -> e.Ast.exp_span | None -> then_.Ast.exp_span
  in
  Ast.if_ cond then_ else_ (Span.merge start.Token.span end_span)

and parse_match p =
  let start = current p in
  ignore (expect_kw p Token.Kw_match);
  let scrut = parse_expr p in
  ignore (expect_kw p Token.Kw_with);
  ignore (consume_punct p Token.Pipe);
  let cases = ref [ parse_case p ] in
  while check_punct p Token.Pipe do
    ignore (advance p);
    cases := parse_case p :: !cases
  done;
  let cases = List.rev !cases in
  let end_span =
    match List.rev cases with
    | c :: _ -> c.Ast.case_span
    | [] -> scrut.Ast.exp_span
  in
  Ast.match_ scrut cases (Span.merge start.Token.span end_span)

and parse_case p =
  let start = current p in
  let pat = parse_pattern p in
  let guard =
    if consume_kw p Token.Kw_when then Some (parse_expr p) else None
  in
  ignore (expect_punct p Token.Arrow);
  let body = parse_expr p in
  Ast.case ~guard pat body (Span.merge start.Token.span body.Ast.exp_span)

and parse_fun p =
  let start = current p in
  ignore (advance p);
  let params = ref [] in
  while is_pattern_atom_start (current p).Token.kind do
    params := parse_pattern_atom p :: !params
  done;
  if not (consume_punct p Token.Arrow || consume_punct p Token.FatArrow) then
    error p (current p).Token.span "expected '->' or '=>' in function";
  let body = parse_expr p in
  Ast.abs (List.rev !params) body
    (Span.merge start.Token.span body.Ast.exp_span)

and parse_let_expr p =
  let start = current p in
  let vbs, is_rec = parse_let_bindings p in
  ignore (expect_kw p Token.Kw_in);
  let body = parse_expr p in
  let span = Span.merge start.Token.span body.Ast.exp_span in
  if is_rec then Ast.letrec vbs body span else Ast.let_ vbs body span

and parse_let_bindings p =
  ignore (expect_kw p Token.Kw_let);
  let is_rec = consume_kw p Token.Kw_rec in
  let first = parse_value_binding p ~is_rec in
  let rest = ref [] in
  while consume_kw p Token.Kw_and do
    rest := parse_value_binding p ~is_rec :: !rest
  done;
  (first :: List.rev !rest, is_rec)

and is_param_list_ahead p =
  match (current p).Token.kind with
  | Token.Ident _ -> (
      match (peek_n p 1).Token.kind with
      | Token.Punct Token.Eq | Token.Punct Token.Colon -> false
      | k -> is_pattern_atom_start k)
  | _ -> false

and parse_value_binding p ~is_rec =
  let start = current p in
  if is_param_list_ahead p then (
    let name_tok = advance p in
    let name = Ident.Intern.intern name_tok.Token.raw in
    let pat = Ast.pvar name name_tok.Token.span in
    let params = ref [] in
    while is_pattern_atom_start (current p).Token.kind do
      params := parse_pattern_atom p :: !params
    done;
    ignore (expect_punct p Token.Eq);
    let body = parse_expr p in
    Ast.value_binding ~is_rec ~params:(List.rev !params) pat body
      (Span.merge start.Token.span body.Ast.exp_span))
  else (
    let pat = parse_pattern p in
    ignore (expect_punct p Token.Eq);
    let body = parse_expr p in
    Ast.value_binding ~is_rec pat body
      (Span.merge start.Token.span body.Ast.exp_span))

and parse_type_decl p =
  let start = current p in
  ignore (expect_kw p Token.Kw_type);
  let first = parse_one_type_decl p start in
  let rest = ref [] in
  while consume_kw p Token.Kw_and do
    rest := parse_one_type_decl p (current p) :: !rest
  done;
  first :: List.rev !rest

and parse_type_params p =
  let rec loop acc =
    match (current p).Token.kind with
    | Token.Ident name when String.length name > 0 && name.[0] = '\'' ->
        ignore (advance p);
        loop (Ident.Intern.intern name :: acc)
    | Token.Ident name
      when String.length name > 0
           && name.[0] >= 'a'
           && name.[0] <= 'z' ->
        (* bare type variable style: type List a *)
        ignore (advance p);
        loop (Ident.Intern.intern name :: acc)
    | Token.Punct Token.Apostrophe ->
        ignore (advance p);
        (match (current p).Token.kind with
        | Token.Ident n ->
            ignore (advance p);
            loop (Ident.Intern.intern ("'" ^ n) :: acc)
        | _ -> List.rev acc)
    | _ -> List.rev acc
  in
  loop []

and parse_one_type_decl p start =
  let params_before = parse_type_params p in
  let name_tok = current p in
  let name =
    match name_tok.Token.kind with
    | Token.UpperIdent n | Token.Ident n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        error p name_tok.Token.span "expected type name";
        Ident.Intern.intern "_"
  in
  let params_after = parse_type_params p in
  let params = params_before @ params_after in
  ignore (expect_punct p Token.Eq);
  let kind = parse_type_kind p in
  Ast.type_decl name params kind
    (Span.merge start.Token.span name_tok.Token.span)

and parse_type_kind p =
  match (current p).Token.kind with
  | Token.Punct Token.Pipe | Token.UpperIdent _ ->
      ignore (consume_punct p Token.Pipe);
      let ctors = ref [ parse_constructor_decl p ] in
      while consume_punct p Token.Pipe do
        ctors := parse_constructor_decl p :: !ctors
      done;
      Ast.Type_variant (List.rev !ctors)
  | Token.Punct Token.LBrace -> (
      let rec_ty = parse_type_record p in
      match rec_ty.Ast.typ_desc with
      | Ast.Typ_record fields -> Ast.Type_record fields
      | _ -> Ast.Type_abbrev rec_ty)
  | _ -> Ast.Type_abbrev (parse_type p)

and parse_constructor_decl p =
  let tok = current p in
  let name =
    match tok.Token.kind with
    | Token.UpperIdent n | Token.Ident n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        error p tok.Token.span "expected constructor name";
        Ident.Intern.intern "_"
  in
  let args =
    if consume_kw p Token.Kw_of then (
      let first = parse_type_atom p in
      let rest = ref [] in
      while consume_punct p Token.Star do
        rest := parse_type_atom p :: !rest
      done;
      first :: List.rev !rest)
    else
      let rec loop acc =
        match (current p).Token.kind with
        | Token.Ident _ | Token.UpperIdent _ | Token.Punct Token.LParen
        | Token.Punct Token.LBracket | Token.Punct Token.LBrace
          when not (check_punct p Token.Pipe) ->
            loop (parse_type_atom p :: acc)
        | _ -> List.rev acc
      in
      loop []
  in
  let span =
    match args with
    | [] -> tok.Token.span
    | xs -> Span.merge tok.Token.span (List.hd (List.rev xs)).Ast.typ_span
  in
  Ast.constructor_decl name args span

and parse_mod_path p =
  let rec loop acc =
    match (current p).Token.kind with
    | Token.UpperIdent n | Token.Ident n ->
        ignore (advance p);
        let id = Ident.Intern.intern n in
        if consume_punct p Token.Dot then loop (id :: acc)
        else List.rev (id :: acc)
    | _ -> List.rev acc
  in
  loop []

and parse_external p =
  let start = current p in
  ignore (expect_kw p Token.Kw_external);
  let name_tok = current p in
  let name =
    match name_tok.Token.kind with
    | Token.Ident n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        error p name_tok.Token.span "expected external name";
        Ident.Intern.intern "_"
  in
  ignore (expect_punct p Token.Colon);
  let ty = parse_type p in
  ignore (expect_punct p Token.Eq);
  let prim_tok = current p in
  let prim =
    match prim_tok.Token.kind with
    | Token.Lit_string s ->
        ignore (advance p);
        s
    | _ ->
        error p prim_tok.Token.span "expected string primitive name";
        ""
  in
  ignore (consume_punct p Token.Semicolon);
  Ast.Top_external
    (name, ty, prim, Span.merge start.Token.span prim_tok.Token.span)

and parse_module p =
  let start = current p in
  ignore (expect_kw p Token.Kw_module);
  let name_tok = current p in
  let name =
    match name_tok.Token.kind with
    | Token.UpperIdent n | Token.Ident n ->
        ignore (advance p);
        Ident.Intern.intern n
    | _ ->
        error p name_tok.Token.span "expected module name";
        Ident.Intern.intern "_"
  in
  ignore (expect_punct p Token.Eq);
  ignore (expect_punct p Token.LBrace);
  let items = ref [] in
  while (not (at_eof p)) && not (check_punct p Token.RBrace) do
    let before = p.index in
    items := parse_toplevel p :: !items;
    if p.index = before then synchronize p
  done;
  let end_tok =
    match expect_punct p Token.RBrace with Ok t -> t | Error t -> t
  in
  ignore (consume_punct p Token.Semicolon);
  Ast.Top_module
    (name, List.rev !items, Span.merge start.Token.span end_tok.Token.span)

and parse_toplevel p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Keyword Token.Kw_let ->
      let vbs, is_rec = parse_let_bindings p in
      ignore (consume_punct p Token.Semicolon);
      if is_rec then Ast.Top_letrec vbs else Ast.Top_let vbs
  | Token.Keyword Token.Kw_type ->
      let tds = parse_type_decl p in
      ignore (consume_punct p Token.Semicolon);
      Ast.Top_type tds
  | Token.Keyword Token.Kw_open ->
      ignore (advance p);
      let path = parse_mod_path p in
      ignore (consume_punct p Token.Semicolon);
      Ast.Top_open (path, tok.Token.span)
  | Token.Keyword Token.Kw_external -> parse_external p
  | Token.Keyword Token.Kw_module -> parse_module p
  | Token.Eof ->
      error p tok.Token.span "unexpected end of file";
      Ast.Top_expr (Ast.unit tok.Token.span)
  | _ ->
      let e = parse_expr p in
      ignore (consume_punct p Token.Semicolon);
      Ast.Top_expr e

let parse_program_items p =
  let items = ref [] in
  while not (at_eof p) do
    let before = p.index in
    items := parse_toplevel p :: !items;
    if p.index = before then synchronize p
  done;
  List.rev !items

let parse_tokens ~filename tokens =
  let p = of_tokens ~filename tokens in
  let items = parse_program_items p in
  let prog = Ast.program items in
  let diags = diagnostics p in
  if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags then
    Error diags
  else Ok prog

let parse_program filename source =
  match Lexer.tokenize ~filename ~source () with
  | Error diags -> Error diags
  | Ok tokens -> parse_tokens ~filename tokens

let parse_expr_string filename source =
  match Lexer.tokenize ~filename ~source () with
  | Error diags -> Error diags
  | Ok tokens ->
      let p = of_tokens ~filename tokens in
      let e = parse_expr p in
      let diags = diagnostics p in
      if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags
      then Error diags
      else Ok e

let parse_type_string filename source =
  match Lexer.tokenize ~filename ~source () with
  | Error diags -> Error diags
  | Ok tokens ->
      let p = of_tokens ~filename tokens in
      let t = parse_type p in
      let diags = diagnostics p in
      if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags
      then Error diags
      else Ok t

let parse_pattern_string filename source =
  match Lexer.tokenize ~filename ~source () with
  | Error diags -> Error diags
  | Ok tokens ->
      let p = of_tokens ~filename tokens in
      let pat = parse_pattern p in
      let diags = diagnostics p in
      if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags
      then Error diags
      else Ok pat

let parse_type_decl_string filename source =
  match Lexer.tokenize ~filename ~source () with
  | Error diags -> Error diags
  | Ok tokens ->
      let p = of_tokens ~filename tokens in
      let tds = parse_type_decl p in
      let diags = diagnostics p in
      if List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error) diags
      then Error diags
      else Ok tds
