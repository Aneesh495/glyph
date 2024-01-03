(** Hand-written Pratt parser for Glyph (flat token kinds). *)

type error = { message : string; span : Span.t }

exception Error of error

type t = {
  file : string;
  source : string;
  tokens : Token.t array;
  mutable index : int;
}

let create ~file ~source tokens =
  let tokens =
    match tokens with
    | [] -> [| Token.make Token.Eof ~span:Span.dummy ~lexeme:"" |]
    | xs -> Array.of_list xs
  in
  { file; source; tokens; index = 0 }

let current p =
  if p.index >= Array.length p.tokens then
    let last = p.tokens.(Array.length p.tokens - 1) in
    Token.make Token.Eof ~span:last.Token.span ~lexeme:""
  else p.tokens.(p.index)

let advance p =
  let tok = current p in
  if not (Token.is_eof tok) then p.index <- p.index + 1;
  tok

let at_end p = Token.is_eof (current p)
let fail span message = raise (Error { message; span })

let check_kw p kw = Token.is_keyword (current p) kw
let check_kind p k = (current p).Token.kind = k

let consume_kw p kw =
  if check_kw p kw then (
    ignore (advance p);
    true)
  else false

let consume_kind p k =
  if check_kind p k then (
    ignore (advance p);
    true)
  else false

let expect_kw p kw =
  let tok = current p in
  if Token.is_keyword tok kw then advance p
  else
    fail tok.Token.span
      (Printf.sprintf "expected '%s', found %s"
         (Token.keyword_to_string kw)
         (Token.kind_to_string tok.Token.kind))

let expect_kind p k =
  let tok = current p in
  if tok.Token.kind = k then advance p
  else
    fail tok.Token.span
      (Printf.sprintf "expected %s, found %s"
         (Token.kind_to_string k)
         (Token.kind_to_string tok.Token.kind))

let binding_power = function
  | Token.Op_pipe -> (1, 2)
  | Token.Op_or -> (3, 4)
  | Token.Op_and -> (5, 6)
  | Token.Op_eq | Token.Op_neq | Token.Op_lt | Token.Op_le | Token.Op_gt
  | Token.Op_ge ->
      (7, 8)
  | Token.Op_cons -> (10, 9)
  | Token.Op_add | Token.Op_sub -> (11, 12)
  | Token.Op_mul | Token.Op_div | Token.Op_mod -> (13, 14)

let current_binop p =
  match (current p).Token.kind with
  | Token.Binop op -> Some op
  | Token.Equal -> Some Token.Op_eq
  | _ -> None

let rec parse_ty p =
  let left = parse_ty_atom p in
  if consume_kind p Token.Arrow then
    let right = parse_ty p in
    Ast.ty (Ast.Ty_arrow (left, right))
      (Span.merge left.Ast.ty_span right.Ast.ty_span)
  else left

and parse_ty_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Ident name when String.length name > 0 && name.[0] = '\'' ->
      ignore (advance p);
      Ast.ty (Ast.Ty_var (Ident.Intern.intern name)) tok.Token.span
  | Token.Ident name | Token.Ctor name ->
      ignore (advance p);
      let id = Ident.Intern.intern name in
      if consume_kind p Token.LParen then (
        let args = ref [] in
        if not (check_kind p Token.RParen) then (
          args := parse_ty p :: !args;
          while consume_kind p Token.Comma do
            args := parse_ty p :: !args
          done);
        ignore (expect_kind p Token.RParen);
        Ast.ty (Ast.Ty_named (id, List.rev !args)) tok.Token.span)
      else Ast.ty (Ast.Ty_named (id, [])) tok.Token.span
  | Token.LParen ->
      ignore (advance p);
      if consume_kind p Token.RParen then Ast.ty Ast.Ty_unit tok.Token.span
      else
        let t0 = parse_ty p in
        if consume_kind p Token.Comma then (
          let ts = ref [ t0 ] in
          ts := parse_ty p :: !ts;
          while consume_kind p Token.Comma do
            ts := parse_ty p :: !ts
          done;
          ignore (expect_kind p Token.RParen);
          Ast.ty (Ast.Ty_tuple (List.rev !ts)) tok.Token.span)
        else (
          ignore (expect_kind p Token.RParen);
          t0)
  | _ -> fail tok.Token.span "expected type"

let is_pat_atom tok =
  match tok.Token.kind with
  | Token.Ident _ | Token.Ctor _ | Token.Int _ | Token.Float _ | Token.String _
  | Token.Char _
  | Token.Keyword (Token.Kw_true | Token.Kw_false)
  | Token.LParen | Token.Underscore ->
      true
  | _ -> false

let rec parse_pattern p =
  let left = parse_pattern_atom p in
  if
    match (current p).Token.kind with
    | Token.Binop Token.Op_cons -> true
    | _ -> false
  then (
    ignore (advance p);
    let right = parse_pattern p in
    Ast.pat
      (Ast.Pat_ctor (Ident.Intern.intern "Cons", [ left; right ]))
      (Span.merge left.Ast.pat_span right.Ast.pat_span))
  else if consume_kw p Token.Kw_as then (
    let ntok = advance p in
    match ntok.Token.kind with
    | Token.Ident n ->
        Ast.pat
          (Ast.Pat_as (left, Ident.Intern.intern n))
          (Span.merge left.Ast.pat_span ntok.Token.span)
    | _ -> fail ntok.Token.span "expected identifier after as")
  else left

and parse_pattern_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Underscore ->
      ignore (advance p);
      Ast.pat Ast.Pat_wild tok.Token.span
  | Token.Ident name ->
      ignore (advance p);
      Ast.pat (Ast.Pat_var (Ident.Intern.intern name)) tok.Token.span
  | Token.Ctor name ->
      ignore (advance p);
      let id = Ident.Intern.intern name in
      let args =
        if check_kind p Token.LParen then (
          ignore (advance p);
          let xs = ref [] in
          if not (check_kind p Token.RParen) then (
            xs := parse_pattern p :: !xs;
            while consume_kind p Token.Comma do
              xs := parse_pattern p :: !xs
            done);
          ignore (expect_kind p Token.RParen);
          List.rev !xs)
        else []
      in
      Ast.pat (Ast.Pat_ctor (id, args)) tok.Token.span
  | Token.Int n ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_int n)) tok.Token.span
  | Token.Float f ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_float f)) tok.Token.span
  | Token.String s ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_string s)) tok.Token.span
  | Token.Char c ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_char c)) tok.Token.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_bool true)) tok.Token.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_bool false)) tok.Token.span
  | Token.LParen ->
      ignore (advance p);
      if consume_kind p Token.RParen then
        Ast.pat (Ast.Pat_lit Ast.Lit_unit) tok.Token.span
      else
        let first = parse_pattern p in
        if consume_kind p Token.Comma then (
          let ps = ref [ first ] in
          ps := parse_pattern p :: !ps;
          while consume_kind p Token.Comma do
            ps := parse_pattern p :: !ps
          done;
          ignore (expect_kind p Token.RParen);
          Ast.pat (Ast.Pat_tuple (List.rev !ps)) tok.Token.span)
        else (
          ignore (expect_kind p Token.RParen);
          first)
  | _ -> fail tok.Token.span "expected pattern"

let is_atom_start tok =
  match tok.Token.kind with
  | Token.Ident _ | Token.Ctor _ | Token.Int _ | Token.Float _ | Token.String _
  | Token.Char _
  | Token.Keyword (Token.Kw_true | Token.Kw_false)
  | Token.LParen ->
      true
  | _ -> false

let rec parse_expr p = parse_binary p 0

and parse_binary p min_bp =
  let left = ref (parse_app p) in
  let cont = ref true in
  while !cont do
    match current_binop p with
    | Some op when fst (binding_power op) >= min_bp ->
        ignore (advance p);
        let _, rbp = binding_power op in
        let right = parse_binary p rbp in
        left :=
          Ast.expr
            (Ast.Expr_bin (op, !left, right))
            (Span.merge !left.Ast.expr_span right.Ast.expr_span)
    | _ -> cont := false
  done;
  !left

and parse_app p =
  let left = ref (parse_unary p) in
  while is_atom_start (current p) do
    let arg = parse_atom p in
    left :=
      (match !left.Ast.expr_desc with
      | Ast.Expr_app (f, args) ->
          Ast.expr
            (Ast.Expr_app (f, args @ [ arg ]))
            (Span.merge !left.Ast.expr_span arg.Ast.expr_span)
      | _ ->
          Ast.expr
            (Ast.Expr_app (!left, [ arg ]))
            (Span.merge !left.Ast.expr_span arg.Ast.expr_span))
  done;
  !left

and parse_unary p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Binop Token.Op_sub ->
      ignore (advance p);
      let e = parse_unary p in
      Ast.expr
        (Ast.Expr_un (Token.Op_neg, e))
        (Span.merge tok.Token.span e.Ast.expr_span)
  | Token.Keyword Token.Kw_not | Token.Ident "not" ->
      ignore (advance p);
      let e = parse_unary p in
      Ast.expr
        (Ast.Expr_un (Token.Op_not, e))
        (Span.merge tok.Token.span e.Ast.expr_span)
  | Token.Keyword Token.Kw_if -> parse_if p
  | Token.Keyword Token.Kw_match -> parse_match p
  | Token.Keyword (Token.Kw_fun | Token.Kw_fn) -> parse_lambda p
  | Token.Keyword Token.Kw_let -> parse_let_expr p
  | _ -> parse_atom p

and parse_atom p =
  let tok = current p in
  match tok.Token.kind with
  | Token.Int n ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_int n)) tok.Token.span
  | Token.Float f ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_float f)) tok.Token.span
  | Token.String s ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_string s)) tok.Token.span
  | Token.Char c ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_char c)) tok.Token.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_bool true)) tok.Token.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_bool false)) tok.Token.span
  | Token.Ident name ->
      ignore (advance p);
      Ast.expr (Ast.Expr_var (Ident.Intern.intern name)) tok.Token.span
  | Token.Ctor name ->
      ignore (advance p);
      Ast.expr (Ast.Expr_ctor (Ident.Intern.intern name)) tok.Token.span
  | Token.LParen ->
      ignore (advance p);
      if consume_kind p Token.RParen then
        Ast.expr (Ast.Expr_lit Ast.Lit_unit) tok.Token.span
      else
        let first = parse_expr p in
        if consume_kind p Token.Comma then (
          let es = ref [ first ] in
          es := parse_expr p :: !es;
          while consume_kind p Token.Comma do
            es := parse_expr p :: !es
          done;
          ignore (expect_kind p Token.RParen);
          Ast.expr (Ast.Expr_tuple (List.rev !es)) tok.Token.span)
        else (
          ignore (expect_kind p Token.RParen);
          first)
  | _ ->
      fail tok.Token.span
        (Printf.sprintf "unexpected token %s"
           (Token.kind_to_string tok.Token.kind))

and parse_if p =
  let start = expect_kw p Token.Kw_if in
  let cond = parse_expr p in
  ignore (expect_kw p Token.Kw_then);
  let then_ = parse_expr p in
  ignore (expect_kw p Token.Kw_else);
  let else_ = parse_expr p in
  Ast.expr
    (Ast.Expr_if (cond, then_, else_))
    (Span.merge start.Token.span else_.Ast.expr_span)

and parse_match p =
  let start = expect_kw p Token.Kw_match in
  let scrut = parse_expr p in
  ignore (expect_kw p Token.Kw_with);
  ignore (consume_kind p Token.Pipe);
  let cases = ref [ parse_case p ] in
  while check_kind p Token.Pipe do
    ignore (advance p);
    cases := parse_case p :: !cases
  done;
  let cases = List.rev !cases in
  Ast.expr
    (Ast.Expr_match (scrut, cases))
    (Span.merge start.Token.span (List.hd (List.rev cases)).Ast.case_span)

and parse_case p =
  let start = current p in
  let pat = parse_pattern p in
  let guard =
    if consume_kw p Token.Kw_when then Some (parse_expr p) else None
  in
  ignore (expect_kind p Token.Arrow);
  let body = parse_expr p in
  {
    Ast.case_pat = pat;
    case_guard = guard;
    case_body = body;
    case_span = Span.merge start.Token.span body.Ast.expr_span;
  }

and parse_lambda p =
  let start = advance p in
  let params = ref [] in
  while
    match (current p).Token.kind with
    | Token.Ident _ -> true
    | _ -> false
  do
    let tok = advance p in
    match tok.Token.kind with
    | Token.Ident n ->
        params :=
          {
            Ast.param_name = Ident.Intern.intern n;
            param_ty = None;
            param_span = tok.Token.span;
          }
          :: !params
    | _ -> ()
  done;
  if not (consume_kind p Token.Arrow || consume_kind p Token.FatArrow) then
    fail (current p).Token.span "expected -> in function";
  let body = parse_expr p in
  Ast.expr
    (Ast.Expr_lambda (List.rev !params, body))
    (Span.merge start.Token.span body.Ast.expr_span)

and parse_let_expr p =
  let lb = parse_let_binding p in
  ignore (expect_kw p Token.Kw_in);
  let body = parse_expr p in
  let span = Span.merge lb.Ast.lb_span body.Ast.expr_span in
  if lb.Ast.lb_rec then Ast.expr (Ast.Expr_let_rec ([ lb ], body)) span
  else Ast.expr (Ast.Expr_let (lb, body)) span

and parse_let_binding p =
  let start = expect_kw p Token.Kw_let in
  let is_rec = consume_kw p Token.Kw_rec in
  let name_tok = advance p in
  let name =
    match name_tok.Token.kind with
    | Token.Ident n -> Ident.Intern.intern n
    | _ -> fail name_tok.Token.span "expected binding name"
  in
  let params = ref [] in
  while
    match (current p).Token.kind with
    | Token.Ident _ | Token.LParen -> true
    | _ -> false
  do
    match (current p).Token.kind with
    | Token.Ident n ->
        let tok = advance p in
        params :=
          {
            Ast.param_name = Ident.Intern.intern n;
            param_ty = None;
            param_span = tok.Token.span;
          }
          :: !params
    | Token.LParen ->
        ignore (advance p);
        let ntok = advance p in
        let pname =
          match ntok.Token.kind with
          | Token.Ident n -> Ident.Intern.intern n
          | _ -> fail ntok.Token.span "expected parameter name"
        in
        let pty =
          if consume_kind p Token.Colon then Some (parse_ty p) else None
        in
        ignore (expect_kind p Token.RParen);
        params :=
          {
            Ast.param_name = pname;
            param_ty = pty;
            param_span = ntok.Token.span;
          }
          :: !params
    | _ -> ()
  done;
  let ann = if consume_kind p Token.Colon then Some (parse_ty p) else None in
  ignore (expect_kind p Token.Equal);
  let body = parse_expr p in
  {
    Ast.lb_name = name;
    lb_params = List.rev !params;
    lb_ty = ann;
    lb_body = body;
    lb_span = Span.merge start.Token.span body.Ast.expr_span;
    lb_rec = is_rec;
  }

let parse_type_def p =
  let start = expect_kw p Token.Kw_type in
  let name_tok = advance p in
  let name =
    match name_tok.Token.kind with
    | Token.Ident n | Token.Ctor n -> Ident.Intern.intern n
    | _ -> fail name_tok.Token.span "expected type name"
  in
  let params = ref [] in
  while
    match (current p).Token.kind with
    | Token.Ident n when String.length n > 0 && n.[0] = '\'' -> true
    | _ -> false
  do
    let tok = advance p in
    match tok.Token.kind with
    | Token.Ident n -> params := Ident.Intern.intern n :: !params
    | _ -> ()
  done;
  ignore (expect_kind p Token.Equal);
  ignore (consume_kind p Token.Pipe);
  let parse_ctor () =
    let tok = advance p in
    let cname =
      match tok.Token.kind with
      | Token.Ctor n | Token.Ident n -> Ident.Intern.intern n
      | _ -> fail tok.Token.span "expected constructor"
    in
    let args = ref [] in
    if consume_kw p Token.Kw_of then (
      args := parse_ty p :: !args;
      let cont = ref true in
      while !cont do
        match (current p).Token.kind with
        | Token.Binop Token.Op_mul | Token.Comma ->
            ignore (advance p);
            args := parse_ty p :: !args
        | _ -> cont := false
      done);
    {
      Ast.ctor_name = cname;
      ctor_args = List.rev !args;
      ctor_span = tok.Token.span;
    }
  in
  let ctors = ref [ parse_ctor () ] in
  while consume_kind p Token.Pipe do
    ctors := parse_ctor () :: !ctors
  done;
  {
    Ast.td_name = name;
    td_params = List.rev !params;
    td_ctors = List.rev !ctors;
    td_span = Span.merge start.Token.span name_tok.Token.span;
  }

let parse_item p =
  match (current p).Token.kind with
  | Token.Keyword Token.Kw_type -> Ast.Item_type (parse_type_def p)
  | Token.Keyword Token.Kw_let ->
      let lb = parse_let_binding p in
      if lb.Ast.lb_params <> [] then Ast.Item_fn lb else Ast.Item_let lb
  | Token.Keyword (Token.Kw_external | Token.Kw_extern) ->
      let start = advance p in
      let name_tok = advance p in
      let name =
        match name_tok.Token.kind with
        | Token.Ident n -> Ident.Intern.intern n
        | _ -> fail name_tok.Token.span "expected extern name"
      in
      ignore (expect_kind p Token.Colon);
      let ty = parse_ty p in
      let rec peel acc t =
        match t.Ast.ty_desc with
        | Ast.Ty_arrow (a, b) -> peel (a :: acc) b
        | _ -> (List.rev acc, t)
      in
      let params, ret = peel [] ty in
      Ast.Item_extern
        {
          Ast.ext_name = name;
          ext_params = params;
          ext_ret = ret;
          ext_span = Span.merge start.Token.span ty.Ast.ty_span;
        }
  | _ ->
      fail (current p).Token.span
        (Printf.sprintf "expected top-level item, found %s"
           (Token.kind_to_string (current p).Token.kind))

let parse_program_tokens ~file ~source tokens =
  let p = create ~file ~source tokens in
  let items = ref [] in
  while not (at_end p) do
    while consume_kind p Token.Semicolon do
      ()
    done;
    if not (at_end p) then items := parse_item p :: !items
  done;
  let items = List.rev !items in
  let span =
    match items with
    | [] -> Span.dummy
    | xs -> Span.merge_list (List.map Ast.span_of_item xs)
  in
  { Ast.items; span }

let wrap f = try Ok (f ()) with Error e -> Error e

let parse_program ?(file = "<input>") source =
  wrap (fun () ->
      match Lexer.tokenize ~filename:file ~source () with
      | Error diags ->
          let d =
            match diags with
            | x :: _ -> x
            | [] -> Diagnostic.error Span.dummy "lex error"
          in
          raise
            (Error { message = d.Diagnostic.message; span = d.Diagnostic.span })
      | Ok tokens -> parse_program_tokens ~file ~source tokens)

let error_to_diagnostic (e : error) = Diagnostic.error e.span e.message
