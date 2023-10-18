(** Hand-written Pratt / recursive-descent parser for Glyph. *)

type error = {
  message : string;
  span : Span.t;
  hint : string option;
}

exception Error of error

type parser = {
  tokens : Token.t array;
  mutable index : int;
  file : string;
  source : string;
}

let error_to_diagnostic (e : error) =
  let d = Diagnostic.error e.span e.message in
  match e.hint with
  | None -> d
  | Some h -> Diagnostic.with_help d h

let make_error ?hint span message = { message; span; hint }

let fail ?hint span message = raise (Error (make_error ?hint span message))

let create ~file ~source tokens =
  { tokens; index = 0; file; source }

let current p =
  if p.index >= Array.length p.tokens then
    let last = p.tokens.(Array.length p.tokens - 1) in
    last
  else p.tokens.(p.index)

let at_end p = Token.is_eof (current p)

let advance p =
  let tok = current p in
  if not (Token.is_eof tok) then p.index <- p.index + 1;
  tok

let check p kind =
  let tok = current p in
  match (tok.kind, kind) with
  | k, k' when k = k' -> true
  | _ -> false

let check_kw p kw =
  match (current p).kind with
  | Token.Keyword kw' when kw' = kw -> true
  | _ -> false

let expect p kind ~msg =
  let tok = current p in
  if tok.kind = kind then advance p
  else
    fail tok.span
      (Printf.sprintf "%s (found %s)" msg (Token.string_of_kind tok.kind))

let expect_kw p kw =
  let tok = current p in
  match tok.kind with
  | Token.Keyword kw' when kw' = kw -> advance p
  | _ ->
      fail tok.span
        (Printf.sprintf "expected keyword '%s' (found %s)"
           (Token.string_of_keyword kw)
           (Token.string_of_kind tok.kind))

let span_merge a b = Span.merge a b

(* ----- Precedence table (Pratt) -----
   Higher binding power = tighter binding.
   pipe     10  left
   or       20  left
   and      30  left
   cmp      40  left
   cons     50  right
   add      60  left
   mul      70  left
   unary    80  prefix
   call/field 90 postfix
*)

let infix_binding = function
  | Token.Binop Token.Op_pipe -> Some (10, 11)
  | Token.Binop Token.Op_or -> Some (20, 21)
  | Token.Binop Token.Op_and -> Some (30, 31)
  | Token.Binop
      ( Token.Op_eq | Token.Op_neq | Token.Op_lt | Token.Op_le | Token.Op_gt
      | Token.Op_ge ) ->
      Some (40, 41)
  | Token.Binop Token.Op_cons -> Some (51, 50) (* right-assoc *)
  | Token.Binop (Token.Op_add | Token.Op_sub) -> Some (60, 61)
  | Token.Binop (Token.Op_mul | Token.Op_div | Token.Op_mod) -> Some (70, 71)
  | _ -> None

let prefix_binding = function
  | Token.Binop Token.Op_sub -> Some 80
  | Token.Keyword Token.Kw_not -> Some 80
  | _ -> None

let ident_of_token tok =
  match tok.Token.kind with
  | Token.Ident s | Token.Ctor s -> Ident.Intern.intern s
  | Token.Underscore -> Ident.Predef.underscore
  | _ ->
      fail tok.span
        (Printf.sprintf "expected identifier (found %s)"
           (Token.string_of_kind tok.kind))

let expect_ident p =
  let tok = current p in
  match tok.kind with
  | Token.Ident s ->
      ignore (advance p);
      Ident.Intern.intern s
  | Token.Underscore ->
      ignore (advance p);
      Ident.Predef.underscore
  | _ ->
      fail tok.span
        (Printf.sprintf "expected identifier (found %s)"
           (Token.string_of_kind tok.kind))

let expect_ctor p =
  let tok = current p in
  match tok.kind with
  | Token.Ctor s ->
      ignore (advance p);
      Ident.Intern.intern s
  | _ ->
      fail tok.span
        (Printf.sprintf "expected constructor name (found %s)"
           (Token.string_of_kind tok.kind))

let expect_name p =
  let tok = current p in
  match tok.kind with
  | Token.Ident s | Token.Ctor s ->
      ignore (advance p);
      Ident.Intern.intern s
  | _ ->
      fail tok.span
        (Printf.sprintf "expected name (found %s)"
           (Token.string_of_kind tok.kind))

(* Forward decls via let rec *)
let rec parse_ty p = parse_ty_arrow p

and parse_ty_atom p =
  let tok = current p in
  match tok.kind with
  | Token.LParen ->
      ignore (advance p);
      if check p Token.RParen then (
        ignore (advance p);
        Ast.ty Ast.Ty_unit (span_merge tok.span (current p).span))
      else
        let first = parse_ty p in
        if check p Token.Comma then (
          let rest = parse_ty_list_comma p in
          ignore (expect p Token.RParen ~msg:"expected ')' after tuple type");
          let span = span_merge tok.span (current p).span in
          Ast.ty (Ast.Ty_tuple (first :: rest)) span)
        else (
          ignore (expect p Token.RParen ~msg:"expected ')' after type");
          first)
  | Token.Underscore ->
      ignore (advance p);
      Ast.ty Ast.Ty_hole tok.span
  | Token.Ident s ->
      ignore (advance p);
      Ast.ty (Ast.Ty_var (Ident.Intern.intern s)) tok.span
  | Token.Ctor s ->
      ignore (advance p);
      let name = Ident.Intern.intern s in
      let args, end_span =
        if check p Token.LBracket then (
          ignore (advance p);
          let args =
            if check p Token.RBracket then []
            else
              let a = parse_ty p in
              a :: parse_ty_list_comma p
          in
          let close = expect p Token.RBracket ~msg:"expected ']' after type arguments" in
          (args, close.span))
        else ([], tok.span)
      in
      Ast.ty (Ast.Ty_named (name, args)) (span_merge tok.span end_span)
  | _ ->
      fail tok.span
        (Printf.sprintf "expected type (found %s)"
           (Token.string_of_kind tok.kind))

and parse_ty_list_comma p =
  if not (check p Token.Comma) then []
  else (
    ignore (advance p);
    let t = parse_ty p in
    t :: parse_ty_list_comma p)

and parse_ty_arrow p =
  let left = parse_ty_atom p in
  if check p Token.Arrow then (
    ignore (advance p);
    let right = parse_ty_arrow p in
    Ast.ty (Ast.Ty_arrow (left, right))
      (span_merge left.ty_span right.ty_span))
  else left

let rec parse_pat p = parse_pat_or p

and parse_pat_or p =
  let left = parse_pat_as p in
  if check p Token.Pipe then (
    (* Careful: '|' starts match arms; only treat as or-pattern inside
       grouped patterns. Here we allow Pat_or when pipe appears mid-pattern
       after consuming a primary — callers in match arms use parse_pat_primary
       paths differently. For simplicity, support `p | q` generally. *)
    ignore (advance p);
    let right = parse_pat_or p in
    Ast.pat (Ast.Pat_or (left, right)) (span_merge left.pat_span right.pat_span))
  else left

and parse_pat_as p =
  let left = parse_pat_annot p in
  match (current p).kind with
  | Token.Ident "as" ->
      (* 'as' is not a keyword in our set — treat as ident conflict.
         Use keyword-less: we don't have as-keyword. Spec says as-patterns —
         add support via reserved word check. *)
      ignore (advance p);
      let name = expect_ident p in
      Ast.pat (Ast.Pat_as (left, name)) (span_merge left.pat_span (current p).span)
  | _ -> left

and parse_pat_annot p =
  let left = parse_pat_ctor p in
  if check p Token.Colon then (
    ignore (advance p);
    let ty = parse_ty p in
    Ast.pat (Ast.Pat_annotate (left, ty))
      (span_merge left.pat_span ty.ty_span))
  else left

and parse_pat_ctor p =
  let tok = current p in
  match tok.kind with
  | Token.Ctor _ ->
      let name = expect_ctor p in
      if check p Token.LParen then (
        ignore (advance p);
        let args =
          if check p Token.RParen then []
          else
            let a = parse_pat p in
            a :: parse_pat_list_comma p
        in
        let close = expect p Token.RParen ~msg:"expected ')' after constructor pattern" in
        Ast.pat (Ast.Pat_ctor (name, args)) (span_merge tok.span close.span))
      else Ast.pat (Ast.Pat_ctor (name, [])) tok.span
  | _ -> parse_pat_atom p

and parse_pat_list_comma p =
  if not (check p Token.Comma) then []
  else (
    ignore (advance p);
    let a = parse_pat p in
    a :: parse_pat_list_comma p)

and parse_pat_atom p =
  let tok = current p in
  match tok.kind with
  | Token.Underscore ->
      ignore (advance p);
      Ast.pat Ast.Pat_wild tok.span
  | Token.Ident _ ->
      let id = expect_ident p in
      Ast.pat (Ast.Pat_var id) tok.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_bool true)) tok.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_bool false)) tok.span
  | Token.Int n ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_int n)) tok.span
  | Token.Float f ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_float f)) tok.span
  | Token.String s ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_string s)) tok.span
  | Token.Char c ->
      ignore (advance p);
      Ast.pat (Ast.Pat_lit (Ast.Lit_char c)) tok.span
  | Token.LParen ->
      ignore (advance p);
      if check p Token.RParen then (
        let close = advance p in
        Ast.pat (Ast.Pat_lit Ast.Lit_unit) (span_merge tok.span close.span))
      else
        let first = parse_pat p in
        if check p Token.Comma then (
          let rest = parse_pat_list_comma p in
          let close =
            expect p Token.RParen ~msg:"expected ')' after tuple pattern"
          in
          Ast.pat (Ast.Pat_tuple (first :: rest))
            (span_merge tok.span close.span))
        else (
          ignore (expect p Token.RParen ~msg:"expected ')' after pattern");
          first)
  | _ ->
      fail tok.span
        (Printf.sprintf "expected pattern (found %s)"
           (Token.string_of_kind tok.kind))

(* Fix as-pattern: 'as' should be recognized. Currently Ident "as" works if
   user writes `x as y` — but `as` isn't a keyword so it lexes as Ident.
   Good. *)

let parse_param_inner p =
  let tok = current p in
  let name = expect_ident p in
  let ty =
    if check p Token.Colon then (
      ignore (advance p);
      Some (parse_ty p))
    else None
  in
  let span = span_merge tok.span (current p).span in
  ({ Ast.param_name = name; param_ty = ty; param_span = span } : Ast.param)

let parse_param_list p =
  ignore (expect p Token.LParen ~msg:"expected '(' after function name");
  let params =
    if check p Token.RParen then []
    else
      let first = parse_param_inner p in
      let rec rest acc =
        if check p Token.Comma then (
          ignore (advance p);
          rest (parse_param_inner p :: acc))
        else List.rev acc
      in
      rest [ first ]
  in
  ignore (expect p Token.RParen ~msg:"expected ')' after parameters");
  params

let rec parse_expr p = parse_expr_bp p 0

and parse_expr_bp p min_bp =
  let left = parse_prefix p in
  parse_infix p left min_bp

and parse_prefix p =
  let tok = current p in
  match prefix_binding tok.kind with
  | Some bp -> (
      ignore (advance p);
      let rhs = parse_expr_bp p bp in
      match tok.kind with
      | Token.Binop Token.Op_sub ->
          Ast.expr (Ast.Expr_un (Token.Op_neg, rhs))
            (span_merge tok.span rhs.expr_span)
      | Token.Keyword Token.Kw_not ->
          Ast.expr (Ast.Expr_un (Token.Op_not, rhs))
            (span_merge tok.span rhs.expr_span)
      | _ -> fail tok.span "invalid prefix operator")
  | None -> parse_postfix p (parse_atom p)

and parse_infix p left min_bp =
  let tok = current p in
  match infix_binding tok.kind with
  | Some (l_bp, r_bp) when l_bp >= min_bp ->
      ignore (advance p);
      let right = parse_expr_bp p r_bp in
      let left =
        match tok.kind with
        | Token.Binop Token.Op_pipe ->
            Ast.expr (Ast.Expr_pipe (left, right))
              (span_merge left.expr_span right.expr_span)
        | Token.Binop op ->
            Ast.expr (Ast.Expr_bin (op, left, right))
              (span_merge left.expr_span right.expr_span)
        | _ -> fail tok.span "expected binary operator"
      in
      parse_infix p left min_bp
  | _ -> left

and parse_postfix p left =
  let tok = current p in
  match tok.kind with
  | Token.LParen ->
      ignore (advance p);
      let args =
        if check p Token.RParen then []
        else
          let a = parse_expr p in
          let rec rest acc =
            if check p Token.Comma then (
              ignore (advance p);
              rest (parse_expr p :: acc))
            else List.rev acc
          in
          rest [ a ]
      in
      let close = expect p Token.RParen ~msg:"expected ')' after arguments" in
      let app =
        Ast.expr (Ast.Expr_app (left, args))
          (span_merge left.expr_span close.span)
      in
      parse_postfix p app
  | Token.Dot ->
      ignore (advance p);
      let field = expect_ident p in
      let e =
        Ast.expr (Ast.Expr_field (left, field))
          (span_merge left.expr_span (current p).span)
      in
      parse_postfix p e
  | Token.Colon ->
      ignore (advance p);
      let ty = parse_ty p in
      Ast.expr (Ast.Expr_annotate (left, ty))
        (span_merge left.expr_span ty.ty_span)
  | _ -> left

and parse_atom p =
  let tok = current p in
  match tok.kind with
  | Token.Int n ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_int n)) tok.span
  | Token.Float f ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_float f)) tok.span
  | Token.String s ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_string s)) tok.span
  | Token.Char c ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_char c)) tok.span
  | Token.Keyword Token.Kw_true ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_bool true)) tok.span
  | Token.Keyword Token.Kw_false ->
      ignore (advance p);
      Ast.expr (Ast.Expr_lit (Ast.Lit_bool false)) tok.span
  | Token.Ident _ ->
      let id = expect_ident p in
      Ast.expr (Ast.Expr_var id) tok.span
  | Token.Ctor _ ->
      let id = expect_ctor p in
      Ast.expr (Ast.Expr_ctor id) tok.span
  | Token.LParen -> parse_paren_expr p
  | Token.LBrace -> parse_block_or_record p
  | Token.Keyword Token.Kw_if -> parse_if p
  | Token.Keyword Token.Kw_match -> parse_match p
  | Token.Keyword Token.Kw_let -> parse_let_expr p
  | Token.Keyword Token.Kw_fn -> parse_lambda p
  | _ ->
      fail tok.span
        (Printf.sprintf "expected expression (found %s)"
           (Token.string_of_kind tok.kind))

and parse_paren_expr p =
  let start = current p in
  ignore (advance p);
  (* consume '(' *)
  if check p Token.RParen then (
    let close = advance p in
    Ast.expr (Ast.Expr_lit Ast.Lit_unit) (span_merge start.span close.span))
  else
    let first = parse_expr p in
    if check p Token.Comma then (
      ignore (advance p);
      let second = parse_expr p in
      let rec rest acc =
        if check p Token.Comma then (
          ignore (advance p);
          rest (parse_expr p :: acc))
        else List.rev acc
      in
      let elems = first :: second :: rest [] in
      let close = expect p Token.RParen ~msg:"expected ')' after tuple" in
      Ast.expr (Ast.Expr_tuple elems) (span_merge start.span close.span))
    else (
      ignore (expect p Token.RParen ~msg:"expected ')'");
      first)

and parse_block_or_record p =
  let start = current p in
  ignore (advance p);
  (* '{' *)
  if check p Token.RBrace then (
    let close = advance p in
    Ast.expr (Ast.Expr_block []) (span_merge start.span close.span))
  else
    (* Lookahead: ident '=' => record; else block of exprs *)
    let tok = current p in
    match tok.kind with
    | Token.Ident _ when
        p.index + 1 < Array.length p.tokens
        && p.tokens.(p.index + 1).kind = Token.Equal ->
        let fields = parse_record_fields p in
        let close = expect p Token.RBrace ~msg:"expected '}' after record" in
        Ast.expr (Ast.Expr_record fields) (span_merge start.span close.span)
    | _ ->
        let exprs = ref [ parse_expr p ] in
        while check p Token.Semicolon do
          ignore (advance p);
          if not (check p Token.RBrace) then
            exprs := parse_expr p :: !exprs
        done;
        let close = expect p Token.RBrace ~msg:"expected '}' after block" in
        Ast.expr
          (Ast.Expr_block (List.rev !exprs))
          (span_merge start.span close.span)

and parse_record_fields p =
  let rec loop acc =
    let name = expect_ident p in
    ignore (expect p Token.Equal ~msg:"expected '=' in record field");
    let value = parse_expr p in
    let acc = (name, value) :: acc in
    if check p Token.Comma then (
      ignore (advance p);
      if check p Token.RBrace then List.rev acc else loop acc)
    else List.rev acc
  in
  loop []

and parse_if p =
  let start = expect_kw p Token.Kw_if in
  let cond = parse_expr p in
  ignore (expect_kw p Token.Kw_then);
  let then_ = parse_expr p in
  ignore (expect_kw p Token.Kw_else);
  let else_ = parse_expr p in
  Ast.expr
    (Ast.Expr_if (cond, then_, else_))
    (span_merge start.span else_.expr_span)

and parse_match p =
  let start = expect_kw p Token.Kw_match in
  let scrut = parse_expr p in
  ignore (expect_kw p Token.Kw_with);
  let cases = parse_cases p in
  let span =
    match cases with
    | [] -> start.span
    | cs ->
        let last = List.hd (List.rev cs) in
        span_merge start.span last.case_span
  in
  Ast.expr (Ast.Expr_match (scrut, cases)) span

and parse_cases p =
  let rec loop acc =
    if check p Token.Pipe then (
      ignore (advance p);
      let case = parse_case p in
      loop (case :: acc))
    else List.rev acc
  in
  (* Allow optional leading pipe *)
  if check p Token.Pipe then loop []
  else
    let case = parse_case p in
    loop [ case ]

and parse_case p =
  let start = current p in
  (* Don't use parse_pat_or here because '|' separates cases.
     Parse a single alternative, then allow `as` / annot / ctor. *)
  let pat = parse_case_pat p in
  let guard =
    if check_kw p Token.Kw_if then (
      ignore (advance p);
      Some (parse_expr p))
    else None
  in
  ignore (expect p Token.Arrow ~msg:"expected '->' in match arm");
  let body = parse_expr p in
  {
    Ast.case_pat = pat;
    case_guard = guard;
    case_body = body;
    case_span = span_merge start.span body.expr_span;
  }

and parse_case_pat p =
  (* like parse_pat but or-patterns use explicit nested parens or
     consecutive pats without consuming case-separating pipes at start *)
  let left = parse_pat_as_no_or p in
  (* Support nested or via (p | q) only — top-level | starts new case.
     For Cons(h, t) patterns we're fine. *)
  left

and parse_pat_as_no_or p =
  let left = parse_pat_annot_no_or p in
  match (current p).kind with
  | Token.Ident s when s = "as" ->
      ignore (advance p);
      let name = expect_ident p in
      Ast.pat (Ast.Pat_as (left, name))
        (span_merge left.pat_span (current p).span)
  | _ -> left

and parse_pat_annot_no_or p =
  let left = parse_pat_ctor p in
  if check p Token.Colon then (
    ignore (advance p);
    let ty = parse_ty p in
    Ast.pat (Ast.Pat_annotate (left, ty))
      (span_merge left.pat_span ty.ty_span))
  else left

and parse_let_expr p =
  let start = expect_kw p Token.Kw_let in
  let is_rec = check_kw p Token.Kw_rec in
  if is_rec then ignore (advance p);
  if is_rec then
    let bindings = parse_rec_bindings p in
    ignore (expect_kw p Token.Kw_in);
    let body = parse_expr p in
    Ast.expr
      (Ast.Expr_let_rec (bindings, body))
      (span_merge start.span body.expr_span)
  else
    let binding = parse_let_binding p ~is_rec:false in
    ignore (expect_kw p Token.Kw_in);
    let body = parse_expr p in
    Ast.expr
      (Ast.Expr_let (binding, body))
      (span_merge start.span body.expr_span)

and parse_rec_bindings p =
  let first = parse_let_binding p ~is_rec:true in
  let rec loop acc =
    if check_kw p Token.Kw_and then (
      ignore (advance p);
      loop (parse_let_binding p ~is_rec:true :: acc))
    else List.rev acc
  in
  loop [ first ]

and parse_let_binding p ~is_rec =
  let start = current p in
  let name = expect_ident p in
  let params =
    if check p Token.LParen then parse_param_list p else []
  in
  let ty =
    if check p Token.Colon then (
      ignore (advance p);
      Some (parse_ty p))
    else None
  in
  ignore (expect p Token.Equal ~msg:"expected '=' in let binding");
  let body = parse_expr p in
  {
    Ast.lb_name = name;
    lb_params = params;
    lb_ty = ty;
    lb_body = body;
    lb_span = span_merge start.span body.expr_span;
    lb_rec = is_rec;
  }

and parse_lambda p =
  let start = expect_kw p Token.Kw_fn in
  let params = parse_param_list p in
  ignore (expect p Token.Arrow ~msg:"expected '->' in lambda");
  let body = parse_expr p in
  Ast.expr
    (Ast.Expr_lambda (params, body))
    (span_merge start.span body.expr_span)

let parse_fn_item p =
  let start = expect_kw p Token.Kw_fn in
  let name = expect_ident p in
  let params = parse_param_list p in
  let ty =
    if check p Token.Arrow then (
      ignore (advance p);
      Some (parse_ty p))
    else if check p Token.Colon then (
      ignore (advance p);
      Some (parse_ty p))
    else None
  in
  ignore (expect p Token.Equal ~msg:"expected '=' after function signature");
  let body = parse_expr p in
  let lb =
    {
      Ast.lb_name = name;
      lb_params = params;
      lb_ty = ty;
      lb_body = body;
      lb_span = span_merge start.span body.expr_span;
      lb_rec = true;
      (* top-level fns are recursive by default *)
    }
  in
  Ast.Item_fn lb

let parse_type_item p =
  let start = expect_kw p Token.Kw_type in
  let name = expect_ctor p in
  let params =
    if check p Token.LBracket then (
      ignore (advance p);
      let ps =
        if check p Token.RBracket then []
        else
          let a = expect_ident p in
          let rec rest acc =
            if check p Token.Comma then (
              ignore (advance p);
              rest (expect_ident p :: acc))
            else List.rev acc
          in
          rest [ a ]
      in
      ignore (expect p Token.RBracket ~msg:"expected ']' after type parameters");
      ps)
    else []
  in
  ignore (expect p Token.Equal ~msg:"expected '=' in type definition");
  let parse_ctor () =
    let cstart = current p in
    let cname = expect_ctor p in
    let args =
      if check p Token.LParen then (
        ignore (advance p);
        let args =
          if check p Token.RParen then []
          else
            let a = parse_ty p in
            a :: parse_ty_list_comma p
        in
        ignore (expect p Token.RParen ~msg:"expected ')' after constructor fields");
        args)
      else []
    in
    {
      Ast.ctor_name = cname;
      ctor_args = args;
      ctor_span = span_merge cstart.span (current p).span;
    }
  in
  (* Optional leading | *)
  if check p Token.Pipe then ignore (advance p);
  let first = parse_ctor () in
  let rec rest acc =
    if check p Token.Pipe then (
      ignore (advance p);
      rest (parse_ctor () :: acc))
    else List.rev acc
  in
  let ctors = rest [ first ] in
  let td =
    {
      Ast.td_name = name;
      td_params = params;
      td_ctors = ctors;
      td_span = span_merge start.span (current p).span;
    }
  in
  Ast.Item_type td

let parse_let_item p =
  let start = expect_kw p Token.Kw_let in
  let is_rec = check_kw p Token.Kw_rec in
  if is_rec then ignore (advance p);
  let binding = parse_let_binding p ~is_rec in
  let binding =
    { binding with lb_span = span_merge start.span binding.lb_span }
  in
  Ast.Item_let binding

let parse_extern_item p =
  let start = expect_kw p Token.Kw_extern in
  ignore (expect_kw p Token.Kw_fn);
  let name = expect_ident p in
  ignore (expect p Token.LParen ~msg:"expected '(' in extern decl");
  let params =
    if check p Token.RParen then []
    else
      let a = parse_ty p in
      a :: parse_ty_list_comma p
  in
  ignore (expect p Token.RParen ~msg:"expected ')' in extern decl");
  ignore (expect p Token.Arrow ~msg:"expected '->' in extern decl");
  let ret = parse_ty p in
  Ast.Item_extern
    {
      ext_name = name;
      ext_params = params;
      ext_ret = ret;
      ext_span = span_merge start.span ret.ty_span;
    }

let parse_item p =
  let tok = current p in
  match tok.kind with
  | Token.Keyword Token.Kw_fn -> parse_fn_item p
  | Token.Keyword Token.Kw_type -> parse_type_item p
  | Token.Keyword Token.Kw_let -> parse_let_item p
  | Token.Keyword Token.Kw_extern -> parse_extern_item p
  | _ ->
      fail tok.span
        (Printf.sprintf
           "expected top-level item (fn, type, let, extern); found %s"
           (Token.string_of_kind tok.kind))

let parse_program_tokens ~file ~source tokens =
  let p = create ~file ~source tokens in
  let items = ref [] in
  while not (at_end p) do
    items := parse_item p :: !items
  done;
  let items = List.rev !items in
  let span =
    match items with
    | [] -> Span.dummy
    | _ ->
        Span.merge_list (List.map Ast.span_of_item items)
  in
  ({ Ast.items; span } : Ast.program)

let wrap f =
  try Ok (f ()) with
  | Error e -> Error e
  | Lexer.Error e ->
      Error { message = e.message; span = e.span; hint = None }

let parse_program ?(file = "<input>") source =
  wrap (fun () ->
      let tokens = Lexer.tokenize_exn ~file source in
      parse_program_tokens ~file ~source tokens)

let parse_program_exn ?(file = "<input>") source =
  match parse_program ~file source with
  | Ok p -> p
  | Error e -> raise (Error e)

let parse_expr ?(file = "<input>") source =
  wrap (fun () ->
      let tokens = Lexer.tokenize_exn ~file source in
      let p = create ~file ~source tokens in
      let e = parse_expr p in
      if not (at_end p) then
        fail (current p).span "unexpected tokens after expression";
      e)

let parse_ty ?(file = "<input>") source =
  wrap (fun () ->
      let tokens = Lexer.tokenize_exn ~file source in
      let p = create ~file ~source tokens in
      let t = parse_ty p in
      if not (at_end p) then
        fail (current p).span "unexpected tokens after type";
      t)
