(** Pretty-printer for Glyph ASTs. *)

let program_to_string (prog : Ast.program) =
  let buf = Buffer.create 256 in
  let pf fmt = Printf.bprintf buf fmt in
  List.iter
    (function
      | Ast.Item_fn lb | Ast.Item_let lb ->
          pf "%s%s %s" 
            (if lb.lb_rec then "let rec " else "let ")
            (Ident.to_string lb.lb_name)
            (String.concat " "
               (List.map
                  (fun (p : Ast.param) -> Ident.to_string p.param_name)
                  lb.lb_params));
          pf " = <expr>\n"
      | Ast.Item_type td ->
          pf "type %s = <variants>\n" (Ident.to_string td.td_name)
      | Ast.Item_extern ext ->
          pf "external %s : <ty>\n" (Ident.to_string ext.ext_name))
    prog.Ast.items;
  Buffer.contents buf
