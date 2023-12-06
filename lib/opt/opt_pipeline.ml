(** Register the standard O2 optimization pipeline with [Pass_manager]. *)

let () =
  Pass_manager.register_standard
    [
      Simplify_cfg.pass;
      Const_prop.pass;
      Copy_prop.pass;
      Sccp.pass;
      Cse.pass;
      Dce.pass;
      Inliner.pass;
      Simplify_cfg.pass;
      Const_prop.pass;
      Dce.pass;
      Simplify_cfg.pass;
    ]
