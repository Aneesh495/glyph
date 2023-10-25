(** Optimization pass manager. *)

type level =
  | O0
  | O1
  | O2
  | O3

type stats = {
  mutable rewritten : int;
  mutable removed : int;
  mutable inlined : int;
}

let empty_stats () = { rewritten = 0; removed = 0; inlined = 0 }

type context = {
  level : level;
  mutable stats : stats;
  verbose : bool;
}

let make_context ?(verbose = false) level =
  { level; stats = empty_stats (); verbose }

type pass = {
  name : string;
  run_func : context -> Mir.func -> Mir.func;
  run_program : context -> Mir.program -> Mir.program;
}

let make ~name ?(run_func = fun _ f -> f)
    ?(run_program =
      fun ctx prog ->
        {
          prog with
          Mir.functions = List.map (fun f -> ctx |> fun c -> f) prog.functions;
        }) () =
  (* Default run_program maps run_func over functions — fixed below. *)
  ignore run_program;
  let run_program ctx prog =
    { prog with Mir.functions = List.map (run_func ctx) prog.functions }
  in
  { name; run_func; run_program }

let make_func_pass ~name run_func =
  {
    name;
    run_func;
    run_program =
      (fun ctx prog ->
        { prog with Mir.functions = List.map (run_func ctx) prog.functions });
  }

let make_prog_pass ~name run_program =
  {
    name;
    run_func = (fun _ f -> f);
    run_program;
  }

let run_pass ctx pass prog =
  if ctx.verbose then
    Format.eprintf "[opt] running %s@." pass.name;
  pass.run_program ctx prog

let run_pipeline ctx passes prog =
  List.fold_left (run_pass ctx) prog passes

let level_to_string = function
  | O0 -> "O0"
  | O1 -> "O1"
  | O2 -> "O2"
  | O3 -> "O3"
