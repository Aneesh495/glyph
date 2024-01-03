(** Register-based bytecode interpreter. *)

type frame = {
  mutable regs : Value.t array;
  mutable ip : int;
  fn_id : int;
  caller_dst : int option;
}

type vm = {
  chunk : Chunk.t;
  heap : Heap.t;
  mutable frames : frame list;
  mutable globals : Value.t array;
  mutable halted : bool;
  mutable exit_value : Value.t;
}

type result =
  | Ok of Value.t
  | Runtime_error of string

let pp_frame fmt (f : frame) =
  Format.fprintf fmt "fn%d ip=%d regs=%d" f.fn_id f.ip (Array.length f.regs)

let current_frame vm =
  match vm.frames with
  | f :: _ -> Some f
  | [] -> None

let const_to_value heap = function
  | Chunk.CInt i -> Value.Int i
  | Chunk.CFloat f -> Value.Float f
  | Chunk.CBool b -> Value.Bool b
  | Chunk.CChar c -> Value.Int (Char.code c)
  | Chunk.CUnit -> Value.Unit
  | Chunk.CString s -> Heap.alloc_string heap s

let reg (f : frame) i =
  if i < 0 || i >= Array.length f.regs then
    failwith (Printf.sprintf "bad register r%d" i);
  f.regs.(i)

let set_reg (f : frame) i v = f.regs.(i) <- v

let read_args f extra =
  Array.map (fun r -> reg f r) extra |> Array.to_list

let resolve_heap vm v =
  match v with
  | Value.Ptr _ -> Heap.resolve vm.heap v
  | other -> other

let get_tag vm v =
  match resolve_heap vm v with
  | Value.Adt (tag, _) -> tag
  | Value.Tuple _ -> 0
  | Value.Bool false -> 0
  | Value.Bool true -> 1
  | Value.Unit -> 0
  | Value.Closure _ -> -1
  | other -> failwith ("get_tag: " ^ Value.to_string other)

let get_fields vm v =
  match resolve_heap vm v with
  | Value.Tuple xs | Value.Adt (_, xs) | Value.Closure (_, xs) -> xs
  | other -> failwith ("get_fields: " ^ Value.to_string other)

let bin_int op a b =
  match (a, b) with
  | Value.Int x, Value.Int y -> Value.Int (op x y)
  | _ ->
      failwith
        (Printf.sprintf "int binop on %s, %s" (Value.to_string a)
           (Value.to_string b))

let bin_float op a b =
  Value.Float (op (Value.as_float a) (Value.as_float b))

let cmp_int op a b =
  Value.Bool (op (Value.as_int a) (Value.as_int b))

let cmp_float op a b =
  Value.Bool (op (Value.as_float a) (Value.as_float b))

let push_frame vm ~fn_id ~nregs ~entry ~args ~caller_dst =
  let regs = Array.make nregs Value.Unit in
  List.iteri (fun i v -> if i < nregs then regs.(i) <- v) args;
  let frame = { regs; ip = entry; fn_id; caller_dst } in
  vm.frames <- frame :: vm.frames

let pop_frame vm =
  match vm.frames with
  | _ :: rest -> vm.frames <- rest
  | [] -> failwith "pop_frame: empty"

let find_proto chunk fn_id = Chunk.find_func chunk fn_id

let call_function vm ~dst ~fn_id ~args =
  let proto = find_proto vm.chunk fn_id in
  if List.length args <> proto.arity then
    failwith
      (Printf.sprintf "call fn%d: arity %d, got %d" fn_id proto.arity
         (List.length args));
  push_frame vm ~fn_id ~nregs:proto.nregs ~entry:proto.entry ~args
    ~caller_dst:(Some dst)

let call_closure vm ~dst ~clo ~args =
  match resolve_heap vm clo with
  | Value.Closure (fn_id, env) ->
      let args' = Array.to_list env @ args in
      call_function vm ~dst ~fn_id ~args:args'
  | Value.Native (name, fn) ->
      let f =
        match current_frame vm with
        | Some fr -> fr
        | None -> failwith "call_closure: no frame"
      in
      set_reg f dst (fn args)
  | v -> failwith ("call_closure: not a closure: " ^ Value.to_string v)

let return_to_caller vm ret_val =
  match vm.frames with
  | callee :: caller :: rest ->
      vm.frames <- caller :: rest;
      (match callee.caller_dst with
      | Some dst -> set_reg caller dst ret_val
      | None -> ());
      caller.ip <- caller.ip (* already pointing past call *)
  | [ _ ] ->
      vm.frames <- [];
      vm.halted <- true;
      vm.exit_value <- ret_val
  | [] ->
      vm.halted <- true;
      vm.exit_value <- ret_val

let tail_call vm ~fn_id ~args =
  let proto = find_proto vm.chunk fn_id in
  match vm.frames with
  | frame :: rest ->
      let regs = Array.make proto.nregs Value.Unit in
      List.iteri (fun i v -> if i < proto.nregs then regs.(i) <- v) args;
      let frame' =
        {
          regs;
          ip = proto.entry;
          fn_id;
          caller_dst = frame.caller_dst;
        }
      in
      vm.frames <- frame' :: rest
  | [] -> failwith "tail_call: no frame"

let exec_binop op a b =
  match op with
  | Opcode.Op_add -> bin_int ( + ) a b
  | Opcode.Op_sub -> bin_int ( - ) a b
  | Opcode.Op_mul -> bin_int ( * ) a b
  | Opcode.Op_div -> bin_int ( / ) a b
  | Opcode.Op_mod -> bin_int ( mod ) a b
  | Opcode.Op_add_f -> bin_float ( +. ) a b
  | Opcode.Op_sub_f -> bin_float ( -. ) a b
  | Opcode.Op_mul_f -> bin_float ( *. ) a b
  | Opcode.Op_div_f -> bin_float ( /. ) a b
  | Opcode.Op_eq -> Value.Bool (Value.equal a b)
  | Opcode.Op_ne -> Value.Bool (not (Value.equal a b))
  | Opcode.Op_lt -> cmp_int ( < ) a b
  | Opcode.Op_le -> cmp_int ( <= ) a b
  | Opcode.Op_gt -> cmp_int ( > ) a b
  | Opcode.Op_ge -> cmp_int ( >= ) a b
  | Opcode.Op_eq_f -> cmp_float Float.equal a b
  | Opcode.Op_ne_f -> cmp_float (fun x y -> not (Float.equal x y)) a b
  | Opcode.Op_lt_f -> cmp_float ( < ) a b
  | Opcode.Op_le_f -> cmp_float ( <= ) a b
  | Opcode.Op_gt_f -> cmp_float ( > ) a b
  | Opcode.Op_ge_f -> cmp_float ( >= ) a b
  | Opcode.Op_and ->
      Value.Bool (Value.is_truthy a && Value.is_truthy b)
  | Opcode.Op_or ->
      Value.Bool (Value.is_truthy a || Value.is_truthy b)
  | _ -> failwith "exec_binop: not a binop"

let stack_root_arrays vm =
  Array.of_list (List.map (fun (f : frame) -> f.regs) vm.frames)

let set_stack_root_arrays vm arrays =
  List.iteri
    (fun i (f : frame) ->
      if i < Array.length arrays then f.regs <- arrays.(i))
    vm.frames

let install_gc vm =
  let roots : Gc.roots =
    {
      get_stack_roots = (fun () -> stack_root_arrays vm);
      get_globals = (fun () -> vm.globals);
      set_stack_roots = (fun a -> set_stack_root_arrays vm a);
      set_globals = (fun g -> vm.globals <- g);
    }
  in
  Gc.install vm.heap roots

let create ?(heap_capacity = 4096) chunk =
  let vm =
    {
      chunk;
      heap = Heap.create ~capacity:heap_capacity ();
      frames = [];
      globals = Array.make (Array.length chunk.globals) Value.Unit;
      halted = false;
      exit_value = Value.Unit;
    }
  in
  install_gc vm;
  vm

let step vm =
  if vm.halted then ()
  else
    match vm.frames with
    | [] -> vm.halted <- true
    | frame :: _ ->
        if frame.ip < 0 || frame.ip >= Array.length vm.chunk.code then
          failwith (Printf.sprintf "ip %d out of range" frame.ip);
        let instr = vm.chunk.code.(frame.ip) in
        frame.ip <- frame.ip + 1;
        let open Opcode in
        match instr.op with
        | Op_nop | Op_gc_safepoint -> ()
        | Op_load_const ->
            let c = vm.chunk.consts.(instr.b) in
            set_reg frame instr.a (const_to_value vm.heap c)
        | Op_move -> set_reg frame instr.a (reg frame instr.b)
        | Op_load_global ->
            set_reg frame instr.a vm.globals.(instr.b)
        | Op_store_global ->
            vm.globals.(instr.a) <- reg frame instr.b
        | Op_add | Op_sub | Op_mul | Op_div | Op_mod | Op_add_f | Op_sub_f
        | Op_mul_f | Op_div_f | Op_eq | Op_ne | Op_lt | Op_le | Op_gt
        | Op_ge | Op_eq_f | Op_ne_f | Op_lt_f | Op_le_f | Op_gt_f
        | Op_ge_f | Op_and | Op_or ->
            set_reg frame instr.a
              (exec_binop instr.op (reg frame instr.b) (reg frame instr.c))
        | Op_neg ->
            set_reg frame instr.a (Value.Int (-Value.as_int (reg frame instr.b)))
        | Op_neg_f ->
            set_reg frame instr.a
              (Value.Float (-.Value.as_float (reg frame instr.b)))
        | Op_not ->
            set_reg frame instr.a
              (Value.Bool (not (Value.is_truthy (reg frame instr.b))))
        | Op_jump -> frame.ip <- instr.a
        | Op_jump_if ->
            if Value.is_truthy (reg frame instr.a) then frame.ip <- instr.b
        | Op_jump_if_not ->
            if not (Value.is_truthy (reg frame instr.a)) then
              frame.ip <- instr.b
        | Op_switch ->
            let tag = get_tag vm (reg frame instr.a) in
            let found = ref false in
            let i = ref 0 in
            while (not !found) && !i < instr.b do
              let case_tag = instr.extra.(2 * !i) in
              let target = instr.extra.((2 * !i) + 1) in
              if case_tag = tag then (
                frame.ip <- target;
                found := true);
              incr i
            done;
            if not !found then frame.ip <- instr.c
        | Op_call ->
            let args = read_args frame instr.extra in
            call_function vm ~dst:instr.a ~fn_id:instr.b ~args
        | Op_call_closure ->
            let args = read_args frame instr.extra in
            call_closure vm ~dst:instr.a ~clo:(reg frame instr.b) ~args
        | Op_tail_call ->
            let args = read_args frame instr.extra in
            tail_call vm ~fn_id:instr.a ~args
        | Op_tail_call_closure -> (
            let args = read_args frame instr.extra in
            match resolve_heap vm (reg frame instr.a) with
            | Value.Closure (fn_id, env) ->
                tail_call vm ~fn_id ~args:(Array.to_list env @ args)
            | v ->
                failwith
                  ("tail_call_closure: " ^ Value.to_string v))
        | Op_ret -> return_to_caller vm (reg frame instr.a)
        | Op_ret_void -> return_to_caller vm Value.Unit
        | Op_alloc_tuple ->
            let fields =
              Array.map (fun r -> reg frame r) instr.extra
            in
            set_reg frame instr.a (Heap.alloc_tuple vm.heap fields)
        | Op_alloc_adt ->
            let fields =
              Array.map (fun r -> reg frame r) instr.extra
            in
            set_reg frame instr.a
              (Heap.alloc_adt vm.heap instr.b fields)
        | Op_alloc_closure ->
            let env = Array.map (fun r -> reg frame r) instr.extra in
            set_reg frame instr.a
              (Heap.alloc_closure vm.heap instr.b env)
        | Op_get_field | Op_tuple_get ->
            let fields = get_fields vm (reg frame instr.b) in
            set_reg frame instr.a fields.(instr.c)
        | Op_set_field -> (
            match reg frame instr.a with
            | Value.Ptr loc -> (
                match Heap.get vm.heap loc with
                | Heap.Tuple xs | Heap.Adt (_, xs) | Heap.Closure (_, xs)
                  ->
                    xs.(instr.b) <- reg frame instr.c
                | Heap.String _ -> failwith "set_field on string")
            | Value.Tuple xs | Value.Adt (_, xs) | Value.Closure (_, xs) ->
                xs.(instr.b) <- reg frame instr.c
            | v -> failwith ("set_field: " ^ Value.to_string v))
        | Op_get_tag ->
            set_reg frame instr.a
              (Value.Int (get_tag vm (reg frame instr.b)))
        | Op_cons ->
            (* Cons as ADT tag=1 fields=[h;t]; Nil is tag=0 *)
            let fields = [| reg frame instr.b; reg frame instr.c |] in
            set_reg frame instr.a (Heap.alloc_adt vm.heap 1 fields)
        | Op_car ->
            let fields = get_fields vm (reg frame instr.b) in
            set_reg frame instr.a fields.(0)
        | Op_cdr ->
            let fields = get_fields vm (reg frame instr.b) in
            set_reg frame instr.a fields.(1)
        | Op_print ->
            ignore (Builtin.print_any [ resolve_heap vm (reg frame instr.a) ])
        | Op_print_int ->
            ignore (Builtin.print_int [ resolve_heap vm (reg frame instr.a) ])
        | Op_print_string ->
            ignore
              (Builtin.print_string [ resolve_heap vm (reg frame instr.a) ])
        | Op_print_bool ->
            ignore (Builtin.print_bool [ resolve_heap vm (reg frame instr.a) ])
        | Op_halt ->
            let v =
              if instr.b <> 0 then reg frame instr.a else Value.Unit
            in
            vm.halted <- true;
            vm.exit_value <- resolve_heap vm v;
            vm.frames <- []

let run vm =
  try
    let main = Chunk.find_func vm.chunk vm.chunk.main in
    push_frame vm ~fn_id:main.fn_id ~nregs:main.nregs ~entry:main.entry
      ~args:[] ~caller_dst:None;
    while (not vm.halted) && vm.frames <> [] do
      step vm
    done;
    Ok vm.exit_value
  with
  | Failure msg -> Runtime_error msg
  | exn -> Runtime_error (Printexc.to_string exn)

let run_chunk chunk =
  let vm = create chunk in
  run vm
