(** Public Glyph virtual machine API.

    {[
      let chunk = Emit.emit_program mir_program in
      let v = Vm.run_chunk chunk
    ]}
*)

type t = Interp.vm

type error =
  | Runtime of string
  | Load of string

exception Error of error

let pp_error fmt = function
  | Runtime msg -> Format.fprintf fmt "runtime error: %s" msg
  | Load msg -> Format.fprintf fmt "load error: %s" msg

let load (chunk : Chunk.t) : t = Interp.create chunk

let load_file path : t =
  try Interp.create (Chunk.read_file path) with
  | Sys_error msg -> raise (Error (Load msg))
  | Failure msg -> raise (Error (Load msg))

let run (vm : t) : Value.t =
  match Interp.run vm with
  | Interp.Ok v -> v
  | Interp.Runtime_error msg -> raise (Error (Runtime msg))

let run_result vm =
  match Interp.run vm with
  | Interp.Ok v -> Ok v
  | Interp.Runtime_error msg -> Error (Runtime msg)

let run_chunk ?(heap_capacity = 4096) (chunk : Chunk.t) : Value.t =
  let vm = Interp.create ~heap_capacity chunk in
  run vm

let run_chunk_result ?heap_capacity chunk =
  try Ok (run_chunk ?heap_capacity chunk) with Error e -> Error e

let run_program ?heap_capacity (prog : Mir.program) : Value.t =
  run_chunk ?heap_capacity (Emit.emit_program prog)

let disassemble (vm : t) = Disasm.to_string vm.chunk
let chunk (vm : t) = vm.chunk
let heap (vm : t) = vm.heap
let halted (vm : t) = vm.halted
let result (vm : t) = vm.exit_value
let step (vm : t) = Interp.step vm

let force_gc (vm : t) =
  let roots : Gc.roots =
    {
      get_stack_roots =
        (fun () ->
          Array.of_list (List.map (fun (f : Interp.frame) -> f.regs) vm.frames));
      get_globals = (fun () -> vm.globals);
      set_stack_roots =
        (fun arrays ->
          List.iteri
            (fun i (f : Interp.frame) ->
              if i < Array.length arrays then f.regs <- arrays.(i))
            vm.frames);
      set_globals = (fun g -> vm.globals <- g);
    }
  in
  Gc.collect vm.heap roots

let pp fmt (vm : t) =
  Format.fprintf fmt "VM{halted=%b result=%a %s}" vm.halted Value.pp
    vm.exit_value (Heap.stats vm.heap)
